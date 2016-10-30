open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Conduit_lwt_unix
open Lwt_log
open Re

type context = { arguments : (string * string) list
               ; connection : Cohttp_lwt_unix.Server.conn
               ; request : Cohttp.Request.t
               ; body : Cohttp_lwt_body.t }
(* Default headers for http requests note support has been
   added for CORS headers.
*)
let headers =
  let h = Header.init_with "Access-Control-Allow-Origin" "*" in
  let h = Header.add h "content-type" "application/json" in
  h

let status_of_code : Api.manager_code -> Cohttp.Code.status_code =
  function
    `OK -> `OK
  | `CREATED -> `Created
  | `ERROR -> `Bad_request
  | `CONFLICT -> `Conflict
  | `NOT_FOUND ->`Not_found
  | `ACCEPTED -> `Accepted

(* Given a result and a way to serialize a success result, return
   to client the the http response.
*)
let result_response
    ~(string_of_success: 'ok -> string)
  : 'ok Api.result -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  Api_common.result_map
    ~ok:(fun (code : Api.manager_code)
          (ok : 'a) ->
          let body : string = string_of_success ok in
          let status = status_of_code code in
             Server.respond_string
               ?headers:(Some headers)
               ~status:status
               ~body:body
                   ())
    ~error:(fun
             (code : Api.manager_code)
             (errors : Api_types_j.errors) ->
             let error_msg : string = Api_types_j.string_of_errors errors in
             let status = status_of_code code in
             (Lwt_log_core.log ~level:Lwt_log_core.Error error_msg)
             >>= (fun _ ->
          Server.respond_string
            ?headers:(Some headers)
            ~status:status
            ~body:error_msg
            ()))

let string_response
    ?(headers = headers)
    ?(code :int = 200)
    ~(body : string)
  : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  Server.respond_string
    ?headers:(Some headers)
    ~status:(Code.status_of_code code)
    ~body:body
    ()

let not_found : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  string_response
    ?headers:None
    ~code:(Code.code_of_status `Not_found)
    ~body:""

type 'a route =
  { path : string ;
    methods : Cohttp.Code.meth list ;
    operation : 'a }

type route_handler =
  (context:context ->
   (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t)
    route

type route_filter =
  (context:context ->
   chain:(context:context -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t) ->
   (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t)
    route

type url_matcher =
  { re : Re.re;
    labels : string list ;
    route : string }

let variable_pattern = Re.compile (Re_perl.re "\\{\\w+\\}")
let label_pattern = Re.compile (Re_perl.re "\\w+")

let create_url_matcher (url : string) : url_matcher =
  let labels =
    Re.matches variable_pattern url in
  let labels =
    List.flatten
      (List.map
         (Re.matches label_pattern)
         labels)
  in
  let pattern =
    Format.sprintf
      "^%s$"
      (Re.replace_string
         ?pos:None
         ?len:None
         ?all:None
         variable_pattern
         (* ~by:"(\\w)" *)
         ~by:"([^\\/]+)"
         url
      )
  in
  let () =
    async
      (fun () ->
         Lwt_log_core.log
           ~level:Lwt_log_core.Debug
           (Format.sprintf " + route : %s" pattern))
  in
  let re =
    Re.compile (Re_perl.re pattern) in
  { re = re;
    labels = labels ;
    route = url ; }
;;

let rec match_url
    (url_matchers : ('a * url_matcher) list)
    (url : string)
  : ('a * (string * string) list) option =
  match url_matchers with
  | (arg,matcher)::tail ->
    (try
       let matching =
         List.tl
           (Array.to_list
              (Re.Group.all
                 (Re.exec matcher.re url)))
       in
       let get_parameters : (string * string) list = List.combine matcher.labels matching in
       let text_parameters : string = String.concat " , " (List.map (fun (key,value) -> key^" : "^value) get_parameters) in
       let () =
         async
           (fun () ->
              Lwt_log_core.log
                ~level:Lwt_log_core.Debug
                (Format.sprintf "match_url :\n+ url : '%s'\n+ route: '%s'\n+ args: { %s }" url matcher.route text_parameters))
       in
       Some (arg,get_parameters)
     with Not_found -> match_url tail url)
  | [] -> None

let route_handler
    (routes : route_handler list) :
  context:context -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  let url_matchers : (route_handler * url_matcher ) list =
    List.map
      (fun route ->
         (route,create_url_matcher route.path))
      routes
  in
  fun ~(context:context) ->
    let url : string =
      Uri.path
        (Request.uri context.request)
    in
    match
      let () =
        async
          (fun () ->
             Lwt_log_core.log
               ~level:Lwt_log_core.Debug
               (Format.sprintf "match_url + url : %s" url))
      in
      match_url
        (List.filter
           (* only select handlers where the method matches *)
           (fun (route,_) ->
              List.mem
                context.request.meth
                route.methods)
           url_matchers)
        url
    with
    | Some (route,arguments) ->
      (*
      let () =
        async
          (fun () -> (Cohttp_lwt_body.to_string context.body)
           >>=
           (fun body ->
             Lwt_log_core.log
               ~level:Lwt_log_core.Debug
               (Format.sprintf "+ body : '%s'\n" body)))
      in
         *)
      let context = { context with arguments = arguments } in
      if context.request.meth = `OPTIONS then
        let h =
          Header.init_with
            "Access-Control-Allow-Origin"
            "*"
        in
        let h =
          Header.add
            h
            "Access-Control-Allow-Methods"
            (String.concat
               " , "
               (List.map Code.string_of_method route.methods))
        in
        let h =
          Header.add
            h
            "Access-Control-Request-Headers" "X-Custom-Header"
        in
        Server.respond_string
          ?headers:(Some h)
          ~status:`OK
          ~body:""
          ()
      else
        route.operation ~context
    | None -> not_found
