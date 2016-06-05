open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Conduit_lwt_unix
open Lwt_log
open Re

type logger = ?exn:exn -> string -> unit Lwt.t
type context = { arguments : (string * string) list
               ; connection : Cohttp_lwt_unix.Server.conn
               ; request : Cohttp.Request.t
               ; body : Cohttp_lwt_body.t
               ; logger : logger }

let headers =
  let h = Header.init_with "Access-Control-Allow-Origin" "*" in
  let h = Header.add h "Accept" "application/kappa-v2+json" in
  let h = Header.add h "content-type" "application/json" in
  h

let server_respond ~(body : string) =
  Server.respond_string
    ?headers:(Some headers)
    ~status:`OK
    ~body:body
    ()

let result_response
    string_of_success
    result =
  match result with
    `Left errors ->
    let error_msg : string = ApiTypes_j.string_of_errors errors in
        Server.respond_string
          ?headers:(Some headers)
          ~status:`Bad_request
          ~body:error_msg ()
(*
    (log error_msg)
    >>=
      (fun _ ->
        Server.respond_string
          ?headers:(Some headers)
          ~status:`Bad_request
          ~body:error_msg ())
*)
  | `Right success ->
     let success_msg : string = string_of_success success in
     Server.respond_string
       ?headers:(Some headers)
       ~status:`OK
       ~body:success_msg ()


let not_found =
  Server.respond_string
    ?headers:(Some headers)
    ~status:`Not_found
    ~body:""
    ()

type route =
  { path : string ;
    methods : Cohttp.Code.meth list ;
    handler : context:context ->
              (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t }

type url_matcher =
  { re : Re.re; labels : string list }

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
    Re.replace_string
      variable_pattern
      "(\\w)"
      url
  in
  let () = print_string pattern in
  let re =
    Re.compile (Re_perl.re pattern) in
  { re = re; labels = labels }
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
         Some
           (arg,
            (List.combine
               matching
               matcher.labels))
       with Not_found ->
         match_url
           tail
           url)
    | [] -> None

let handler
    (routes : route list)
    (logger : logger)
    :
    context:context -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  let url_matchers : (route * url_matcher ) list =
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
            route.handler context
        | None -> not_found
