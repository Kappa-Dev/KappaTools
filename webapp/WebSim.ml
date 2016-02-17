module Runtime = Api.Base
module ApiTypes = ApiTypes_j

open Lwt
open Cohttp_lwt_unix
open Cohttp
open Request
open Api
open Runtime
open ApiTypes


let runtime_state = new Runtime.runtime (Lwt_main.yield)
let headers =
  let h = Header.init_with "Access-Control-Allow-Origin" "*" in
  let h = Header.add h "content-type" "application/json" in
  h

let server_respond (body : string) =
  Server.respond_string ?headers:(Some headers) ~status:`OK ~body:body ()

let get_token path : int option =
  let process_url_pattern = (Str.regexp "^/v1/process/\\([0-9]+\\)") in
  if Str.string_match process_url_pattern path 0 then
    Some (int_of_string (Str.matched_group 1 path))
  else
    None

let handler (conn : Cohttp_lwt_unix.Server.conn)
            (request : Cohttp.Request.t)
            (body : Cohttp_lwt_body.t) : (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t =
  let uri = Request.uri request in
  let bad_request = Server.respond_string ?headers:(Some headers) ~status:`Bad_request ~body:"" () in
  match Uri.path uri with
  (* GET /version *)
  | "/v1/version" ->
     server_respond (ApiTypes.string_of_version { build = Version.version_msg ;
                                                  version = "v1" })
  (* GET /parse *)
  | "/v1/parse" ->
     Uri.get_query_param uri "code" |>
       (fun code -> match code with
                      None ->
                      Server.respond_string
                        `Unprocessable_entity
                        "missing code"
                        ()
                    | Some code ->
                       (runtime_state#parse code)
                       >>=
                         (fun error -> let error_msg : string = ApiTypes.string_of_error error in
                                       server_respond error_msg)
       )(* GET /process *)
  | "/v1/process" when request.meth = `GET ->
     (runtime_state#list ())
     >>=
       (fun catalog -> let catalog_msg : string = ApiTypes.string_of_result
                                                    ApiTypes.write_catalog
                                                    catalog
                       in server_respond catalog_msg
       )
  (* POST /process *)
  | "/v1/process" when request.meth = `POST ->
     (Cohttp_lwt_body.to_string body)
     >>= (fun body -> Lwt.return (ApiTypes.parameter_of_string body))
     >>= (fun (parameter : ApiTypes.parameter) -> runtime_state#start parameter)
     >>= (fun token -> let token_msg : string = ApiTypes.string_of_result
                                                  ApiTypes.write_token
                                                  token
                       in server_respond token_msg)
  (* DELETE /v1/process/[token] *)
  | x when request.meth = `DELETE && None != get_token x ->
     (match get_token x with
        None -> bad_request
      | Some token ->
         (runtime_state#stop token)
         >>=
           (fun unit -> let unit_msg : string = ApiTypes.string_of_result
                                                  ApiTypes.write_alias_unit
                                                  unit
            in server_respond unit_msg
           )
     )
  (* GET /v1/process/[token] *)
  | x when request.meth = `GET ->
     (match get_token x with
        None -> bad_request
      | Some token ->
         (runtime_state#status token)
         >>=
           (fun status -> let status_msg : string = ApiTypes.string_of_result
                                                      ApiTypes.write_state
                                                      status
            in server_respond status_msg
           )
     )
  | _ -> bad_request

let server =
  Server.create (Server.make handler ())

let () = ignore (Lwt_main.run server)
