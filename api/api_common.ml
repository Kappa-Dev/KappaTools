open Lwt.Infix

(* Helper functions for result *)
let result_ok ?(result_code:Api.manager_code = `OK)
    (ok:'ok) : 'ok Api.result =
  { Api_types_t.result_data = Result.Ok ok ;
    Api_types_t.result_code = result_code }
let error_msg
    ?(severity:Api_types_t.severity = `Error)
    (message:string) : Api_types_t.message =
  { Api_types_t.message_severity = severity;
    Api_types_t.message_text = message;
    Api_types_t.message_range = None }
let result_error_msg
    ?(severity:Api_types_t.severity = `Error)
    ?(result_code:Api.manager_code = `Bad_request)
    (message:string) : 'ok Api.result =
  { Api_types_t.result_data =
      Result.Error [{ Api_types_t.message_severity = severity;
                Api_types_t.message_text = message;
                Api_types_t.message_range = None }];
    Api_types_t.result_code = result_code }

let result_messages
    ?(result_code:Api.manager_code = `Bad_request)
    (messages : Api_types_t.errors) : 'ok Api.result =
  { Api_types_t.result_data = Result.Error messages ;
    Api_types_t.result_code = result_code }

let result_error_exception
    ?(severity:Api_types_t.severity = `Error)
    ?(result_code:Api.manager_code = `Bad_request)
    (e : exn) : 'ok Api.result =
  let message = (try  (Printexc.to_string e)
                 with _ -> "unspecified exception thrown")
  in result_error_msg
    ~severity:severity
    ~result_code:result_code
    message
let result_map :
  ok:('code -> 'ok -> 'a) ->
  error:('code -> Api_types_t.errors -> 'a) ->
  ('ok, 'code) Api_types_t.result -> 'a =
  fun
    ~(ok:'code -> 'ok -> 'a)
    ~(error:'code -> Api_types_t.errors -> 'a)
    (result:('ok,'code) Api_types_t.result)
  ->  ((match result.Api_types_t.result_data with
      | Result.Ok data -> ok result.Api_types_t.result_code data
      | Result.Error data -> error result.Api_types_t.result_code data) : 'a)

let result_bind :
  ok:('ok -> ('a_ok, 'a_code) Api_types_t.result) ->
  ('ok, 'a_code) Api_types_t.result ->
  ('a_ok, 'a_code) Api_types_t.result =
  fun
    ~(ok:'ok -> ('a_ok,'a_code) Api_types_t.result)
    (result:('ok,'code) Api_types_t.result) ->
    ((match result.Api_types_t.result_data with
        | Result.Ok data -> ok data
        | Result.Error data ->
          { Api_types_t.result_data = Result.Error data ;
            Api_types_t.result_code = result.Api_types_t.result_code }) :
       ('a_ok,'a_code) Api_types_t.result)

let result_bind_lwt :
  ok:('ok -> ('a_ok, 'a_code) Api_types_t.result Lwt.t) ->
  ('ok, 'a_code) Api_types_t.result ->
  ('a_ok, 'a_code) Api_types_t.result Lwt.t =
  fun
    ~(ok:'ok -> ('a_ok,'a_code) Api_types_t.result Lwt.t)
    (result:('ok,'code) Api_types_t.result) ->
  (match result.Api_types_t.result_data with
  | Result.Ok data -> ok data
  | Result.Error data ->
    Lwt.return
      { Api_types_t.result_data = Result.Error data ;
        Api_types_t.result_code = result.Api_types_t.result_code }
    : ('a_ok,'a_code) Api_types_t.result Lwt.t)

let rec result_fold_lwt :
  f:(('ok, 'a_code) Api_types_t.result ->
     'value ->
     ('ok, 'a_code) Api_types_t.result Lwt.t) ->
  id:('ok, 'a_code) Api_types_t.result ->
  ('value list) ->
  ('a_ok, 'a_code) Api_types_t.result Lwt.t =
  fun
    ~(f : (('ok, 'a_code) Api_types_t.result ->
           'value ->
           ('ok, 'a_code) Api_types_t.result Lwt.t))
    ~(id : ('ok, 'a_code) Api_types_t.result)
    (l : ('value list)) ->
    match l with
    | [] -> Lwt.return id
    | h::t ->
      (f id h)>>=
      (fun result -> result_fold_lwt ~f:f ~id:result t)

let rec result_combine : unit Api.result list -> unit Api.result =
  function
  | [] -> result_ok ()
  | l::t ->
    let r = result_combine t in
    result_map
      ~ok:(fun _ _-> r)
      ~error:(fun _ (data_1 : Api_types_t.errors) ->
          result_map
            ~ok:(fun _ _-> l)
            ~error:(fun result_code (data_r : Api_types_t.errors) ->
                { Api_types_t.result_data = Result.Error (data_1@data_r);
                  Api_types_t.result_code = result_code }
              )
            r
        )
      l

let result_lift = function
  | Result.Ok o -> result_ok o
  | Result.Error e -> result_error_msg e

let md5sum text = Digest.to_hex (Digest.string text)
