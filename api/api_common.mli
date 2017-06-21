val result_ok : ?result_code:Api.manager_code -> 'ok -> 'ok Api.result
val error_msg :
  ?severity:Api_types_j.severity -> string -> Api_types_j.message
val result_error_msg :
  ?severity:Api_types_j.severity ->
  ?result_code:Api.manager_code -> string -> 'ok Api.result
val result_messages :
  ?result_code:Api.manager_code -> Api_types_j.errors -> 'ok Api.result
val result_error_exception :
  ?severity:Api_types_j.severity ->
  ?result_code:Api.manager_code -> exn -> 'ok Api.result
val result_map :
  ok:('code -> 'ok -> 'a) ->
  error:('code -> Api_types_j.errors -> 'a) ->
  ('ok, 'code) Api_types_j.result -> 'a
val result_bind :
  ok:('ok -> ('a_ok, 'a_code) Api_types_j.result) ->
  ('ok, 'a_code) Api_types_j.result -> ('a_ok, 'a_code) Api_types_j.result
val result_bind_lwt :
  ok:('ok -> ('a_ok, 'a_code) Api_types_j.result Lwt.t) ->
  ('ok, 'a_code) Api_types_j.result ->
  ('a_ok, 'a_code) Api_types_j.result Lwt.t
val result_fold_lwt :
  f:(('a_ok, 'a_code) Api_types_j.result ->
     'value -> ('a_ok, 'a_code) Api_types_j.result Lwt.t) ->
  id:('a_ok, 'a_code) Api_types_j.result ->
  'value list -> ('a_ok, 'a_code) Api_types_j.result Lwt.t
val result_combine : unit Api.result list -> unit Api.result
val result_lift : ('a,string) Result.result -> 'a Api.result

val md5sum : string -> string
