val result_ok : ?result_code:Api.manager_code -> 'ok -> 'ok Api.result
val error_msg :
  ?severity:Api_types_t.severity -> string -> Api_types_t.message
val result_error_msg :
  ?severity:Api_types_t.severity ->
  ?result_code:Api.manager_code -> string -> 'ok Api.result
val result_messages :
  ?result_code:Api.manager_code -> Api_types_t.errors -> 'ok Api.result
val result_error_exception :
  ?severity:Api_types_t.severity ->
  ?result_code:Api.manager_code -> exn -> 'ok Api.result

val method_handler_errors :
  ?severity:Api_types_t.severity ->
  Exception_without_parameter.method_handler -> Api_types_t.errors
val method_handler_messages :
  ?severity:Api_types_t.severity -> ?result_code:Api.manager_code ->
  Exception_without_parameter.method_handler -> 'a Api.result
val result_kasa :
  ('a, Exception_without_parameter.method_handler) Result.result ->
  'a Api.result

val result_map :
  ok:('code -> 'ok -> 'a) ->
  error:('code -> Api_types_t.errors -> 'a) ->
  ('ok, 'code) Api_types_t.result -> 'a
val result_bind :
  ok:('ok -> ('a_ok, 'a_code) Api_types_t.result) ->
  ('ok, 'a_code) Api_types_t.result -> ('a_ok, 'a_code) Api_types_t.result
val result_bind_lwt :
  ok:('ok -> ('a_ok, 'a_code) Api_types_t.result Lwt.t) ->
  ('ok, 'a_code) Api_types_t.result ->
  ('a_ok, 'a_code) Api_types_t.result Lwt.t
val result_fold_lwt :
  f:(('a_ok, 'a_code) Api_types_t.result ->
     'value -> ('a_ok, 'a_code) Api_types_t.result Lwt.t) ->
  id:('a_ok, 'a_code) Api_types_t.result ->
  'value list -> ('a_ok, 'a_code) Api_types_t.result Lwt.t
val result_combine : unit Api.result list -> unit Api.result
val result_lift : ('a,string) Result.result -> 'a Api.result

val md5sum : string -> string
