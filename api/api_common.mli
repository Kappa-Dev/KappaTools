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
module type COLLECTION_TYPE =
sig
  type id
  type collection
  type item
  val label : string
  val list : collection -> item list
  val identifier : item -> id
  val id_to_string : id -> string
end
module type COLLECTION_OPERATIONS =
sig
  type id
  type collection
  type item
  val refs : id -> item -> bool
  val exists : id -> collection -> bool
  val filter : id -> collection -> item list
  val bind :
    id ->
    collection -> (item -> 'a Api.result Lwt.t) -> 'a Api.result Lwt.t
end
module CollectionOperations :
  functor (C : COLLECTION_TYPE) ->
  sig
    type id = C.id
    type collection = C.collection
    type item = C.item
    val refs : id -> item -> bool
    val exists : id -> collection -> bool
    val filter : id -> collection -> item list
    val bind :
      id ->
      collection -> (item -> 'a Api.result Lwt.t) -> 'a Api.result Lwt.t
  end
module FileCollection :
sig
  type id = Api_types_j.file_id
  type collection = Api_environment.project
  type item = Api_types_j.file
  val label : string
  val list : collection -> item list
  val identifier : item -> id
  val id_to_string : id -> string
end
module FileOperations :
sig
  type id = FileCollection.id
  type collection = FileCollection.collection
  type item = FileCollection.item
  val refs : id -> item -> bool
  val exists : id -> collection -> bool
  val filter : id -> collection -> item list
  val bind :
    id ->
    collection -> (item -> 'a Api.result Lwt.t) -> 'a Api.result Lwt.t
end
val bind_simulation :
  Api_types_j.project_id ->
  Api_environment.project ->
  (Api_environment.simulation -> 'a Api.result Lwt.t) ->
  'a Api.result Lwt.t
val bind_file :
  Api_environment.project ->
  Api_types_j.file_id ->
  (FileOperations.item -> 'a Api.result Lwt.t) ->
  'a Api.result Lwt.t
