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
  Api_environment.project ->
  (Api_environment.simulation -> 'a Api.result Lwt.t) ->
  'a Api.result Lwt.t
val bind_file :
  Api_environment.project ->
  Api_types_j.file_id ->
  (FileOperations.item -> 'a Api.result Lwt.t) ->
  'a Api.result Lwt.t
