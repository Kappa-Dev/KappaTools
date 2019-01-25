(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* functor to deal with collections *)
module type COLLECTION_TYPE = sig
  type id
  type collection
  type item
  val label : string
  val list : collection -> item list
  val identifier : item -> id
  val id_to_string : id -> string
end

module type COLLECTION_OPERATIONS = sig
  type id
  type collection
  type item
  val refs : id -> item -> bool
  val exists : id -> collection -> bool
  val filter : id -> collection -> item list
  val bind :
    id ->
    collection ->
    (item -> 'a Api.result Lwt.t) ->
    'a Api.result Lwt.t
end

module CollectionOperations (C : COLLECTION_TYPE) : COLLECTION_OPERATIONS
  with type id = C.id
  and type collection = C.collection
  and type item = C.item =
struct
  type id = C.id
  type collection = C.collection
  type item = C.item

  let refs (id : id) (item : item) : bool =
    id = C.identifier item

  let exists (id : id) (collection : collection) : bool =
    List.exists (fun item -> refs id item) (C.list collection)

  let filter (id : id) (collection : collection) : item list =
  List.filter (fun item -> refs id item) (C.list collection)

  let bind
      (id : id)
      (collection : collection)
      (operation : item -> 'a Api.result Lwt.t)
    : 'a Api.result Lwt.t =
  match filter id collection with
  | item::_ -> operation item
  | [] ->
     let m : string = Format.sprintf "%s : %s id not found" (C.id_to_string id) C.label in
     Lwt.return (Api_common.result_error_msg ~result_code:`Not_found m)

end;;

module FileCollection : COLLECTION_TYPE
  with type id = Api_types_t.file_id
  and type collection = Api_environment.project
  and type item = Api_types_t.file
=
struct
  type id = Api_types_t.file_id
  type collection = Api_environment.project
  type item = Api_types_t.file
  let label : string = "file"
  let list
      (project : Api_environment.project) =
    project#get_files ()
  let identifier (file : Api_types_t.file) =
    file.Api_types_t.file_metadata.Api_types_t.file_metadata_id
  let id_to_string (file_id : Api_types_t.file_id) : string =
    Format.sprintf "%s" file_id
end;;

module FileOperations = CollectionOperations(FileCollection)

let bind_simulation project handler =
  match project#get_simulation () with
  | Some simulation -> handler simulation
  | None ->
    let m  = "No simulation available" in
    Lwt.return (Api_common.result_error_msg ~result_code:`Not_found m)

let bind_file project (file_id : Api_types_t.file_id) handler =
  FileOperations.bind file_id project (fun file -> handler file)
