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
    ?(result_code:Api.manager_code = `ERROR)
    (message:string) : 'ok Api.result =
  { Api_types_t.result_data =
      Result.Error [{ Api_types_t.message_severity = severity;
                Api_types_t.message_text = message;
                Api_types_t.message_range = None }];
    Api_types_t.result_code = result_code }

let result_messages
    ?(result_code:Api.manager_code = `ERROR)
    (messages : Api_types_t.errors) : 'ok Api.result =
  { Api_types_t.result_data = Result.Error messages ;
    Api_types_t.result_code = result_code }

let result_error_exception
    ?(severity:Api_types_t.severity = `Error)
    ?(result_code:Api.manager_code = `ERROR)
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
     Lwt.return (result_error_msg ~result_code:`NOT_FOUND m)

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
    Lwt.return (result_error_msg ~result_code:`NOT_FOUND m)

let bind_file project (file_id : Api_types_t.file_id) handler =
  FileOperations.bind file_id project (fun file -> handler file)
