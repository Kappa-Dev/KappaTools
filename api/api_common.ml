open Lwt.Infix

(* utility for results *)
let result_data_map
    ~(ok:'ok -> 'a)
    ~(error:'error -> 'a)
  =
  function
  | `Ok o -> ok o
  | `Error e -> error e

(* Helper functions for result *)
let result_ok ?(result_code:Api.manager_code = `OK)
    (ok:'ok) : 'ok Api.result =
  { Api_types_j.result_data = `Ok ok ;
    Api_types_j.result_code = result_code }
let error_msg
    ?(severity:Api_types_j.severity = `Error)
    (message:string) : Api_types_j.message =
  { Api_types_j.message_severity = severity;
    Api_types_j.message_text = message;
    Api_types_j.message_range = None }
let result_error_msg
    ?(severity:Api_types_j.severity = `Error)
    ?(result_code:Api.manager_code = `ERROR)
    (message:string) : 'ok Api.result =
  { Api_types_j.result_data =
      `Error [{ Api_types_j.message_severity = severity;
                Api_types_j.message_text = message;
                Api_types_j.message_range = None }];
    Api_types_j.result_code = result_code }

let result_messages
    ?(result_code:Api.manager_code = `ERROR)
    (messages : Api_types_j.errors) : 'ok Api.result =
  { Api_types_j.result_data = `Error messages ;
    Api_types_j.result_code = result_code }

let result_error_exception
    ?(severity:Api_types_j.severity = `Error)
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
  error:('code -> Api_types_j.errors -> 'a) ->
  ('ok, 'code) Api_types_j.result -> 'a =
  fun
    ~(ok:'code -> 'ok -> 'a)
    ~(error:'code -> Api_types_j.errors -> 'a)
    (result:('ok,'code) Api_types_j.result)
  ->  ((match result.Api_types_j.result_data with
      | `Ok data -> ok result.Api_types_j.result_code data
      | `Error data -> error result.Api_types_j.result_code data) : 'a)

let result_bind :
  ok:('ok -> ('a_ok, 'a_code) Api_types_j.result) ->
  ('ok, 'a_code) Api_types_j.result ->
  ('a_ok, 'a_code) Api_types_j.result =
  fun
    ~(ok:'ok -> ('a_ok,'a_code) Api_types_j.result)
    (result:('ok,'code) Api_types_j.result) ->
    ((match result.Api_types_j.result_data with
        | `Ok data -> ok data
        | `Error data ->
          { Api_types_j.result_data = `Error data ;
            Api_types_j.result_code = result.Api_types_j.result_code }) :
       ('a_ok,'a_code) Api_types_j.result)

let result_bind_lwt :
  ok:('ok -> ('a_ok, 'a_code) Api_types_j.result Lwt.t) ->
  ('ok, 'a_code) Api_types_j.result ->
  ('a_ok, 'a_code) Api_types_j.result Lwt.t =
  fun
    ~(ok:'ok -> ('a_ok,'a_code) Api_types_j.result Lwt.t)
    (result:('ok,'code) Api_types_j.result) ->
  (match result.Api_types_j.result_data with
  | `Ok data -> ok data
  | `Error data ->
    Lwt.return
      { Api_types_j.result_data = `Error data ;
        Api_types_j.result_code = result.Api_types_j.result_code }
    : ('a_ok,'a_code) Api_types_j.result Lwt.t)

let rec result_fold_lwt :
  f:(('ok, 'a_code) Api_types_j.result ->
     'value ->
     ('ok, 'a_code) Api_types_j.result Lwt.t) ->
  id:('ok, 'a_code) Api_types_j.result ->
  ('value list) ->
  ('a_ok, 'a_code) Api_types_j.result Lwt.t =
  fun
    ~(f : (('ok, 'a_code) Api_types_j.result ->
           'value ->
           ('ok, 'a_code) Api_types_j.result Lwt.t))
    ~(id : ('ok, 'a_code) Api_types_j.result)
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
      ~error:(fun _ (data_1 : Api_types_j.errors) ->
          result_map
            ~ok:(fun _ _-> result_ok ())
            ~error:(fun result_code (data_r : Api_types_j.errors) ->
                { Api_types_j.result_data = `Error (data_1@data_r);
                  Api_types_j.result_code = result_code }
              )
            r
        )
      l


let md5sum text = Digest.to_hex (Digest.string text)

let project_kappa_code project : string =
  String.concat ""
    (List.map
       (fun file -> file.Api_types_j.file_content)
       (List.sort
          (fun l r -> compare l.Api_types_j.file_metadata.Api_types_j.file_metadata_position
              r.Api_types_j.file_metadata.Api_types_j.file_metadata_position) project#get_files))

(* functor to deal with collections *)
module type COLLECTION_TYPE = sig
  type id
  type collection
  type item
  val label : string
  val list : collection -> item list
  val update : collection -> item list -> unit
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

module ProjectCollection : COLLECTION_TYPE
  with type id = Api_types_j.project_id
  and type collection = Api_environment.environment
  and type item = Api_environment.project
=
struct
  type id = Api_types_j.project_id
  type collection = Api_environment.environment
  type item = Api_environment.project
  let label : string = "project"
  let list
      (workspace : Api_environment.environment) =
    workspace#get_projects ()
  let update
      (workspace : Api_environment.environment)
      (projects : Api_environment.project list) : unit =
    workspace#set_projects projects
  let identifier (project : Api_environment.project) =
    project#get_project_id ()
  let id_to_string (project_id : Api_types_j.project_id) : string =
    Format.sprintf "%s" project_id
end;;

module ProjectOperations = CollectionOperations(ProjectCollection)

module SimulationCollection : COLLECTION_TYPE
  with type id = Api_types_j.simulation_id
  and type collection = Api_environment.project
  and type item = Api_environment.simulation
=
struct
  type id = Api_types_j.simulation_id
  type collection = Api_environment.project
  type item = Api_environment.simulation
  let label : string = "simulation"
  let list
      (project : Api_environment.project) =
    project#get_simulations ()
  let update
      (project : Api_environment.project)
      (simulations : Api_environment.simulation list) : unit =
    project#set_simulations simulations
  let identifier (simulation : Api_environment.simulation) =
    simulation#get_simulation_id ()
  let id_to_string (simulation_id : Api_types_j.simulation_id) : string =
    Format.sprintf "%s" simulation_id
end;;

module SimulationOperations = CollectionOperations(SimulationCollection)

module FileCollection : COLLECTION_TYPE
  with type id = Api_types_j.file_id
  and type collection = Api_environment.project
  and type item = Api_types_j.file
=
struct
  type id = Api_types_j.file_id
  type collection = Api_environment.project
  type item = Api_types_j.file
  let label : string = "file"
  let list
      (project : Api_environment.project) =
    project#get_files ()
  let update
      (project : Api_environment.project)
      (files : Api_types_j.file list) : unit =
    (* WARNING : DONT CALL THIS IT WILL SKEW THE VERSIONING
       SEE : file_manager.update_file
    *)
    ignore(project#set_files files)
  let identifier (file : Api_types_j.file) =
    file.Api_types_j.file_metadata.Api_types_j.file_metadata_id
  let id_to_string (file_id : Api_types_j.file_id) : string =
    Format.sprintf "%s" file_id
end;;

module FileOperations = CollectionOperations(FileCollection)

let bind_simulation
    environment
    (project_id : Api_types_j.project_id)
    (simulation_id : Api_types_j.simulation_id)
    handler
    =
    ProjectOperations.bind
      (project_id : Api_types_j.project_id)
      environment
      (fun project -> SimulationOperations.bind simulation_id
          project
          (fun simulation -> handler project simulation))

let bind_file
    environment
    (project_id : Api_types_j.project_id)
    (file_id : Api_types_j.file_id)
    handler
    =
    ProjectOperations.bind
      (project_id : Api_types_j.project_id)
      environment
      (fun project -> FileOperations.bind file_id
          project
          (fun file -> handler project file))
