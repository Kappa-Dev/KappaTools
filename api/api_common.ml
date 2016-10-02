(* utility for results *)
let result_data_map
    ~(ok:'ok -> 'a)
    ~(error:'error -> 'a)
  =
  function
  | `Ok o -> ok o
  | `Error e -> error e

(* Helper functions for result *)
let result_ok ?(result_code:Api.manager_code = Api.OK)
    (ok:'ok) : 'ok Api.result =
  { Api_types_j.result_data = `Ok ok ;
    Api_types_j.result_code = result_code }

let result_error_msg
    ?(severity:Api_types_j.severity = `Error)
    ?(result_code:Api.manager_code = Api.ERROR)
    (message:string) : 'ok Api.result =
  { Api_types_j.result_data =
      `Error [{ Api_types_j.message_severity = severity;
                Api_types_j.message_text = message;
                Api_types_j.message_range = None }];
    Api_types_j.result_code = result_code }

let result_map
    ~(ok:'code -> 'ok -> 'a)
    ~(error:'code -> Api_types_j.errors -> 'a)
    ~(result:('ok,'code) Api_types_j.result)
  : 'a =
  match result.Api_types_j.result_data with
  | `Ok data -> ok result.Api_types_j.result_code data
  | `Error data -> error result.Api_types_j.result_code data

let result_bind
    ~(ok:'ok -> ('a_ok,'a_code) Api_types_j.result)
    ~(result:('ok,'code) Api_types_j.result)
  : ('a_ok,'a_code) Api_types_j.result =
  match result.Api_types_j.result_data with
  | `Ok data -> ok data
  | `Error data ->
    { Api_types_j.result_data = `Error data ;
      Api_types_j.result_code = result.Api_types_j.result_code }

let md5sum text = Digest.to_hex (Digest.string text)

(* data structures *)
(* Manager state *)
type simulation = { simulation_id : Api_types_j.simulation_id ;
                    runtime_state : Kappa_facade.t;
                  }
type parse_state = ParseOk of Api_types_j.project_parse
                 | ParseErrors of Api_types_j.errors

type project = { project_id : Api_types_j.project_id ;
                 mutable simulations : simulation list ;
                 mutable files : Api_types_j.file list;
                 mutable parse_state : parse_state ; }

let project_kappa_code project : string =
  String.concat ""
    (List.map
       (fun file -> file.Api_types_j.file_content)
       (List.sort
	  (fun l r -> compare l.Api_types_j.file_metadata.Api_types_j.file_metadata_position
            r.Api_types_j.file_metadata.Api_types_j.file_metadata_position) project.files))
type workspace = { workspace_id : Api_types_j.workspace_id ;
                   mutable projects : project list }
type environment = { mutable workspaces : workspace list ;  }
let environment_new () : environment = { workspaces = [] }

(* functor to deal with collections *)
module type COLLECTION_TYPE = sig
  type id
  type collection
  type item
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
     let m : string = Format.sprintf "id %s not found" (C.id_to_string id) in
     Lwt.return (result_error_msg ~result_code:Api.NOT_FOUND m)

end;;

module WorkspaceCollection : COLLECTION_TYPE
  with type id = Api_types_j.workspace_id
  and type collection = environment
  and type item = workspace
=
struct
  type id = Api_types_j.workspace_id
  type collection = environment
  type item = workspace
  let list
      (environment : environment) =
    environment.workspaces
  let update
      (environment : environment)
      (workspaces : workspace list) =
    environment.workspaces <- workspaces
  let identifier
      (workspace : workspace) =
    workspace.workspace_id
  let id_to_string
      (workspace_id : Api_types_j.workspace_id) : string =
    Format.sprintf "%s" workspace_id
end;;
module WorkspaceOperations = CollectionOperations(WorkspaceCollection)

module ProjectCollection : COLLECTION_TYPE
  with type id = Api_types_j.project_id
  and type collection = workspace
  and type item = project
=
struct
  type id = Api_types_j.project_id
  type collection = workspace
  type item = project
  let list
      (workspace : workspace) =
    workspace.projects
  let update
      (workspace : workspace)
      (projects : project list) : unit =
    workspace.projects <- projects
  let identifier (project : project) =
    project.project_id
  let id_to_string (project_id : Api_types_j.project_id) : string =
    Format.sprintf "%s" project_id
end;;

module ProjectOperations = CollectionOperations(ProjectCollection)

module SimulationCollection : COLLECTION_TYPE
  with type id = Api_types_j.simulation_id
  and type collection = project
  and type item = simulation
=
struct
  type id = Api_types_j.simulation_id
  type collection = project
  type item = simulation
  let list
      (project : project) =
    project.simulations
  let update
      (project : project)
      (simulations : simulation list) : unit =
    project.simulations <- simulations
  let identifier (simulation : simulation) =
    simulation.simulation_id
  let id_to_string (simulation_id : Api_types_j.simulation_id) : string =
    Format.sprintf "%s" simulation_id
end;;

module SimulationOperations = CollectionOperations(SimulationCollection)


let bind_project
    environment
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    handler
    =
  WorkspaceOperations.bind
    workspace_id
    environment
    (fun workspace ->
      ProjectOperations.bind
        project_id
        workspace
        (fun project -> handler workspace project))

let bind_simulation
    environment
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (simulation_id : Api_types_j.simulation_id)
    handler
    =
  bind_project
    environment
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (fun workspace project ->
      SimulationOperations.bind
	simulation_id
	project
	(fun simulation -> handler workspace project simulation))
