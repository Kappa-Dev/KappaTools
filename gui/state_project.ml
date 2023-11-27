(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

let plotPeriodParamId = Js.string "kappappPlotPeriod"
let pauseConditionParamId = Js.string "kappappPauseCondition"
let seedParamId = Js.string "kappappDefaultSeed"
let storeTraceParamId = Js.string "kappappStoreTrace"
let showDeadRulesParamId = Js.string "kappappShowDeadRules"
let showDeadAgentsParamId = Js.string "kappappShowDeadAgents"

let showIrreversibleTransitionsParamId =
  Js.string "kappappShowIrreversibleTransition"

type parameters = {
  plot_period: float;
  pause_condition: string;
  seed: int option;
  store_trace: bool;
  show_dead_rules: bool;
  show_dead_agents: bool;
  show_non_weakly_reversible_transitions: bool;
}

type a_project = {
  project_id: string;
  project_is_computing: bool React.S.t;
  project_watcher_cancel: bool ref;
  project_manager: Api.concrete_manager;
}

type state = {
  project_current: a_project option;
  project_catalog: a_project list;
  project_version: int;
  default_parameters: parameters;
  project_parameters: parameters Mods.StringMap.t;
}

type project_model = {
  model_project_id: string;
  model_project_is_computing: bool React.S.t;
}

type model = {
  model_current_id: string option;
  model_catalog: project_model list;
  model_project_version: int;
  model_parameters: parameters;
}

let project_equal a b = a.project_id = b.project_id

let catalog_equal x y =
  try List.for_all2 project_equal x y with Invalid_argument _ -> false

let state_equal a b =
  Option_util.equal project_equal a.project_current b.project_current
  && a.project_version = b.project_version
  && a.default_parameters = b.default_parameters
  && Mods.StringMap.equal
       (fun x y -> compare x y = 0)
       a.project_parameters b.project_parameters
  && catalog_equal a.project_catalog b.project_catalog

let model_equal a b =
  Option_util.equal
    (fun x y -> String.compare x y = 0)
    a.model_current_id b.model_current_id
  && (try
        List.for_all2
          (fun x y -> x.model_project_id = y.model_project_id)
          a.model_catalog b.model_catalog
      with Invalid_argument _ -> false)
  && a.model_project_version = b.model_project_version
  && a.model_parameters = b.model_parameters

let init_default_parameters =
  {
    plot_period = 1.;
    pause_condition = "[false]";
    seed = None;
    store_trace = false;
    show_dead_rules = true;
    show_dead_agents = true;
    show_non_weakly_reversible_transitions = false;
  }

let init_state =
  {
    project_current = None;
    project_catalog = [];
    project_version = -1;
    default_parameters = init_default_parameters;
    project_parameters = Mods.StringMap.empty;
  }

let state, set_state = React.S.create ~eq:state_equal init_state

let update_parameters handler =
  let st = React.S.value state in
  let default_parameters, project_parameters =
    match st.project_current with
    | None -> handler st.default_parameters, st.project_parameters
    | Some proj ->
      ( st.default_parameters,
        Mods.StringMap.add proj.project_id
          (handler
             (Mods.StringMap.find_default st.default_parameters proj.project_id
                st.project_parameters))
          st.project_parameters )
  in
  set_state
    {
      project_current = st.project_current;
      project_catalog = st.project_catalog;
      project_version = st.project_version;
      default_parameters;
      project_parameters;
    }

let set_parameters_as_default () =
  let st = React.S.value state in
  let pa =
    match st.project_current with
    | None -> st.default_parameters
    | Some proj ->
      Mods.StringMap.find_default st.default_parameters proj.project_id
        st.project_parameters
  in
  let () =
    Js.Optdef.iter Dom_html.window##.localStorage (fun ls ->
        let () =
          ls##setItem plotPeriodParamId
            (Js.string (string_of_float pa.plot_period))
        in
        let () =
          ls##setItem pauseConditionParamId (Js.string pa.pause_condition)
        in
        let () =
          match pa.seed with
          | None -> ls##removeItem seedParamId
          | Some va -> ls##setItem seedParamId (Js.string (string_of_int va))
        in
        let () =
          if pa.store_trace then
            ls##setItem storeTraceParamId (Js.string "true")
          else
            ls##removeItem storeTraceParamId
        in
        let () =
          if pa.show_dead_rules then
            ls##setItem showDeadRulesParamId (Js.string "true")
          else
            ls##setItem showDeadRulesParamId (Js.string "false")
        in
        let () =
          if pa.show_dead_agents then
            ls##setItem showDeadAgentsParamId (Js.string "true")
          else
            ls##setItem showDeadAgentsParamId (Js.string "false")
        in
        let () =
          if pa.show_non_weakly_reversible_transitions then
            ls##setItem showIrreversibleTransitionsParamId (Js.string "true")
          else
            ls##removeItem showIrreversibleTransitionsParamId
        in
        ())
  in
  set_state
    {
      project_current = st.project_current;
      project_catalog = st.project_catalog;
      project_version = st.project_version;
      project_parameters = st.project_parameters;
      default_parameters = pa;
    }

let set_plot_period plot_period =
  update_parameters (fun param -> { param with plot_period })

let set_pause_condition pause_condition =
  update_parameters (fun param -> { param with pause_condition })

let set_seed seed = update_parameters (fun param -> { param with seed })

let set_store_trace store_trace =
  update_parameters (fun param -> { param with store_trace })

let set_show_dead_rules show_dead_rules =
  update_parameters (fun param -> { param with show_dead_rules })

let set_show_dead_agents show_dead_agents =
  update_parameters (fun param -> { param with show_dead_agents })

let set_show_non_weakly_reversible_transitions
    show_non_weakly_reversible_transitions =
  update_parameters (fun param ->
      { param with show_non_weakly_reversible_transitions })

let update_state me project_catalog default_parameters project_parameters =
  me.project_manager#project_parse ~patternSharing:Pattern.Compatible_patterns
    []
  >>= fun out ->
  let () =
    set_state
      {
        project_current = Some me;
        project_catalog;
        default_parameters;
        project_parameters;
        project_version = 1;
      }
  in
  Lwt.return out

let computing_watcher manager setter =
  let delay = 1. in
  let cancelled = ref false in
  let rec loop () =
    let () = setter manager#is_computing in
    if !cancelled then
      Lwt.return_unit
    else
      Js_of_ocaml_lwt.Lwt_js.sleep delay >>= loop
  in
  let () = Lwt.async loop in
  cancelled

let add_project is_new project_id : unit Api.result Lwt.t =
  let state_va = React.S.value state in
  (* TODO: Is it ok to get the value like this ? *)
  let catalog = state_va.project_catalog in
  (try
     Lwt.return
       (Result_util.ok
          ( List.find (fun x -> x.project_id = project_id) catalog,
            catalog,
            state_va.project_parameters ))
   with Not_found ->
     State_runtime.create_manager ~is_new project_id
     >>= Api_common.result_bind_lwt ~ok:(fun project_manager ->
             let project_is_computing, set_computes = React.S.create true in
             let project_watcher_cancel =
               computing_watcher project_manager (set_computes ?step:None)
             in
             let me =
               {
                 project_id;
                 project_manager;
                 project_is_computing;
                 project_watcher_cancel;
               }
             in
             let default_parameters = state_va.default_parameters in
             let params =
               Mods.StringMap.add project_id default_parameters
                 state_va.project_parameters
             in
             Lwt.return (Result_util.ok (me, me :: catalog, params))))
  >>= Api_common.result_bind_lwt ~ok:(fun (me, catalog, params) ->
          update_state me catalog state_va.default_parameters params)

let create_project project_id = add_project true project_id
let set_project project_id = add_project false project_id

let dummy_model =
  {
    model_current_id = None;
    model_catalog = [];
    model_project_version = -1;
    model_parameters = init_default_parameters;
  }

let model : model React.signal =
  React.S.map ~eq:model_equal
    (fun state ->
      let model_catalog =
        List.map
          (fun p ->
            {
              model_project_id = p.project_id;
              model_project_is_computing = p.project_is_computing;
            })
          state.project_catalog
      in
      let model_parameters =
        match state.project_current with
        | None -> state.default_parameters
        | Some proj ->
          Mods.StringMap.find_default state.default_parameters proj.project_id
            state.project_parameters
      in
      {
        model_current_id =
          Option_util.map (fun x -> x.project_id) state.project_current;
        model_catalog;
        model_project_version = state.project_version;
        model_parameters;
      })
    state

let sync () : unit Api.result Lwt.t =
  match (React.S.value state).project_current with
  | None -> Lwt.return (Result_util.ok ())
  | Some current ->
    current.project_manager#project_parse
      ~patternSharing:Pattern.Compatible_patterns []
    >>= fun out ->
    let st = React.S.value state in
    let () = set_state { st with project_version = succ st.project_version } in
    Lwt.return out

let remove_files manager =
  manager#file_catalog
  >>= Api_common.result_bind_lwt ~ok:(fun catalog ->
          Lwt_list.iter_p
            (fun m ->
              manager#file_delete m.Kfiles.id >>= fun _ -> Lwt.return_unit)
            catalog
          >>= fun () -> Lwt.return (Result_util.ok ()))

let remove_project project_id =
  let state = React.S.value state in
  try
    let current =
      List.find (fun x -> x.project_id = project_id) state.project_catalog
    in
    remove_files current.project_manager >>= fun out' ->
    let () = current.project_watcher_cancel := true in
    let project_catalog =
      List.filter
        (fun x -> x.project_id <> current.project_id)
        state.project_catalog
    in
    let project_current =
      if
        match state.project_current with
        | None -> false
        | Some v -> v.project_id = current.project_id
      then (
        match project_catalog with
        | [] -> None
        | h :: _ -> Some h
      ) else
        state.project_current
    in
    let () =
      set_state
        {
          project_current;
          project_catalog;
          default_parameters = state.default_parameters;
          project_parameters =
            Mods.StringMap.remove project_id state.project_parameters;
          project_version = -1;
        }
    in
    let () = current.project_manager#terminate in
    sync () >>= fun out'' ->
    Lwt.return (Api_common.result_combine [ out'; out'' ])
  with Not_found ->
    Lwt.return
      (Api_common.result_error_msg
         ("Project " ^ project_id ^ " does not exists"))

let rec init_plot_period (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: t ->
    (try set_plot_period (float_of_string h)
     with Failure _ ->
       let msg = Format.sprintf "failed to parse init_plot_period '%s'" h in
       let () = Common.debug (Js.string msg) in
       init_plot_period t)

let init_pause_condition (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: _ -> set_pause_condition h

let rec init_model_seed (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: t ->
    (try set_plot_period (float_of_string h)
     with Failure _ ->
       let msg = Format.sprintf "failed to parse model_seed '%s'" h in
       let () = Common.debug (Js.string msg) in
       init_model_seed t)

let init_store_trace (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: _ -> set_store_trace (h <> "false")

let init_show_dead_rules (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: _ -> set_show_dead_rules (h <> "false")

let init_show_dead_agents (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: _ -> set_show_dead_agents (h <> "false")

let init_show_non_weakly_reversible_transitions (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h :: _ -> set_show_non_weakly_reversible_transitions (h <> "false")

let init existing_projects : unit Lwt.t =
  let arg_plot_period =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem plotPeriodParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "plot_period"
  in
  let arg_pause_condition =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem pauseConditionParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "pause_condition"
  in
  let arg_model_seed =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem seedParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "model_seed"
  in
  let arg_store_trace =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem storeTraceParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "store_trace"
  in
  let arg_show_dead_rules =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem showDeadRulesParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "show_dead_rules"
  in
  let arg_show_dead_agents =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem showDeadAgentsParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "show_dead_agents"
  in
  let arg_show_irreversible_transitions =
    let default =
      Js.Optdef.case
        Dom_html.window##.localStorage
        (fun () -> [])
        (fun st ->
          Js.Opt.case
            (st##getItem showIrreversibleTransitionsParamId)
            (fun () -> [])
            (fun x -> [ Js.to_string x ]))
    in
    Common_state.url_args ~default "show_non_weakly_reversible_transitions"
  in
  let () = init_plot_period arg_plot_period in
  let () = init_pause_condition arg_pause_condition in
  let () = init_model_seed arg_model_seed in
  let () = init_store_trace arg_store_trace in
  let () = init_show_dead_rules arg_show_dead_rules in
  let () = init_show_dead_agents arg_show_dead_agents in
  let () =
    init_show_non_weakly_reversible_transitions
      arg_show_irreversible_transitions
  in

  let projects = Common_state.url_args ~default:[ "default" ] "project" in
  let rec add_projects projects : unit Lwt.t =
    match projects with
    | [] -> Lwt.return_unit
    | project :: projects ->
      add_project
        (List.for_all (fun x -> x <> project) existing_projects)
        project
      >>= Result_util.fold
            ~ok:(fun () -> add_projects projects)
            ~error:(fun errors ->
              let msg =
                Format.asprintf "creating project %s error @[%a@]" project
                  (Pp.list Pp.space Result_util.print_message)
                  errors
              in
              let () =
                Common.debug
                  (Js.string (Format.sprintf "State_project.init 2 : %s" msg))
              in
              add_projects projects)
  in
  add_projects existing_projects >>= fun () -> add_projects projects

let with_project :
      'a.
      label:string ->
      (Api.concrete_manager -> 'a Api.result Lwt.t) ->
      'a Api.result Lwt.t =
 fun ~label handler ->
  match (React.S.value state).project_current with
  | None ->
    let error_msg : string =
      Format.sprintf "Failed %s due to unavailable project." label
    in
    Lwt.return (Api_common.result_error_msg error_msg)
  | Some current -> handler current.project_manager

let on_project_change_async ?eq ~on ?(others_eq = ( = )) init_others others
    default handler =
  let eq_pair = Mods.pair_equal state_equal others_eq in
  React.S.hold ?eq default
    (Lwt_react.E.map_p
       (fun (st, oth) ->
         match st.project_current with
         | None -> Lwt.return default
         | Some current -> handler current.project_manager oth)
       (React.S.changes
          (React.S.on ~eq:eq_pair on (init_state, init_others)
             (React.S.Pair.pair ~eq:eq_pair state others))))
