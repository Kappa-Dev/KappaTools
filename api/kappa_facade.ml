open Lwt
(** Interface to kappa runtime *)
(* Error messages *)
let msg_process_not_running =
  "process not running"
let msg_process_already_paused =
  "process already paused"
let msg_process_not_paused =
  "process not paused"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"
let msg_missing_perturbation_context =
  "Invalid runtime state missing missing perturbation context"

(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process =
  object
    method log : ?exn:exn -> string -> unit Lwt.t
    method yield : unit -> unit Lwt.t
    method min_run_duration : unit -> float
  end

(** Trivial implementation primarily for unit testing. *)
class null_process : system_process =
  object
    method log ?exn (_ : string) =
      let () = ignore(exn) in
      Lwt.return_unit
    method yield () = Lwt.return_unit
    method min_run_duration() = 0.0
  end;;

(** State of the running simulation. *)
type t =
  { mutable is_running : bool ;
    mutable run_finalize : bool ;
    counter : Counter.t ;
    log_buffer : Buffer.t ;
    log_form : Format.formatter ;
    mutable plot : Api_types_j.plot ;
    mutable distances : Api_types_j.distances ;
    mutable snapshots : Api_types_j.snapshot list ;
    mutable flux_maps : Api_types_j.flux_map list ;
    mutable files : Api_types_j.file_line list ;
    mutable error_messages : Api_types_j.errors ;
    contact_map : Primitives.contact_map ;
    env : Environment.t ;
    mutable domain : Connected_component.Env.t ;
    mutable graph : Rule_interpreter.t ;
    mutable state : State_interpreter.t ;
    store_distances : bool ;
    init_l : (Alg_expr.t * Primitives.elementary_rule * Location.t) list ;
    has_tracking : (bool * bool * bool) option ;
    mutable lastyield : float ;
  }

let catch_error : 'a . (Api_types_j.errors -> 'a) -> exn -> 'a =
  fun handler ->
    (function
      |  ExceptionDefn.Syntax_Error ((message,location) : string Location.annot) ->
        handler
          (Api_data.api_message_errors
             ~region:(Some (Location.to_range location))
             message)
      | ExceptionDefn.Malformed_Decl ((message,location) : string Location.annot) ->
        handler
          (Api_data.api_message_errors
             ~region:(Some (Location.to_range location))
             message)
      | ExceptionDefn.Internal_Error ((message,location) : string Location.annot) ->
        handler
          (Api_data.api_message_errors
             ~region:(Some (Location.to_range location))
             message)
      | Invalid_argument error ->
        handler (Api_data.api_message_errors ("Runtime error "^ error))
      | exn -> handler (Api_data.api_exception_errors exn))

let build_ast
    (code : string)
    (yield : unit -> unit Lwt.t) =
  let lexbuf : Lexing.lexbuf = Lexing.from_string code in
  let simulation_log_buffer = Buffer.create 512 in
  let simulation_log_form =
    Format.formatter_of_buffer simulation_log_buffer in
  Lwt.catch
    (fun () ->
       (Lwt.wrap3 KappaParser.start_rule KappaLexer.token
          lexbuf Ast.empty_compil) >>=
       (fun raw_ast ->
          (yield ()) >>=
          (fun () ->
             (Lwt.wrap2 LKappa.compil_of_ast [] raw_ast) >>=
             (fun
               (sig_nd,
                tk_nd,
                _updated_vars,
                result :
                  Signature.s * unit NamedDecls.t * int list *
                  (Ast.agent,
                   LKappa.rule_agent list,
                   int,
                   LKappa.rule) Ast.compil) ->
               (yield ()) >>=
               (fun () ->
                  (* The last yield is updated after the last yield.
                     It is gotten here for the initial last yeild value. *)
                  let lastyield = Sys.time () in
                  (Lwt.wrap3
                     Eval.init_kasa
                     Remanent_parameters_sig.JS
                     sig_nd
                     raw_ast) >>=
                  (fun (contact_map,_kasa_state) ->
                     Eval.compile
                       ~pause:(fun f -> Lwt.bind (yield ()) f)
                       ~return:Lwt.return ?rescale_init:None
                       ~outputs:(function
                           | Data.Log s ->
                             Format.fprintf simulation_log_form "%s@." s
                           | Data.Snapshot _
                           | Data.Flux _
                           | Data.Plot _
                           | Data.Print _
                           | Data.UnaryDistance _ -> assert false)
                       sig_nd tk_nd contact_map result >>=
                     (fun (env,domain,has_tracking,
                           store_distances,_,init_l) ->
                       let store_distances = store_distances<>None in
                       let simulation_counter =
                         Counter.create
                           ~init_t:(0. : float) ~init_e:(0 : int)
                           ?max_t:None ?max_e:None ~nb_points:200 in
                       let simulation =
                         { is_running = true ;
                           run_finalize = false ;
                           counter = simulation_counter ;
                           log_buffer = simulation_log_buffer ;
                           log_form = simulation_log_form ;
                           plot = { Api_types_j.plot_legend = [] ;
                                    Api_types_j.plot_time_series = [] ; } ;
                           distances = [] ;
                           error_messages = [] ;
                           snapshots = [] ;
                           flux_maps = [] ;
                           files = [] ;
                           contact_map = contact_map ;
                           env = env ;
                           domain = domain ;
                           graph = Rule_interpreter.empty ~store_distances env ;
                           state = State_interpreter.empty env [] [] ;
                           store_distances;
                           has_tracking;
                           init_l;
                           lastyield;
                         } in
                       Lwt.return (`Ok simulation))))))))
    (catch_error (fun e -> Lwt.return (`Error e)))

let outputs (simulation : t) =
  function
  | Data.Flux flux_map ->
    simulation.flux_maps <- flux_map::simulation.flux_maps
  | Data.Plot (time,new_observables) ->
    let new_values =
      List.map (fun nbr -> Nbr.to_float nbr)
        (Array.to_list new_observables) in
    simulation.plot <-
      {simulation.plot with
       Api_types_j.plot_time_series =
         { Api_types_j.observable_time = time ;
           Api_types_j.observable_values = new_values ; }
         :: simulation.plot.Api_types_j.plot_time_series }
  | Data.Print file_line ->
    simulation.files <- file_line::simulation.files
  | Data.Snapshot snapshot ->
    simulation.snapshots <-
      (Api_data.label_snapshot
         (Environment.signatures simulation.env)
         snapshot)::simulation.snapshots
  | Data.UnaryDistance d -> simulation.distances <-
      {Api_types_j.distance_rule =
         Format.asprintf
           "%a" (Environment.print_ast_rule ~env:simulation.env)
           d.Data.distance_rule;
       Api_types_j.distance_time = d.Data.distance_time;
       Api_types_j.distance_length = d.Data.distance_length}
      :: simulation.distances
  | Data.Log s -> Format.fprintf simulation.log_form "%s@." s

let parse
    ~(system_process : system_process)
    ~(kappa_code : string)
  : (t,Api_types_j.errors) Api_types_j.result_data Lwt.t
  = Lwt.bind
    (build_ast kappa_code system_process#yield)
    (function
      | `Ok simulation -> Lwt.return (`Ok simulation)
      | `Error e -> Lwt.return (`Error e))

let time_yield
    ~(system_process : system_process)
    ~(t : t) : unit Lwt.t =
        let time = Sys.time () in
        if time -. t.lastyield > system_process#min_run_duration () then
          let () = t.lastyield <- time in
          system_process#yield ()
        else Lwt.return_unit

let finalize_simulation
    ~(t : t) : unit =
        let _ =
          State_interpreter.end_of_simulation
            ~outputs:(outputs t)
            t.log_form t.env
            t.counter t.graph
            t.state
        in ()

let run_simulation
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  Lwt.catch
    (fun () ->
       let rstop = ref false in
       let rec iter () =
         let () =
           while (not !rstop) &&
                 Sys.time () -. t.lastyield < system_process#min_run_duration ()
           do
             let (stop,graph',state') =
                     State_interpreter.a_loop
                       ~outputs:(outputs t)
                       t.env t.domain
                       t.counter
                       t.graph t.state in
                   rstop := stop;
                   t.graph <- graph';
                   t.state <- state'
                 done in
               if !rstop then
                 let () = t.is_running <- false in
                 Lwt.return_unit
              else if t.is_running then
                (system_process#yield ()) >>= (fun () ->
                    let () = t.lastyield <- Sys.time () in iter ())
              else
                Lwt.return_unit
             in
             (iter ()) >>=
             (fun () ->
                let () =
                  if t.run_finalize then
                    finalize_simulation ~t:t
                  else
                    ()
                in
                Lwt.return (`Ok ())))
    (catch_error (fun e -> Lwt.return (`Error e)))



let start
    ~(system_process : system_process)
    ~(parameter : Api_types_j.simulation_parameter)
    ~(t : t)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () =
    Lwt.async
      (fun () ->
         Lwt.catch (fun () ->
             let story_compression =
               Tools.option_map
                 (fun  _ ->
                    ((false,false,false),true))
                 t.has_tracking in
             let () =
               Counter.set_max_time
                 t.counter
                 parameter.Api_types_j.simulation_max_time
             in
             let () =
               Counter.set_max_events
                 t.counter
                 parameter.Api_types_j.simulation_max_events
             in
             let () =
               Counter.set_nb_points
                 t.counter
                 parameter.Api_types_j.simulation_nb_plot
             in
             Eval.build_initial_state
               ~bind:(fun x f ->
                   (time_yield ~system_process:system_process ~t:t) >>=
                   (fun () -> x >>= f))
               ~return:Lwt.return []
               t.counter
               t.env
               t.domain
               story_compression
               ~store_distances:t.store_distances
               t.init_l >>=
             (fun (graph,state) ->
                let () = t.graph <- graph;
                  t.state <- state in
                let log_form =
                  Format.formatter_of_buffer
                    t.log_buffer
                in
                let () =
                  ExceptionDefn.flush_warning
                    log_form
                in
                let legend =
                  Environment.map_observables
                    (Format.asprintf
                       "%a"
                       (Kappa_printer.alg_expr
                          ~env:t.env))
                    t.env in
                let first_obs =
                  State_interpreter.observables_values
                    t.env t.counter graph state in
                let first_values =
                  List.map (fun nbr -> Nbr.to_float nbr)
                    (Array.to_list first_obs) in

                let () =
                  t.plot <-
                    { Api_types_j.plot_legend =
                        Array.to_list legend;
                      Api_types_j.plot_time_series =
                        [
                          { Api_types_j.observable_time =
                              Counter.current_time t.counter;
                            Api_types_j.observable_values =
                              first_values;
                          }
                        ]} in
                run_simulation ~system_process:system_process ~t:t
             )
           )
           (catch_error
              (fun e ->
                 let () = t.error_messages <- e in
                 Lwt.return (`Error e))
           )) in
  Lwt.return (`Ok ())

let pause
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  let () = if t.is_running then
      t.is_running <- false
    else
      ()
  in
  Lwt.return (`Ok ())

let stop
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
        Lwt.catch
          (fun () ->
             let () = t.run_finalize <- true in
               (if t.is_running then
                  (pause ~system_process:system_process ~t:t)
                else
                  let () = finalize_simulation ~t:t in
                  Lwt.return (`Ok ()))
          )
          (catch_error (fun e -> Lwt.return (`Error e)))

let perturbation
    ~(system_process : system_process)
    ~(t : t)
    ~(perturbation:Api_types_j.simulation_perturbation)
  : (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  let lexbuf =
    Lexing.from_string perturbation.Api_types_j.perturbation_code
  in
  Lwt.catch
    (fun () ->
       if t.is_running then
         Lwt.return (`Error (Api_data.api_message_errors msg_process_not_paused))
       else
         let cc_preenv =
           Connected_component.PreEnv.of_env t.domain in
         let e',_ =
           Tools.list_fold_right_map
             (LKappa.modif_expr_of_ast
                (Environment.signatures t.env)
                (Environment.tokens_finder t.env)
                (Environment.algs_finder t.env))
             (KappaParser.effect_list KappaLexer.token lexbuf) [] in
         let cc_preenv', e'' = Eval.compile_modifications_no_track
             t.contact_map cc_preenv e' in
         let graph' =
           if cc_preenv == cc_preenv' then t.graph
           else
             let () =
               t.domain <-
                 Connected_component.PreEnv.finalize cc_preenv' in
             List.fold_left
               Rule_interpreter.incorporate_extra_pattern
               t.graph
               (Primitives.extract_connected_components_modifications e'') in
         let _,graph'',state' =
           List.fold_left
             (fun (stop,graph',state' as acc) x ->
                if stop then acc else
                  State_interpreter.do_modification
                    ~outputs:(outputs t) t.env
                    t.domain t.counter graph' state' x)
             (false,graph',t.state) e'' in
         let () = t.graph <- graph'' in
         let () = t.state <- state' in
         Lwt.return (`Ok ()))
    (catch_error (fun e -> Lwt.return (`Error e)))

let continue
    ~(system_process : system_process)
    ~(t : t)
    ~(parameter : Api_types_j.simulation_parameter)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  Lwt.catch
    (fun () ->
       if t.is_running then
         Lwt.return (`Ok ())
       else
         let () = t.is_running <- true in
         let () =
           Counter.set_max_time
             t.counter
             parameter.Api_types_j.simulation_max_time
         in
         let () =
           Counter.set_max_events
             t.counter
             parameter.Api_types_j.simulation_max_events
         in
         Lwt.return (`Ok ())
    )
    (catch_error
       (fun e -> Lwt.return (`Error e)))

let create_info ~(t : t) =
  { Api_types_j.simulation_plot = Some t.plot ;
    Api_types_j.simulation_distances = Some t.distances ;
    Api_types_j.simulation_time = Counter.time t.counter ;
    Api_types_j.simulation_time_percentage = Counter.time_percentage t.counter ;
    Api_types_j.simulation_event = Counter.event t.counter ;
    Api_types_j.simulation_event_percentage = Counter.event_percentage t.counter ;
    Api_types_j.simulation_tracked_events = Counter.tracked_events t.counter ;
    Api_types_j.simulation_log_messages = [Buffer.contents t.log_buffer] ;
    Api_types_j.simulation_snapshots = t.snapshots ;
    Api_types_j.simulation_flux_maps = t.flux_maps ;
    Api_types_j.simulation_files = t.files ;
    Api_types_j.simulation_is_running = t.is_running ;
  }

let info
    ~(system_process : system_process)
    ~(t : t) :
  (Api_types_j.simulation_info,Api_types_j.errors)
    Api_types_j.result_data
    Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
         Lwt.return (`Ok (create_info ~t:t)))
      (catch_error (fun e -> Lwt.return (`Error e)))
  | _ -> Lwt.return (`Error t.error_messages)

let get_contact_map (t : t) : Api_types_j.site_node array =
  Api_data.api_contact_map
    (Environment.signatures t.env)
    t.contact_map
