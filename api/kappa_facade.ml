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

type file_index =
  { file_index_file_id : Api_types_j.file_id ;
    file_index_line_offset : int ;
    file_index_char_offset : int ;
    file_line_count : int ; }

type kappa_file =
  { kappa_file_id : Api_types_j.file_id ;
    kappa_file_code : string ;
  }

type kappa_code = kappa_file list

let rec count_char c s =
  try
    let sp = String.index s c in
    1 + (count_char c (String.sub s (sp+1) (String.length s - sp - 1)))
  with Not_found -> 0


let project_text (files : Api_types_j.file list) :
  (string * file_index list) =
  let files : Api_types_j.file list =
    (List.stable_sort
       (fun l r ->
          compare
            l.Api_types_j.file_metadata.Api_types_j.file_metadata_position
            r.Api_types_j.file_metadata.Api_types_j.file_metadata_position
       )
       files) in
  List.fold_left
    (fun (buffer,file_indexes) file ->
       match file_indexes with
         [] -> (file.Api_types_j.file_content,
                [{ file_index_file_id = file.Api_types_j.file_metadata.Api_types_j.file_metadata_id ;
                   file_index_line_offset = 0 ;
                   file_index_char_offset = 0 ;
                   file_line_count = count_char '\n' buffer ;
                 }])
       | h::_ -> (buffer^"\n"^file.Api_types_j.file_content ,
                  { file_index_file_id = file.Api_types_j.file_metadata.Api_types_j.file_metadata_id ;
                    file_index_line_offset = h.file_index_line_offset + h.file_line_count ;
                    file_index_char_offset = String.length buffer ;
                    file_line_count = count_char '\n' buffer ;
                  }::file_indexes)
    )
    ("",[])
    files

let rec localize_range (range : Api_types_j.range) =
  function
  | [] -> range
  | h::t ->
    let in_position (p : Api_types_j.position) : bool =
      h.file_index_line_offset <= p.Api_types_j.line &&
      p.Api_types_j.line <= h.file_index_line_offset + h.file_line_count
    in
    let shift_position (p : Api_types_j.position)  =
      { p with Api_types_j.line =
                 p.Api_types_j.line - h.file_index_line_offset }
    in
    if in_position range.Api_types_j.from_position
    || in_position range.Api_types_j.to_position
    then
      { Api_types_j.file = h.file_index_file_id ;
        Api_types_j.from_position = shift_position range.Api_types_j.from_position;
        Api_types_j.to_position = shift_position range.Api_types_j.to_position; }
    else
      localize_range range t

let localize_message (message : Api_types_j.message) (indexes : file_index list) =
  match message.Api_types_j.message_range with
  | None ->
    message
  | Some range ->
    { message with
      Api_types_j.message_range =
        Some (localize_range range indexes) }

let localize_errors (errors : Api_types_j.errors) (indexes : file_index list) =
  List.map (fun message -> localize_message message indexes) errors


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
    contact_map : Signature.contact_map ;
    mutable env : Environment.t ;
    mutable graph : Rule_interpreter.t ;
    mutable state : State_interpreter.t ;
    store_distances : bool ;
    init_l : (Alg_expr.t * Primitives.elementary_rule * Location.t) list ;
    has_tracking : (bool * bool * bool) option ;
    mutable lastyield : float ;
    mutable file_indexes : file_index list option;
  }

let t_errors (t : t) (errors : Api_types_j.errors) =
  match t.file_indexes with
  | None -> errors
  | Some indexes ->  localize_errors errors indexes
let t_range (t : t) (range : Api_types_j.range) =
  match t.file_indexes with
  | None -> range
  | Some indexes ->  localize_range range indexes

let create_t
    ~contact_map
    ~env
    ~graph
    ~state
    ~store_distances
    ~init_l
    ~has_tracking
    ~lastyield
    ~file_indexes
  : t =
  let counter =
    Counter.create
      ~init_t:(0. : float) ~init_e:(0 : int)
      ?max_time:None ?max_event:None ~plot_period:1. in
  let log_buffer = Buffer.create 512 in
  let log_form = Format.formatter_of_buffer log_buffer in
  {
    is_running = true; run_finalize = false; counter; log_buffer; log_form;
    plot = { Api_types_j.plot_legend = [] ;
             Api_types_j.plot_time_series = [] ; } ;
    distances = [];
    snapshots = [];
    flux_maps = [];
    files = [];
    error_messages = [];
    contact_map; env; graph; state; store_distances; init_l; has_tracking;
    lastyield; file_indexes;
  }

let clone_t t =
  create_t
    ~contact_map:t.contact_map
    ~env:t.env
    ~graph:t.graph
    ~state:t.state
    ~store_distances:t.store_distances
    ~init_l:t.init_l
    ~has_tracking:t.has_tracking
    ~lastyield:t.lastyield
    ~file_indexes:t.file_indexes


let catch_error : 'a .
                    (Api_types_j.range -> Api_types_j.range) ->
  (Api_types_j.errors -> 'a) -> exn -> 'a =
  fun f handler ->
    (function
      |  ExceptionDefn.Syntax_Error ((message,location) : string Location.annot) ->
        handler
          (Api_data.api_message_errors
             ~region:(Some (f (Location.to_range location)))
             message)
      | ExceptionDefn.Malformed_Decl ((message,location) : string Location.annot) ->
        handler
          (Api_data.api_message_errors
             ~region:(Some (f (Location.to_range location)))
             message)
      | ExceptionDefn.Internal_Error ((message,location) : string Location.annot) ->
        handler
          (Api_data.api_message_errors
             ~region:(Some (f (Location.to_range location)))
             message)
      | Invalid_argument error ->
        handler (Api_data.api_message_errors ("Runtime error "^ error))
      | exn -> handler (Api_data.api_exception_errors exn))

let build_ast
    (indexes : file_index list)
    (random_state : Random.State.t)
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
                contact_map,
                tk_nd,
                _updated_vars,
                (result :
                   (Ast.agent,
                    LKappa.rule_agent list,
                    int,
                    LKappa.rule) Ast.compil)) ->
               (yield ()) >>=
               (fun () ->
                  (* The last yield is updated after the last yield.
                     It is gotten here for the initial last yeild value. *)
                  let lastyield = Sys.time () in
                  Eval.compile
                    ~pause:(fun f -> Lwt.bind (yield ()) f)
                    ~return:Lwt.return ?rescale_init:None
                    ~outputs:(function
                        | Data.Log s ->
                          Format.fprintf simulation_log_form "%s@." s
                        | Data.Snapshot _
                        | Data.Flux _
                        | Data.Plot _
                        | Data.TraceStep _
                        | Data.Print _
                        | Data.UnaryDistance _ -> assert false)
                    sig_nd tk_nd contact_map result >>=
                  (fun (env,has_tracking,store_distances,_,_,init_l) ->
                     let store_distances = store_distances<>None in
                     let simulation =
                       create_t
                         ~contact_map:contact_map
                         ~env:env
                         ~graph:(Rule_interpreter.empty
                                   ~with_trace:(has_tracking <>None)
                                   ~store_distances random_state env)
                         ~state:(State_interpreter.empty env [] [])
                         ~store_distances ~init_l ~has_tracking ~lastyield
                         ~file_indexes:(Some indexes)
                     in
                     Lwt.return (`Ok simulation)))))))
    (catch_error
       (fun range -> localize_range range indexes)
       (fun e -> Lwt.return (`Error e)))

let prepare_plot_value x =
  Array.fold_right
    (fun nbr acc -> let c = Nbr.to_float nbr in
      match classify_float c with
      | FP_infinite | FP_nan -> None :: acc
      | FP_zero | FP_normal | FP_subnormal -> Some c::acc)
    x []

let outputs (simulation : t) =
  function
  | Data.Flux flux_map ->
    simulation.flux_maps <- flux_map::simulation.flux_maps
  | Data.Plot (time,new_observables) ->
    let new_values = prepare_plot_value new_observables in
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
  | Data.TraceStep _ -> () (*TODO*)

let parse
    ~(system_process : system_process)
    ~(kappa_files : Api_types_t.file list)
  : (t,Api_types_j.errors) Api_types_j.result_data Lwt.t
  =
  let (kappa_code,indexes) = project_text kappa_files in
  Lwt.bind
    (build_ast
       indexes
       (Random.State.make_self_init ())
       kappa_code system_process#yield)
    (function
      | `Ok simulation -> Lwt.return (`Ok simulation)
      | `Error e -> Lwt.return (`Error (localize_errors e indexes)))

let time_yield
    ~(system_process : system_process)
    ~(t : t) : unit Lwt.t =
  let time = Sys.time () in
  if time -. t.lastyield > system_process#min_run_duration () then
    let () = t.lastyield <- time in
    system_process#yield ()
  else Lwt.return_unit

let finalize_simulation ~(t : t) : unit =
  State_interpreter.end_of_simulation
    ~outputs:(outputs t) t.log_form t.env t.counter t.state

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
                 t.env t.counter t.graph t.state in
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
    (catch_error (t_range t) (fun e -> Lwt.return (`Error e)))



let start
    ~(system_process : system_process)
    ~(parameter : Api_types_j.simulation_parameter)
    ~(t : t)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () =
    Lwt.async
      (fun () ->
         Lwt.catch (fun () ->
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
               Counter.set_plot_period
                 t.counter
                 parameter.Api_types_j.simulation_plot_period
             in
             let random_state =
               match parameter.Api_types_j.simulation_seed with
               | None -> Random.State.make_self_init ()
               | Some seed -> Random.State.make [|seed|] in
             Eval.build_initial_state
               ~bind:(fun x f ->
                   (time_yield ~system_process:system_process ~t:t) >>=
                   (fun () -> x >>= f))
               ~return:Lwt.return ~outputs:(outputs t) []
               ~with_trace:(t.has_tracking <> None)
               t.counter
               t.env
               ~store_distances:t.store_distances
               random_state
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
                let first_values = prepare_plot_value first_obs in

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
              (t_range t)
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
    (catch_error (t_range t) (fun e -> Lwt.return (`Error e)))

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
           Pattern.PreEnv.of_env (Environment.domain t.env) in
         let contact_map' = Array.map Array.copy t.contact_map in
         let e',_ =
           Tools.list_fold_right_map
             (LKappa.modif_expr_of_ast
                (Environment.signatures t.env)
                (Environment.tokens_finder t.env)
                (Environment.algs_finder t.env) contact_map')
             (KappaParser.effect_list KappaLexer.token lexbuf) [] in
         if Tools.array_fold_lefti
             (fun n -> Tools.array_fold_lefti
                 (fun s b x -> b || x != t.contact_map.(n).(s)))
             false contact_map' then
           Lwt.fail (ExceptionDefn.Malformed_Decl
                       (Location.dummy_annot "Creating new link type is forbidden"))
         else
           let cc_preenv', e'' = Eval.compile_modifications_no_track
               t.contact_map cc_preenv e' in
           let graph' =
             if cc_preenv == cc_preenv' then t.graph
             else
               let domain' = Pattern.PreEnv.finalize cc_preenv' in
               let () =
                 t.env <- Environment.new_domain domain' t.env in
               List.fold_left
                 (Rule_interpreter.incorporate_extra_pattern domain')
                 t.graph (Primitives.extract_connected_components_modifications e'') in
           let _,graph'',state' =
             List.fold_left
               (fun (stop,graph',state' as acc) x ->
                  if stop then acc else
                    State_interpreter.do_modification
                      ~outputs:(outputs t) t.env t.counter graph' state' x)
               (false,graph',t.state) e'' in
           let () = t.graph <- graph'' in
           let () = t.state <- state' in
           Lwt.return (`Ok ()))
    (catch_error (t_range t) (fun e -> Lwt.return (`Error e)))

let continue
    ~(system_process : system_process)
    ~(t : t)
    ~(parameter : Api_types_j.simulation_parameter)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
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
         let () =
           Lwt.async
             (fun () ->
                run_simulation ~system_process:system_process ~t:t)
         in
         Lwt.return (`Ok ())
    )
    (catch_error
       (t_range t)
       (fun e -> Lwt.return (`Error e)))

let create_info ~(t : t) : Api_types_j.simulation_detail =
  let progress :  Api_types_j.simulation_progress =
    { Api_types_j.simulation_progress_time =
        Counter.time t.counter ;
      Api_types_j.simulation_progress_time_percentage =
        Counter.time_percentage t.counter ;
      Api_types_j.simulation_progress_event =
        Counter.event t.counter ;
      Api_types_j.simulation_progress_event_percentage =
        Counter.event_percentage t.counter ;
      Api_types_j.simulation_progress_tracked_events =
        Counter.tracked_events t.counter ;
      Api_types_j.simulation_progress_is_running =
        t.is_running ;
    } in
  let output : Api_types_j.simulation_detail_output =
    { Api_types_j.simulation_output_plot =
        Some t.plot ;
      Api_types_j.simulation_output_distances =
        Some t.distances ;
      Api_types_j.simulation_output_flux_maps =
        t.flux_maps ;
      Api_types_j.simulation_output_file_lines =
        t.files ;
      Api_types_j.simulation_output_snapshots =
        t.snapshots ;
      Api_types_j.simulation_output_log_messages =
        [Buffer.contents t.log_buffer] ; }
  in
  { Api_types_j.simulation_detail_progress =
      progress ;
    Api_types_j.simulation_detail_output =
      output ; }

let info
    ~(system_process : system_process)
    ~(t : t) :
  (Api_types_j.simulation_detail,Api_types_j.errors)
    Api_types_j.result_data
    Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  match t.error_messages with
  | [] ->
    Lwt.catch
      (fun () ->
         Lwt.return (`Ok (create_info ~t:t)))
      (catch_error (t_range t) (fun e -> Lwt.return (`Error e)))
  | _ -> Lwt.return (`Error t.error_messages)

let get_contact_map (t : t) : Api_types_j.site_node array =
  Api_data.api_contact_map
    (Environment.signatures t.env)
    t.contact_map
