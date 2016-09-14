
open Lwt.Infix

let msg_token_not_found =
  "token not found"
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

let () = Printexc.record_backtrace true

let catch_error : 'a . (ApiTypes_j.errors -> 'a) -> exn -> 'a =
  fun handler ->
  (function
    |  ExceptionDefn.Syntax_Error e ->
      handler (Api_data.api_location_errors e)
    | ExceptionDefn.Malformed_Decl e ->
      handler  (Api_data.api_location_errors e)
    | ExceptionDefn.Internal_Error error ->
      handler (Api_data.api_location_errors error)
    | Invalid_argument error ->
      handler (Api_data.api_message_errors ("Runtime error "^ error))
    | exn -> handler (Api_data.api_message_errors (Printexc.to_string exn))
  )


class type api_runtime =
  object
    method parse :
      ApiTypes_j.code ->
      ApiTypes_j.parse ApiTypes_j.result Lwt.t
    method start :
      ApiTypes_j.parameter ->
      ApiTypes_j.token ApiTypes_j.result Lwt.t
    method status :
      ApiTypes_j.token ->
      ApiTypes_j.state ApiTypes_j.result Lwt.t
    method list :
      unit ->
      ApiTypes_j.catalog ApiTypes_j.result Lwt.t
    method stop :
      ApiTypes_j.token ->
      unit ApiTypes_j.result Lwt.t
    method perturbate :
      ApiTypes_j.token ->
      ApiTypes_j.perturbation ->
      unit ApiTypes_j.result Lwt.t
    method pause :
      ApiTypes_j.token ->
      unit ApiTypes_j.result Lwt.t
    method continue :
      ApiTypes_j.token ->
      ApiTypes_j.parameter ->
      unit ApiTypes_j.result Lwt.t
  end;;

module Base : sig

  class virtual base_runtime :
    float -> object
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t
      inherit api_runtime
    end;;
end = struct
  module IntMap = Mods.IntMap
  type simulator_state =
    { mutable is_running : bool
    ; mutable run_finalize : bool
    ; counter : Counter.t
    ; log_buffer : Buffer.t
    ; log_form : Format.formatter
    ; mutable plot : ApiTypes_j.plot
    ; mutable distances : ApiTypes_j.distances
    ; mutable snapshots : ApiTypes_j.snapshot list
    ; mutable flux_maps : ApiTypes_j.flux_map list
    ; mutable files : ApiTypes_j.file_line list
    ; mutable error_messages : ApiTypes_j.errors
    ; contact_map : Primitives.contact_map
    ; env : Environment.t
    ; mutable domain : Connected_component.Env.t
    ; mutable graph : Rule_interpreter.t
    ; mutable state : State_interpreter.t
    }
  type context = { states : simulator_state IntMap.t
                 ; id : int }

  let build_ast
      (code : string)
      yield
      (_ : ?exn:exn -> string -> unit Lwt.t) =
    let lexbuf : Lexing.lexbuf = Lexing.from_string code in
    Lwt.catch
      (fun () ->
         (Lwt.wrap3 KappaParser.start_rule KappaLexer.token
            lexbuf Ast.empty_compil) >>=
         (fun raw_ast ->
            (yield ()) >>=
            (fun () ->
               (Lwt.wrap2 LKappa.compil_of_ast [] raw_ast) >>=
               (fun (sigs,_,_,_ as ast :
                                Signature.s * unit NamedDecls.t * int list *
                                (Ast.agent,
                                 LKappa.rule_agent list,
                                 int,
                                 LKappa.rule) Ast.compil) ->
                 (yield ()) >>=
                 (fun () ->
                    (Lwt.wrap3
                       Eval.init_kasa
                       Remanent_parameters_sig.JS
                       sigs
                       raw_ast) >>=
                    (fun (contact_map,_kasa_state) ->
                       Lwt.return (`Right (ast,contact_map))))))))
      (catch_error (fun e -> Lwt.return (`Left e)))

      let outputs simulation =
        function
        | Data.Flux flux_map ->
          simulation.flux_maps <-
            ((Api_data.api_flux_map flux_map)::simulation.flux_maps)
        | Data.Plot (time,new_observables) ->
          let new_values =
            List.map (fun nbr -> Nbr.to_float nbr)
              (Array.to_list new_observables) in
          simulation.plot <-
            {simulation.plot with
             ApiTypes_j.time_series =
               { ApiTypes_j.observation_time = time ;
                 ApiTypes_j.observation_values = new_values ; }
               :: simulation.plot.ApiTypes_j.time_series }
        | Data.Print file_line ->
          simulation.files <-
            ((Api_data.api_file_line file_line)::simulation.files)
        | Data.Snapshot snapshot ->
          simulation.snapshots <-
            ((Api_data.api_snapshot
                (Environment.signatures simulation.env) snapshot)
             ::simulation.snapshots)
        | Data.UnaryDistances unary_distances ->
          simulation.distances <-
            let (one_big_list,_) =
              Array.fold_left
                (fun (l,i) a ->
                   match a with
                   | Some ls ->
                     let add_rule_id =
                       List.map
                         (fun (t,d) ->
                            {ApiTypes_j.rule_dist =
                               unary_distances.Data.distances_rules.(i);
                             ApiTypes_j.time_dist = t;
                             ApiTypes_j.dist = d}) ls
                     in (List.append l add_rule_id, i+1)
                   | None -> (l, i+1))
                ([],0) unary_distances.Data.distances_data in
            one_big_list
        | Data.Log s -> Format.fprintf simulation.log_form "%s@." s

  class virtual base_runtime min_run_duration =
    object(self)
      val mutable lastyield = Sys.time ()
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t
      val mutable context = { states = IntMap.empty
                            ; id = 0 }
      (* not sure if this is good *)
      val start_time : float = Sys.time ()

      method private time_yield () =
        let t = Sys.time () in
        if t -. lastyield > min_run_duration then
          let () = lastyield <- t in
          self#yield ()
        else Lwt.return_unit

      method parse
          (code : ApiTypes_j.code) : ApiTypes_j.parse ApiTypes_j.result Lwt.t =
        Lwt.bind
          (build_ast code self#time_yield self#log)
          (function
            | `Right ((sigs,_,_,_),contact_map) ->
              Lwt.return
                (`Right
                   { ApiTypes_j.contact_map =
                       Api_data.api_contact_map sigs contact_map })
            | `Left e -> Lwt.return (`Left e))

      method private new_id () : int =
        let result = context.id + 1 in
        let () = context <- { context with id = context.id + 1 } in
        result

      method start
          (parameter : ApiTypes_j.parameter) :
        ApiTypes_j.token ApiTypes_j.result Lwt.t =
        if parameter.ApiTypes_j.nb_plot > 0 then
          let current_id = self#new_id () in
          let simulation_log_buffer = Buffer.create 512 in
          let simulation_log_form =
             Format.formatter_of_buffer simulation_log_buffer in
          Lwt.catch
            (fun () ->
               (build_ast parameter.ApiTypes_j.code self#time_yield self#log) >>=
               (function
                   `Right ((sig_nd,tk_nd,_updated_vars,result),contact_map) ->
                   let simulation_counter =
                     Counter.create
                       ~init_t:(0. : float)
                       ~init_e:(0 : int)
                       ?max_t:parameter.ApiTypes_j.max_time
                       ?max_e:parameter.ApiTypes_j.max_events
                       ~nb_points:(parameter.ApiTypes_j.nb_plot : int) in
                   Eval.compile
                     ~pause:(fun f -> Lwt.bind (self#time_yield ()) f)
                     ~return:Lwt.return ?rescale_init:None
                     ~outputs:(function
                         | Data.Log s ->
                           Format.fprintf simulation_log_form "%s@." s
                         | Data.Snapshot _
                         | Data.Flux _
                         | Data.Plot _
                         | Data.Print _
                         | Data.UnaryDistances _ -> assert false)
                     sig_nd tk_nd contact_map
                     simulation_counter result >>=
                   (fun (env,domain,has_tracking,
                         store_distances,_,init_l) ->
                     let simulation =
                       { is_running = true ;
                         run_finalize = false ;
                         counter = simulation_counter ;
                         log_buffer = simulation_log_buffer ;
                         log_form = simulation_log_form ;
                         plot = { ApiTypes_j.legend = [] ;
                                  ApiTypes_j.time_series = [] ; } ;
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
                       } in
                     let () =
                       context <-
                         { context with
                           states =
                             IntMap.add current_id simulation context.states } in
                     let () =
                       Lwt.async
                          (fun () ->
                             Lwt.catch (fun () ->
                                 let story_compression =
                                   Tools.option_map
                                     (fun  _ ->
                                        ((false,false,false),true))
                                     has_tracking
                                 in
                                 Eval.build_initial_state
                                   ~bind:(fun x f ->
                                       (self#time_yield ()) >>=
                                       (fun () -> x >>= f))
                                   ~return:Lwt.return []
                                   simulation.counter
                                   simulation.env
                                   simulation.domain
                                   story_compression
                                   store_distances init_l >>=
                                 (fun (graph,state) ->
                                    let () = simulation.graph <- graph;
                                      simulation.state <- state in
                                    let log_form =
                                      Format.formatter_of_buffer
                                        simulation.log_buffer
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
                                              ~env:simulation.env))
                                        simulation.env in
                                    let () =
                                      simulation.plot <-
                                        { simulation.plot
                                          with ApiTypes_j.legend =
                                                 Array.to_list legend }
                                    in
                                    self#run simulation
                                 )
                              )
                               (catch_error
                                  (fun e ->
                                     let () = simulation.error_messages <- e in
                                     Lwt.return (`Left e))
                               )
                          ) in
                     Lwt.return (`Right current_id))
                 | `Left _ as out -> Lwt.return out))
            (catch_error (fun e -> Lwt.return (`Left e)))
        else
          Api_data.lwt_msg msg_observables_less_than_zero

      method perturbate token perturbation :
        unit ApiTypes_j.result Lwt.t =
        let lexbuf =
          Lexing.from_string perturbation.ApiTypes_j.perturbation_code
        in
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None ->
               Api_data.lwt_msg msg_token_not_found
             | Some simulation ->
               if simulation.is_running then
                 Api_data.lwt_msg msg_process_not_paused
               else
                 let cc_preenv =
                   Connected_component.PreEnv.of_env simulation.domain in
                 let e',_ =
                   Tools.list_fold_right_map
                     (LKappa.modif_expr_of_ast
                        (Environment.signatures simulation.env)
                        (Environment.tokens_finder simulation.env)
                        (Environment.algs_finder simulation.env))
                        (KappaParser.effect_list KappaLexer.token lexbuf) [] in
                  let cc_preenv', e'' = Eval.compile_modifications_no_track
                      simulation.contact_map cc_preenv e' in
                 if cc_preenv == cc_preenv' then
                   let _,graph',state' =
                     List.fold_left
                       (fun (stop,graph',state' as acc) x ->
                          if stop then acc else
                            State_interpreter.do_modification
                              ~outputs:(outputs simulation) simulation.env
                              simulation.domain simulation.counter graph' state' x)
                       (false,simulation.graph,simulation.state) e'' in
                   let () = simulation.graph <- graph' in
                   let () = simulation.state <- state' in
                   Lwt.return (`Right ())
                 else (* Connected_component.PreEnv.finalize cc_preenv' *)
                   Api_data.lwt_msg
                     "Tracking a new pattern on the fly is impossible (for now?)")
          (catch_error (fun e -> Lwt.return (`Left e)))
      val create_state = fun state ->
        { ApiTypes_j.plot = Some state.plot ;
          ApiTypes_j.distances = Some state.distances ;
          ApiTypes_j.time = Counter.time state.counter ;
          ApiTypes_j.time_percentage = Counter.time_percentage state.counter ;
          ApiTypes_j.event = Counter.event state.counter ;
          ApiTypes_j.event_percentage = Counter.event_percentage state.counter ;
          ApiTypes_j.tracked_events = Counter.tracked_events state.counter ;
          ApiTypes_j.log_messages = [Buffer.contents state.log_buffer] ;
          ApiTypes_j.snapshots = state.snapshots ;
          ApiTypes_j.flux_maps = state.flux_maps ;
          ApiTypes_j.files = state.files ;
          is_running = state.is_running ;
        }
      method status (token : ApiTypes_j.token) :
        ApiTypes_j.state ApiTypes_j.result Lwt.t =
        match IntMap.find_option token context.states with
        | None ->
          Api_data.lwt_msg msg_token_not_found
        | Some state ->
          self#log (string_of_bool state.is_running) >>=
          (fun () ->
             (match state.error_messages with
              | [] ->
                Lwt.catch
                  (fun () ->
                     Lwt.return (`Right (create_state state)))
                  (catch_error (fun e -> Lwt.return (`Left e)))
              | _ -> Lwt.return (`Left state.error_messages))
          )

      method list () : ApiTypes_j.catalog ApiTypes_j.result Lwt.t =
        Lwt.return (`Right (List.map fst (IntMap.bindings context.states)))

      method pause (token : ApiTypes_j.token) :
        unit ApiTypes_j.result Lwt.t =
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None -> Api_data.lwt_msg msg_token_not_found
             | Some state ->
               let () =
                 (if state.is_running then
                    state.is_running <- false
                  else
                    ())
               in
               Lwt.return_unit
               >>= (fun _ -> Lwt.return (`Right ())))
          (catch_error (fun e -> Lwt.return (`Left e)))

      method private run (simulation : simulator_state) :
        unit ApiTypes_j.result Lwt.t =
        let () = Lwt.async (fun () -> self#log "run.1") in
        (self#log "run.2")
        >>=
        (fun () ->
        Lwt.catch
          (fun () ->
             let rstop = ref false in
             let rec iter () =
               let () =
                 while (not !rstop) &&
                       Sys.time () -. lastyield < min_run_duration do
                   let (stop,graph',state') =
                     State_interpreter.a_loop
                       ~outputs:(outputs simulation)
                       simulation.env simulation.domain
                       simulation.counter
                       simulation.graph simulation.state in
                   rstop := stop;
                   simulation.graph <- graph';
                   simulation.state <- state'
                 done in
               if !rstop then
                 let () = Lwt.async (fun () -> self#log "run.3") in
                 let () = simulation.is_running <- false in
                 Lwt.return_unit
              else if simulation.is_running then
                 let () = Lwt.async (fun () -> self#log "run.4") in
                 (let () = lastyield <- Sys.time () in
                  self#yield ()) >>= iter
              else
                let () = Lwt.async (fun () -> self#log "run.5") in
                 Lwt.return_unit
             in
             (iter ()) >>=
             (fun () ->
                let () =
                  if simulation.run_finalize then
                    self#finalize(simulation)
                  else
                    ()
                in
                Lwt.return (`Right ())))
          (catch_error (fun e -> Lwt.return (`Left e)))
        )

      method private finalize(simulation : simulator_state) : unit =
        let _ =
          State_interpreter.end_of_simulation
            ~outputs:(outputs simulation)
            simulation.log_form simulation.env
            simulation.counter simulation.graph
            simulation.state
        in ()
      method continue
          (token : ApiTypes_j.token)
          (parameter : ApiTypes_j.parameter) :
        unit ApiTypes_j.result Lwt.t =
        let () = Lwt.async (fun () -> self#log "continue.1") in
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None ->
               let () = Lwt.async (fun () -> self#log "continue.2") in
               Api_data.lwt_msg msg_token_not_found
             | Some simulation ->
               let () = Lwt.async (fun () -> self#log "continue.3") in
               if simulation.is_running then
                 let () = Lwt.async (fun () -> self#log "continue.4") in
                 Lwt.return (`Right ())
               else
                 let () = Lwt.async (fun () -> self#log "continue.5") in
                 let () = simulation.is_running <- true in
                 let () =
                   Counter.set_max_time
                     simulation.counter
                     parameter.ApiTypes_j.max_time
                 in
                 let () =
                   Counter.set_max_events
                     simulation.counter
                     parameter.ApiTypes_j.max_events
                 in
                 let () = Lwt.async (fun () -> self#log "continue.6") in
                 let () = Lwt.async (fun () -> self#run simulation) in
                 Lwt.return (`Right ())
          )
          (catch_error
             (fun e ->
                let () = Lwt.async (fun () -> self#log "continue.7") in
                Lwt.return (`Left e)))

      method stop (token : ApiTypes_j.token) : unit ApiTypes_j.result Lwt.t =
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None -> Api_data.lwt_msg msg_token_not_found
             | Some simulation ->
               let () = simulation.run_finalize <- true in
               (if simulation.is_running then
                  (self#pause token)
                else
                  let () = self#finalize(simulation) in
                  Lwt.return (`Right ()))
               >>=
               (fun _ ->
                  let () =
                    context <-
                      { context
                        with states = IntMap.remove token context.states }
                  in
                  Lwt.return (`Right ()))
          )
          (catch_error (fun e -> Lwt.return (`Left e)))

      initializer
        Lwt.async (fun () -> self#log "created runtime")
    end;;

end;;
