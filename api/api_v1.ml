open Lwt.Infix
module ApiTypes = ApiTypes_j

let msg_process_not_running =
  "process not running"
let msg_token_not_found =
  "token not found"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"

let () = Printexc.record_backtrace true

type runtime =
  < parse : ApiTypes.code -> ApiTypes.parse ApiTypes.result Lwt.t;
    start : ApiTypes.parameter -> ApiTypes.token ApiTypes.result Lwt.t;
    status : ApiTypes.token -> ApiTypes.state ApiTypes.result Lwt.t;
    list : unit -> ApiTypes.catalog ApiTypes.result Lwt.t;
    stop : ApiTypes.token -> unit ApiTypes.result Lwt.t;
  >;;

module Base : sig

  class virtual runtime :
    float -> object
      method parse : ApiTypes.code -> ApiTypes.parse ApiTypes.result Lwt.t
      method start : ApiTypes.parameter -> ApiTypes.token ApiTypes.result Lwt.t
      method status : ApiTypes.token -> ApiTypes.state ApiTypes.result Lwt.t
      method list : unit -> ApiTypes.catalog ApiTypes.result Lwt.t
      method stop : ApiTypes.token -> unit ApiTypes.result Lwt.t
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t
    end;;
end = struct
  module IntMap = Mods.IntMap
  type simulator_state =
    { switch : Lwt_switch.t
    ; counter : Counter.t
    ; log_buffer : Buffer.t
    ; log_form : Format.formatter
    ; mutable plot : ApiTypes.plot
    ; mutable distances : ApiTypes.distances
    ; mutable snapshots : ApiTypes.snapshot list
    ; mutable flux_maps : ApiTypes.flux_map list
    ; mutable files : ApiTypes.file_line list
    ; mutable error_messages : ApiTypes.errors
    ; contact_map : Primitives.contact_map
    ; env : Environment.t
    ; mutable domain : Connected_component.Env.t
    ; mutable graph : Rule_interpreter.t
    ; mutable state : State_interpreter.t
    }
  type context = { states : simulator_state IntMap.t
                 ; id : int }

  let format_error_message (message,linenumber) =
    Format.sprintf "Error at %s : %s"
      (Location.to_string linenumber)
      message
  let build_ast
      (code : string)
      yield
      (log : ?exn:exn -> string -> unit Lwt.t) =
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
      (function
          ExceptionDefn.Syntax_Error e ->
          Lwt.return
            (`Left
               (Api_data.api_location_errors e))
        | ExceptionDefn.Malformed_Decl e ->
          Lwt.return
            (`Left
               (Api_data.api_location_errors e))
        | exn -> log ~exn "" >>=
          (fun () -> Lwt.fail exn))

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
             ApiTypes.time_series =
               { ApiTypes.time = time ; values = new_values }
               :: simulation.plot.ApiTypes.time_series }
        | Data.Print file_line ->
          simulation.files <-
            ((Api_data.api_file_line file_line)::simulation.files)
        | Data.Snapshot snapshot ->
          simulation.snapshots <-
            ((Api_data.api_snapshot (Environment.signatures simulation.env) snapshot)
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
                            {ApiTypes.rule_dist =
                               unary_distances.Data.distances_rules.(i);
                             ApiTypes.time_dist = t;
                             ApiTypes.dist = d}) ls
                     in (List.append l add_rule_id, i+1)
                   | None -> (l, i+1))
                ([],0) unary_distances.Data.distances_data in
            one_big_list
        | Data.Log s -> Format.fprintf simulation.log_form "%s@." s

  class virtual runtime min_run_duration =
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
          (code : ApiTypes.code) : ApiTypes.parse ApiTypes.result Lwt.t =
        Lwt.bind
          (build_ast code self#time_yield self#log)
          (function
            | `Right ((sigs,_,_,_),contact_map) ->
              Lwt.return
                (`Right
                   { ApiTypes.contact_map =
                       Api_data.api_contact_map sigs contact_map })
            | `Left e -> Lwt.return (`Left e))

      method private new_id () : int =
        let result = context.id + 1 in
        let () = context <- { context with id = context.id + 1 } in
        result

      method start
          (parameter : ApiTypes.parameter) :
        ApiTypes.token ApiTypes.result Lwt.t =
        if parameter.ApiTypes.nb_plot > 0 then
          let current_id = self#new_id () in
          let simulation_log_buffer = Buffer.create 512 in
          let simulation_log_form =
             Format.formatter_of_buffer simulation_log_buffer in
          Lwt.catch
            (fun () ->
               (build_ast parameter.ApiTypes.code self#time_yield self#log) >>=
               (function
                   `Right ((sig_nd,tk_nd,updated_vars,result),contact_map) ->
                   let simulation_counter =
                     Counter.create
                       ~init_t:(0. : float)
                       ~init_e:(0 : int)
                       ?max_t:parameter.ApiTypes.max_time
                       ?max_e:parameter.ApiTypes.max_events
                       ~nb_points:(parameter.ApiTypes.nb_plot : int) in
                   Eval.compile
                     ~pause:(fun f -> Lwt.bind (self#time_yield ()) f)
                     ~return:Lwt.return ?rescale_init:None
                     ~outputs:(function
                         | Data.Log s ->
                           Format.fprintf simulation_log_form "%s@." s
                         | Data.Snapshot _ | Data.Flux _ | Data.Plot _ |
                           Data.Print _ | Data.UnaryDistances _ -> assert false)
                     sig_nd tk_nd contact_map
                     simulation_counter result >>=
                   (fun (env,domain,has_tracking,
                         store_distances,_,init_l) ->
                     let env' =
                       Environment.propagate_constant
                         updated_vars simulation_counter env in
                     let simulation =
                       { switch = Lwt_switch.create ()
                       ; counter = simulation_counter
                       ; log_buffer = simulation_log_buffer
                       ; log_form = simulation_log_form
                       ; plot =
                           { ApiTypes.legend = []; ApiTypes.time_series = [] }
                       ; distances = []
                       ; error_messages = []
                       ; snapshots = []
                       ; flux_maps = []
                       ; files = []
                       ; contact_map = contact_map
                       ; env = env'
                       ; domain = domain
                       ; graph = Rule_interpreter.empty ~store_distances env'
                       ; state = State_interpreter.empty env' [] []
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
                                     (fun  _ -> ((false,false,false),true))
                                     has_tracking
                                 in
                                 Eval.build_initial_state
                                   ~bind:(fun x f ->
                                       (self#time_yield ()) >>= (fun () -> x >>= f))
                                   ~return:Lwt.return [] simulation.counter
                                   simulation.env simulation.domain story_compression
                                   store_distances init_l >>=
                                 (fun (graph,state) ->
                                    let () = simulation.graph <- graph;
                                      simulation.state <- state in
                                    let log_form =
                                      Format.formatter_of_buffer simulation.log_buffer in
                                    let () = ExceptionDefn.flush_warning log_form in
                                    let legend =
                                      Environment.map_observables
                                        (Format.asprintf
                                           "%a"
                                           (Kappa_printer.alg_expr ~env:simulation.env))
                                        simulation.env in
                                    let () =
                                      simulation.plot <-
                                        { simulation.plot
                                          with ApiTypes.legend = Array.to_list legend}
                                    in
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
                                        (Lwt_switch.turn_off simulation.switch)
                                        >>= (fun () -> Lwt.return_unit)
                                      else
                                      if Lwt_switch.is_on simulation.switch
                                      then (let () = lastyield <- Sys.time () in
                                            self#yield ()) >>= iter
                                      else Lwt.return_unit in
                                    (iter ()) >>=
                                    (fun () ->
                                       let _ =
                                         State_interpreter.end_of_simulation
                                           ~outputs:(outputs simulation)
                                           simulation.log_form simulation.env
                                           simulation.counter simulation.graph
                                           simulation.state in
                                       Lwt.return_unit)))
                               (function
                                 | ExceptionDefn.Internal_Error error as exn ->
                                   let () = simulation.error_messages <-
                                       (Api_data.api_location_errors error)
                                   in
                                   self#log ~exn ""
                                 | Invalid_argument error as exn ->
                                   let () = simulation.error_messages <-
                                       (Api_data.api_message_errors ("Runtime error "^ error))
                                   in
                                   self#log ~exn ""
                                 | exn ->
                                   let () = simulation.error_messages <-
                                       (Api_data.api_message_errors (Printexc.to_string exn)) in
                                   self#log ~exn ""
                               )) in
                     Lwt.return (`Right current_id))
                 | `Left _ as out -> Lwt.return out))
            (function
              | ExceptionDefn.Malformed_Decl error ->
                Lwt.return (`Left (Api_data.api_location_errors error))
              | ExceptionDefn.Internal_Error error ->
                Lwt.return (`Left (Api_data.api_location_errors error))
              | Invalid_argument error ->
                Lwt.return (`Left (Api_data.api_message_errors ("Runtime error "^ error)))
              | exn ->
                Lwt.return (`Left (Api_data.api_message_errors (Printexc.to_string exn))))
        else
          Api_data.lwt_msg msg_observables_less_than_zero

       method private perturbate token text =
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None ->
               Api_data.lwt_msg msg_token_not_found
             | Some simulation ->
               if Lwt_switch.is_on simulation.switch then
                 failwith "BAAAAD" (*TODO*)
               else
                 let cc_preenv =
                   Connected_component.PreEnv.of_env simulation.domain in
                 match LKappa.modif_expr_of_ast
                         (Environment.signatures simulation.env)
                         (Environment.tokens_finder simulation.env)
                         (Environment.algs_finder simulation.env)
                         (KappaParser.effect KappaLexer.token text) [] with
                 | _, _::_ ->
                    Api_data.lwt_msg "$UPDATE is not implemented (yet?)"
                | e', [] ->
                  let cc_preenv', e'' = Eval.compile_modification_no_update
                      simulation.contact_map cc_preenv e' in
                  let cc_env' =
                    if cc_preenv == cc_preenv' then simulation.domain
                    else Connected_component.PreEnv.finalize cc_preenv' in
                  simulation.domain <- cc_env';
                  let _,graph',state' =
                    List.fold_left
                      (fun (stop,graph',state' as acc) x ->
                         if stop then acc else
                           State_interpreter.do_modification
                             ~outputs:(outputs simulation) simulation.env cc_env'
                             simulation.counter graph' state' x)
                      (false,simulation.graph,simulation.state) e'' in
                  simulation.graph <- graph';
                  simulation.state <- state';
                  Lwt.return (`Right ()))
          (function
              ExceptionDefn.Syntax_Error e ->
              Lwt.return
                (`Left
                   (Api_data.api_location_errors e))
            | ExceptionDefn.Malformed_Decl e ->
              Lwt.return
                (`Left
                   (Api_data.api_location_errors e))
            | exn ->
              (self#log ~exn "")
              >>=
              (fun _ -> Api_data.lwt_msg (Printexc.to_string exn))
          )

      method status
          (token : ApiTypes.token) : ApiTypes.state ApiTypes.result Lwt.t =
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None ->
               Api_data.lwt_msg msg_token_not_found
             | Some state ->
               Lwt.return
                 (match state.error_messages with
                    [] ->
                    `Right
                      ({ ApiTypes.plot =
                           Some state.plot;
                         ApiTypes.distances =
                           Some state.distances;
                         ApiTypes.time =
                           Counter.time state.counter;
                         ApiTypes.time_percentage =
                           Counter.time_percentage state.counter;
                         ApiTypes.event =
                           Counter.event state.counter;
                         ApiTypes.event_percentage =
                           Counter.event_percentage state.counter;
                         ApiTypes.tracked_events =
                           Counter.tracked_events state.counter;
                         ApiTypes.log_messages =
                           [Buffer.contents state.log_buffer] ;
                         ApiTypes.snapshots =
                           state.snapshots;
                         ApiTypes.flux_maps =
                           state.flux_maps;
                         ApiTypes.files =
                           state.files;
                         is_running =
                           Lwt_switch.is_on state.switch
                       } : ApiTypes.state )
                  | _ ->
                    `Left state.error_messages
                 )
          )
          (fun exn ->
             (self#log ~exn "")
             >>=
             (fun _ -> Api_data.lwt_msg (Printexc.to_string exn))
          )
      method list () : ApiTypes.catalog ApiTypes.result Lwt.t =
        Lwt.return (`Right (List.map fst (IntMap.bindings context.states)))

      method stop (token : ApiTypes.token) : unit ApiTypes.result Lwt.t =
        Lwt.catch
          (fun () ->
             match IntMap.find_option token context.states with
             | None -> Api_data.lwt_msg msg_token_not_found
             | Some state ->
               if Lwt_switch.is_on state.switch then
                 Lwt_switch.turn_off state.switch
                 >>= (fun _ -> Lwt.return (`Right ()))
               else
                 Api_data.lwt_msg msg_process_not_running)
          (fun e -> Api_data.lwt_msg (Printexc.to_string e))

    end;;

end;;
