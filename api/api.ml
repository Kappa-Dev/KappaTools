open Lwt.Infix
module Api_types = ApiTypes_j
open ApiTypes_j

let msg_process_not_running =
  "process not running"
let msg_token_not_found =
  "token not found"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"

let time_yield (seconds : float)
               (yield : (unit -> unit Lwt.t)) : (unit -> unit Lwt.t) =
  let lastyield = ref (Sys.time ()) in
  fun () -> let t = Sys.time () in
            if t -. !lastyield > seconds then
              let () = lastyield := t in
              yield ()
            else Lwt.return_unit

let () = Printexc.record_backtrace true

type runtime =
  < parse : Api_types.code -> Api_types.parse Api_types.result Lwt.t;
  start : Api_types.parameter -> Api_types.token Api_types.result Lwt.t;
  status : Api_types.token -> Api_types.state Api_types.result Lwt.t;
  list : unit -> Api_types.catalog Api_types.result Lwt.t;
  stop : Api_types.token -> unit Api_types.result Lwt.t >;;

module Base : sig
  class virtual runtime :
  object
    method parse : Api_types.code -> Api_types.parse Api_types.result Lwt.t
    method start : Api_types.parameter -> Api_types.token Api_types.result Lwt.t
    method status : Api_types.token -> Api_types.state Api_types.result Lwt.t
    method list : unit -> Api_types.catalog Api_types.result Lwt.t
    method stop : Api_types.token -> unit Api_types.result Lwt.t
    method virtual log : ?exn:exn -> string -> unit Lwt.t
    method virtual yield : unit -> unit Lwt.t
  end;;
end = struct
  module IntMap = Mods.IntMap
  type simulator_state =
    { switch : Lwt_switch.t
    ; counter : Counter.t
    ; log_buffer : Buffer.t
    ; plot : Api_types.plot ref
    ; distances : Api_types.distances ref
    ; snapshots : Api_types.snapshot list ref
    ; flux_maps : Api_types.flux_map list ref
    ; files : Api_types.file_line list ref
    ; error_messages : Api_types.errors ref
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
               (fun (ast :
                       Signature.s * unit NamedDecls.t * int list *
                         (Ast.agent,
                          LKappa.rule_agent list,
                          int,
                          LKappa.rule) Ast.compil) ->
                (yield ()) >>=
                  (fun () ->
                   (Lwt.wrap2
                      Eval.init_kasa
                      Remanent_parameters_sig.JS
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

  class virtual runtime =
  object(self)
  method virtual log : ?exn:exn -> string -> unit Lwt.t
  method virtual yield : unit -> unit Lwt.t
  val mutable context = { states = IntMap.empty
                         ; id = 0 }
    method parse
      (code : Api_types.code) : Api_types.parse Api_types.result Lwt.t =
      Lwt.bind
        (build_ast code self#yield self#log)
        (function
          | `Right (_,contact_map) ->
             Lwt.return
               (`Right { Api_types.contact_map =
                           Api_data.api_contact_map contact_map })
          | `Left e -> Lwt.return (`Left e))

    method private new_id () : int =
      let result = context.id + 1 in
      let () = context <- { context with id = context.id + 1 } in
      result

    method start
      (parameter : Api_types.parameter) :
      Api_types.token Api_types.result Lwt.t =
      if parameter.nb_plot > 0 then
        let current_id = self#new_id () in
        let plot : Api_types.plot ref =
          ref { Api_types.legend = []
              ; Api_types.observables = [] }
        in
        let distances : Api_types.distances ref = ref [] in
        let error_messages : Api_types.errors ref = ref [] in
        let snapshots : Api_types.snapshot list ref = ref [] in
        let flux_maps : Api_types.flux_map list ref = ref [] in
        let files : Api_types.file_line list ref = ref [] in
        let simulation =
          { switch = Lwt_switch.create ()
          ; counter = Counter.create
              ~init_t:(0. : float)
              ~init_e:(0 : int)
              ?max_t:parameter.max_time
              ?max_e:parameter.max_events
              ~nb_points:(parameter.nb_plot : int)
          ; log_buffer = Buffer.create 512
          ; plot = plot
          ; distances = distances
          ; error_messages = error_messages
          ; snapshots = snapshots
          ; flux_maps = flux_maps
          ; files = files
          } in
        let () =
          context <-
            { context with
              states = IntMap.add current_id simulation context.states } in
        let log_form = Format.formatter_of_buffer simulation.log_buffer in
        let () = Counter.reinitialize simulation.counter in
        let outputs sigs = function
          | Data.Flux flux_map ->
             flux_maps := ((Api_data.api_flux_map flux_map)::!flux_maps)
          | Data.Plot (time,new_observables) ->
             let new_values =
               List.map (fun nbr -> Nbr.to_float nbr)
                        (Array.to_list new_observables) in
             plot := {!plot with Api_types.observables =
                                   { Api_types.time = time ;
                                     values = new_values }
                                   :: !plot.Api_types.observables }
          | Data.Print file_line ->
             files := ((Api_data.api_file_line file_line)::!files)
          | Data.Snapshot snapshot ->
             snapshots := ((Api_data.api_snapshot sigs snapshot)::!snapshots)
          | Data.UnaryDistances unary_distances ->
                distances :=
                  let (one_big_list,_) =
                  Array.fold_left
                    (fun (l,i) a ->
                     match a with
                     | Some ls ->
                        let add_rule_id =
                          List.map (fun (t,d) ->
                                    {Api_types.rule_dist =
                                       unary_distances.Data.distances_rules.(i);
                                     Api_types.time_dist = t;
                                     Api_types.dist = d}) ls
                        in (List.append l add_rule_id, i+1)
                     | None -> (l, i+1))
                    ([],0) unary_distances.Data.distances_data in
                  one_big_list
          | Data.Log s ->
            Format.fprintf log_form "%s@." s
        in
        let () =
          Lwt.async
            (fun () ->
             (Lwt.catch
                (fun () ->
                 (build_ast parameter.code self#yield self#log) >>=
                   (function
                       `Right ((sig_nd,tk_nd,updated_vars,result)
                              ,contact_map) ->
                       Eval.compile
                         ~pause:(fun f -> Lwt.bind (self#yield ()) f)
                         ~return:Lwt.return
                         ?rescale_init:None
                         ~outputs:(outputs (Signature.create []))
                         sig_nd tk_nd contact_map
                         simulation.counter result >>=
                         (fun (env_store,domain,has_tracking,
                               store_distances,_,init_l) ->
                          let story_compression =
                            Tools.option_map
                              (fun  _ -> ((false,false,false),true))
                              has_tracking
                          in
                          let (env,graph_state) =
                            Eval.build_initial_state
                              ~bind:(fun x f ->
                                     (self#yield ()) >>= (fun () -> x >>= f))
                              ~return:Lwt.return [] simulation.counter
                              env_store domain story_compression
                              store_distances updated_vars init_l in
                          graph_state >>=
                            (fun (graph,state) ->
                             let () = ExceptionDefn.flush_warning log_form in
                             let sigs = Environment.signatures env in
                             let legend =
                               Environment.map_observables
                                 (Format.asprintf
                                    "%a"
                                    (Kappa_printer.alg_expr ~env))
                                 env in
                             let () =
                               plot :=
                                 { !plot with legend = Array.to_list legend} in
                             let rec iter graph state =
                               let (stop,graph',state') =
                                 State_interpreter.a_loop
                                   ~outputs:(outputs sigs)
                                   env domain simulation.counter graph state in
                               if stop then
                                 (Lwt_switch.turn_off simulation.switch)
                                   >>= (fun () -> Lwt.return (graph',state'))
                               else
                                 if Lwt_switch.is_on simulation.switch
                                 then (self#yield ()) >>=
                                        (fun () -> iter graph' state')
                                 else Lwt.return (graph',state') in
                             (iter graph state) >>=
                               (fun (graph,state) ->
                                let _ =
                                  State_interpreter.end_of_simulation
                                    ~outputs:(outputs sigs) log_form env
                                    simulation.counter graph state in
                                Lwt.return_unit)))
                     | `Left e ->
                        let () = error_messages := e
                        in Lwt.return_unit)
                )
                (function
                  | ExceptionDefn.Malformed_Decl error as exn ->
                     let () = error_messages :=
                       (Api_data.api_location_errors error)
                     in
                     self#log ~exn ""
                  | ExceptionDefn.Internal_Error error as exn ->
                     let () = error_messages :=
                       (Api_data.api_location_errors error)
                     in
                     self#log ~exn ""
                  | Invalid_argument error as exn ->
                     let () = error_messages :=
                       (Api_data.api_message_errors ("Runtime error "^ error))
                     in
                     self#log ~exn ""
                  | exn ->
                     let () = error_messages :=
                       (Api_data.api_message_errors (Printexc.to_string exn)) in
                     self#log ~exn ""
                )))
        in
        Lwt.return (`Right current_id)
      else
        Api_data.lwt_msg msg_observables_less_than_zero

    method status
      (token : Api_types.token) : Api_types.state Api_types.result Lwt.t =
      Lwt.catch
        (fun () ->
         match IntMap.find_option token context.states with
         | None ->
           Api_data.lwt_msg msg_token_not_found
         | Some state ->
            let () =
              if not (Lwt_switch.is_on state.switch) then
                context <-
                  { context with states = IntMap.remove token context.states }
            in
            Lwt.return
              (match !(state.error_messages) with
                 [] ->
                 `Right
                   ({ Api_types.plot =
                       Some !(state.plot);
                      Api_types.distances =
                       Some !(state.distances);
                      Api_types.time =
                       Counter.time state.counter;
                      Api_types.time_percentage =
                       Counter.time_percentage state.counter;
                      Api_types.event =
                       Counter.event state.counter;
                      Api_types.event_percentage =
                       Counter.event_percentage state.counter;
                      Api_types.tracked_events =
                       Some (Counter.tracked_events state.counter);
                      Api_types.log_messages =
                       [Buffer.contents state.log_buffer] ;
                      Api_types.snapshots =
                       !(state.snapshots);
                      Api_types.flux_maps =
                       !(state.flux_maps);
                      Api_types.files =
                       !(state.files);
                      is_running =
                       Lwt_switch.is_on state.switch
                    } : Api_types.state )
              | _ ->
                `Left !(state.error_messages)
              )
        )
        (fun exn ->
          (self#log ~exn "")
          >>=
            (fun _ -> Api_data.lwt_msg (Printexc.to_string exn))
        )
    method list () : Api_types.catalog Api_types.result Lwt.t =
      Lwt.return (`Right (List.map fst (IntMap.bindings context.states)))

    method stop (token : Api_types.token) : unit Api_types.result Lwt.t =
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
