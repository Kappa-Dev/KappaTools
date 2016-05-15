open Lwt
module Api_types = ApiTypes_j
open Api_types

let msg_process_not_running = "process not running"
let msg_token_not_found = "token not found"
let msg_observables_less_than_zero = "Plot observables must be greater than zero"

let time_yield (seconds : float)
               (yield : (unit -> unit Lwt.t)) : (unit -> unit Lwt.t) =
  let lastyield = ref (Sys.time ()) in
  fun () -> let t = Sys.time () in
            if t -. !lastyield > seconds then
              let () = lastyield := t in
              yield ()
            else Lwt.return_unit

let () = Printexc.record_backtrace true

type runtime = < parse : Api_types.code -> Api_types.parse Api_types.result Lwt.t;
                 start : Api_types.parameter -> Api_types.token Api_types.result Lwt.t;
                 status : Api_types.token -> Api_types.state Api_types.result Lwt.t;
                 list : unit -> Api_types.catalog Api_types.result Lwt.t;
                 stop : Api_types.token -> unit Api_types.result Lwt.t >;;

module Base : sig
  class virtual runtime : object
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
  type simulator_state = { switch : Lwt_switch.t
                         ; counter : Counter.t
                         ; log_buffer : Buffer.t
                         ; plot : Api_types.plot ref
                         ; snapshots : Api_types.snapshot list ref
                         ; flux_maps : Api_types.flux_map list ref
                         ; files : Api_types.file_line list ref
                         ; error_messages : string list ref
                         }
  type context = { states : simulator_state IntMap.t
                  ; id : int }

  let format_error_message (message,linenumber) =
    Format.sprintf "Error at %s : %s"
                   (Location.to_string linenumber)
                   message
  let build_ast (code : string) success failure (log : ?exn:exn -> string -> unit Lwt.t) =
    let lexbuf : Lexing.lexbuf = Lexing.from_string code in
    try
      let raw_ast =
        KappaParser.start_rule KappaLexer.token lexbuf Ast.empty_compil in
      let ast :
            Signature.s * unit NamedDecls.t * int list *
              (Ast.agent, LKappa.rule_agent list, int, LKappa.rule) Ast.compil
        = LKappa.compil_of_ast [] raw_ast in
      let contact_map,_kasa_state =
        Eval.init_kasa Remanent_parameters_sig.JS raw_ast
      in success (ast,contact_map)
    with ExceptionDefn.Syntax_Error e ->
         failure (format_error_message e)
       | ExceptionDefn.Malformed_Decl e ->
         failure (format_error_message e)
       | exn -> log ~exn "" >>=
		  (fun () -> Lwt.fail exn)

  class virtual runtime =
  object(self)
  method virtual log : ?exn:exn -> string -> unit Lwt.t
  method virtual yield : unit -> unit Lwt.t
  val mutable context = { states = IntMap.empty
                         ; id = 0 }
    method parse (code : Api_types.code) : Api_types.parse Api_types.result Lwt.t =
      build_ast
	code
        (fun (_,contact_map) ->
         Lwt.return
	   (`Right { Api_types.contact_map =
		       Api_data.api_contact_map contact_map }))
        (fun e -> Lwt.return (`Left [e]))
        self#log
    method private new_id () : int =
      let result = context.id + 1 in
      let () = context <- { context with id = context.id + 1 } in
      result

    method start (parameter : Api_types.parameter) : Api_types.token Api_types.result Lwt.t =
      if parameter.nb_plot > 0 then
        catch
        (fun () ->
         Lwt.bind
           (build_ast parameter.code
                     (fun ast -> Lwt.return (`Right ast))
                     (fun e -> Lwt.return (`Left [e]))
                     self#log)
           (function
           `Right ((sig_nd,tk_nd,updated_vars,result),contact_map) ->
           let current_id = self#new_id () in
           let plot : Api_types.plot ref = ref { Api_types.legend = [];
                                                 Api_types.observables = [] } in
           let error_messages : string list ref = ref [] in
           let snapshots : Api_types.snapshot list ref = ref [] in
           let flux_maps : Api_types.flux_map list ref = ref [] in
           let files : Api_types.file_line list ref = ref [] in
           let simulation = { switch = Lwt_switch.create ()
                            ; counter = Counter.create
                                          ~init_t:(0. : float)
                                          ~init_e:(0 : int)
                                          ?max_t:parameter.max_time
                                          ?max_e:parameter.max_events
                                          ~nb_points:(parameter.nb_plot : int)
                            ; log_buffer = Buffer.create 512
                            ; plot = plot
                            ; error_messages = error_messages
                            ; snapshots = snapshots
                            ; flux_maps = flux_maps
                            ; files = files
                            } in
           let () = context <- { context with states = IntMap.add current_id simulation context.states } in
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
				      { Api_types.time = time ; values = new_values }
				      :: !plot.Api_types.observables }
	     | Data.Print file_line ->
		files := ((Api_data.api_file_line file_line)::!files)
	     | Data.Snapshot snapshot ->
		snapshots := ((Api_data.api_snapshot sigs snapshot)::!snapshots)
	     | Data.UnaryDistances _ -> ()
	     | Data.Log s -> Format.fprintf log_form "%s@." s in
           let () = Lwt.async
                      (fun () ->
                       (catch
                          (fun () ->
                           Eval.compile
			     ~pause:(fun f -> Lwt.bind (self#yield ()) f)
			     ~return:Lwt.return
			     ?rescale_init:None
			     ~outputs:(outputs (Signature.create []))
                             sig_nd tk_nd contact_map
                             simulation.counter result >>=
                             (fun (env_store,domain,has_tracking,init_l) ->
			      let (env,graph,state) =
				Eval.build_initial_state
				  [] simulation.counter env_store domain has_tracking
				  updated_vars init_l in
			      let () = ExceptionDefn.flush_warning log_form in
			      let sigs = Environment.signatures env in
                              let legend = Environment.map_observables
                                               (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
                                               env
                                in
                                let () = plot := { !plot with legend = Array.to_list legend} in
                                let rec iter graph state =
                                  let (stop,graph',state') =
                                    State_interpreter.a_loop
                                      ~outputs:(outputs sigs)
                                      env domain simulation.counter graph state in
                                  if stop then
                                    let () = ExceptionDefn.flush_warning log_form in
                                    Lwt_switch.turn_off simulation.switch
                                  else
                                    if Lwt_switch.is_on simulation.switch
                                    then Lwt.bind (self#yield ()) (fun () -> iter graph' state')
                                    else Lwt.return_unit in
                                iter graph state)
                          )
                          (function
                            | ExceptionDefn.Malformed_Decl error as exn ->
                               let () = error_messages := [format_error_message error] in
                               self#log ~exn ""
                            | ExceptionDefn.Internal_Error error as exn ->
                               let () = error_messages := [format_error_message error] in
                               self#log ~exn ""
                            | Invalid_argument error as exn ->
                               let () = error_messages := [Format.sprintf "Runtime error %s" error] in
                               self#log ~exn ""
                            | exn ->
                               let () = error_messages := [Printexc.to_string exn] in
                               self#log ~exn ""
                          )
                       )
                      )
           in
           Lwt.return (`Right current_id)
         | `Left e ->  Lwt.return (`Left e)
        ))
        (function
          | ExceptionDefn.Malformed_Decl error ->
             Lwt.return (`Left [format_error_message error])
          | ExceptionDefn.Internal_Error error ->
             Lwt.return (`Left [format_error_message error])
          | Invalid_argument error ->
             let message = Format.sprintf "Runtime error %s" error in
             Lwt.return (`Left [message])
          | Sys_error message ->
             Lwt.return (`Left [message])
          | exn -> (self#log ~exn "")
                 >>= (fun _ -> Lwt.return (`Left [Printexc.to_string exn]))
        )
      else
        Lwt.return (`Left [msg_observables_less_than_zero])

    method status (token : Api_types.token) : Api_types.state Api_types.result Lwt.t =
      Lwt.catch
        (fun () ->
         match IntMap.find_option token context.states with
	 | None ->  Lwt.return (`Left [msg_token_not_found])
	 | Some state ->
            let () =
	      if not (Lwt_switch.is_on state.switch) then
		context <- { context with states = IntMap.remove token context.states }
            in
            Lwt.return
              (match !(state.error_messages) with
		 [] ->
		 `Right ({ Api_types.plot = Some !(state.plot);
			   Api_types.time = Counter.time state.counter;
			   Api_types.time_percentage = Counter.time_percentage state.counter;
			   Api_types.event = Counter.event state.counter;
			   Api_types.event_percentage = Counter.event_percentage state.counter;
			   Api_types.tracked_events = Some (Counter.tracked_events state.counter);
			   Api_types.log_messages = [Buffer.contents state.log_buffer] ;
			   Api_types.snapshots = !(state.snapshots);
			   Api_types.flux_maps = !(state.flux_maps);
			   Api_types.files = !(state.files);
			   is_running = Lwt_switch.is_on state.switch
			 } : Api_types.state )
               | _ -> `Left !(state.error_messages))
        )
        (fun exn -> (self#log ~exn "")
                 >>= (fun _ -> Lwt.return (`Left [Printexc.to_string exn]))
        )
    method list () : Api_types.catalog Api_types.result Lwt.t =
      Lwt.return (`Right (List.map fst (IntMap.bindings context.states)))

    method stop (token : Api_types.token) : unit Api_types.result Lwt.t =
    catch
      (fun () ->
       match IntMap.find_option token context.states with
       | None -> Lwt.return (`Left [msg_token_not_found])
       | Some state ->
	  if Lwt_switch.is_on state.switch then
            Lwt_switch.turn_off state.switch
            >>= (fun _ -> Lwt.return (`Right ()))
	  else
            Lwt.return (`Left [msg_process_not_running]))
      (fun e -> Lwt.return (`Left [Printexc.to_string e])
      )
  end;;
end;;
