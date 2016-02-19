open Lwt
module ApiTypes = ApiTypes_j
open ApiTypes
open Pp_svg

type runtime = < parse : ApiTypes.code -> ApiTypes.error Lwt.t;
                 start : ApiTypes.parameter -> ApiTypes.token ApiTypes.result Lwt.t;
                 status : ApiTypes.token -> ApiTypes.state ApiTypes.result Lwt.t;
                 list : unit -> ApiTypes.catalog ApiTypes.result Lwt.t;
                 stop : ApiTypes.token -> unit ApiTypes.result Lwt.t >;;

module Base : sig
  class runtime : (unit -> unit Lwt.t) -> object
                         method parse : ApiTypes.code -> ApiTypes.error Lwt.t
                         method start : ApiTypes.parameter -> ApiTypes.token ApiTypes.result Lwt.t
                         method status : ApiTypes.token -> ApiTypes.state ApiTypes.result Lwt.t
                         method list : unit -> ApiTypes.catalog ApiTypes.result Lwt.t
                         method stop : ApiTypes.token -> unit ApiTypes.result Lwt.t
                       end;;
end = struct
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  type simulator_state = { switch : Lwt_switch.t
                         ; counter : Counter.t
                         ; log_buffer : Buffer.t
                         ; svg_store : Pp_svg.store option ref }
  type context = { states : simulator_state IntMap.t
                  ; id : int }

  let format_error_message (message,linenumber) =
    Format.sprintf "Error at %s : %s"
                   (Location.to_string linenumber)
                   message
  let build_ast code success failure =
    let lexbuf : Lexing.lexbuf = Lexing.from_string code in
    try let ast :
              (Ast.agent, Ast.mixture, string, Ast.rule) Ast.compil
          = KappaParser.start_rule
              KappaLexer.token
              lexbuf
              Ast.empty_compil
        in success ast
    with ExceptionDefn.Syntax_Error e ->
      failure (format_error_message e)

  class runtime (yield:unit -> unit Lwt.t) =
  object(self)
    val mutable context = { states = IntMap.empty
                         ; id = 0 }

    method parse (code : ApiTypes.code) : ApiTypes.error Lwt.t =
      build_ast code
                (fun _ -> Lwt.return [])
                (fun e -> Lwt.return [e])

    method private new_id () : int =
      let result = context.id + 1 in
      let () = context <- { context with id = context.id + 1 } in
      result

    method start (parameter : ApiTypes.parameter) : ApiTypes.token ApiTypes.result Lwt.t =
      if parameter.nb_plot > 0 then
      catch
        (fun () ->
         match
           build_ast parameter.code
                     (fun ast -> `Right ast)
                     (fun e -> `Left [e])
         with
           `Right result ->
           let current_id = self#new_id () in
           let svg_store : Pp_svg.store option ref = ref None in
           let outputs (data : Data.t) =
             match data with
               Data.Flux _flux_map -> ()
             | Data.Plot (time,observables) ->
                (match !svg_store with
                   Some svg_store-> (svg_store.points <- ((time,observables) :: svg_store.points))
                 | None -> ()
                )
             | Data.Print _file_line -> ()
             | Data.Snapshot _snapshot -> ()
             | Data.UnaryDistances _ -> ()
           in
           let simulation = { switch = Lwt_switch.create ()
                            ; counter = Counter.create
                                          ~init_t:(0. : float)
                                          ~init_e:(0 : int)
                                          ?max_t:parameter.max_time
                                          ?max_e:parameter.max_events
                                          ~nb_points:(parameter.nb_plot : int)
                            ; log_buffer = Buffer.create 512
                            ; svg_store = svg_store } in
           let () = context <- { context with states = IntMap.add current_id simulation context.states } in
           let log_form = Format.formatter_of_buffer simulation.log_buffer in
           let () = Counter.reinitialize simulation.counter in
           let () = Lwt.async (fun () ->
                               wrap4 (Eval.initialize ?rescale_init:None)
                                     log_form [] simulation.counter result
                               >>= (fun (_kasa_state,env,domain,graph,state) ->
                                    let legend =
                                      Environment.map_observables
                                        (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
                                        env in
                                    let () = svg_store := Some { file = "filename";
                                                                 title = "title";
                                                                 descr = "";
                                                                 legend = legend;
                                                                 points = [];
                                                               }
                                    in
                                    State_interpreter.loop_cps
                                      ~outputs:outputs
                                      log_form
                                      (fun f -> if Lwt_switch.is_on simulation.switch
                                                then Lwt.bind (yield ()) f
                                                else Lwt.return_unit
                                      )
                                      (fun (_ : Rule_interpreter.t)(_ : State_interpreter.t)  ->
                                       let () = ExceptionDefn.flush_warning log_form in
                                       Lwt_switch.turn_off simulation.switch)
                                      env domain simulation.counter graph state)) in
           Lwt.return (`Right current_id)
         | `Left e ->  Lwt.return (`Left e)
        )
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
          | e -> fail e
        )
      else
        Lwt.return (`Left ["Plot points must be greater than zero"])

    method status (token : ApiTypes.token) : ApiTypes.state ApiTypes.result Lwt.t =
      Lwt.catch
        (fun () ->
         let state : simulator_state = IntMap.find token context.states in
         let () = if Lwt_switch.is_on state.switch then
                    ()
                  else
                    context <- { context with states = IntMap.remove token context.states }
         in
         Lwt.return (`Right ({ plot = (match !(state.svg_store) with
                                         Some svg_store -> Pp_svg.to_string ~width:500 svg_store
                                       | None -> "");
                              time = Counter.time state.counter;
                              time_percentage = Counter.time_percentage state.counter;
                              event = Counter.event state.counter;
                              event_percentage = Counter.event_percentage state.counter;
                              tracked_events = Some (Counter.tracked_events state.counter);
                              log_messages = [Buffer.contents state.log_buffer] ;
                              is_running = Lwt_switch.is_on state.switch
                            } : ApiTypes.state )))
        (function Not_found -> Lwt.return (`Left ["token not found"])
                                          | e -> fail e)
    method list () : ApiTypes.catalog ApiTypes.result Lwt.t =
      Lwt.return (`Right (List.map fst (IntMap.bindings context.states)))

    method stop (token : ApiTypes.token) : unit ApiTypes.result Lwt.t =
    catch
      (fun () ->
       let state : simulator_state = IntMap.find token context.states in
       if Lwt_switch.is_on state.switch then
         Lwt_switch.turn_off state.switch
         >>= (fun _ -> Lwt.return (`Right ()))
       else
         Lwt.return (`Left ["process not running"]))
      (function Not_found -> Lwt.return (`Left ["token not found"])
              | e -> fail e
      )
  end;;
end;;
