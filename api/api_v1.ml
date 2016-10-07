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

let catch_error : 'a . (Api_types_v1_j.errors -> 'a) -> exn -> 'a =
  fun handler ->
  (function
    |  ExceptionDefn.Syntax_Error e ->
      handler (Api_data_v1.api_location_errors e)
    | ExceptionDefn.Malformed_Decl e ->
      handler  (Api_data_v1.api_location_errors e)
    | ExceptionDefn.Internal_Error error ->
      handler (Api_data_v1.api_location_errors error)
    | Invalid_argument error ->
      handler (Api_data_v1.api_message_errors ("Runtime error "^ error))
    | exn -> handler (Api_data_v1.api_message_errors (Printexc.to_string exn))
  )


class type api_runtime =
  object
    method parse :
      Api_types_v1_j.code ->
      Api_types_v1_j.parse Api_types_v1_j.result Lwt.t
    method start :
      Api_types_v1_j.parameter ->
      Api_types_v1_j.token Api_types_v1_j.result Lwt.t
    method status :
      Api_types_v1_j.token ->
      Api_types_v1_j.state Api_types_v1_j.result Lwt.t
    method list :
      unit ->
      Api_types_v1_j.catalog Api_types_v1_j.result Lwt.t
    method stop :
      Api_types_v1_j.token ->
      unit Api_types_v1_j.result Lwt.t
    method perturbate :
      Api_types_v1_j.token ->
      Api_types_v1_j.perturbation ->
      unit Api_types_v1_j.result Lwt.t
    method pause :
      Api_types_v1_j.token ->
      unit Api_types_v1_j.result Lwt.t
    method continue :
      Api_types_v1_j.token ->
      Api_types_v1_j.parameter ->
      unit Api_types_v1_j.result Lwt.t
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
  type context = { states : Kappa_facade.t IntMap.t ; id : int }


  class virtual base_runtime min_run_duration =
    object(self)
      val mutable lastyield = Sys.time ()
      method private system_process () : Kappa_facade.system_process =
        object
          method log ?exn:exn (message : string) : unit Lwt.t =
            self#log ?exn:exn message
          method min_run_duration () : float =
            min_run_duration
          method yield () : unit Lwt.t =
            self#yield ()
        end
      method virtual log : ?exn:exn -> string -> unit Lwt.t
      method virtual yield : unit -> unit Lwt.t

      val mutable context = { states = IntMap.empty ; id = 0 }
      (* not sure if this is good *)
      val start_time : float = Sys.time ()

      method private time_yield () =
        let t = Sys.time () in
        if t -. lastyield > min_run_duration then
          let () = lastyield <- t in
          self#yield ()
        else Lwt.return_unit

      method parse
          (code : Api_types_v1_j.code) :
        Api_types_v1_j.parse Api_types_v1_j.result Lwt.t =
        Kappa_facade.parse
          ~system_process:(self#system_process ())
          ~kappa_code:code
        >>=
        (Api_common.result_data_map
           ~ok:(fun t -> Lwt.return
                   (`Right
                      { Api_types_v1_j.contact_map =
                          Api_data_v1.api_contact_map (Kappa_facade.get_contact_map t)
                      }))
           ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))

      method private new_id () : int =
        let result = context.id + 1 in
        let () = context <- { context with id = context.id + 1 } in
        result

      method start
          (parameter : Api_types_v1_j.parameter) :
        Api_types_v1_j.token Api_types_v1_j.result Lwt.t =
        (Kappa_facade.parse
           ~system_process:(self#system_process ())
           ~kappa_code:parameter.Api_types_v1_j.code
        )
        >>=
        (function
          |`Ok t ->
            (Kappa_facade.start
               ~system_process:(self#system_process ())
               ~parameter:(Api_data.api_parameter parameter)
               ~t:t
            )
            >>=
            (Api_common.result_data_map
               ~ok:(fun () ->
                   let current_id = self#new_id () in
                   let () = context <-
                       { context with
                         states = IntMap.add current_id t context.states } in
                   Lwt.return (`Right current_id))
               ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))
          |`Error errors ->
                Lwt.return (`Left (Api_data.api_errors errors))
        )

      method perturbate
          (token : Api_types_v1_j.token)
          (perturbation : Api_types_v1_j.perturbation) :
        unit Api_types_v1_j.result Lwt.t =
        match IntMap.find_option token context.states with
        | None ->
          Api_data_v1.lwt_msg msg_token_not_found
        | Some t ->
          (Kappa_facade.perturbation
             ~system_process:(self#system_process ())
             ~t:t
             ~perturbation:{ Api_types_t.perturbation_code = perturbation.Api_types_v1_j.perturbation_code; })
           >>=
           (Api_common.result_data_map
              ~ok:(fun () -> Lwt.return (`Right ()))
              ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))

      method status (token : Api_types_v1_j.token) :
        Api_types_v1_j.state Api_types_v1_j.result Lwt.t =
        match IntMap.find_option token context.states with
        | None ->
          Api_data_v1.lwt_msg msg_token_not_found
        | Some t ->
          (Kappa_facade.info
             ~system_process:(self#system_process ())
             ~t:t)
          >>=
          (Api_common.result_data_map
             ~ok:(fun status -> Lwt.return (`Right (Api_data.api_status status)))
             ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))

      method list () : Api_types_v1_j.catalog Api_types_v1_j.result Lwt.t =
        Lwt.return (`Right (List.map fst (IntMap.bindings context.states)))

      method pause (token : Api_types_v1_j.token) :
        unit Api_types_v1_j.result Lwt.t =
        match IntMap.find_option token context.states with
        | None ->
          Api_data_v1.lwt_msg msg_token_not_found
        | Some t ->
          (Kappa_facade.pause
             ~system_process:(self#system_process ())
             ~t:t)
          >>=
          (Api_common.result_data_map
             ~ok:(fun () -> Lwt.return (`Right ()))
             ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))

      method continue
          (token : Api_types_v1_j.token)
          (parameter : Api_types_v1_j.parameter) :
        unit Api_types_v1_j.result Lwt.t =
        match IntMap.find_option token context.states with
        | None ->
          Api_data_v1.lwt_msg msg_token_not_found
        | Some t ->
          (Kappa_facade.continue
             ~system_process:(self#system_process ())
             ~t:t
             ~parameter:(Api_data.api_parameter parameter)
          )
          >>=
          (Api_common.result_data_map
             ~ok:(fun () -> Lwt.return (`Right ()))
             ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))

      method stop (token : Api_types_v1_j.token) : unit Api_types_v1_j.result Lwt.t =
        match IntMap.find_option token context.states with
        | None ->
          Api_data_v1.lwt_msg msg_token_not_found
        | Some t ->
          (Kappa_facade.stop
             ~system_process:(self#system_process ())
             ~t:t
          )
          >>=
          (Api_common.result_data_map
             ~ok:(fun () -> Lwt.return (`Right ()))
             ~error:(fun errors -> Lwt.return (`Left (Api_data.api_errors errors))))

      initializer
        Lwt.async (fun () -> self#log "created runtime")
    end;;

end;;
