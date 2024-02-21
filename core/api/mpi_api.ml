(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

exception BadResponse of Mpi_message_j.response_content

let on_message (manager : Api.manager_simulation)
    (post_message : string -> unit Lwt.t) (text_message : string) : unit Lwt.t =
  let message : Mpi_message_j.request Mpi_message_j.message =
    Mpi_message_j.message_of_string Mpi_message_j.read_request text_message
  in
  let handler :
        'a.
        ('a -> Mpi_message_j.response_content) -> 'a Api.result -> unit Lwt.t =
   fun pack result ->
    let message : Mpi_message_j.response Mpi_message_j.message =
      {
        Mpi_message_j.id = message.Mpi_message_j.id;
        Mpi_message_j.data = Result_util.map pack result;
      }
    in
    let text : string =
      Mpi_message_j.string_of_message Mpi_message_j.write_response message
    in
    post_message text
  in
  match message.Mpi_message_j.data with
  | `ProjectLoad Api_types_t.{ pattern_sharing; ast; variable_overwritten } ->
    manager#secret_simulation_load pattern_sharing ast variable_overwritten
    >>= handler (fun () -> `ProjectLoad)
  | `SimulationContinue simulation_parameter ->
    manager#simulation_continue simulation_parameter
    >>= handler (fun () -> `SimulationContinue)
  | `SimulationDelete ->
    manager#simulation_delete >>= handler (fun () -> `SimulationDelete)
  | `SimulationDetailFileLine file_line_id ->
    manager#simulation_detail_file_line file_line_id
    >>= handler (fun result -> `SimulationDetailFileLine result)
  | `SimulationDetailDIN din_id ->
    manager#simulation_detail_din din_id
    >>= handler (fun result -> `SimulationDetailDIN result)
  | `SimulationDetailLogMessage ->
    manager#simulation_detail_log_message
    >>= handler (fun result -> `SimulationDetailLogMessage result)
  | `SimulationDetailPlot plot_parameter ->
    manager#simulation_detail_plot plot_parameter
    >>= handler (fun result -> `SimulationDetailPlot result)
  | `SimulationDetailSnapshot snapshot_id ->
    manager#simulation_detail_snapshot snapshot_id
    >>= handler (fun result -> `SimulationDetailSnapshot result)
  | `SimulationInfo ->
    manager#simulation_info >>= handler (fun result -> `SimulationInfo result)
  | `SimulationEfficiency ->
    manager#simulation_efficiency
    >>= handler (fun result -> `SimulationEfficiency result)
  | `SimulationCatalogFileLine ->
    manager#simulation_catalog_file_line
    >>= handler (fun result -> `SimulationCatalogFileLine result)
  | `SimulationCatalogDIN ->
    manager#simulation_catalog_din
    >>= handler (fun result -> `SimulationCatalogDIN result)
  | `SimulationCatalogSnapshot ->
    manager#simulation_catalog_snapshot
    >>= handler (fun result -> `SimulationCatalogSnapshot result)
  | `SimulationParameter ->
    manager#simulation_parameter
    >>= handler (fun result -> `SimulationParameter result)
  | `SimulationTrace ->
    manager#simulation_raw_trace
    >>= handler (fun result -> `SimulationTrace result)
  | `SimulationOutputsZip ->
    manager#simulation_outputs_zip
    >>= handler (fun result -> `SimulationOutputsZip (Base64.encode result))
  | `SimulationPause ->
    manager#simulation_pause >>= handler (fun () -> `SimulationPause)
  | `SimulationIntervention simulation_intervention ->
    manager#simulation_intervention simulation_intervention
    >>= handler (fun s -> `SimulationIntervention s)
  | `SimulationStart simulation_parameter ->
    manager#simulation_start simulation_parameter
    >>= handler (fun result -> `SimulationStart result)

class type virtual manager_base_type = object
  method private virtual message :
    Mpi_message_j.request -> Mpi_message_j.response Lwt.t

  inherit Api.manager_simulation
end

class virtual manager_base () : manager_base_type =
  object (self)
    method private virtual message
        : Mpi_message_j.request -> Mpi_message_j.response Lwt.t

    method secret_simulation_load pattern_sharing ast variable_overwritten
        : unit Api.result Lwt.t =
      self#message
        (`ProjectLoad
          Api_types_t.{ pattern_sharing; ast; variable_overwritten })
      >>= Api_common.result_bind_lwt ~ok:(function
            | `ProjectLoad -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_continue (pause_condition : string)
        : unit Api.result Lwt.t =
      self#message (`SimulationContinue pause_condition)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationContinue -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_delete : unit Api.result Lwt.t =
      self#message `SimulationDelete
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationDelete -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_detail_file_line (file_line_id : string)
        : string list Api.result Lwt.t =
      self#message (`SimulationDetailFileLine file_line_id)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationDetailFileLine file_line_list ->
              Lwt.return (Result_util.ok file_line_list)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_detail_din (flux_map_id : Api_types_t.din_id)
        : Api_types_t.din Api.result Lwt.t =
      self#message (`SimulationDetailDIN flux_map_id)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationDetailDIN flux_map ->
              Lwt.return (Result_util.ok flux_map)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_detail_log_message
        : Api_types_j.log_message Api.result Lwt.t =
      self#message `SimulationDetailLogMessage
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationDetailLogMessage log_message ->
              Lwt.return (Result_util.ok log_message)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_detail_plot (plot_parameter : Api_types_j.plot_parameter)
        : Api_types_j.plot Api.result Lwt.t =
      self#message (`SimulationDetailPlot plot_parameter)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationDetailPlot plot -> Lwt.return (Result_util.ok plot)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_detail_snapshot (snapshot_id : Api_types_j.snapshot_id)
        : Api_types_j.snapshot Api.result Lwt.t =
      self#message (`SimulationDetailSnapshot snapshot_id)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationDetailSnapshot snapshot ->
              Lwt.return (Result_util.ok snapshot)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_info : Api_types_j.simulation_info Api.result Lwt.t =
      self#message `SimulationInfo
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationInfo simulation_status ->
              Lwt.return (Result_util.ok simulation_status)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_efficiency : Counter.Efficiency.t Api.result Lwt.t =
      self#message `SimulationEfficiency
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationEfficiency efficiency ->
              Lwt.return (Result_util.ok efficiency)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_catalog_file_line
        : Api_types_j.file_line_catalog Api.result Lwt.t =
      self#message `SimulationCatalogFileLine
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationCatalogFileLine info ->
              Lwt.return (Result_util.ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_catalog_din : Api_types_j.din_catalog Api.result Lwt.t =
      self#message `SimulationCatalogDIN
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationCatalogDIN info -> Lwt.return (Result_util.ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_catalog_snapshot
        : Api_types_j.snapshot_catalog Api.result Lwt.t =
      self#message `SimulationCatalogSnapshot
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationCatalogSnapshot info ->
              Lwt.return (Result_util.ok info)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_pause : unit Api.result Lwt.t =
      self#message `SimulationPause
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationPause -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_raw_trace : string Api.result Lwt.t =
      self#message `SimulationTrace
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationTrace result -> Lwt.return (Result_util.ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_outputs_zip =
      self#message `SimulationOutputsZip
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationOutputsZip result ->
              Lwt.return (Result_util.ok (Base64.decode result))
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_parameter
        : Api_types_j.simulation_parameter Api.result Lwt.t =
      self#message `SimulationParameter
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationParameter result -> Lwt.return (Result_util.ok result)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_intervention
        (simulation_intervention : Api_types_j.simulation_intervention)
        : string Api.result Lwt.t =
      self#message (`SimulationIntervention simulation_intervention)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationIntervention s -> Lwt.return (Result_util.ok s)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))

    method simulation_start
        (simulation_parameter : Api_types_j.simulation_parameter)
        : Api_types_j.simulation_artifact Api.result Lwt.t =
      self#message (`SimulationStart simulation_parameter)
      >>= Api_common.result_bind_lwt ~ok:(function
            | `SimulationStart simulation_id ->
              Lwt.return (Result_util.ok simulation_id)
            | response ->
              Lwt.return
                (Api_common.result_error_exception (BadResponse response)))
  end

module IntMap = Mods.IntMap

type context = { mailboxes: Mpi_message_j.response Lwt.u IntMap.t; id: int }

class type virtual manager_mpi_type = object
  method private virtual sleep : float -> unit Lwt.t
  method private virtual post_message : string -> unit
  method private message : Mpi_message_j.request -> Mpi_message_j.response Lwt.t
  method private receive : string -> unit
  inherit Api.manager_simulation
  method private sim_is_computing : bool
  method virtual is_running : bool
end

class virtual manager () : manager_mpi_type =
  object (self)
    val mutable context = { mailboxes = IntMap.empty; id = 0 }
    method private virtual sleep : float -> unit Lwt.t
    method private virtual post_message : string -> unit
    method virtual is_running : bool

    method private receive (response_text : string) =
      let message : Mpi_message_j.response Mpi_message_j.message =
        Mpi_message_j.message_of_string Mpi_message_j.read_response
          response_text
      in
      match IntMap.pop message.Mpi_message_j.id context.mailboxes with
      | Some value, mailboxes ->
        let () = context <- { context with mailboxes } in
        Lwt.wakeup value message.Mpi_message_j.data
      | None, mailboxes -> context <- { context with mailboxes }

    method private message (request : Mpi_message_j.request)
        : Mpi_message_j.response Lwt.t =
      if self#is_running then (
        let result, feeder = Lwt.task () in
        let () = context <- { context with id = context.id + 1 } in
        let message : Mpi_message_j.request Mpi_message_j.message =
          { Mpi_message_j.id = context.id; Mpi_message_j.data = request }
        in
        let message_text : string =
          Mpi_message_j.string_of_message Mpi_message_j.write_request message
        in
        let () = self#post_message message_text in
        let () =
          context <-
            {
              context with
              mailboxes = IntMap.add context.id feeder context.mailboxes;
            }
        in
        result
      ) else
        Lwt.return (Api_common.result_error_msg "Kappa has died")

    method private sim_is_computing = not (IntMap.is_empty context.mailboxes)
    inherit manager_base ()
  end
