(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

exception BadResponse of Mpi_message_j.response_content

class type virtual manager_simulation_msg_type = object
  method private virtual message :
    Mpi_message_j.request -> Mpi_message_j.response Lwt.t

  inherit Api.manager_simulation
end

class virtual manager_simulation_msg () : manager_simulation_msg_type =
  object (self)
    method private virtual message
        : Mpi_message_j.request -> Mpi_message_j.response Lwt.t

    method secret_simulation_load pattern_sharing ast variable_overwritten
        : unit Api.lwt_result =
      self#message
        (`SimulationLoad
          Api_types_t.{ pattern_sharing; ast; variable_overwritten })
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationLoad -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_continue (pause_condition : string) : unit Api.lwt_result
        =
      self#message (`SimulationContinue pause_condition)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationContinue -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_delete : unit Api.lwt_result =
      self#message `SimulationDelete
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationDelete -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_detail_file_line (file_line_id : string)
        : string list Api.lwt_result =
      self#message (`SimulationDetailFileLine file_line_id)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationDetailFileLine file_line_list ->
              Lwt.return (Result_util.ok file_line_list)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_detail_din (flux_map_id : Api_types_t.din_id)
        : Api_types_t.din Api.lwt_result =
      self#message (`SimulationDetailDIN flux_map_id)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationDetailDIN flux_map ->
              Lwt.return (Result_util.ok flux_map)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_detail_log_message
        : Api_types_j.log_message Api.lwt_result =
      self#message `SimulationDetailLogMessage
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationDetailLogMessage log_message ->
              Lwt.return (Result_util.ok log_message)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_detail_plot (plot_parameter : Api_types_j.plot_parameter)
        : Api_types_j.plot Api.lwt_result =
      self#message (`SimulationDetailPlot plot_parameter)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationDetailPlot plot -> Lwt.return (Result_util.ok plot)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_detail_snapshot (snapshot_id : Api_types_j.snapshot_id)
        : Api_types_j.snapshot Api.lwt_result =
      self#message (`SimulationDetailSnapshot snapshot_id)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationDetailSnapshot snapshot ->
              Lwt.return (Result_util.ok snapshot)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_info : Api_types_j.simulation_info Api.lwt_result =
      self#message `SimulationInfo
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationInfo simulation_status ->
              Lwt.return (Result_util.ok simulation_status)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_efficiency : Counter.Efficiency.t Api.lwt_result =
      self#message `SimulationEfficiency
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationEfficiency efficiency ->
              Lwt.return (Result_util.ok efficiency)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_catalog_file_line
        : Api_types_j.file_line_catalog Api.lwt_result =
      self#message `SimulationCatalogFileLine
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationCatalogFileLine info ->
              Lwt.return (Result_util.ok info)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_catalog_din : Api_types_j.din_catalog Api.lwt_result =
      self#message `SimulationCatalogDIN
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationCatalogDIN info -> Lwt.return (Result_util.ok info)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_catalog_snapshot
        : Api_types_j.snapshot_catalog Api.lwt_result =
      self#message `SimulationCatalogSnapshot
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationCatalogSnapshot info ->
              Lwt.return (Result_util.ok info)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_pause : unit Api.lwt_result =
      self#message `SimulationPause
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationPause -> Lwt.return (Result_util.ok ())
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_raw_trace : string Api.lwt_result =
      self#message `SimulationTrace
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationTrace result -> Lwt.return (Result_util.ok result)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_outputs_zip =
      self#message `SimulationOutputsZip
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationOutputsZip result ->
              Lwt.return (Result_util.ok (Base64.decode result))
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_parameter
        : Api_types_j.simulation_parameter Api.lwt_result =
      self#message `SimulationParameter
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationParameter result -> Lwt.return (Result_util.ok result)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_intervention
        (simulation_intervention : Api_types_j.simulation_intervention)
        : string Api.lwt_result =
      self#message (`SimulationIntervention simulation_intervention)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationIntervention s -> Lwt.return (Result_util.ok s)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))

    method simulation_start
        (simulation_parameter : Api_types_j.simulation_parameter)
        : Api_types_j.simulation_artifact Api.lwt_result =
      self#message (`SimulationStart simulation_parameter)
      >>= Api_common.result_bind_with_lwt ~ok:(function
            | `SimulationStart simulation_id ->
              Lwt.return (Result_util.ok simulation_id)
            | response ->
              Lwt.return
                (Api_common.err_result_of_exception (BadResponse response)))
  end

module IntMap = Mods.IntMap

type context = { mailboxes: Mpi_message_j.response Lwt.u IntMap.t; id: int }

class type virtual manager_simulation_mpi = object
  inherit Api.manager_simulation
  method private virtual sleep : float -> unit Lwt.t
  method private message : Mpi_message_j.request -> Mpi_message_j.response Lwt.t
  method private receive : string -> unit
  method private sim_is_computing : bool
  method virtual is_running : bool
end

class virtual new_client ~post () : manager_simulation_mpi =
  object (self)
    inherit manager_simulation_msg ()
    val mutable context = { mailboxes = IntMap.empty; id = 0 }
    method private virtual sleep : float -> unit Lwt.t
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
        let () = post message_text in
        let () =
          context <-
            {
              context with
              mailboxes = IntMap.add context.id feeder context.mailboxes;
            }
        in
        result
      ) else
        Lwt.return (Api_common.err_result_of_string "Kasim has died")

    method private sim_is_computing = not (IntMap.is_empty context.mailboxes)
  end
