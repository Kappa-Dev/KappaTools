(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix

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
  | `SimulationLoad Api_types_t.{ pattern_sharing; ast; variable_overwritten }
    ->
    manager#secret_simulation_load pattern_sharing ast variable_overwritten
    >>= handler (fun () -> `SimulationLoad)
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
