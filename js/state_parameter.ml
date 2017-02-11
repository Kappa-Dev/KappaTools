(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let model_plot_period, set_model_plot_period = React.S.create 1.
let model_pause_condition, set_model_pause_condition = React.S.create ""
let model_seed, set_model_seed = React.S.create (None : int option)

let create_parameter (simulation_id : Api_types_j.simulation_id ) :
  Api_types_j.simulation_parameter =
  { Api_types_j.simulation_plot_period = React.S.value model_plot_period ;
    Api_types_j.simulation_pause_condition = React.S.value model_pause_condition ;
    Api_types_j.simulation_seed = React.S.value model_seed;
    Api_types_j.simulation_id = simulation_id ;
  }

let rec init_plot_period (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h::t ->
    try set_model_plot_period (float_of_string h)
    with Failure _ ->
      let msg = Format.sprintf "failed to parse init_plot_period '%s'" h in
      let () = Common.debug (Js.string msg) in
      init_plot_period t

let init_pause_condition (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h::_ -> set_model_pause_condition h

let rec init_model_seed (arg : string list) : unit =
  match arg with
  | [] -> ()
  | h::t ->
    try set_model_plot_period (float_of_string h)
    with Failure _ ->
      let msg = Format.sprintf "failed to parse model_seed '%s'" h in
      let () = Common.debug (Js.string msg) in
      init_model_seed t

let init () : unit Lwt.t =
  let arg_plot_period = Common_state.url_args "plot_period" in
  let arg_pause_condition = Common_state.url_args ~default:["[T] > 100"] "pause_condition" in
  let arg_model_seed = Common_state.url_args "model_seed" in
  let () = init_plot_period arg_plot_period in
  let () = init_pause_condition arg_pause_condition in
  let () = init_model_seed arg_model_seed in
  Lwt.return_unit
