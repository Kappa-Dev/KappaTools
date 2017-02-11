(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let model_perturbation, set_model_perturbation = React.S.create ""

let init_perturbation : string list -> unit =
  function
  | [] -> ()
  | h::_ -> set_model_perturbation h

let init () : unit Lwt.t =
  let arg_perturbation = Common_state.url_args "perturbation" in
  let () = init_perturbation arg_perturbation in
  Lwt.return_unit
