(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let tag f s = Format.fprintf f "%s@." s

let tag_if_debug s =
  if !Parameter.debugModeOn
  then Format.kfprintf (fun f -> Format.pp_print_newline f ())
		       Format.std_formatter s
  else Format.ifprintf Format.std_formatter s

let global_sigs : Signature.s ref = ref (Signature.create false [||])
