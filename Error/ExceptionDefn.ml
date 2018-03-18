(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception IntFound of int
exception StringFound of string
exception MapFound of (int Mods.IntMap.t)
exception False
exception True
exception Break of int

(* 0:unary rule with binary instance *)
(* 1:binary rule with unary instance *)
(* 2:clashing instance *)
(* 3:overapproximation clash *)
(* 4:invalid injection clash *)
(* 5: perturbation interrupting time*)
exception Null_event of int
exception StopReached of string

exception Syntax_Error of string Locality.annot
exception Malformed_Decl of string Locality.annot
exception Internal_Error of string Locality.annot
exception Unsatisfiable

let warning_buffer:
      (Locality.t option*(Format.formatter -> unit)) list ref = ref []
