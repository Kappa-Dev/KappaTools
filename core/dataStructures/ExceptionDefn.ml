(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

exception Syntax_Error of string Locality.annot
exception Malformed_Decl of string Locality.annot
exception Internal_Error of string Locality.annot

let warning_buffer : (Locality.t option * (Format.formatter -> unit)) list ref =
  ref []
