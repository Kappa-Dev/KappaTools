(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Utilities to show progress on stdout *)

type t

val create : int -> char -> Format.formatter -> t
val tick : float -> float -> int -> float -> Format.formatter -> t -> unit
val complete_progress_bar : float -> int -> Format.formatter -> t -> unit
