(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Utilities to show progress on stdout *)

type t

val create : int -> char -> t

val tick :
  efficiency:bool -> float -> float option -> int -> float option -> t -> unit

val complete_progress_bar : float -> int -> t -> unit
