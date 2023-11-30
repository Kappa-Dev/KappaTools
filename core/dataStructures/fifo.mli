(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** FIFO *)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> 'a t * 'a option
