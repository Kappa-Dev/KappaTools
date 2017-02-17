(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val split : string -> char -> string * string option
val find : ('a -> bool) -> 'a list -> 'a option
val option_map : ('a -> 'b) -> 'a option -> 'b option
val option_bind : ('a -> 'b option) -> 'a option -> 'b option
val input_float_string : float -> string
