(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = { mutable backtrace           : bool ;
	   mutable debug               : bool;
	   mutable timeIndependent     : bool }

val default : t
(* return options *)
val options : t -> (string * Arg.spec * string) list
