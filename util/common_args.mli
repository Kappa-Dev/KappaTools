(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val data_set: string * int
val output: string * int
val semantics: string * int
val integration_settings: string * int
val model_reduction: string * int
val static_analysis: string * int
val debug_mode: string * int 

type t = { mutable backtrace           : bool ;
	   mutable debug               : bool;
	   mutable timeIndependent     : bool }

type t_gui

val default : t
val default_gui : t_gui
(* return options *)
val options : t -> (string * Arg.spec * string) list
val options_gui :
  t_gui ->
  (string * Superarg.spec * string * (Superarg.category * Superarg.position)  list * Superarg.level) list

val copy_from_gui: t_gui -> t -> unit
