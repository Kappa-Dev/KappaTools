(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val data_set : Superarg.category
val output : Superarg.category
val semantics : Superarg.category
val integration_settings : Superarg.category
val model_reduction : Superarg.category
val static_analysis : Superarg.category
val debug_mode : Superarg.category

type t = { mutable backtrace: bool; mutable debug: bool }
type t_gui

val default : t
val default_gui : t_gui

(* return options *)
val options : t -> (string * Arg.spec * string) list

val options_gui :
  t_gui ->
  (string
  * Superarg.spec
  * string
  * (Superarg.category * Superarg.position) list
  * Superarg.level)
  list

val copy_from_gui : t_gui -> t -> unit
