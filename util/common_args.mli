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

type t_gui

val default : t
val default_gui : t_gui
(* return options *)
val options : t -> (string * Arg.spec * string) list
val options_gui :
  t_gui ->
  (string * Superarg.spec * string * string list * Superarg.level) list

val copy_from_gui: t_gui -> t -> unit
