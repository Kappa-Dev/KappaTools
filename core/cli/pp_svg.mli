(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type store = {
  file: string;
  title: string;
  descr: string;
  legend: string array;
  mutable points: Nbr.t array list;
}

val to_string : ?width:int -> store -> string
val to_file : store -> unit
