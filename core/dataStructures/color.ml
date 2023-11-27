(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type color = Red | Grey | Lightblue | Black

let triple_of_color = function
  | Red -> 255, 0, 0
  | Grey -> 128, 128, 128
  | Lightblue -> 0, 0, 128
  | Black -> 0, 0, 0
