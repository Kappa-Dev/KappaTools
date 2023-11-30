(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type ('a, 'b) stop = Stop of 'b | Success of 'a

let success a = Success a
let stop a = Stop a

let success_or_stop f g x =
  match x with
  | Success a -> f a
  | Stop a -> g a
