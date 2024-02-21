(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let unsome default = function
  | None -> default
  | Some a -> a

let unsome_or_raise
    ?(excep = Invalid_argument "unsome_or_raise was passed a None") = function
  | None -> raise excep
  | Some a -> a

let map f = function
  | Some x -> Some (f x)
  | None -> None

let bind f = function
  | None -> None
  | Some o -> f o

let fold f x = function
  | None -> x
  | Some y -> f x y

let equal eq a b =
  match a, b with
  | None, None -> true
  | Some _, None | None, Some _ -> false
  | Some x, Some y -> eq x y
