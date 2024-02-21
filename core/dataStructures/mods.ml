(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let int_compare (x : int) y = Stdlib.compare x y

let int_pair_compare (p, q) (p', q') =
  let o = int_compare p p' in
  if o = 0 then
    int_compare q q'
  else
    o

let string_pair_compare (p, q) (p', q') =
  let o = String.compare p p' in
  if o = 0 then
    String.compare q q'
  else
    o

let pair_equal eqa eqb (xa, xb) (ya, yb) = eqa xa ya && eqb xb yb

module StringSetMap = SetMap.Make (struct
  type t = string

  let compare = String.compare
  let print = Format.pp_print_string
end)

module StringSet = StringSetMap.Set
module StringMap = StringSetMap.Map

module String2SetMap = SetMap.Make (struct
  type t = string * string

  let compare = string_pair_compare
  let print f (a, b) = Format.fprintf f "(%s, %s)" a b
end)

module String2Map = String2SetMap.Map

module IntSetMap = SetMap.Make (struct
  type t = int

  let compare = int_compare
  let print = Format.pp_print_int
end)

module IntSet = IntSetMap.Set
module IntMap = IntSetMap.Map

module Int2SetMap = SetMap.Make (struct
  type t = int * int

  let compare = int_pair_compare
  let print f (a, b) = Format.fprintf f "(%i, %i)" a b
end)

module Int2Set = Int2SetMap.Set
module Int2Map = Int2SetMap.Map

module CharSetMap = SetMap.Make (struct
  type t = char

  let compare = compare
  let print = Format.pp_print_char
end)

module CharSet = CharSetMap.Set
module CharMap = CharSetMap.Map
module DynArray = DynamicArray.DynArray (LargeArray)
