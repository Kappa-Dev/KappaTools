(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  bag: int Mods.DynArray.t;
  mutable size: int;
  dict: (int, int) Hashtbl.t;
}

let create size =
  { size = 0; bag = Mods.DynArray.create size (-1); dict = Hashtbl.create size }

let print f s =
  if s.size <= 0 then
    Pp.empty_set f
  else (
    let () = Format.pp_print_string f "{ " in
    let () =
      for i = 0 to s.size - 2 do
        Format.pp_print_int f (Mods.DynArray.get s.bag i);
        Pp.comma f
      done
    in
    let () = Format.pp_print_int f (Mods.DynArray.get s.bag (s.size - 1)) in
    Format.pp_print_string f " }"
  )

let is_empty s = s.size = 0

let add x s =
  if not (Hashtbl.mem s.dict x) then (
    let () = Mods.DynArray.set s.bag s.size x in
    let () = Hashtbl.replace s.dict x s.size in
    s.size <- succ s.size
  )

let remove x s =
  try
    let pos = Hashtbl.find s.dict x in
    let () = Hashtbl.remove s.dict x in
    let () =
      if pos < s.size - 1 then (
        let last = Mods.DynArray.get s.bag (s.size - 1) in
        let () = Hashtbl.replace s.dict last pos in
        Mods.DynArray.set s.bag pos last
      )
    in
    s.size <- pred s.size
  with Not_found -> ()

let size s = s.size

let random rs s =
  if s.size < 1 then
    None
  else
    Some (Mods.DynArray.get s.bag (Random.State.int rs s.size))

let fold f s acc =
  Tools.recti (fun acc i -> f (Mods.DynArray.get s.bag i) acc) acc s.size

let mem x s = Hashtbl.mem s.dict x
