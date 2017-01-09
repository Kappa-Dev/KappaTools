(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = Lexing.position * Lexing.position
type 'a annot = 'a * t
type 'a maybe = ?pos:t -> 'a

let of_pos x y = (x,y)
let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

let dummy_annot x = (x, dummy)
let has_dummy_annot (_,(b_pos,e_pos)) =
  b_pos = Lexing.dummy_pos &&
    (e_pos = Lexing.dummy_pos || failwith "half dummy_pos")

let print f (beg_pos,end_pos) =
  let () = assert (beg_pos.Lexing.pos_fname = end_pos.Lexing.pos_fname) in
  let pr_f f =
    if beg_pos.Lexing.pos_fname <> "" then
      Format.fprintf f "File \"%s\", " beg_pos.Lexing.pos_fname in
  let pr_l f =
    if beg_pos.Lexing.pos_lnum = end_pos.Lexing.pos_lnum
    then Format.fprintf f "line %i" beg_pos.Lexing.pos_lnum
    else Format.fprintf f "lines %i-%i" beg_pos.Lexing.pos_lnum
                        end_pos.Lexing.pos_lnum
  in
  Format.fprintf f "%t%t, characters %i-%i:" pr_f pr_l
                 (beg_pos.Lexing.pos_cnum - beg_pos.Lexing.pos_bol)
                 (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

let to_string (beg_pos,end_pos) =
  let () = assert (beg_pos.Lexing.pos_fname = end_pos.Lexing.pos_fname) in
  let pr_f =
    if beg_pos.Lexing.pos_fname <> "" then
      Printf.sprintf "File \"%s\", " beg_pos.Lexing.pos_fname
    else "" in
  let pr_l =
    if beg_pos.Lexing.pos_lnum = end_pos.Lexing.pos_lnum
    then Printf.sprintf "line %i" beg_pos.Lexing.pos_lnum
    else Printf.sprintf "lines %i-%i" beg_pos.Lexing.pos_lnum
                        end_pos.Lexing.pos_lnum
  in
  Printf.sprintf "%s%s, characters %i-%i:" pr_f pr_l
                 (beg_pos.Lexing.pos_cnum - beg_pos.Lexing.pos_bol)
                 (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)


let print_annot pr f (x,l) =
  Format.fprintf f "%a@ %a" print l pr x

type position = { chr: int; line: int }
type range = { file : string ;
               from_position: position;
               to_position: position }

let to_range (location : t) : range =
  let (start_location,end_location) = location in
  { file = start_location.Lexing.pos_fname;
    from_position =
      { chr =  start_location.Lexing.pos_bol
      ; line = start_location.Lexing.pos_lnum };
    to_position =
     { chr =  end_location.Lexing.pos_bol
     ; line = end_location.Lexing.pos_lnum } }

let to_json loc =
  let (start,stop) = loc in
  `Assoc
    [
      "file", `String start.Lexing.pos_fname;
      "from_pos",
      `Assoc ["line",`Int start.Lexing.pos_lnum;
              "chr",`Int (start.Lexing.pos_cnum - start.Lexing.pos_bol)];
      "to_pos",
      `Assoc ["line",`Int stop.Lexing.pos_lnum;
              "chr",`Int (stop.Lexing.pos_cnum - stop.Lexing.pos_bol)];
    ]

let position_of_json file = function
  | `Assoc [ "line", `Int l; "chr", `Int  c ] |
    `Assoc [ "chr", `Int c; "line", `Int l ] ->
    {
      Lexing.pos_fname = file;
      Lexing.pos_lnum = l;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = c;
    }
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a position",x))
let of_json = function
  | `Assoc [ "file", `String f; "from_pos", fr; "to_pos", t ] |
    `Assoc [ "file", `String f; "to_pos", t; "from_pos", fr ] |
    `Assoc [ "from_pos", fr; "to_pos", t; "file", `String f ] |
    `Assoc [ "to_pos", t; "from_pos", fr; "file", `String f ] |
    `Assoc [ "from_pos", fr; "file", `String f; "to_pos", t ] |
    `Assoc [ "to_pos", t; "file", `String f; "from_pos", fr ] ->
    (position_of_json f fr,position_of_json f t)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x))

let annot_to_json f (x,l) =
  `Assoc [ "val", f x; "loc", to_json l]
let annot_of_json f = function
  | `Assoc [ "val", x; "loc", l ] | `Assoc [ "loc", l; "val", x ] ->
    (f x, of_json l)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x))
