(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type position = { chr: int; line: int }
type range = { file : string ;
               from_position: position;
               to_position: position }

type t = range

type 'a annot = 'a * t
type 'a maybe = ?pos:t -> 'a

let of_pos start_location end_location =
  let () = assert
    (start_location.Lexing.pos_fname = end_location.Lexing.pos_fname) in
  {
    file = start_location.Lexing.pos_fname;
    from_position = {
      chr = start_location.Lexing.pos_cnum - start_location.Lexing.pos_bol;
      line = start_location.Lexing.pos_lnum;
    };
    to_position = {
      chr = end_location.Lexing.pos_cnum - end_location.Lexing.pos_bol;
      line = end_location.Lexing.pos_lnum;
    };
  }

let dummy_position = {
  chr =  Lexing.dummy_pos.Lexing.pos_cnum - Lexing.dummy_pos.Lexing.pos_bol;
  line = Lexing.dummy_pos.Lexing.pos_lnum;
}

let dummy = {
  file = Lexing.dummy_pos.Lexing.pos_fname;
  from_position = dummy_position;
  to_position = dummy_position;
}

let dummy_annot x = (x, dummy)
let has_dummy_annot (_,loc) =
  loc.file = Lexing.dummy_pos.Lexing.pos_fname &&
  loc.from_position = dummy_position &&
  loc.to_position = dummy_position

let print f loc =
  let pr_f f =
    if loc.file <> "" then
      Format.fprintf f "File \"%s\", " loc.file in
  let pr_l f =
    if loc.from_position.line = loc.to_position.line
    then Format.fprintf f "line %i" loc.from_position.line
    else Format.fprintf f "lines %i-%i"
        loc.from_position.line loc.to_position.line
  in
  Format.fprintf f "%t%t, characters %i-%i:" pr_f pr_l
                 loc.from_position.chr loc.to_position.chr

let to_string loc = Format.asprintf "@[<h>%a@]" print loc

let print_annot pr f (x,l) =
  Format.fprintf f "%a@ %a" print l pr x

let position_to_yojson pos =
  `Assoc [ "line",`Int pos.line; "chr",`Int pos.chr ]

let to_yojson loc =
  `Assoc
    [
      "file", `String loc.file;
      "from_pos", position_to_yojson loc.from_position;
      "to_pos", position_to_yojson loc.to_position;
    ]

let to_compact_yojson decls loc =
  `Assoc
    ((if loc.from_position.line <> loc.to_position.line
      then fun l -> ("eline", `Int loc.to_position.line) :: l
      else fun l -> l)
       [
         ("file",
          match Mods.StringMap.find_option loc.file decls with
          | Some i -> `Int i
          | None -> `String loc.file);
         "bline", `Int loc.from_position.line;
         "bchr", `Int loc.from_position.chr;
         "echr", `Int loc.to_position.chr;
       ])

let position_of_json = function
  | `Assoc [ "line", `Int line; "chr", `Int  chr ] |
    `Assoc [ "chr", `Int chr; "line", `Int line ] ->
    { line; chr }
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a position",x))

let of_yojson = function
  | `Assoc [ "file", `String file; "from_pos", fr; "to_pos", t ] |
    `Assoc [ "file", `String file; "to_pos", t; "from_pos", fr ] |
    `Assoc [ "from_pos", fr; "to_pos", t; "file", `String file ] |
    `Assoc [ "to_pos", t; "from_pos", fr; "file", `String file ] |
    `Assoc [ "from_pos", fr; "file", `String file; "to_pos", t ] |
    `Assoc [ "to_pos", t; "file", `String file; "from_pos", fr ] -> {
      file;
      from_position = position_of_json fr;
      to_position = position_of_json t
    }
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x))

let of_compact_yojson decls = function
  | `Assoc l as x when List.length l <= 5 ->
    begin
      try
        let file = match List.assoc "file" l with
          | `String x -> x
          | `Int i -> decls.(i)
          | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x)) in
        let of_line = match List.assoc "bline" l with
          | `Int i -> i
          | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x)) in
        let of_chr = match List.assoc "bchr" l with
          | `Int i -> i
          | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x)) in
        let to_chr = match List.assoc "echr" l with
          | `Int i -> i
          | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x)) in
        let to_line = match Yojson.Basic.Util.member "eline" x with
          | `Null -> of_line
          | `Int i -> i
          | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x)) in
        {
          file;
          from_position = { line = of_line; chr = of_chr };
          to_position = { line = to_line; chr = to_chr };
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect AST arrow_notation",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x))

let annot_to_yojson ?filenames f (x,l) =
  `Assoc [ "val", f x;
           "loc",
           match filenames with
           | None -> to_yojson l
           | Some decls -> to_compact_yojson decls l]
let annot_of_yojson ?filenames f = function
  | `Assoc [ "val", x; "loc", l ] | `Assoc [ "loc", l; "val", x ] ->
    (f x,
     match filenames with
     | None -> of_yojson l
     | Some decls -> of_compact_yojson decls l)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location",x))

let write_range ob f = Yojson.Basic.to_outbuf ob (to_yojson f)

let string_of_range ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_range ob x;
  Bi_outbuf.contents ob

let read_range p lb =
  of_yojson (Yojson.Basic.from_lexbuf ~stream:true p lb)

let range_of_string s =
  read_range (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
