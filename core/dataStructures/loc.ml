(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type position = { chr: int; line: int }
type t = { file: string; from_position: position; to_position: position }
type 'a annoted = 'a * t

let v (v, _) = v
let get_annot (_, annot) = annot
let copy_annot (_, loc) a = a, loc
let map_annot f (a, loc) = f a, loc

let of_pos start_location end_location =
  let () =
    assert (start_location.Lexing.pos_fname = end_location.Lexing.pos_fname)
  in
  {
    file = start_location.Lexing.pos_fname;
    from_position =
      {
        chr = start_location.Lexing.pos_cnum - start_location.Lexing.pos_bol;
        line = start_location.Lexing.pos_lnum;
      };
    to_position =
      {
        chr = end_location.Lexing.pos_cnum - end_location.Lexing.pos_bol;
        line = end_location.Lexing.pos_lnum;
      };
  }

let dummy_position =
  {
    chr = Lexing.dummy_pos.Lexing.pos_cnum - Lexing.dummy_pos.Lexing.pos_bol;
    line = Lexing.dummy_pos.Lexing.pos_lnum;
  }

let dummy =
  {
    file = Lexing.dummy_pos.Lexing.pos_fname;
    from_position = dummy_position;
    to_position = dummy_position;
  }

let annot_with_dummy x = x, dummy

let is_dummy loc =
  loc.file = Lexing.dummy_pos.Lexing.pos_fname
  && loc.from_position = dummy_position
  && loc.to_position = dummy_position

let is_annoted_with_dummy (_, loc) = is_dummy loc

let print f loc =
  let pr_f f =
    if loc.file <> "" then Format.fprintf f "File \"%s\", " loc.file
  in
  let pr_l f =
    if loc.from_position.line = loc.to_position.line then
      Format.fprintf f "line %i" loc.from_position.line
    else
      Format.fprintf f "lines %i-%i" loc.from_position.line loc.to_position.line
  in
  Format.fprintf f "%t%t, characters %i-%i:" pr_f pr_l loc.from_position.chr
    loc.to_position.chr

let to_string loc = Format.asprintf "@[<h>%a@]" print loc
let print_annoted pr f (x, l) = Format.fprintf f "%a@ %a" print l pr x

let read_position p lb =
  match Yojson.Basic.from_lexbuf ~stream:true p lb with
  | `Assoc [ ("line", `Int line); ("chr", `Int chr) ]
  | `Assoc [ ("chr", `Int chr); ("line", `Int line) ] ->
    { line; chr }
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid position", x))

let write_position ob { line; chr } =
  Yojson.write_assoc ob [ "line", `Int line; "chr", `Int chr ]

let to_compact_yojson decls loc =
  if is_dummy loc then
    `Null
  else
    `Assoc
      ((if loc.from_position.line <> loc.to_position.line then
          fun l ->
        ("eline", `Int loc.to_position.line) :: l
        else
          fun l ->
        l)
         [
           ( "file",
             match
               Option_util.bind (Mods.StringMap.find_option loc.file) decls
             with
             | Some i -> `Int i
             | None -> `String loc.file );
           "bline", `Int loc.from_position.line;
           "bchr", `Int loc.from_position.chr;
           "echr", `Int loc.to_position.chr;
         ])

let of_compact_yojson ?(filenames = [||]) = function
  | `Null -> dummy
  | `Assoc l as x when List.length l <= 5 ->
    (try
       let file =
         match List.assoc "file" l with
         | `String x -> x
         | `Int i -> filenames.(i)
         | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))
       in
       let of_line =
         match List.assoc "bline" l with
         | `Int i -> i
         | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))
       in
       let of_chr =
         match List.assoc "bchr" l with
         | `Int i -> i
         | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))
       in
       let to_chr =
         match List.assoc "echr" l with
         | `Int i -> i
         | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))
       in
       let to_line =
         match Yojson.Basic.Util.member "eline" x with
         | `Null -> of_line
         | `Int i -> i
         | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))
       in
       {
         file;
         from_position = { line = of_line; chr = of_chr };
         to_position = { line = to_line; chr = to_chr };
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Incorrect AST arrow_notation", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))

let yojson_of_annoted ?filenames f (x, l) =
  let jp = to_compact_yojson filenames l in
  if jp = `Null then
    `Assoc [ "val", f x ]
  else
    `Assoc [ "val", f x; "loc", jp ]

let annoted_of_yojson ?filenames f = function
  | `Assoc [ ("val", x); ("loc", l) ] | `Assoc [ ("loc", l); ("val", x) ] ->
    f x, of_compact_yojson ?filenames l
  | `Assoc [ ("val", x) ] -> f x, dummy
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid location", x))

let write_range ob f = Yojson.Basic.to_buffer ob (to_compact_yojson None f)

let string_of_range ?(len = 1024) x =
  let ob = Buffer.create len in
  write_range ob x;
  Buffer.contents ob

let read_range p lb =
  of_compact_yojson ?filenames:None (Yojson.Basic.from_lexbuf ~stream:true p lb)

let range_of_string s =
  read_range (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let is_included_in file { line; chr } range =
  file = range.file
  && line >= range.from_position.line
  && line <= range.to_position.line
  && (line <> range.from_position.line || chr >= range.from_position.chr)
  && (line <> range.to_position.line || chr <= range.to_position.chr)

let merge b e =
  if is_dummy b then
    e
  else if is_dummy e then
    b
  else (
    let () = assert (b.file = e.file) in
    {
      file = b.file;
      from_position = b.from_position;
      to_position = e.to_position;
    }
  )

(** Annoted yojson helpers *)

let string_annoted_to_json ~filenames =
  yojson_of_annoted ~filenames JsonUtil.of_string

let string_annoted_of_json ~filenames =
  annoted_of_yojson ~filenames (JsonUtil.to_string ?error_msg:None)

let string_option_annoted_to_json ~filenames =
  yojson_of_annoted ~filenames (JsonUtil.of_option JsonUtil.of_string)

let string_option_annoted_of_json ~filenames =
  annoted_of_yojson ~filenames
    (JsonUtil.to_option (JsonUtil.to_string ?error_msg:None))

type 'a rename_pos = (t -> t option)-> 'a -> 'a 
type ('parameters,'errors,'a) rename_pos_with_errors = 'parameters -> 'errors -> (t -> t option)-> 'a -> 'errors * 'a 

let rename_loc rename pos = 
   match rename pos with 
    | None ->  pos 
    | Some pos -> pos

let rename_pos rename_pos rename (elt,pos) = 
  (rename_pos rename elt,rename_loc rename pos) 

let rename_pos_with_errors rename_pos_with_errors parameters error rename (elt,pos) = 
  let error, elt = rename_pos_with_errors parameters error rename elt in 
  error, (elt, rename_loc rename pos)

let rename_pos_flat rename (elt,pos) = 
  rename_pos (fun _ a -> a) rename  (elt,pos) 

let rename_pos_flat_with_errors parameters errors rename (elt,pos) =       
   rename_pos_with_errors  (fun _ e _ a -> e, a) parameters errors rename (elt,pos)
let rename_pos_opt rename_pos rename elt_opt = 
  match elt_opt with 
  | None -> None 
  | Some a -> Some (rename_pos rename a) 

let rename_pos_opt_with_errors rename_pos parameters errors rename elt_opt = 
  match elt_opt with 
  | None -> errors, None 
  | Some a -> 
    let errors, elt = rename_pos parameters errors rename a in 
    errors, Some (elt) 
    

let rename_pos_list rename_pos rename list = 
  List.rev_map 
  (rename_pos rename)
  (List.rev list)

let rename_pos_list_with_errors rename_pos_with_errors parameters errors rename list =
  List.fold_left 
    (fun (errors, l) elt -> 
        let errors, elt = 
          rename_pos_with_errors parameters errors rename elt
        in (errors, elt::l))
   (errors, []) (List.rev list) 

let rename_pos_pair rename_pos1 rename_pos2 rename p = 
  (rename_pos1 rename (fst p), 
   rename_pos2 rename (snd p))
   
let rename_pos_pair_with_errors rename_pos1 rename_pos2 parameters errors rename p = 
  let errors, a = rename_pos1 parameters errors rename (fst p) in 
  let errors, b = rename_pos2 parameters errors rename (snd p) in 
  errors, (a,b) 
   
type 'a diff_pos = ('a -> 'a -> (t*t) list -> (t*t) list)

let diff_pos_empty = [] 

let diff_pos loc loc' l = 
  if loc == loc' || loc = loc' 
  then l 
  else (loc,loc')::l 


let diff_pos_flat _ _ l = l 

let diff_pos_annoted diff_pos' (a,pos) (a',pos') l = 
  diff_pos' a a' (diff_pos pos pos' l)

let diff_pos_opt diff_pos a a' l = 
  match a,a' with 
  | Some a,Some a' -> diff_pos a a' l 
  | None, None -> l 
  | Some _, None | None, Some _ -> raise (invalid_arg "diff_pos_opt")

let diff_pos_pair diff_pos diff_pos' (a,b) (a',b') l = 
  diff_pos' b b' (diff_pos a a' l)

let diff_pos_list diff_pos l l' acc = 
  List.fold_left2 
    (fun acc a a' -> diff_pos a a' acc)
    acc l l' 

module LocMap = 
  Map.Make (struct 
    type t_above = t
    type t = t_above  
   (** type t = t *)
    
      let compare=compare 
end)

let fun_of_list l = 
  let m = 
    List.fold_left 
      (fun m (a,b) -> LocMap.add a b m) 
      LocMap.empty l 
  in 
  (fun elt -> LocMap.find_opt elt m)

  