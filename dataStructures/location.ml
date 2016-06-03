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
