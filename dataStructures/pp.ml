open Format

let list pr_sep pr_el f l =
  let rec aux f = function
  | [] -> ()
  | [el] -> pr_el f el
  | h :: t -> fprintf f "%a%t%a" pr_el h pr_sep aux t
  in aux f l

let set elements pr_sep pr_el f set =
  list pr_sep pr_el f (elements set)

let hashtbl pr_sep pr_el f tbl =
  list pr_sep pr_el f (Hashtbl.fold (fun a b l -> (a,b)::l) tbl [])

let comma f = fprintf f ",@ "
let colon f = fprintf f ";@ "
let space f = pp_print_space f ()
let empty f = fprintf f ""
let option pr f = function
  | None -> ()
  | Some x -> fprintf f "@ %a" pr x

let array pr_el f a =
  let rec aux i f =
    if i < Array.length a then
      let () = Format.fprintf f "%i:%a" i pr_el a.(i) in
      if i < Array.length a - 1 then
	Format.fprintf f ";@,%t" (aux (succ i))
  in Format.fprintf f "[|%t|]" (aux 0)

let position f (beg_pos,end_pos) =
  let () = assert (beg_pos.Lexing.pos_fname = end_pos.Lexing.pos_fname) in
  let pr_l f =
    if beg_pos.Lexing.pos_lnum = end_pos.Lexing.pos_lnum
    then fprintf f "line %i" beg_pos.Lexing.pos_lnum
    else fprintf f "lines %i-%i" beg_pos.Lexing.pos_lnum end_pos.Lexing.pos_lnum
  in
  fprintf f "File \"%s\", %t, characters %i-%i:" beg_pos.Lexing.pos_fname pr_l
	  (beg_pos.Lexing.pos_cnum - beg_pos.Lexing.pos_bol)
	  (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

let error pr (x,pos) =
  eprintf "%a:@ %a@." position pos pr x

let list_to_string pr_sep pr_el () l =
  let rec aux () = function
  | [] -> ""
  | [el] -> pr_el () el
  | h :: t -> sprintf "%a%t%a" pr_el h pr_sep aux t
  in aux () l

let set_to_string elements pr_sep pr_el () set =
  list_to_string pr_sep pr_el () (elements set)
