open Printf

let list pr_sep pr_el f l =
  let rec aux f = function
  | [] -> ()
  | [el] -> pr_el f el
  | h :: t -> fprintf f "%a%t%a" pr_el h pr_sep aux t
  in fprintf f "[%a]" aux l

let set elements pr_sep pr_el f set =
  fprintf f"{%a}" (list pr_sep pr_el) (elements set)

let string f s = fprintf f "%s" s
let int f i = fprintf f "%i" i
let comma f = fprintf f ", "
let colon f = fprintf f "; "
let space f = fprintf f " "

let error pr (x,(beg_pos,end_pos)) =
  let () = assert (beg_pos.Lexing.pos_fname = end_pos.Lexing.pos_fname) in
  let pr_l f =
    if beg_pos.Lexing.pos_lnum = end_pos.Lexing.pos_lnum
    then fprintf f "line %i" beg_pos.Lexing.pos_lnum
    else fprintf f "lines %i-%i" beg_pos.Lexing.pos_lnum end_pos.Lexing.pos_lnum
  in
  eprintf "File \"%s\", %t, characters %i-%i:@ %a" beg_pos.Lexing.pos_fname pr_l
	  (beg_pos.Lexing.pos_cnum - beg_pos.Lexing.pos_bol)
	  (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol) pr x
