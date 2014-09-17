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
