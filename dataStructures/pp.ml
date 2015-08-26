open Format

let listi ?(trailing=(fun _ -> ())) pr_sep pr_el f l =
  let rec aux acc f = function
    | [] -> ()
    | [el] -> fprintf f "%a%t" (pr_el acc) el trailing
    | h :: t -> fprintf f "%a%t%a" (pr_el acc) h pr_sep (aux (succ acc)) t
  in aux 0 f l

let list ?trailing pr_sep pr_el f l =
  listi ?trailing pr_sep (fun _ f x -> pr_el f x) f l

let set ?trailing elements pr_sep pr_el f set =
  list ?trailing pr_sep pr_el f (elements set)

let hashtbl pr_sep pr_el f tbl =
  list pr_sep pr_el f (Hashtbl.fold (fun a b l -> (a,b)::l) tbl [])

let bottom f = Format.pp_print_string f "\xE2\x8A\xA5"
let nu f = Format.pp_print_string f "\xCE\xBD"
let comma f = fprintf f ",@ "
let colon f = fprintf f ";@ "
let space f = pp_print_space f ()
let cut f = pp_print_cut f ()
let empty f = fprintf f ""
let option pr f = function
  | None -> ()
  | Some x -> fprintf f "@ %a" pr x

let array ?(trailing=(fun _ -> ())) pr_sep pr_el f a =
  let rec aux i f =
    if i < Array.length a then
      let () = Format.fprintf f "%a" (pr_el i) a.(i) in
      if i < Array.length a - 1 then
	Format.fprintf f "%t%t" pr_sep (aux (succ i))
      else if i > 0 then trailing f
  in aux 0 f

let plain_array pr_el f a =
  let rec aux i f =
    if i < Array.length a then
      let () = Format.fprintf f "%i:%a" i pr_el a.(i) in
      if i < Array.length a - 1 then
	Format.fprintf f ";@,%t" (aux (succ i))
  in Format.fprintf f "[|%t|]" (aux 0)

let error pr x =
  eprintf "%a@." (Location.print_annot pr) x

let list_to_string pr_sep pr_el () l =
  let rec aux () = function
  | [] -> ""
  | [el] -> pr_el () el
  | h :: t -> sprintf "%a%t%a" pr_el h pr_sep aux t
  in aux () l

let set_to_string elements pr_sep pr_el () set =
  list_to_string pr_sep pr_el () (elements set)
