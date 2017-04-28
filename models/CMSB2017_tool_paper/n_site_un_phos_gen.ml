let states = ["u";"p"]

(*Print Signatures*)
let print_interface fmt lst =
  let rec print_elements = function
    | [] -> ()
    | [site,state] -> Format.fprintf fmt "%s~%s" site state
    | (site,state) :: tl -> Format.fprintf fmt "%s~%s," site state;
      print_elements tl
  in
  Format.fprintf fmt  "(";
  print_elements lst;
  Format.fprintf fmt   ")"

let print_signatures fmt n =
  let () = Format.fprintf fmt "%%agent: A(" in
  let rec aux k =
    if k>n then ()
    else
      let () =
        if k>1 then Format.fprintf fmt ","
      in
      let () = Format.fprintf fmt "x%i" k in
      let () = List.iter (Format.fprintf fmt "~%s") states in
      aux (k+1)
  in
  let () = aux 1 in
  let () = Format.fprintf fmt  ")\n\n" in
  ()

let print_agent fmt interface =
  Format.fprintf fmt "A";
  print_interface fmt interface

let print_init fmt =
  Format.fprintf fmt "%%init: 100 ";
  print_agent fmt [];
  Format.fprintf fmt "\n"

let site_list n =
  let rec aux k l =
    if k=0 then l
    else aux (k-1) (("x"^(string_of_int k))::l)
  in aux n []



let potential_valuations list =
  let list = List.rev list in
  let rec aux remaining_site partial_valuations =
    match remaining_site with
    | [] -> partial_valuations
    | h::tl ->
      let partial_valuations =
        List.fold_left
          (fun extended_partial_valuations state ->
             List.fold_left
               (fun extended_partial_valuations partial_valuation ->
                  ((h,state)::partial_valuation)::extended_partial_valuations)
          extended_partial_valuations
          partial_valuations)
          []
          states
      in aux tl partial_valuations
  in
  aux list [[]]

let flip (s,state) =
  if state="u" then "p",(s,"p")
  else if state = "p" then "u",(s,"u")
  else "",(s,state)

let count_p interface =
  List.fold_left
    (fun n (_,state) -> if state="p" then n+1 else n) 0 interface

let rate_of kind interface =
  let n = count_p interface in
  "k"^kind^(string_of_int n)

let print_rule fmt interface interface_post rate =
  print_agent fmt interface ;
  Format.fprintf fmt " -> " ;
  print_agent fmt interface_post ;
  Format.fprintf fmt " @%s\n" rate

let rec exp i j =
  if j=0 then 1
  else if j=1 then i
  else let q,r = j/2, j mod 2 in
    let root = exp i q in
    let square = root*root in
    if r=0 then square
    else i*square

let rate state k =
  if state = "u" then 3*(exp 5 k)
  else if state = "p" then 2*(exp 7 k)
  else 0

let declare_rate fmt n =
  let rec aux k =
    if k=n then ()
    else
      let () = Format.fprintf fmt "%%var: 'kp%i' %i\n" k (rate "u" k) in
      let () = Format.fprintf fmt "%%var: 'ku%i' %i\n" (k+1) (rate "p" (k+1)) in
      aux (k+1)
  in
  aux 0

let deal_with_one_valuation fmt interface =
  let rec aux suffix prefix =
    match suffix with
    | [] -> ()
    | h::tl ->
      let kind,h' = flip h in
      let interface_post =
        List.fold_left (fun list elt -> elt::list) (h'::prefix) tl
      in
      let rate = rate_of kind interface in
      let () = print_rule fmt interface interface_post rate in
      aux tl (h::prefix)
  in
  aux (List.rev interface) []

let main n =
  let file = "ex_"^(string_of_int n)^".ka" in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () = print_signatures fmt n in
  let () = print_init fmt in
  let () = Format.fprintf fmt "\n\n" in
  let () = declare_rate fmt n in
  let () = Format.fprintf fmt "\n\n" in
  let sites = site_list n in
  let potential_valuations = potential_valuations sites in
  let () =
    List.iter
      (fun valuation ->
         deal_with_one_valuation fmt valuation;
         Format.fprintf fmt "\n\n")
      potential_valuations
  in
  let () = close_out channel in
  ()


let () =
  match Array.length Sys.argv with
  | 2 -> main (int_of_string Sys.argv.(1))
  | 3 ->
    let n = int_of_string Sys.argv.(2) in
    let rec aux k =
      if k>n then ()
      else
        let () = main k in aux (k+1)
    in aux (int_of_string Sys.argv.(1))
  | _ -> Printf.printf "Please call with one or two int arguments\n\n"
