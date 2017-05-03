

let states = (["u";"p"], ["u!1"; "p!1"])
(*let states_binding = ["u!1";"p!1"]*)

(********************************************************)
(*initial states*)
(********************************************************)

(*full sites information*)
let print_sites fmt n =
  let rec aux k =
    if k>n then ()
    else
      let () =
        if k>1 then Format.fprintf fmt ","
      in
      let () = Format.fprintf fmt "x%i" k in
      let (unbinding, binding) = states in
      let () =
        List.iter (Format.fprintf fmt "~%s") unbinding
      in
      aux (k+1)
  in
  let () = aux 1 in
  let () = Format.fprintf fmt  ")\n\n" in
  ()

(*initial states *)

let print_init fmt n =
  Format.fprintf fmt "%%init: 100 E(s)\n";
  Format.fprintf fmt "%%init: 100 F(s)\n";
  Format.fprintf fmt "%%init: 100 S(";
  print_sites fmt n

(********************************************************)
(*Print Signatures*)

(*print agent and init*)
let print_signatures fmt n =
  let () = Format.fprintf fmt "%%agent: E(s) \n" in
  let () = Format.fprintf fmt "%%agent: F(s) \n" in
  let () = Format.fprintf fmt "%%agent: S(" in
  let () = print_sites fmt n in
  ()

(********************************************************)
(*RULE*)
(********************************************************)

let print_agent fmt s interface =
  let rec print_elements = function
    | [] -> ()
    | (site, state, state') :: tl ->
      Format.fprintf fmt "%s" s;
      Format.fprintf fmt "(%s~%s)" site state;
      print_elements tl
  in
  print_elements interface

  let print_agent_binding fmt s interface =
    let rec print_elements = function
      | [] -> ()
      | (site, state, state') :: tl ->
        Format.fprintf fmt "%s" s;
        Format.fprintf fmt "(%s~%s)" site state';
        print_elements tl
    in
    print_elements interface

let print_rule fmt interface interface1 f_a rate rate1=
  let () =
    let (a,b) = f_a in
    Format.fprintf fmt "%s, " b;
    print_agent fmt "S" interface;
    Format.fprintf fmt " <-> ";
    Format.fprintf fmt "%s, " a;
    print_agent fmt "S" interface1;
    Format.fprintf fmt " @%s{%s},%s\n" rate1 rate rate1
  in
  ()

let print_rule_binding fmt interface interface1 f_a rate  =
  let () =
    let (a, b) = f_a in
    Format.fprintf fmt "%s, " a;
    print_agent fmt "S" interface;
    Format.fprintf fmt " -> ";
    Format.fprintf fmt "%s, " b;
    print_agent_binding fmt "S" interface1;
    Format.fprintf fmt " @%s\n" rate
  in
  ()

(******************************************************)
(*rate*)

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

let count_p interface =
  List.fold_left
    (fun n (_,state,_) -> if state="p" then n+1 else n) 0 interface

let rate_of kind interface =
  let n = count_p interface in
  "k"^kind^(string_of_int n)

(******************************************************)
(*deal with loop *)

let potential_valuations fmt list =
  let list = List.rev list in
  let rec aux remaining_site partial_valuations =
    match remaining_site with
    | [] -> partial_valuations
    | h::tl ->
      let (unbinding, binding) = states in
      let p =
        List.fold_left (fun ext state ->
            List.fold_left (fun ex_ext state' ->
                [(h, state, state')] :: ext
              ) ext binding
          ) partial_valuations unbinding
      in
      aux tl p
  in
  aux list [[]]

let flip_binding (s,state, state') =
  if state = "u" then "p", (s, "u!1", "p")
  else if state = "p" then "u" , (s, "p!1", "u")
  else if state' = "p!1" then "u!1", (s, "p", "p")
  else if state' = "u!1" then "p!1", (s, "u", "u")
  else "", (s, state, state')

let first_agent  (state, state') =
  if state = "p" then ("F(s!1)", "F(s)")
  else if state = "u" then ("E(s!1)", "E(s)")
  else if state' = "p!1" then ("F(s)", "F(s!1)")
  else if state' = "u!1" then ("E(s)", "E(s!1)")
  else "",""

let deal_with_one_valuation fmt interface =
  let rec aux suffix prefix =
    match suffix with
    | [] -> ()
    | (h,s,s')::tl ->
      let kind,flip = flip_binding (h,s,s') in
      let f_a = first_agent  (s, s') in
      let interface1 = flip :: tl in
      let rate1 = rate_of kind interface1 in
      let rate = rate_of kind interface in
      let () = print_rule fmt interface interface1 f_a rate rate1 in
      let () = Format.fprintf fmt "\n" in
      let () = print_rule_binding fmt interface1 interface1 f_a
          rate in
      aux tl (h::prefix)
  in
  aux (List.rev interface) []

(******************************************************)
(*print a list of site *)

let site_list n =
  let rec aux k l =
    if k=0 then l
    else aux (k-1) (("x"^(string_of_int k))::l)
  in aux n []

(******************************************************)
(*main function*)

let main n =
  let file = "ex_mul_"^(string_of_int n)^".ka" in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () = print_signatures fmt n in
  let () = print_init fmt n in
  let () = declare_rate fmt n in
  let () = Format.fprintf fmt "\n" in
  let sites = site_list n in
  let potential_valuations = potential_valuations fmt sites in
  let () =
    List.iter
      (fun valuation ->
         deal_with_one_valuation fmt valuation;
         Format.fprintf fmt "\n\n"
      )
      potential_valuations
  in
  let () = close_out channel in
  ()

(*******************************************************)
(*input the number of sites*)

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
