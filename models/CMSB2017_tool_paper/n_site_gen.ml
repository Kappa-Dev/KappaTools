

let states = (["u";"p"], ["u!1"; "p!1"])
(*let states_binding = ["u!1";"p!1"]*)

(********************************************************)
(*initial states*)
(********************************************************)

(*full sites information*)
let print_sites_init fmt n =
  let rec aux k =
    if k>n then ()
    else
      let () =
        if k>1 then Format.fprintf fmt ","
      in
      let () = Format.fprintf fmt "x%i" k in
      let (unbinding, _binding) = states in
      let () =
        match unbinding with
        | [] -> ()
        | x :: _ ->  Format.fprintf fmt "~%s" x
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
  print_sites_init fmt n

(********************************************************)
(*Print Signatures*)

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

let print_rule fmt interface interface1 f_a =
  let () =
    let (a,b) = f_a in
    Format.fprintf fmt "%s, " b;
    print_agent fmt "S" interface;
    Format.fprintf fmt " <-> ";
    Format.fprintf fmt "%s, " a;
    print_agent fmt "S" interface1;
    let () =
      if b = "E(s)" then
        Format.fprintf fmt " @'kES','kdES'\n"
      else Format.fprintf fmt " @'kFS','kdFS' \n"
    in
    ()
  in
  ()

let print_rule_binding fmt interface interface1 f_a =
  let () =
    let (a, b) = f_a in
    Format.fprintf fmt "%s, " a;
    print_agent fmt "S" interface;
    Format.fprintf fmt " -> ";
    Format.fprintf fmt "%s, " b;
    print_agent_binding fmt "S" interface1;
    let () =
      if b = "E(s)" then
        Format.fprintf fmt " @'kpS'\n"
      else Format.fprintf fmt " @'kuS'"
    in
    ()
  in
  ()

(******************************************************)
(*rate*)

let declare_rate fmt =
  let () = Format.fprintf fmt "%%var: 'kES' 0.01\n" in
  let () = Format.fprintf fmt "%%var: 'kdES' 1\n" in
  let () = Format.fprintf fmt "%%var: 'kpS' 0.1\n" in
  let () = Format.fprintf fmt "%%var: 'kFS' 0.001\n" in
  let () = Format.fprintf fmt "%%var: 'kdFS' 0.1\n" in
  let () = Format.fprintf fmt "%%var: 'kuS' 0.01\n" in
  ()

let declare_rate_equal fmt =
  let () = Format.fprintf fmt "%%var: 'kES' 0.01\n" in
  let () = Format.fprintf fmt "%%var: 'kdES' 1\n" in
  let () = Format.fprintf fmt "%%var: 'kpS' 0.1\n" in
  let () = Format.fprintf fmt "%%var: 'kFS' 0.01\n" in
  let () = Format.fprintf fmt "%%var: 'kdFS' 1\n" in
  let () = Format.fprintf fmt "%%var: 'kuS' 0.1\n"in
  ()

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
  if state = "u" then (s, "u!1", "p")
  else if state = "p" then (s, "p!1", "u")
  else if state' = "p!1" then (s, "p", "p")
  else if state' = "u!1" then (s, "u", "u")
  else (s, state, state')

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
      let flip = flip_binding (h,s,s') in
      let f_a = first_agent  (s, s') in
      let interface1 = flip :: tl in
      let () = print_rule fmt interface interface1 f_a  in
      let () = Format.fprintf fmt "\n" in
      let () = print_rule_binding fmt interface1 interface1 f_a in
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
  let () = declare_rate fmt in
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

let main2 n =
  let file = "ex_mul_equal_rate_"^(string_of_int n)^".ka" in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () = print_signatures fmt n in
  let () = print_init fmt n in
  let () = declare_rate_equal fmt in
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

let () =
  match Array.length Sys.argv with
  | 2 -> main2 (int_of_string Sys.argv.(1))
  | 3 ->
    let n = int_of_string Sys.argv.(2) in
    let rec aux k =
      if k>n then ()
      else
        let () = main2 k in aux (k+1)
    in aux (int_of_string Sys.argv.(1))
  | _ -> Printf.printf "Please call with one or two int arguments\n\n"
