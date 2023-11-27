let ex_file_name = "kin_phos_"

let parameters =
  [
    "Stot", "100";
    "kKS", "0.01";
    "kdKS", "1.";
    "kpS", "0.1";
    "kPS", "0.001";
    "kdPS", "0.1";
    "kuS", "0.01";
  ]

let parameters_equal =
  [
    "Stot", "100";
    "kKS", "0.01";
    "kdKS", "1.";
    "kpS", "0.1";
    "kPS", "0.01";
    "kdPS", "1.";
    "kuS", "0.1";
  ]

type format = Kappa | BNGL | BNGL_compact
type label = int
type state = U | P
type binding = FREE | Bound of int

let agent_sep_bound format =
  match format with
  | Kappa -> " , "
  | BNGL | BNGL_compact -> "."

let agent_sep_not_bound format =
  match format with
  | Kappa -> " , "
  | BNGL | BNGL_compact -> "+"

let rate_symbol format =
  match format with
  | Kappa -> "@"
  | BNGL | BNGL_compact -> ""

let internal_states = [ U; P ]
let binding_states = [ FREE, Bound 1 ]

let string_of_internal_state = function
  | U -> "u"
  | P -> "p"

let dual_internal = function
  | U -> P
  | P -> U

let string_of_binding_state = function
  | FREE -> ""
  | Bound i -> "!" ^ string_of_int i

let dual_binding = function
  | FREE -> Bound 1
  | Bound _ -> FREE

let site_name format k =
  Format.sprintf "x%s"
    (match format with
    | Kappa | BNGL -> string_of_int k
    | BNGL_compact -> "")

(********************************************************)
(*initial states*)
(********************************************************)

(*full sites information*)
let print_sites_init fmt format n =
  let rec aux k =
    if k > n then
      ()
    else (
      let () = if k > 1 then Format.fprintf fmt "," in
      let () = Format.fprintf fmt "%s" (site_name format k) in
      let () =
        match internal_states with
        | [] -> ()
        | x :: _ -> Format.fprintf fmt "~%s" (string_of_internal_state x)
      in
      aux (k + 1)
    )
  in
  let () = aux 1 in
  let () = Format.fprintf fmt ")" in
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact -> Format.fprintf fmt " Stot"
  in
  let () = Format.fprintf fmt "\n" in
  ()

(*initial states *)

let print_init fmt format n =
  match format with
  | Kappa ->
    Format.fprintf fmt "%%init: 'Stot' K(s)\n";
    Format.fprintf fmt "%%init: 'Stot' P(s)\n";
    Format.fprintf fmt "%%init: 'Stot' S(";
    print_sites_init fmt format n;
    Format.fprintf fmt "\n"
  | BNGL | BNGL_compact ->
    Format.fprintf fmt "begin seed species\n";
    Format.fprintf fmt "K(s) Stot\n";
    Format.fprintf fmt "P(s) Stot\n";
    Format.fprintf fmt "S(";
    let () = print_sites_init fmt format n in
    Format.fprintf fmt "end seed species\n\n"

(********************************************************)
(*Print Signatures*)

let print_sites fmt format n =
  let rec aux k =
    if k > n then
      ()
    else (
      let () = if k > 1 then Format.fprintf fmt "," in
      let () = Format.fprintf fmt "%s" (site_name format k) in
      let () =
        List.iter
          (fun state ->
            Format.fprintf fmt "~%s" (string_of_internal_state state))
          internal_states
      in
      aux (k + 1)
    )
  in
  let () = aux 1 in
  let () = Format.fprintf fmt ")\n" in
  ()

(*print agent and init*)
let print_signatures fmt format n =
  let () =
    match format with
    | Kappa ->
      let () = Format.fprintf fmt "%%agent: K(s) \n" in
      let () = Format.fprintf fmt "%%agent: P(s) \n" in
      let () = Format.fprintf fmt "%%agent: S(" in
      ()
    | BNGL | BNGL_compact ->
      let () = Format.fprintf fmt "begin molecule types\n" in
      let () = Format.fprintf fmt "K(s)\n" in
      let () = Format.fprintf fmt "P(s)\n" in
      let () = Format.fprintf fmt "S(" in
      ()
  in
  let () = print_sites fmt format n in
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact -> Format.fprintf fmt "end molecule types\n"
  in
  Format.fprintf fmt "\n"

(********************************************************)
(*RULE*)
(********************************************************)

let print_agent fmt s interface =
  let rec print_elements = function
    | [] -> ()
    | (site, state, _state') :: tl ->
      Format.fprintf fmt "%s" s;
      Format.fprintf fmt "(%s~%s)" site state;
      print_elements tl
  in
  print_elements interface

let print_agent_binding fmt s interface =
  let rec print_elements = function
    | [] -> ()
    | (site, _state, state') :: tl ->
      Format.fprintf fmt "%s" s;
      Format.fprintf fmt "(%s~%s)" site state';
      print_elements tl
  in
  print_elements interface

let rate format k =
  match format with
  | Kappa -> Format.sprintf "'%s'" k
  | BNGL | BNGL_compact -> k

(******************************************************)
(*rate*)

let declare_rate_list fmt format l =
  match format with
  | Kappa ->
    List.iter (fun (a, b) -> Format.fprintf fmt "%%var: '%s' %s\n" a b) l
  | BNGL | BNGL_compact ->
    let () = Format.fprintf fmt "begin parameters\n" in
    let () = List.iter (fun (a, b) -> Format.fprintf fmt "%s %s\n" a b) l in
    let () = Format.fprintf fmt "end parameters\n\n" in
    ()

let declare_rate fmt format = declare_rate_list fmt format parameters

let declare_rate_equal fmt format =
  declare_rate_list fmt format parameters_equal

(******************************************************)
(*print a list of site *)

let print_module fmt format n =
  let site = site_name format n in
  let doit k u p =
    let () =
      Format.fprintf fmt "%s(s)%sS(%s~%s) <-> %s(s!1)%sS(%s~%s!1) %s %s,%s \n" k
        (agent_sep_not_bound format)
        site u k (agent_sep_bound format) site u (rate_symbol format)
        (rate format ("k" ^ k ^ "S"))
        (rate format ("kd" ^ k ^ "S"))
    in
    let () =
      Format.fprintf fmt "%s(s!1)%sS(%s~%s!1) -> %s(s)%sS(%s~%s) %s %s \n" k
        (agent_sep_bound format) site u k
        (agent_sep_not_bound format)
        site p (rate_symbol format)
        (rate format ("k" ^ p ^ "S"))
    in
    ()
  in
  let () = doit "K" "u" "p" in
  let () = doit "P" "p" "u" in
  ()

(*let declare_rules fmt format n =
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact ->
      Format.fprintf fmt "begin reaction rules\n"
  in
  (*print BNGL*)
  let rec aux k =
    if k>n then ()
    else
      begin
        print_module fmt format k ;
        aux (k+1)
      end
  in
  let () = aux 1 in
  (*print BNGL_compact*)
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact ->
      Format.fprintf fmt
        "end reaction rules\n\ngenerate_network({overwrite=>1});"
  in
  ()*)

let declare_rules fmt format n =
  let () =
    match format with
    | Kappa ->
      let rec aux k =
        if k > n then
          ()
        else (
          print_module fmt format k;
          aux (k + 1)
        )
      in
      let () = aux 1 in
      ()
    | BNGL ->
      let () = Format.fprintf fmt "begin reaction rules\n" in
      (*print BNGL*)
      let rec aux k =
        if k > n then
          ()
        else (
          print_module fmt format k;
          aux (k + 1)
        )
      in
      let () = aux 1 in
      ()
    | BNGL_compact ->
      let () = Format.fprintf fmt "begin reaction rules\n" in
      print_module fmt format 1
  in
  (*print BNGL_compact*)
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact ->
      Format.fprintf fmt
        "end reaction rules\n\ngenerate_network({overwrite=>1});"
  in
  ()

(******************************************************)
(*main function*)

let main rep eq format n =
  let equal_rate =
    if eq then
      "equal_rate_"
    else
      ""
  in
  let ext =
    match format with
    | Kappa -> ".ka"
    | BNGL -> ".bngl"
    | BNGL_compact -> "_sym.bngl"
  in
  let file = rep ^ "/" ^ ex_file_name ^ equal_rate ^ string_of_int n ^ ext in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () =
    (if eq then
       declare_rate_equal
     else
       declare_rate)
      fmt format
  in
  let () = print_signatures fmt format n in
  let () = print_init fmt format n in
  let () = Format.fprintf fmt "\n" in
  let () = declare_rules fmt format n in
  let () = close_out channel in
  ()

let do_it rep k =
  main rep true Kappa k;
  main rep true BNGL k;
  main rep true BNGL_compact k;
  main rep false Kappa k;
  main rep false BNGL k;
  main rep false BNGL_compact k
(*******************************************************)
(*input the number of sites*)

let () =
  match Array.length Sys.argv with
  | 3 -> do_it Sys.argv.(1) (int_of_string Sys.argv.(2))
  | 4 ->
    let n = int_of_string Sys.argv.(3) in
    let rec aux k =
      if k > n then
        ()
      else (
        let () = do_it Sys.argv.(1) k in
        aux (k + 1)
      )
    in
    let () = aux (int_of_string Sys.argv.(2)) in
    ()
  | _ -> Printf.printf "Please call with two or three int arguments\n\n"
