let ex_name = "n_phos_sites_"

type format = Kappa | BNGL | BNGL_compact
type label = int
type state = U | P

let states = [ U; P ]

let string_of_state = function
  | U -> "u"
  | P -> "p"

let rate_symbol format =
  match format with
  | Kappa -> "@"
  | BNGL | BNGL_compact -> ""

let dual_internal = function
  | U -> P
  | P -> U

(*Print Signatures*)
let print_interface fmt lst =
  let rec print_elements = function
    | [] -> ()
    | [ (site, state) ] ->
      Format.fprintf fmt "%s~%s" site (string_of_state state)
    | (site, state) :: tl ->
      Format.fprintf fmt "%s~%s," site (string_of_state state);
      print_elements tl
  in
  Format.fprintf fmt "(";
  print_elements lst;
  Format.fprintf fmt ")"

let string_of_site format k =
  match format with
  | Kappa | BNGL -> "s" ^ string_of_int k
  | BNGL_compact -> "s"

let print_signatures fmt format n =
  let () =
    match format with
    | Kappa -> Format.fprintf fmt "%%agent: A("
    | BNGL | BNGL_compact -> Format.fprintf fmt "begin molecule types\nA("
  in
  let rec aux k =
    if k > n then
      ()
    else (
      let () = if k > 1 then Format.fprintf fmt "," in
      let () = Format.fprintf fmt "%s" (string_of_site format k) in
      let () =
        List.iter
          (fun state -> Format.fprintf fmt "~%s" (string_of_state state))
          states
      in
      aux (k + 1)
    )
  in
  let () = aux 1 in
  let () = Format.fprintf fmt ")" in
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact -> Format.fprintf fmt "\nend molecule types"
  in
  let () = Format.fprintf fmt "\n\n" in
  ()

let print_agent fmt interface =
  Format.fprintf fmt "A";
  print_interface fmt interface

let site_list format n =
  let rec aux k l =
    if k = 0 then
      l
    else
      aux (k - 1) (string_of_site format k :: l)
  in
  aux n []

let print_init fmt format n =
  match format with
  | Kappa ->
    Format.fprintf fmt "%%init: 100 ";
    print_agent fmt
      (List.rev_map (fun site -> site, U) (List.rev (site_list format n)));
    Format.fprintf fmt "\n"
  | BNGL | BNGL_compact ->
    let () = Format.fprintf fmt "begin seed species\n" in
    let () = Format.fprintf fmt "%%init: " in
    let () =
      print_agent fmt
        (List.rev_map (fun site -> site, U) (List.rev (site_list format n)))
    in
    Format.fprintf fmt " Stot\nend seed species\n\n"

let potential_valuations format list =
  match format with
  | Kappa | BNGL ->
    let list = List.rev list in
    let rec aux remaining_site partial_valuations =
      match remaining_site with
      | [] -> partial_valuations
      | h :: tl ->
        let partial_valuations =
          List.fold_left
            (fun extended_partial_valuations state ->
              List.fold_left
                (fun extended_partial_valuations partial_valuation ->
                  ((h, state) :: partial_valuation)
                  :: extended_partial_valuations)
                extended_partial_valuations partial_valuations)
            [] states
        in
        aux tl partial_valuations
    in
    aux list [ [] ]
  | BNGL_compact ->
    let n = List.length list in
    let rec aux state k list =
      if k = 0 then
        list
      else
        aux state (k - 1) (state :: list)
    in
    let valuation k = aux U k (aux P (n - k) []) in
    let rec aux k l =
      if k > n then
        l
      else (
        let valuation =
          List.rev_map2 (fun a b -> a, b) (List.rev list) (valuation k)
        in
        aux (k + 1) (valuation :: l)
      )
    in
    aux 0 []

let flip (s, state) =
  match state with
  | U -> P, (s, P)
  | P -> U, (s, U)

let count_p interface =
  List.fold_left
    (fun n (_, state) ->
      match state with
      | P -> n + 1
      | U -> n)
    0 interface

let rate_of kind interface =
  let n = count_p interface in
  "k" ^ string_of_state kind ^ string_of_int n

(* kp_i/n-i
    ku_i/i
*)

let rate_of_sym kind interface n =
  let n_p = count_p interface in
  let s =
    match kind with
    | P ->
      let rec aux k =
        let i = n - k in
        "k" ^ string_of_state kind ^ string_of_int k ^ "/" ^ "n_k"
        ^ string_of_int i
      in
      aux n_p
    | U ->
      "k" ^ string_of_state kind ^ string_of_int n_p ^ "/" ^ "n_k"
      ^ string_of_int n_p
  in
  s

let string_of_rate format rate =
  match format with
  | Kappa -> "'" ^ rate ^ "'"
  | BNGL | BNGL_compact -> rate

let print_rule fmt format interface interface_post rate =
  print_agent fmt interface;
  Format.fprintf fmt " -> ";
  print_agent fmt interface_post;
  Format.fprintf fmt " %s%s\n" (rate_symbol format) (string_of_rate format rate)

let rec exp i j =
  if j = 0 then
    1
  else if j = 1 then
    i
  else (
    let q, r = j / 2, j mod 2 in
    let root = exp i q in
    let square = root * root in
    if r = 0 then
      square
    else
      i * square
  )

let rate state k =
  match state with
  | U -> 3 * exp 5 k
  | P -> 2 * exp 7 k

let declare_rate_list fmt format l =
  match format with
  | Kappa ->
    List.iter (fun (a, b) -> Format.fprintf fmt "%%var: '%s' %i\n" a b) l
  | BNGL | BNGL_compact ->
    let () = Format.fprintf fmt "begin parameters\n" in
    let () = List.iter (fun (a, b) -> Format.fprintf fmt "%s %i\n" a b) l in
    let () = Format.fprintf fmt "end parameters\n\n" in
    ()

let declare_rate fmt format n =
  let rec aux k list =
    if k < 0 then
      list
    else
      aux (k - 1)
        (("n_k" ^ string_of_int k, k)
        :: ("kp" ^ string_of_int k, rate U k)
        :: ("ku" ^ string_of_int (k + 1), rate P (k + 1))
        :: list)
  in
  let list = aux n [] in
  let list =
    match format with
    | Kappa -> list
    | BNGL | BNGL_compact -> ("Stot", 100) :: list
  in
  let () = declare_rate_list fmt format list in
  ()

let deal_with_one_valuation fmt format interface =
  match format with
  | Kappa | BNGL ->
    let rec aux suffix prefix =
      match suffix with
      | [] -> ()
      | (h : string * state) :: tl ->
        let kind, h' = flip h in
        let interface_post =
          List.fold_left (fun list elt -> elt :: list) (h' :: prefix) tl
        in
        let rate = rate_of kind interface in
        let () = print_rule fmt format interface interface_post rate in
        aux tl (h :: prefix)
    in
    aux (List.rev interface) []
  | BNGL_compact ->
    let n = List.length interface in
    let rec aux suffix prefix =
      match suffix with
      | [] -> ()
      | (h : string * state) :: tl ->
        let kind, h' = flip h in
        let interface_post =
          List.fold_left (fun list elt -> elt :: list) (h' :: prefix) tl
        in
        let rate = rate_of_sym kind interface n in
        let () = print_rule fmt format interface interface_post rate in
        aux tl (h :: prefix)
    in
    aux (List.rev interface) []

let declare_rules fmt format n =
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact -> Format.fprintf fmt "begin reaction rules\n"
  in
  let sites = site_list format n in
  let potential_valuations = potential_valuations format sites in
  let () =
    List.iter
      (fun valuation ->
        deal_with_one_valuation fmt format valuation;
        Format.fprintf fmt "\n\n")
      potential_valuations
  in
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact ->
      Format.fprintf fmt
        "end reaction rules\n\ngenerate_network({overwrite=>1});"
  in
  ()

let main rep format n =
  let ext =
    match format with
    | Kappa -> ".ka"
    | BNGL -> ".bngl"
    | BNGL_compact -> "_sym.bngl"
  in
  let file = rep ^ "/" ^ ex_name ^ string_of_int n ^ ext in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () = declare_rate fmt format n in
  let () = print_signatures fmt format n in
  let () = print_init fmt format n in
  let () = Format.fprintf fmt "\n" in
  let () = declare_rules fmt format n in
  let () = close_out channel in
  ()

let do_it rep k =
  main rep Kappa k;
  main rep BNGL k;
  main rep BNGL_compact k

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
    aux (int_of_string Sys.argv.(2))
  | _ -> Printf.printf "Please call with two or three int arguments\n\n"
