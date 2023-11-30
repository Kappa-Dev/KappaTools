let ex_file_name = "n_phos_sites_with_counter_"

type format = Kappa | BNGL | BNGL_compact
type label = int
type state = U | P

let agent_sep format =
  match format with
  | Kappa -> " , "
  | BNGL | BNGL_compact -> "."

let rate_symbol format =
  match format with
  | Kappa -> "@"
  | BNGL | BNGL_compact -> ""

let states = [ U; P ]

let string_of_state = function
  | U -> "u"
  | P -> "p"

let dual = function
  | U -> P
  | P -> U

let int_list i k =
  let rec aux k list =
    if k < i then
      list
    else
      aux (k - 1) (k :: list)
  in
  aux k []

let site_list k format =
  match format with
  | Kappa | BNGL ->
    List.rev_map (fun i -> "s" ^ string_of_int i) (List.rev (int_list 1 k))
  | BNGL_compact -> [ "s" ]

let site_list_init k format =
  match format with
  | Kappa | BNGL ->
    List.rev_map (fun i -> "s" ^ string_of_int i) (List.rev (int_list 1 k))
  | BNGL_compact -> List.rev_map (fun i -> "s") (List.rev (int_list 1 k))

let next (l : label) : label = succ l
let string_of_label (l : label) : string = string_of_int l

(*Print Signatures*)
let print_interface fmt lst =
  let rec print_elements = function
    | [] -> ()
    | (site, state) :: tl ->
      Format.fprintf fmt "%s~%s," site state;
      print_elements tl
  in
  Format.fprintf fmt "(";
  print_elements lst;
  Format.fprintf fmt "p!1";
  Format.fprintf fmt ")"

let print_signatures fmt n format =
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
      let () =
        match format with
        | Kappa | BNGL -> Format.fprintf fmt "s%i" k
        | BNGL_compact -> Format.fprintf fmt "s"
      in
      let () =
        List.iter
          (fun state -> Format.fprintf fmt "~%s" (string_of_state state))
          states
      in
      aux (k + 1)
    )
  in
  let () = aux 1 in
  let () = Format.fprintf fmt ",p" in
  let () = Format.fprintf fmt ")\n" in
  let () =
    match format with
    | Kappa -> Format.fprintf fmt "%%agent: P(l,r)\n\n"
    | BNGL | BNGL_compact -> Format.fprintf fmt "P(l,r)\nend molecule types\n\n"
  in
  ()

let print_agent fmt interface =
  Format.fprintf fmt "A";
  print_interface fmt interface

let print_combinator fmt k l_opt format =
  let s =
    match l_opt with
    | None -> ""
    | Some label -> "!" ^ string_of_label label
  in
  Format.fprintf fmt "%sP(l!%i,r%s)" (agent_sep format) k s

let rec print_chain fmt n label format =
  if n = 0 then
    ()
  else (
    let next_label = next label in
    let succ_k =
      if n = 1 then
        None
      else
        Some next_label
    in
    print_combinator fmt label succ_k format;
    print_chain fmt (n - 1) next_label format
  )

let print_pattern_n fmt interface n format =
  print_agent fmt interface;
  print_chain fmt n 1 format

let print_pattern_succ_n fmt interface n format =
  print_agent fmt interface;
  print_chain fmt n 2 format;
  print_combinator fmt 1 (Some 2) format

let print_init fmt n format =
  match format with
  | Kappa ->
    Format.fprintf fmt "%%init: 100 ";
    print_pattern_n fmt [] 1 format;
    Format.fprintf fmt "\n"
  | BNGL | BNGL_compact ->
    Format.fprintf fmt "begin seed species\n";
    print_pattern_n fmt
      (List.rev_map (fun s -> s, "u") (List.rev (site_list_init n format)))
      1 format;
    Format.fprintf fmt " Stot\nend seed species\n"

let rate_of state k = "k" ^ string_of_state (dual state) ^ string_of_int k

let print_rule fmt site state k format =
  let k' = succ k in
  match state with
  | U ->
    print_pattern_n fmt [ site, string_of_state state ] k' format;
    Format.fprintf fmt " -> ";
    print_pattern_succ_n fmt [ site, string_of_state (dual state) ] k' format;
    Format.fprintf fmt " %s%s\n" (rate_symbol format) (rate_of state k)
  | P ->
    print_pattern_succ_n fmt [ site, string_of_state state ] k format;
    Format.fprintf fmt " -> ";
    print_pattern_n fmt [ site, string_of_state (dual state) ] k format;
    Format.fprintf fmt " %s%s\n" (rate_symbol format) (rate_of state k)

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
  if state = "u" then
    3 * exp 5 k
  else if state = "p" then
    2 * exp 7 k
  else
    0

let declare_rate fmt n format =
  match format with
  | Kappa ->
    let rec aux k =
      if k = n then
        ()
      else (
        let () = Format.fprintf fmt "%%var: 'kp%i' %i\n" k (rate "u" k) in
        let () =
          Format.fprintf fmt "%%var: 'ku%i' %i\n" (k + 1) (rate "p" (k + 1))
        in
        aux (k + 1)
      )
    in
    aux 0
  | BNGL | BNGL_compact ->
    let () = Format.fprintf fmt "begin parameters\n" in
    let rec aux k =
      if k = n then
        ()
      else (
        let () = Format.fprintf fmt "kp%i %i\n" k (rate "u" k) in
        let () = Format.fprintf fmt "ku%i %i\n" (k + 1) (rate "p" (k + 1)) in
        aux (k + 1)
      )
    in
    let () = aux 0 in
    let () = Format.fprintf fmt "Stot 100\n" in
    Format.fprintf fmt "end parameters\n\n"

let main rep n format =
  let ext =
    match format with
    | Kappa -> ".ka"
    | BNGL -> ".bngl"
    | BNGL_compact -> "_sym.bngl"
  in
  let file = rep ^ "/" ^ ex_file_name ^ string_of_int n ^ ext in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () = declare_rate fmt n format in
  let () = print_signatures fmt n format in
  let () = print_init fmt n format in
  let () = Format.fprintf fmt "\n\n" in
  let () = Format.fprintf fmt "\n\n" in
  let sites = site_list n format in
  let intlist = int_list 0 n in
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact -> Format.fprintf fmt "begin reaction rules\n"
  in
  let () =
    List.iter
      (fun k ->
        let states =
          if k = 0 then
            [ U ]
          else if k = n then
            [ P ]
          else
            states
        in
        List.iter
          (fun site ->
            List.iter (fun state -> print_rule fmt site state k format) states)
          sites)
      intlist
  in
  let () =
    match format with
    | Kappa -> ()
    | BNGL | BNGL_compact ->
      Format.fprintf fmt
        "end reaction rules\n\ngenerate_network({overwrite=>1});"
  in
  let () = close_out channel in
  ()

let do_it rep k =
  let () = main rep k Kappa in
  let () = main rep k BNGL in
  let () = main rep k BNGL_compact in
  ()

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
