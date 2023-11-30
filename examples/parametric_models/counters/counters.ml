let ex_name = "counters"

type label = int
type state = int

let string_of_state i =
  if i = 0 then
    "u"
  else
    "p" ^ string_of_int i

let string_of_site k = "s" ^ string_of_int k
let rate_symbol = "@"
let next_internal = succ

(*Print Signatures*)
let print_interface fmt lst counter =
  let rec print_elements = function
    | [] -> ()
    | [ (site, state) ] ->
      Format.fprintf fmt "%s{%s}" (string_of_site site) (string_of_state state)
    | (site, state) :: tl ->
      Format.fprintf fmt "%s{%s}," (string_of_site site) (string_of_state state);
      print_elements tl
  in
  let () = Format.fprintf fmt "(" in
  let () = print_elements lst in
  let () =
    match counter with
    | None -> ()
    | Some i ->
      if i > 0 then
        Format.fprintf fmt ",c{+=%i}" i
      else if i < 0 then
        Format.fprintf fmt ",c{-=%i}" (-i)
      else
        ()
  in
  Format.fprintf fmt ")"

let list_first_last first last =
  let rec aux k l =
    if k < first then
      l
    else
      aux (k - 1) (k :: l)
  in
  aux last []

let print_signatures fmt n_sites n_phos =
  let l_sites = list_first_last 1 n_sites in
  let l_states = list_first_last 0 n_phos in
  let () = Format.fprintf fmt "%%agent: A(" in
  let () =
    List.iter
      (fun site ->
        let () = if site > 1 then Format.fprintf fmt "," in
        let () = Format.fprintf fmt "%s{" (string_of_site site) in
        let () =
          List.iter
            (fun state ->
              let () = if state > 0 then Format.fprintf fmt " " in
              Format.fprintf fmt "%s" (string_of_state state))
            l_states
        in
        let () = Format.fprintf fmt "}" in
        ())
      l_sites
  in
  Format.fprintf fmt ")\n"

let print_agent fmt interface counter =
  Format.fprintf fmt "A";
  print_interface fmt interface counter

let site_list n =
  let rec aux k l =
    if k = 0 then
      l
    else
      aux (k - 1) (string_of_site k :: l)
  in
  aux n []

let print_init fmt = Format.fprintf fmt "%%init: 100 A()\n"

let print_rule fmt interface interface_post counter =
  print_agent fmt interface None;
  Format.fprintf fmt " -> ";
  print_agent fmt interface_post counter;
  Format.fprintf fmt " %s 1\n" rate_symbol

let declare_rules fmt n_sites n_phos =
  let site_list = list_first_last 1 n_sites in
  let state_list = list_first_last 0 n_phos in
  let () =
    List.iter
      (fun site ->
        List.iter
          (fun state ->
            let state' = state + 1 in
            let () =
              if state' <= n_phos then
                print_rule fmt [ site, state ] [ site, state' ] (Some 1)
            in
            let state' = state - 1 in
            let () =
              if state' >= 0 then
                print_rule fmt [ site, state ] [ site, state' ] (Some (-1))
            in
            ())
          state_list)
      site_list
  in
  ()

let string_of_int_bis i =
  if i >= 0 && i < 10 then
    "0" ^ string_of_int i
  else
    string_of_int i

let main rep n_sites n_phos =
  let ext = ".ka" in
  let file =
    rep ^ "/" ^ ex_name ^ string_of_int_bis n_sites ^ "_"
    ^ string_of_int_bis n_phos ^ ext
  in
  let channel = open_out file in
  let fmt = Format.formatter_of_out_channel channel in
  let () = print_signatures fmt n_sites n_phos in
  let () = print_init fmt in
  let () = Format.fprintf fmt "\n" in
  let () = declare_rules fmt n_sites n_phos in
  let () = close_out channel in
  ()

let () =
  match Array.length Sys.argv with
  | 4 ->
    main Sys.argv.(1) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3))
  | 5 ->
    let n = int_of_string Sys.argv.(3) in
    let rec aux k =
      if k > n then
        ()
      else (
        let () = main Sys.argv.(1) k (int_of_string Sys.argv.(4)) in
        aux (k + 1)
      )
    in
    aux (int_of_string Sys.argv.(2))
  | 6 ->
    let n = int_of_string Sys.argv.(3) in
    let n' = int_of_string Sys.argv.(5) in
    let rec aux k =
      if k > n then
        ()
      else
        aux2 k (int_of_string Sys.argv.(4))
    and aux2 k k' =
      if k' > n' then
        aux (k + 1)
      else (
        let () = main Sys.argv.(1) k k' in
        aux2 k (k' + 1)
      )
    in
    aux (int_of_string Sys.argv.(2))
  | _ -> Printf.printf "Please call with two, three, or four int arguments\n\n"
