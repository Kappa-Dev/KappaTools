type trans =
  | Bool of Ckappa_sig.c_site_name * Ckappa_sig.c_state
  | Site of Ckappa_sig.c_site_name
  | Counter of Ckappa_sig.c_site_name
  | Affine_cst

let p x y =
  match x, y with
  | Affine_cst, Affine_cst -> 0
  | _, Affine_cst -> -1
  | Affine_cst, _ -> 1
  | Bool (a, b), Bool (a', b') ->
    let cmp = Ckappa_sig.compare_site_name a a' in
    if cmp = 0 then
      Ckappa_sig.compare_state_index b b'
    else
      cmp
  | Bool _, _ -> 1
  | _, Bool _ -> -1
  | Site c, Site c' -> Ckappa_sig.compare_site_name c c'
  | _, Site _ -> -1
  | Site _, _ -> 1
  | Counter c, Counter c' -> Ckappa_sig.compare_site_name c c'
(*| _, Counter _ -> -1
  | Counter _, _ -> 1*)

let po x y = p x y > 0

let string_of_trans x =
  match x with
  | Affine_cst -> "Affine constant"
  | Counter c -> "Counter_" ^ string_of_int (Ckappa_sig.int_of_site_name c)
  | Bool (a, b) ->
    "Is_site_"
    ^ string_of_int (Ckappa_sig.int_of_site_name a)
    ^ "_in_state_"
    ^ string_of_int (Ckappa_sig.int_of_state_index b)
  | Site a -> "Site_" ^ string_of_int (Ckappa_sig.int_of_site_name a)

let print_trans parameters x =
  match x with
  | Affine_cst -> ()
  | Counter c ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "Counter_%i"
        (Ckappa_sig.int_of_site_name c)
    in
    ()
  | Site c ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "Site_%i"
        (Ckappa_sig.int_of_site_name c)
    in
    ()
  | Bool (a, b) ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "Is_site_%i_in_state_%i"
        (Ckappa_sig.int_of_site_name a)
        (Ckappa_sig.int_of_state_index b)
    in
    ()
