type trans =
| Bool of Ckappa_sig.c_site_name * Ckappa_sig.c_state_index
| Counter of Ckappa_sig.c_site_name
| Affine_cst

let p x y =
  match x,y with
| Affine_cst, Affine_cst -> 0
| _, Affine_cst -> -1
| Affine_cst, _ -> 1
| Counter c, Counter c' ->
  Ckappa_sig.compare_site_name c c'
| _, Counter _ -> -1
| Counter _, _ -> 1
| Bool (a,b), Bool (a',b') ->
let cmp =
Ckappa_sig.compare_site_name a a'
in
if cmp = 0 then Ckappa_sig.compare_state_index b b'
else cmp

let po x y = ((p x y)>0)

let print_trans parameters x =
  match x with
  | Affine_cst  ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "Affine constant" x
    in
    Loggers.print_newline (Remanent_parameters.get_logger parameters)
  | Counter c ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "Counter_%i"
        (Ckappa_sig.int_of_site_name c)
    in
    Loggers.print_newline (Remanent_parameters.get_logger parameters)
  | Bool(a,b)  ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "Is_site_%i_in_state_%i"
        (Ckappa_sig.int_of_site_name a) (Ckappa_sig.int_of_state_index b)
    in
    Loggers.print_newline (Remanent_parameters.get_logger parameters)
