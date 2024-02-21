type trans =
  | Bool of Ckappa_sig.c_site_name * Ckappa_sig.c_state
  | Site of Ckappa_sig.c_site_name
  | Counter of Ckappa_sig.c_site_name
  | Affine_cst

val string_of_trans : trans -> string
val print_trans : Remanent_parameters_sig.parameters -> trans -> unit
val po : trans -> trans -> bool
val p : trans -> trans -> int
