val local_trace : bool

module Int_Set_and_Map : SetMap.S with type elt = int

val scan_compil :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ( Ckappa_sig.agent,
    Ckappa_sig.mixture,
    Ckappa_sig.mixture,
    'a,
    Ckappa_sig.mixture Ckappa_sig.rule )
  Ast.compil ->
  Exception_without_parameter.method_handler * Cckappa_sig.kappa_handler

val empty_handler :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler * Cckappa_sig.kappa_handler
