val local_trace : bool

module Int_Set_and_Map : SetMap.S with type elt = int

val scan_compil :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  ( Ckappa_sig.agent,
    Ckappa_sig.agent_sig,
    Ckappa_sig.mixture,
    Ckappa_sig.mixture,
    string,
    Ckappa_sig.mixture Ckappa_sig.rule )
  Ast.compil ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * Cckappa_sig.kappa_handler

val empty_handler :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.exceptions_caught_and_uncaught ->
  Exception_without_parameter.exceptions_caught_and_uncaught
  * Cckappa_sig.kappa_handler
