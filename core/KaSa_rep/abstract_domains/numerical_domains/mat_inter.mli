module type Mat_inter = sig
  type prod
  type var

  val addzero : bool
  val list_var : Remanent_parameters_sig.parameters -> prod -> var list

  val solve_inf :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    var list ->
    Exception.exceptions_caught_and_uncaught * prod option

  val create : Remanent_parameters_sig.parameters -> int -> prod

  val plonge :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    var list ->
    Exception.exceptions_caught_and_uncaught * prod

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    Exception.exceptions_caught_and_uncaught * prod

  (*val exclusion:
      Remanent_parameters_sig.parameters ->
      Exception.exceptions_caught_and_uncaught ->
      prod -> var list ->
      Exception.exceptions_caught_and_uncaught * bool

    val all_here :
      Remanent_parameters_sig.parameters ->
      Exception.exceptions_caught_and_uncaught ->
      prod -> var list -> Exception.exceptions_caught_and_uncaught * prod option*)

  val guard :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    (var * Counters_domain_type.comparison_op * int) list ->
    Exception.exceptions_caught_and_uncaught * prod option

  val solve_all :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    Exception.exceptions_caught_and_uncaught * prod option

  val compt_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    var list ->
    Exception.exceptions_caught_and_uncaught * prod

  val affiche_mat :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    Exception.exceptions_caught_and_uncaught

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    prod ->
    Exception.exceptions_caught_and_uncaught * prod option

  val is_vide : prod -> var -> bool

  val string_of_pro :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    var ->
    Exception.exceptions_caught_and_uncaught * string

  val interval_of_pro :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    var ->
    Exception.exceptions_caught_and_uncaught * (Fraction.ffraction * Fraction.ffraction) option

  val is_infinite : prod -> var -> bool

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    prod ->
    Exception.exceptions_caught_and_uncaught * prod

  val widen :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    prod ->
    Exception.exceptions_caught_and_uncaught * (prod * bool)

  val union_incr :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    prod ->
    Exception.exceptions_caught_and_uncaught * (prod * bool)

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    var ->
    Fraction.fraction ->
    Exception.exceptions_caught_and_uncaught * prod

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    prod ->
    var list ->
    Exception.exceptions_caught_and_uncaught * prod
end

module Mat_int : Mat_inter with type var = Occu1.trans
