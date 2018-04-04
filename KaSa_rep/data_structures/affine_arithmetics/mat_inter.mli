
module type Mat_inter =
sig
  type prod
  type var
  val addzero : bool
  val list_var : Remanent_parameters_sig.parameters -> prod -> var list
  val solve_inf:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod -> var list-> Exception.method_handler

  val create : Remanent_parameters_sig.parameters -> int -> prod
  val plonge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler -> prod -> var list ->
    Exception.method_handler * prod

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->
    Exception.method_handler * prod

  val exclusion:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod -> var list ->
    Exception.method_handler * bool

  val all_here :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod -> var list -> Exception.method_handler * prod

  val solve_all :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->
    Exception.method_handler

  val compt_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    var list ->
    Exception.method_handler * prod

  val affiche_mat :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod -> Exception.method_handler

  val merge:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->prod->
    Exception.method_handler * prod
  val is_vide: prod -> var->bool
  val string_of_pro:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod -> var  -> Exception.method_handler * string

  val is_infinite:prod->var->bool
  val union:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->prod->
    Exception.method_handler * prod
  val plus:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->prod->
    Exception.method_handler * prod
  val widen:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->prod->
    Exception.method_handler * (prod * var list)

  val push:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    prod->var -> Fraction.fraction->
    Exception.method_handler * prod

  val pushbool:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler -> prod -> var ->
    Exception.method_handler * prod
end

module Mat_int:Mat_inter