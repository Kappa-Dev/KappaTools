module type Matrice = sig
  type point
  type line = Occu1.trans list * (Occu1.trans, Fraction.fraction) Hashtbl.t
  type matrice
  type var

  val mat_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    var list ->
    Exception.exceptions_caught_and_uncaught * matrice

  val make : Remanent_parameters_sig.parameters -> int -> matrice

  val affiche :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    Exception.exceptions_caught_and_uncaught

  val affiche_cons :
    Remanent_parameters_sig.parameters ->
    int list * (int, Fraction.fraction) Hashtbl.t * Intervalles.intervalle ->
    unit

  val get_all_entry : matrice -> var Working_list_imperative.working_list
  val add_entry : matrice -> var -> unit

  val plonge :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    var list ->
    Exception.exceptions_caught_and_uncaught * matrice

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * matrice

  val normalise :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    Exception.exceptions_caught_and_uncaught

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    var ->
    Fraction.fraction ->
    Exception.exceptions_caught_and_uncaught

  val pushbool :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    var ->
    Exception.exceptions_caught_and_uncaught * matrice

  val new_copy_ligne :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    line ->
    Exception.exceptions_caught_and_uncaught

  val new_empty_ligne :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    Exception.exceptions_caught_and_uncaught

  val n_ligne : matrice -> int

  val get_line :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    int ->
    Exception.exceptions_caught_and_uncaught * line

  val get_trans_list : line -> Occu1.trans list

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * matrice

  val pivot : matrice -> int -> var
  val read_val : matrice -> int -> var -> Fraction.fraction
  val addligne : matrice -> int -> Fraction.fraction -> int -> unit

  val swap :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    int ->
    int ->
    Exception.exceptions_caught_and_uncaught

  val mulligne :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    int ->
    Fraction.fraction ->
    Exception.exceptions_caught_and_uncaught

  val del_ligne : matrice -> int -> unit
  val del_last_ligne : matrice -> unit

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * matrice

  val get_all_key : matrice -> var list
  val is_key : matrice -> var -> bool

  val decomp_affine :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * (point * matrice)

  val somme_affine :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * matrice

  val insert_0 :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * matrice

  val equal :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    matrice ->
    Exception.exceptions_caught_and_uncaught * bool

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    matrice ->
    var list ->
    Exception.exceptions_caught_and_uncaught * matrice
end

module Matrice : Matrice with type var = Occu1.trans
