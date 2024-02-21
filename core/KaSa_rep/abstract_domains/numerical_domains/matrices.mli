module type Matrice = sig
  type point
  type line = Occu1.trans list * (Occu1.trans, Fraction.fraction) Hashtbl.t
  type matrice
  type var

  val mat_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    var list ->
    Exception.method_handler * matrice

  val make : Remanent_parameters_sig.parameters -> int -> matrice

  val affiche :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler

  val affiche_cons :
    Remanent_parameters_sig.parameters ->
    int list * (int, Fraction.fraction) Hashtbl.t * Intervalles.intervalle ->
    unit

  val get_all_entry : matrice -> var Working_list_imperative.working_list
  val add_entry : matrice -> var -> unit

  val plonge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var list ->
    Exception.method_handler * matrice

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler * matrice

  val normalise :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var ->
    Fraction.fraction ->
    Exception.method_handler

  val pushbool :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var ->
    Exception.method_handler * matrice

  val new_copy_ligne :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    line ->
    Exception.method_handler

  val new_empty_ligne :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler

  val n_ligne : matrice -> int

  val get_line :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    int ->
    Exception.method_handler * line

  val get_trans_list : line -> Occu1.trans list

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * matrice

  val pivot : matrice -> int -> var
  val read_val : matrice -> int -> var -> Fraction.fraction
  val addligne : matrice -> int -> Fraction.fraction -> int -> unit

  val swap :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    int ->
    int ->
    Exception.method_handler

  val mulligne :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    int ->
    Fraction.fraction ->
    Exception.method_handler

  val del_ligne : matrice -> int -> unit
  val del_last_ligne : matrice -> unit

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * matrice

  val get_all_key : matrice -> var list
  val is_key : matrice -> var -> bool

  val decomp_affine :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler * (point * matrice)

  val somme_affine :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * matrice

  val insert_0 :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    Exception.method_handler * matrice

  val equal :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    matrice ->
    Exception.method_handler * bool

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    matrice ->
    var list ->
    Exception.method_handler * matrice
end

module Matrice : Matrice with type var = Occu1.trans
