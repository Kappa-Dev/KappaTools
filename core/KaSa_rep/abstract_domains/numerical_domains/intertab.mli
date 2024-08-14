module type Tabinter = sig
  type var
  type intervalle
  type intervalle_tab

  val make : int -> intervalle_tab
  val set : intervalle_tab -> var -> intervalle -> unit
  val read : intervalle_tab -> var -> intervalle

  val affiche :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab

  val clef : intervalle_tab -> var list

  val wide_place :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * var list

  val union_place :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * var list

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab

  val inter :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab

  val somme :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab

  val int_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    var list ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab

  val push : intervalle_tab -> var -> Fraction.fraction -> intervalle_tab
  val pushbool : intervalle_tab -> var -> intervalle_tab
  val equal : intervalle_tab -> intervalle_tab -> bool

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    intervalle_tab ->
    var list ->
    Exception.exceptions_caught_and_uncaught * intervalle_tab
end

module Tabinter :
  Tabinter
    with type var = Occu1.trans
     and type intervalle = Intervalles.intervalle
