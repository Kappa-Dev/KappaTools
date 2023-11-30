module type Tabinter = sig
  type var
  type intervalle
  type intervalle_tab

  val make : int -> intervalle_tab
  val set : intervalle_tab -> var -> intervalle -> unit
  val read : intervalle_tab -> var -> intervalle

  val affiche :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    Exception.method_handler

  val copy :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val clef : intervalle_tab -> var list

  val wide_place :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * var list

  val union_place :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * var list

  val union :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val inter :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val somme :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val int_of_var_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    var list ->
    Exception.method_handler * intervalle_tab

  val push : intervalle_tab -> var -> Fraction.fraction -> intervalle_tab
  val pushbool : intervalle_tab -> var -> intervalle_tab
  val equal : intervalle_tab -> intervalle_tab -> bool

  val merge :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    intervalle_tab ->
    Exception.method_handler * intervalle_tab

  val abstract_away :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    intervalle_tab ->
    var list ->
    Exception.method_handler * intervalle_tab
end

module Tabinter :
  Tabinter
    with type var = Occu1.trans
     and type intervalle = Intervalles.intervalle
