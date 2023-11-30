exception Intervalle_vide

type intervalle = { inf: Fraction.ffraction; sup: Fraction.ffraction }

val zero : intervalle
val set_wide_max : Fraction.fraction -> unit
val get_wide_max : unit -> Fraction.fraction
val trans_convexe : intervalle -> Fraction.fraction -> intervalle
val sub_convexe : intervalle -> intervalle -> bool

val string_of_intervalle :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  intervalle ->
  Exception_without_parameter.method_handler * string

val contient_zero : intervalle -> bool

val combinaison_lineaire_convexe :
  ((int * Fraction.fraction) list * Fraction.fraction) * intervalle array ->
  intervalle

val iiplus : intervalle -> Fraction.fraction -> intervalle -> intervalle
val inter_convexe : intervalle array -> intervalle array -> intervalle array
val wide_en_place : intervalle array -> intervalle array -> int list

val wide_union_convexe :
  intervalle array -> intervalle array -> intervalle array

val union : intervalle -> intervalle -> intervalle
val union_convexe : intervalle array -> intervalle array -> intervalle array
val cap_inter : intervalle -> intervalle -> intervalle
val wide_max : Fraction.fraction ref
