module type Label_handler = sig
  type label
  type label_set
  type label_set_couple

  val label_of_int :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    int ->
    Exception.method_handler * label

  val member : label -> label_set -> bool
  val empty : label_set
  val empty_couple : label_set_couple
  val is_empty_couple : label_set_couple -> bool
  val add_set : label -> label_set -> label_set
  val remove_set : label -> label_set -> label_set

  val add_couple :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    bool ->
    label_set ->
    label_set ->
    label_set_couple ->
    Exception.method_handler * label_set_couple

  val dump :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set ->
    Exception.method_handler

  val dump_couple :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set_couple ->
    Exception.method_handler

  val filter_couple :
    Remanent_parameters_sig.parameters ->
    'a ->
    Cckappa_sig.kappa_handler ->
    ('a -> label -> label -> 'a * bool) ->
    label_set_couple ->
    'a * label_set_couple

  val to_string :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set ->
    Exception.method_handler * string list

  val to_string_couple :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Cckappa_sig.kappa_handler ->
    label_set_couple ->
    Exception.method_handler * string list

  val convert_label_set_couple : label_set_couple -> (int * int) list
end

module type Labels = sig
  type label

  val label_of_int :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    int ->
    Exception.method_handler * label

  val to_string :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    label ->
    Exception.method_handler * string

  val dump :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    label ->
    Exception.method_handler

  val print : Format.formatter -> label -> unit
  val int_of_label : label -> int
end

module Int_labels : Labels with type label = int

module Implicit : functor (L : Labels) ->
  Label_handler with type label = L.label

module Extensive : functor (L : Labels) ->
  Label_handler with type label = L.label

module Empty : Label_handler with type label = unit
