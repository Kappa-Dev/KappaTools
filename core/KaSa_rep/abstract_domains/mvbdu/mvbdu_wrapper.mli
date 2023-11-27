(**
   * mvbdu_wrapper.mli
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 08/03/2010
   * Last modification: Time-stamp: <Dec 09 2018>
   * *
   * This library provides test benchmarks for the library of sets of finite maps from integers to integers
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(** API of the multi-valued binary decision diagrams library *)

module type Mvbdu = sig
  type key
  type value

  type handler =
    ( Boolean_mvbdu.memo_tables,
      Boolean_mvbdu.mvbdu_dic,
      Boolean_mvbdu.association_list_dic,
      Boolean_mvbdu.range_list_dic,
      Boolean_mvbdu.variables_list_dic,
      bool,
      int )
    Memo_sig.handler

  type mvbdu
  type hconsed_range_list
  type hconsed_association_list
  type hconsed_variables_list
  type hconsed_renaming_list

  type 'output constant =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    Exception.method_handler * handler * 'output

  type ('input, 'output) unary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input ->
    Exception.method_handler * handler * 'output

  type ('input1, 'input2, 'output) binary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input1 ->
    'input2 ->
    Exception.method_handler * handler * 'output

  type ('input1, 'input2, 'input3, 'output) ternary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input1 ->
    'input2 ->
    'input3 ->
    Exception.method_handler * handler * 'output

  val init :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Exception.method_handler * handler

  val is_init : unit -> bool

  val get_handler :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Exception.method_handler * handler

  val reset :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Exception.method_handler * handler

  val equal : mvbdu -> mvbdu -> bool
  val equal_with_logs : (mvbdu, mvbdu, bool) binary
  val mvbdu_false : mvbdu constant
  val mvbdu_true : mvbdu constant
  val mvbdu_not : (mvbdu, mvbdu) unary
  val mvbdu_id : (mvbdu, mvbdu) unary
  val mvbdu_unary_true : (mvbdu, mvbdu) unary
  val mvbdu_unary_false : (mvbdu, mvbdu) unary
  val mvbdu_and : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_or : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_xor : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nand : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nor : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_imply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_rev_imply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_equiv : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nimply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nrev_imply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_bi_true : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_bi_false : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_fst : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_snd : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nfst : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nsnd : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_redefine : (mvbdu, hconsed_association_list, mvbdu) binary
  val mvbdu_redefine_range : (mvbdu, hconsed_range_list, mvbdu) binary
  val mvbdu_subseteq : (mvbdu, mvbdu, bool) binary
  val mvbdu_of_hconsed_asso : (hconsed_association_list, mvbdu) unary
  val mvbdu_of_association_list : ((key * value) list, mvbdu) unary
  val mvbdu_of_sorted_association_list : ((key * value) list, mvbdu) unary

  val mvbdu_of_reverse_sorted_association_list :
    ((key * value) list, mvbdu) unary

  val mvbdu_of_hconsed_range : (hconsed_range_list, mvbdu) unary

  val mvbdu_of_range_list :
    ((key * (value option * value option)) list, mvbdu) unary

  val mvbdu_of_sorted_range_list :
    ((key * (value option * value option)) list, mvbdu) unary

  val mvbdu_of_reverse_sorted_range_list :
    ((key * (value option * value option)) list, mvbdu) unary

  val mvbdu_rename : (mvbdu, hconsed_renaming_list, mvbdu) binary
  val mvbdu_project_keep_only : (mvbdu, hconsed_variables_list, mvbdu) binary

  val mvbdu_project_abstract_away :
    (mvbdu, hconsed_variables_list, mvbdu) binary

  val mvbdu_cartesian_decomposition_depth :
    (mvbdu, int, mvbdu option * mvbdu list) binary

  val mvbdu_full_cartesian_decomposition : (mvbdu, mvbdu list) unary
  val mvbdu_cartesian_abstraction : (mvbdu, mvbdu list) unary

  val build_association_list :
    ((key * value) list, hconsed_association_list) unary

  val build_sorted_association_list :
    ((key * value) list, hconsed_association_list) unary

  val build_reverse_sorted_association_list :
    ((key * value) list, hconsed_association_list) unary

  val empty_association_list : hconsed_association_list constant

  val build_range_list :
    ((key * (value option * value option)) list, hconsed_range_list) unary

  val build_sorted_range_list :
    ((key * (value option * value option)) list, hconsed_range_list) unary

  val build_reverse_sorted_range_list :
    ((key * (value option * value option)) list, hconsed_range_list) unary

  val empty_range_list : hconsed_range_list constant
  val build_variables_list : (key list, hconsed_variables_list) unary
  val build_sorted_variables_list : (key list, hconsed_variables_list) unary

  val build_reverse_sorted_variables_list :
    (key list, hconsed_variables_list) unary

  val empty_variables_list : hconsed_variables_list constant
  val build_renaming_list : ((key * key) list, hconsed_renaming_list) unary

  val build_sorted_renaming_list :
    ((key * key) list, hconsed_renaming_list) unary

  val build_reverse_sorted_renaming_list :
    ((key * key) list, hconsed_renaming_list) unary

  val empty_renaming_list : hconsed_renaming_list constant

  val overwrite_association_lists :
    ( hconsed_association_list,
      hconsed_association_list,
      hconsed_association_list )
    binary

  val merge_variables_lists :
    ( hconsed_variables_list,
      hconsed_variables_list,
      hconsed_variables_list )
    binary

  val nbr_variables : (hconsed_variables_list, int) unary
  val extensional_of_variables_list : (hconsed_variables_list, key list) unary

  val extensional_of_association_list :
    (hconsed_association_list, (key * value) list) unary

  val extensional_of_range_list :
    (hconsed_range_list, (key * (value option * value option)) list) unary

  val extensional_of_mvbdu : (mvbdu, (key * value) list list) unary
  val variables_list_of_mvbdu : (mvbdu, hconsed_variables_list) unary
  val print : Remanent_parameters_sig.parameters -> mvbdu -> unit

  val print_association_list :
    Remanent_parameters_sig.parameters -> hconsed_association_list -> unit

  val print_variables_list :
    Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit

  val store_by_variables_list :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'data ->
    List_sig.hash_key ->
    'map ->
    Exception.method_handler * 'data) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    List_sig.hash_key ->
    'data ->
    'map ->
    Exception.method_handler * 'map) ->
    'data ->
    ('data, 'data, 'data) binary ->
    (hconsed_variables_list, 'data, 'map, 'map) ternary

  val store_by_mvbdu :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'data ->
    Mvbdu_sig.hash_key ->
    'map ->
    Exception.method_handler * 'data) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Mvbdu_sig.hash_key ->
    'data ->
    'map ->
    Exception.method_handler * 'map) ->
    'data ->
    ('data, 'data, 'data) binary ->
    (mvbdu, 'data, 'map, 'map) ternary

  val last_entry : (unit, int) unary
  val hash_of_range_list : hconsed_range_list -> int
  val hash_of_association_list : hconsed_association_list -> int
  val hash_of_variables_list : hconsed_variables_list -> int
end

module type Internalized_mvbdu = sig
  type key
  type value
  type mvbdu

  type handler =
    ( Boolean_mvbdu.memo_tables,
      Boolean_mvbdu.mvbdu_dic,
      Boolean_mvbdu.association_list_dic,
      Boolean_mvbdu.range_list_dic,
      Boolean_mvbdu.variables_list_dic,
      bool,
      int )
    Memo_sig.handler

  type hconsed_range_list
  type hconsed_association_list
  type hconsed_variables_list
  type hconsed_renaming_list

  val init : Remanent_parameters_sig.parameters -> unit
  val import_handler : handler -> unit

  val export_handler :
    Exception.method_handler -> Exception.method_handler * handler option

  val is_init : unit -> bool
  val equal : mvbdu -> mvbdu -> bool
  val mvbdu_false : unit -> mvbdu
  val mvbdu_true : unit -> mvbdu
  val mvbdu_not : mvbdu -> mvbdu
  val mvbdu_id : mvbdu -> mvbdu
  val mvbdu_unary_true : mvbdu -> mvbdu
  val mvbdu_unary_false : mvbdu -> mvbdu
  val mvbdu_and : mvbdu -> mvbdu -> mvbdu
  val mvbdu_or : mvbdu -> mvbdu -> mvbdu
  val mvbdu_xor : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nand : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nor : mvbdu -> mvbdu -> mvbdu
  val mvbdu_imply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_rev_imply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_equiv : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nimply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nrev_imply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_bi_true : mvbdu -> mvbdu -> mvbdu
  val mvbdu_bi_false : mvbdu -> mvbdu -> mvbdu
  val mvbdu_fst : mvbdu -> mvbdu -> mvbdu
  val mvbdu_snd : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nfst : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nsnd : mvbdu -> mvbdu -> mvbdu
  val mvbdu_redefine : mvbdu -> hconsed_association_list -> mvbdu
  val mvbdu_redefine_range : mvbdu -> hconsed_range_list -> mvbdu
  val mvbdu_subseteq : mvbdu -> mvbdu -> bool
  val mvbdu_of_hconsed_asso : hconsed_association_list -> mvbdu
  val mvbdu_of_association_list : (key * value) list -> mvbdu
  val mvbdu_of_sorted_association_list : (key * value) list -> mvbdu
  val mvbdu_of_reverse_sorted_association_list : (key * value) list -> mvbdu
  val mvbdu_of_hconsed_range : hconsed_range_list -> mvbdu
  val mvbdu_of_range_list : (key * (value option * value option)) list -> mvbdu

  val mvbdu_of_sorted_range_list :
    (key * (value option * value option)) list -> mvbdu

  val mvbdu_of_reverse_sorted_range_list :
    (key * (value option * value option)) list -> mvbdu

  val mvbdu_rename : mvbdu -> hconsed_renaming_list -> mvbdu
  val mvbdu_project_abstract_away : mvbdu -> hconsed_variables_list -> mvbdu
  val mvbdu_project_keep_only : mvbdu -> hconsed_variables_list -> mvbdu
  val mvbdu_cartesian_abstraction : mvbdu -> mvbdu list

  val mvbdu_cartesian_decomposition_depth :
    mvbdu -> int -> mvbdu option * mvbdu list

  val mvbdu_full_cartesian_decomposition : mvbdu -> mvbdu list
  val build_association_list : (key * value) list -> hconsed_association_list

  val build_sorted_association_list :
    (key * value) list -> hconsed_association_list

  val build_reverse_sorted_association_list :
    (key * value) list -> hconsed_association_list

  val empty_association_list : unit -> hconsed_association_list

  val build_range_list :
    (key * (value option * value option)) list -> hconsed_range_list

  val build_sorted_range_list :
    (key * (value option * value option)) list -> hconsed_range_list

  val build_reverse_sorted_range_list :
    (key * (value option * value option)) list -> hconsed_range_list

  val empty_range_list : unit -> hconsed_range_list
  val build_variables_list : key list -> hconsed_variables_list
  val build_sorted_variables_list : key list -> hconsed_variables_list
  val build_reverse_sorted_variables_list : key list -> hconsed_variables_list
  val empty_variables_list : unit -> hconsed_variables_list
  val build_renaming_list : (key * key) list -> hconsed_renaming_list
  val build_sorted_renaming_list : (key * key) list -> hconsed_renaming_list

  val build_reverse_sorted_renaming_list :
    (key * key) list -> hconsed_renaming_list

  val empty_renaming_list : unit -> hconsed_renaming_list

  val overwrite_association_lists :
    hconsed_association_list ->
    hconsed_association_list ->
    hconsed_association_list

  val merge_variables_lists :
    hconsed_variables_list -> hconsed_variables_list -> hconsed_variables_list

  val nbr_variables : hconsed_variables_list -> int
  val extensional_of_variables_list : hconsed_variables_list -> key list

  val extensional_of_association_list :
    hconsed_association_list -> (key * value) list

  val extensional_of_mvbdu : mvbdu -> (key * value) list list
  val variables_list_of_mvbdu : mvbdu -> hconsed_variables_list
  val print : Remanent_parameters_sig.parameters -> mvbdu -> unit

  val print_association_list :
    Remanent_parameters_sig.parameters -> hconsed_association_list -> unit

  val print_variables_list :
    Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit

  val hash_of_association_list : hconsed_association_list -> int
  val hash_of_variables_list : hconsed_variables_list -> int
end

module type Nul = sig end

module Optimize (M : Mvbdu with type key = int and type value = int) :
  Mvbdu with type mvbdu = M.mvbdu and type key = int and type value = int

module Internalize (M : Mvbdu with type key = int and type value = int) :
  Internalized_mvbdu
    with type mvbdu = M.mvbdu
     and type key = int
     and type value = int

module Optimize_internalized
    (M : Internalized_mvbdu with type key = int and type value = int) :
  Internalized_mvbdu
    with type mvbdu = M.mvbdu
     and type key = int
     and type value = int

module Make : functor (_ : Nul) ->
  Mvbdu with type key = int and type value = int

module Mvbdu : Mvbdu with type key = int and type value = int

module IntMvbdu :
  Internalized_mvbdu
    with type key = int
     and type value = int
     and type mvbdu = Mvbdu.mvbdu

module Optimized_Mvbdu : Mvbdu with type key = int and type value = int

module Optimized_IntMvbdu :
  Internalized_mvbdu with type key = int and type value = int

module Optimized_IntMvbdu_bis :
  Internalized_mvbdu with type key = int and type value = int
