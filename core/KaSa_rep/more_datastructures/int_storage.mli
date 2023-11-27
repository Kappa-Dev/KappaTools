(**
   * int_storage.mli
   *
   * Creation:                      <2016-03-14 feret>
   * Last modification: Time-stamp: <Apr 13 2018>
   *
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   *
   * This library provides primitives to deal with storage functions
   *
   * Copyright 2010,2011,2012,2013,2014,2015 Institut National
   * de Recherche en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type ('a, 'b) unary =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  Exception.method_handler * 'b

type ('a, 'b, 'c) binary =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  'b ->
  Exception.method_handler * 'c

type ('a, 'b, 'c, 'd) ternary =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  'b ->
  'c ->
  Exception.method_handler * 'd

type ('a, 'b, 'c, 'd, 'e) quaternary =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  'b ->
  'c ->
  'd ->
  Exception.method_handler * 'e

type ('a, 'b, 'c, 'd, 'e, 'f, 'g) sexternary =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  'b ->
  'c ->
  'd ->
  'e ->
  'f ->
  Exception.method_handler * 'g

type 'a unary_no_output =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  Exception.method_handler

type ('a, 'b) binary_no_output =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  'a ->
  'b ->
  Exception.method_handler

module type Storage = sig
  type 'a t
  type key
  type dimension

  val create : (dimension, 'a t) unary
  val create_biggest_key : (key, 'a t) unary
  val expand_and_copy : ('a t, dimension, 'a t) binary
  val init : (dimension, (key, 'a) unary, 'a t) binary
  val set : (key, 'a, 'a t, 'a t) ternary
  val free : (key, 'a t, 'a t) binary
  val get : (key, 'a t, 'a option) binary
  val unsafe_get : (key, 'a t, 'a option) binary
  val dimension : ('a t, dimension) unary
  val print : ('a unary_no_output, 'a t) binary_no_output
  val key_list : ('a t, key list) unary
  val iter : ((key, 'a) binary_no_output, 'a t) binary_no_output
  val fold_with_interruption : ((key, 'a, 'b, 'b) ternary, 'a t, 'b, 'b) ternary
  val fold : ((key, 'a, 'b, 'b) ternary, 'a t, 'b, 'b) ternary

  val fold2 :
    ( (key, 'a, 'c, 'c) ternary,
      (key, 'b, 'c, 'c) ternary,
      (key, 'a, 'b, 'c, 'c) quaternary,
      'a t,
      'b t,
      'c,
      'c )
    sexternary

  val fold2_common :
    ((key, 'a, 'b, 'c, 'c) quaternary, 'a t, 'b t, 'c, 'c) quaternary

  val for_all : ((key, 'a, bool) binary, 'a t, bool) binary
  val free_all : ('a t, 'a t) unary
end

(** Cartesian product *)
module Extend (Extension : Storage) (Underlying : Storage) :
  Storage
    with type key = Extension.key * Underlying.key
     and type dimension = Extension.dimension * Underlying.dimension

(** also record the list of key, for more efficient fold/iter *)
module Quick_key_list (Basic : Storage) :
  Storage with type key = Basic.key and type dimension = Basic.dimension

(** simple array implementation *)
module Int_storage_imperatif :
  Storage with type key = int and type dimension = int

(** expandable arrays (the size is still limited by max_int *)
module Nearly_infinite_arrays : functor
  (_ : Storage with type dimension = int and type key = int)
  -> Storage with type key = int and type dimension = int

(** expandable 1-dim array *)
module Nearly_inf_Imperatif :
  Storage with type key = int and type dimension = int

(** expandable 1-dim array with sparse fold/iter *)
module Quick_Nearly_inf_Imperatif :
  Storage with type key = int and type dimension = int

(** 2-dim matrices with sparse fold/iter *)
module Int_Int_storage_Imperatif_Imperatif :
  Storage with type key = int * int and type dimension = int * int

(** 2-dim expandable matrices *)
module Nearly_Inf_Int_Int_storage_Imperatif_Imperatif :
  Storage with type key = int * int and type dimension = int * int

(** 3-dim expandable matrices *)
module Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif :
  Storage
    with type key = int * (int * int)
     and type dimension = int * (int * int)
