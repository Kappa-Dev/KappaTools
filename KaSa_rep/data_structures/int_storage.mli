(**
   * int_storage.mli
   *
   * Creation:                      <2016-03-14 feret>
   * Last modification: Time-stamp: <2016-03-14 16:01:22 feret>
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


module type Storage =
sig
  type 'a t
  type key
  type dimension

  val keys: dimension -> key list
  val create: Remanent_parameters_sig.parameters -> Exception.method_handler -> dimension -> Exception.method_handler * 'a t
  val init: Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a t -> dimension -> Exception.method_handler * 'a t
  val create_diag: Remanent_parameters_sig.parameters -> Exception.method_handler -> dimension -> Exception.method_handler * key t
  val set: Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a -> 'a t -> Exception.method_handler * 'a t
  val get: Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a t -> Exception.method_handler * 'a option
  val unsafe_get: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a t -> Exception.method_handler * 'a option
  val dimension: Exception.method_handler -> 'a t -> Exception.method_handler * dimension
  val print: Exception.method_handler -> (Exception.method_handler -> Remanent_parameters_sig.parameters -> 'a -> Exception.method_handler) -> Remanent_parameters_sig.parameters -> 'a t -> Exception.method_handler
  val print_var_f: Exception.method_handler -> (Exception.method_handler -> Remanent_parameters_sig.parameters -> 'a -> Exception.method_handler) -> Remanent_parameters_sig.parameters -> 'a t -> Exception.method_handler
  val print_site_f: Exception.method_handler -> (Exception.method_handler -> Remanent_parameters_sig.parameters -> 'a -> Exception.method_handler) -> Remanent_parameters_sig.parameters -> 'a t -> Exception.method_handler

  val key_list: Remanent_parameters_sig.parameters -> Exception.method_handler -> 'a t -> (Exception.method_handler * key list)
  val iter:Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key  -> 'a  ->  Exception.method_handler ) -> 'a t ->  Exception.method_handler
  val fold_with_interruption: Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key  -> 'a  -> 'b-> Exception.method_handler  * 'b ) -> 'a t -> 'b ->  Exception.method_handler * 'b
  val fold: Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key  -> 'a  -> 'b-> Exception.method_handler  * 'b ) -> 'a t -> 'b ->  Exception.method_handler * 'b
  val fold2_common:Remanent_parameters_sig.parameters -> Exception.method_handler -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a -> 'b -> 'c ->  Exception.method_handler  * 'c ) -> 'a t -> 'b t ->  'c ->  Exception.method_handler * 'c

end

(** simple array implementation *)
module Int_storage_imperatif:
  Storage with type key = int and type dimension = int

(** expandable arrays (the size is still limited by max_int *)
module Nearly_infinite_arrays
  (Basic:Storage
   with type dimension = int
   and type key = int) :
  Storage with type key = int and type dimension = int

(** Cartesian product *)
module Extend
  (Extension:Storage)
  (Underlying:Storage) :
  Storage
  with type key = Extension.key * Underlying.key
  and type dimension = Extension.dimension * Underlying.dimension

(** also record the list of key, for more efficient fold/iter *)
module Quick_key_list
  (Basic:Storage) :
  Storage
  with type key = Basic.key
  and type dimension = Basic.dimension

(** expandable 1-dim array *)
module Nearly_inf_Imperatif:
  Storage
  with type key = int
  and type dimension = int

(** expandable 1-dim array with sparse fold/iter *)
module Quick_Nearly_inf_Imperatif:
  Storage
  with type key = int
  and type dimension = int

(** 2-dim matrices with sparse fold/iter *)
module Int_Int_storage_Imperatif_Imperatif:
  Storage
  with type key = int * int
  and type dimension = int * int

(** 2-dim expandable matrices *)
module Nearly_Inf_Int_Int_storage_Imperatif_Imperatif:
  Storage
  with type key = int * int
  and type dimension = int * int

(** 3-dim expandable matrices *)
module Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif:
  Storage
  with type key = int * (int * int)
  and type dimension = int * (int * int)
