(**
    * hash.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 18/10/2010
    * Last modification: Time-stamp: <Jul 04 2017>
    * *
    * This library provides signature for hash tables and several implementations
    * *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

module type Hash = sig
  type key
  type 'a hash

  val create : int -> 'a hash

  val add :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    'a ->
    int ->
    'a hash ->
    Exception.method_handler * 'a hash

  val overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    'a ->
    int ->
    'a hash ->
    Exception.method_handler * 'a hash

  val add_or_overwrite :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    'a ->
    int ->
    'a hash ->
    Exception.method_handler * 'a hash

  val find_option :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    'a hash ->
    Exception.method_handler * (int * 'a) option

  val find_option_without_logs :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    'a hash ->
    Exception.method_handler * (int * 'a) option

  val find_option_log_on_the_fly : key -> 'a hash -> (int * 'a) option
  val iter : (key -> 'a -> int -> unit) -> 'a hash -> unit
  val fold : (key -> 'b -> int -> 'a -> 'a) -> 'b hash -> 'a -> 'a
end

module Hash =
functor
  (Map : Map_wrapper.S_with_logs)
  ->
  (
    struct
      type key = Map.elt
      type 'a hash = (int * 'a) Map.Map.t

      let create _ = Map.Map.empty

      let add parameter error key asso int =
        Map.Map.add parameter error key (int, asso)

      let overwrite parameter error key asso int =
        Map.Map.overwrite parameter error key (int, asso)

      let add_or_overwrite parameter error key asso int =
        Map.Map.add_or_overwrite parameter error key (int, asso)

      let find_option parameter error a b =
        let error', output = Map.Map.find_option parameter error a b in
        if error == error' then
          error', output
        else
          Exception.warn parameter error' __POS__
            ~message:"attempt to read an unexisting association" Not_found
            output

      let find_option_without_logs = Map.Map.find_option_without_logs

      let find_option_log_on_the_fly a b =
        Lift_error_logs.lift_with_on_the_fly_logging_binary find_option a b

      let iter f = Map.Map.iter (fun (a : key) (b, c) -> f a c b)
      let fold f = Map.Map.fold (fun a (b, c) d -> f a c b d)
    end :
      Hash with type key = Map.elt)

module Hash_of_Ord =
functor (O : SetMap.OrderedType) -> Hash (Map_wrapper.Make (SetMap.Make (O)))
