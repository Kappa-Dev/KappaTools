(**
  * black_list.mli
  *
  * Creation:                      <2016-02-14 10:29:42 feret>
  * Last modification: Time-stamp: <2016-02-14 10:50:28 feret>
  *
  * Causal flow compression: a module for KaSim
  * Jerome Feret, projet Antique, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS
  *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012,2013,2014,2015,2016
  *  Institut National de Recherche en
  * Informatique et en Automatique.  All rights reserved.
  * This file is distributed under the terms of the GNU Library
  * General Public License *)

module type Event =
  sig
    type event
    type eid
    type 'a t
    val key_of_event: event -> eid option
    val init: int -> 'a -> 'a t
    val set: 'a t -> eid -> 'a -> 'a t
    val get: 'a t -> eid -> 'a
  end

module type Blacklist =
  sig
    type t
    module Event: Event

    val init: int -> t
    val black_list: Event.event -> t -> t
    val is_black_listed: Event.event -> t -> bool
  end

module Make:
  functor (Event:Event) -> Blacklist
