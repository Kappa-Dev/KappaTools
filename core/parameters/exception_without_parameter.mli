(**
    * exception.mli
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 08/03/2010
 * Last modification: Time-stamp: <Nov 27 2016>
    * *
    * This library declares exceptions
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    *  under the terms of the GNU Library General Public License *)

(* The logic in this files describes a `degraded` mode for exceptions, before
   the remanent_parameters were loaded.
   these exceptions did not stop execution, so `uncaught` and `caught` may not
   be good naming here. `caught` here basically add a trace to uncaught info
   TODO: revamp/rename this?
*)

type uncaught_exception

exception Uncaught_exception of uncaught_exception

type caught_exception

exception Caught_exception of caught_exception

type exceptions_caught_and_uncaught

val raise_exception : string option -> unit -> string option -> exn -> unit

val build_uncaught_exception :
  ?file_name:string -> ?message:string -> exn -> uncaught_exception

val build_caught_exception :
  string option -> string option -> exn -> string list -> caught_exception

val add_uncaught_error :
  ?to_ui:bool ->
  uncaught_exception ->
  exceptions_caught_and_uncaught ->
  exceptions_caught_and_uncaught

val stringlist_of_exception : exn -> string list -> string list
val stringlist_of_uncaught : uncaught_exception -> string list -> string list
val stringlist_of_caught : caught_exception -> string list -> string list
val stringlist_of_caught_light : caught_exception -> string list -> string list
val pp_exception : Format.formatter -> exn -> unit
val pp_uncaught : Format.formatter -> uncaught_exception -> unit
val pp_caught : Format.formatter -> caught_exception -> unit
val empty_exceptions_caught_and_uncaught : exceptions_caught_and_uncaught

val is_empty_exceptions_caught_and_uncaught :
  exceptions_caught_and_uncaught -> bool

val get_caught_exception_list :
  exceptions_caught_and_uncaught -> caught_exception list

val get_caught_exception_list_to_ui :
  exceptions_caught_and_uncaught -> caught_exception list

val get_uncaught_exception_list :
  exceptions_caught_and_uncaught -> uncaught_exception list

val get_uncaught_exception_list_to_ui :
  exceptions_caught_and_uncaught -> uncaught_exception list

val to_json : exceptions_caught_and_uncaught -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> exceptions_caught_and_uncaught
