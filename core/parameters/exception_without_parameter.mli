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

type uncaught_exception

exception Uncaught_exception of uncaught_exception

type caught_exception

exception Caught_exception of caught_exception

type method_handler

val raise_exception : string option -> unit -> string option -> exn -> unit

val build_uncaught_exception :
  ?file_name:string -> ?message:string -> exn -> uncaught_exception

val build_caught_exception :
  string option -> string option -> exn -> string list -> caught_exception

val add_uncaught_error :
  ?to_ui:bool -> uncaught_exception -> method_handler -> method_handler

val stringlist_of_exception : exn -> string list -> string list
val stringlist_of_uncaught : uncaught_exception -> string list -> string list
val stringlist_of_caught : caught_exception -> string list -> string list
val stringlist_of_caught_light : caught_exception -> string list -> string list
val pp_exception : Format.formatter -> exn -> unit
val pp_uncaught : Format.formatter -> uncaught_exception -> unit
val pp_caught : Format.formatter -> caught_exception -> unit
val empty_error_handler : method_handler
val is_empty_error_handler : method_handler -> bool
val get_caught_exception_list : method_handler -> caught_exception list
val get_caught_exception_list_to_ui : method_handler -> caught_exception list
val get_uncaught_exception_list : method_handler -> uncaught_exception list

val get_uncaught_exception_list_to_ui :
  method_handler -> uncaught_exception list

val to_json : method_handler -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> method_handler
