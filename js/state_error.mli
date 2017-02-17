(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Clear errors. *)
val clear_errors : unit -> unit
(** Return true if errors are present. *)
val has_errors : unit -> bool
(** Set errors

    Called with the location macro:
    set_error  __LOC__ msg
    The location and message will be logged
    debugging is enabled.

    @param location of error the macro __LOC__ is expected.
    @paramer errors to be saved
 *)
val set_errors : string -> Api_types_j.errors -> unit
(** Signal containing the error. *)
val errors : Api_types_j.errors option React.signal
(** This displays the error *)
val wrap : string -> 'a Api.result Lwt.t -> 'a Api.result Lwt.t
