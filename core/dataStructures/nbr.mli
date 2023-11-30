(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Kappa numbers (either float, int or int64) and operations on them *)

type t = F of float | I of int | I64 of Int64.t

val compare : t -> t -> int
val is_greater : t -> t -> bool
val is_smaller : t -> t -> bool
val is_equal : t -> t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val mult : t -> t -> t

val internal_div : t -> t -> t
(** euler division when only int are involved, float div else *)

val rem : t -> t -> t
val pow : t -> t -> t
val min : t -> t -> t
val max : t -> t -> t
val succ : t -> t
val pred : t -> t
val neg : t -> t

val to_float : t -> float option
(** [None] when infinity or Not a Number *)

val to_int : t -> int
val zero : t
val is_zero : t -> bool
val one : t
val is_strictly_positive : t -> bool
val print : Format.formatter -> t -> unit

val pretty_print : Format.formatter -> t -> unit
(** Floats are compactly printed *)

val print_option : Format.formatter -> t -> unit
(** Prints nothing in case of infinity or Not a Number *)

val iteri : (t -> 'a -> 'a) -> 'a -> t -> 'a
(** [iteri f x n]
@return f (n - k) (... (f (n - 1) (f n x))) where k < n <= k+1 *)

val maybe_iteri : (t -> 'a -> 'a option) -> 'a -> t -> 'a
(** [maybe_iteri f x n]
@return f (n - k) (... (f (n - 1) (f n x))) up to k < n <= k+1
or [f] returns [None] *)

val to_string : t -> string

val of_string : string -> t
(** @raise Failure "float_of_string" *)

val to_yojson : t -> Yojson.Basic.t

val of_yojson : Yojson.Basic.t -> t
(** @raise Yojson.Basic.Util.Type_error if incorrect *)

val write_t : Buffer.t -> t -> unit
(** Output a JSON value of type {!t}. *)

val string_of_t : ?len:int -> t -> string
(** Serialize a value of type {!t} into a JSON string.
    @param len specifies the initial length
               of the buffer used internally.
               Default: 1024. *)

val read_t : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
(** Input JSON data of type {!t}. *)

val t_of_string : string -> t
(** Deserialize JSON data of type {!t}. *)

val of_un_alg_op : Operator.un_alg_op -> t -> t
val of_bin_alg_op : Operator.bin_alg_op -> t -> t -> t
val of_compare_op : Operator.compare_op -> t -> t -> bool
