(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type links =
  | LINKS of ((int * int) * int) list
  | WHATEVER
  | SOME
  | TYPE of string * string

type cc_port = {
  port_links: links;
  port_states: string list option;  (** [None] means WHATEVER *)
}

type site = Port of cc_port | Counter of int
type cc_site = { site_name: string; site_type: site }

type cc_node = {
  node_type: string;
  node_id: int option;
  node_sites: cc_site array;
}

type connected_component = cc_node option array array

val print_cc : Format.formatter -> connected_component -> unit
val print_dot_cc : int -> Format.formatter -> connected_component -> unit
val links_of_yojson : Yojson.Basic.t -> links

val write_connected_component : Buffer.t -> connected_component -> unit
(** Output a JSON value of type {!connected_component}. *)

val string_of_connected_component : ?len:int -> connected_component -> string
(** Serialize a value of type {!connected_component}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_connected_component :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> connected_component
(** Input JSON data of type {!connected_component}. *)

val connected_component_of_string : string -> connected_component
(** Deserialize JSON data of type {!connected_component}. *)
