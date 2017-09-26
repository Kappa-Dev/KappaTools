(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type cc_port = {
  port_links: (int * int) list;
  port_states: string list;
}
type site =
  | Port of cc_port
  | Counter of int
type cc_site = {
  site_name: string;
  site_type: site
}type cc_node = {
  node_type: string;
  node_sites: cc_site array;
}
type connected_component = cc_node array

val print_cc :
  explicit_free:bool -> compact:bool ->
  Format.formatter -> connected_component -> unit

val print_dot_cc : int -> Format.formatter -> connected_component -> unit

val write_connected_component :
  Bi_outbuf.t -> connected_component -> unit
  (** Output a JSON value of type {!connected_component}. *)

val string_of_connected_component :
  ?len:int -> connected_component -> string
  (** Serialize a value of type {!connected_component}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_connected_component :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> connected_component
  (** Input JSON data of type {!connected_component}. *)

val connected_component_of_string :
  string -> connected_component
  (** Deserialize JSON data of type {!connected_component}. *)
