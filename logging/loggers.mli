(**
  * loggers.mli
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 26/01/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type encoding =
  | Matrix | HTML_Graph | Js_Graph | HTML | HTML_Tabular
  | DOT | TXT | TXT_Tabular | XLS
  | Octave | Matlab | Maple | Mathematica | SBML | DOTNET
  | Json

module type FormatMap =
sig
  type 'a t
  val add : encoding -> 'a  -> 'a t -> 'a t
  val find : encoding -> 'a t -> 'a
  val empty : 'a t
end

module FormatMap:FormatMap

type t

val refresh_id: t -> unit
val get_encoding_format: t -> encoding
val fprintf: t -> ('a, Format.formatter, unit) format -> 'a
val print_newline: t -> unit
val print_cell: t -> string -> unit
val print_as_logger: t -> (Format.formatter -> unit) -> unit
val flush_logger: t -> unit
val close_logger: t -> unit
val open_infinite_buffer: ?mode:encoding -> unit -> t
val open_circular_buffer: ?mode:encoding -> ?size:int -> unit -> t
val open_logger_from_formatter: ?mode:encoding -> Format.formatter -> t
val open_logger_from_channel: ?mode:encoding -> out_channel -> t
val open_row: t -> unit
val close_row: t -> unit
val print_breakable_space: t -> unit
val print_breakable_hint: t -> unit
val dummy_txt_logger: t
val dummy_html_logger: t
val redirect: t -> Format.formatter -> t
val formatter_of_logger: t -> Format.formatter option
val channel_of_logger: t -> out_channel option
val flush_buffer: t -> Format.formatter -> unit
val flush_and_clean: t -> Format.formatter -> unit
val int_of_string_id: t -> string -> int

val graph_of_logger: t -> Graph_loggers_sig.graph
val add_node: t -> string -> Graph_loggers_sig.options list -> unit
val add_edge: t -> string -> string -> Graph_loggers_sig.options list -> unit
val dump_json: t -> Yojson.Basic.json -> unit
val get_edge_map: t -> Graph_loggers_sig.options list list Mods.String2Map.t
val get_nodes: t -> (string * Graph_loggers_sig.options list) list
val get_expr:
  t -> Ode_loggers_sig.variable ->
  (Ode_loggers_sig.ode_var_id,Ode_loggers_sig.ode_var_id)
    Alg_expr.e Locality.annot option
val set_expr:
  t -> Ode_loggers_sig.variable ->
  (
    Ode_loggers_sig.ode_var_id,
    Ode_loggers_sig.ode_var_id)
    Alg_expr.e Locality.annot -> unit
val is_const:
  t -> Ode_loggers_sig.variable -> bool

val of_json: Yojson.Basic.json -> string list
val to_json: t -> Yojson.Basic.json
val get_fresh_obs_id: t -> int
val get_fresh_reaction_id: t -> int
val get_fresh_meta_id: t -> int
val set_id_of_global_parameter: t -> Ode_loggers_sig.variable -> string -> unit
val get_id_of_global_parameter: t -> Ode_loggers_sig.variable -> string
val is_dangerous_ode_variable: t -> Ode_loggers_sig.variable -> bool
val flag_dangerous: t -> Ode_loggers_sig.variable -> string -> unit
val has_forbidden_char: t -> string -> bool
val allocate_fresh_name: t -> string -> string -> string
val allocate: t -> string -> unit

val print_binding_type:
  t -> ?binding_type_symbol:string -> agent_name:string ->
  site_name:string -> unit

val set_ode : mode:encoding -> string -> unit
val get_ode : mode:encoding -> string
