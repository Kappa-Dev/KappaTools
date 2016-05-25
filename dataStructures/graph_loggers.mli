(**
  * graph_loggers.mli
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 23/05/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type direction = Direct | Reverse | Undirected | Both
type options =
  | Label of string
  | Width of string
  | Height of string
  | Direction of direction
  | DotStyle of string

val print_graph_preamble: Loggers.t -> string -> unit
val print_graph_foot: Loggers.t -> unit
val print_comment: Loggers.t -> string -> unit
val open_asso: Loggers.t -> unit
val close_asso: Loggers.t -> unit
val print_asso: Loggers.t -> string -> string -> unit
val print_node: Loggers.t -> ?directives:options list -> string -> unit
val print_edge: Loggers.t -> ?directives:options list -> string -> string -> unit
val print_one_to_n_relation: Loggers.t -> ?directives:options list -> ?style_one:string -> ?style_n:string -> string -> string list -> unit
