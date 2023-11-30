(**
  * graph_loggers_sig.ml
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

type t
type direction = Direct | Reverse | Undirected | Both
type shape = Invisible | House | Rect | Ellipse | Circle | Invhouse
type headkind = Normal | Vee | Tee | No_head
type linestyle = Plain | Dotted | Dashed

type color =
  | Red
  | Green
  | White
  | Blue
  | Black
  | LightSkyBlue
  | PaleGreen
  | Brown
  | Yellow
  | Grey

type options =
  | Color of color
  | FillColor of color
  | Label of string
  | Width of int (*pixel*)
  | Height of int (*pixel*)
  | Direction of direction
  | Shape of shape
  | ArrowHead of headkind
  | ArrowTail of headkind
  | LineStyle of linestyle
  | OnClick of Yojson.Basic.t
  | Contextual_help of string
  | Position of Loc.t list

type graph =
  (string * options list) list * (string * string * options list) list

val extend_logger : Loggers.t -> t
val lift : t -> Loggers.t
val refresh_id : t -> unit
val int_of_string_id : t -> string -> int
val graph_of_logger : t -> graph
val add_node : t -> string -> options list -> unit
val add_edge : t -> string -> string -> options list -> unit
val get_edge_map : t -> options list list Mods.String2Map.t
val get_nodes : t -> (string * options list) list
