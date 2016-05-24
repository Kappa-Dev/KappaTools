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
