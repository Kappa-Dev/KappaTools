(** Pathes to explore a mixture *)

type id_upto_alpha =
    Existing of int
  | Fresh of Edges.agent

type port = id_upto_alpha * int

type arrow = ToNode of port | ToNothing | ToInternal of int

type step = port * arrow

type t = step list

val print_step : Signature.s -> (int -> int) -> Format.formatter -> step -> unit
(** [print_step signatures find_existing_type step] *)

val rename_step : Renaming.t -> step -> step

val compatible_point : Renaming.t list -> step -> step -> Renaming.t list
(** Retuns all the extension of the given injections so that the second edge
    is the image of the first one by this injection *)

val check_edge : Edges.t -> step -> bool
val injection_for_one_more_edge :
  ?root:int -> Renaming.t -> Edges.t -> step -> Renaming.t option
