(** Pathes to explore a mixture *)

type id_upto_alpha =
    Existing of int
  | Fresh of Agent.t

type port = id_upto_alpha * int

type arrow = ToNode of port | ToNothing | ToInternal of int

type step = port * arrow

type t = step list

val print : Signature.s -> (int -> int) -> Format.formatter -> t -> unit
(** [print signatures find_existing_type nav] *)

val step_to_yojson : step -> Yojson.Basic.json
val step_of_yojson : Yojson.Basic.json -> step

val to_yojson : t -> Yojson.Basic.json
val of_yojson : Yojson.Basic.json -> t

val rename : Renaming.t -> t -> Renaming.t * t

val compatible_point : Renaming.t -> step -> step -> Renaming.t option
(** Retuns the extension of the given injections so that the second edge
    is the image of the first *)

val is_subnavigation : Renaming.t -> t -> t -> (Renaming.t * t) option
(** [is_subnavigation inj_sub2nav nav subpart] *)

val check_edge : Edges.t -> step -> bool
val injection_for_one_more_edge :
  ?root:Agent.t -> Renaming.t -> Edges.t -> step -> Renaming.t option
