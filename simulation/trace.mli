type event_kind =
  | OBS of string
  | RULE of int
  | INIT of int list (* the agents *)
  | PERT of string (* the rule *)

val print_event_kind :
  ?env:Environment.t -> Format.formatter -> event_kind -> unit
val print_event_kind_dot_annot :
  Environment.t -> Format.formatter -> event_kind -> unit
