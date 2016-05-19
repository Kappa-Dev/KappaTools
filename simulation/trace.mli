(** Trace of simulation *)

type event_kind =
  | OBS of string
  | RULE of int
  | INIT of int list (** the agents *)
  | PERT of string (** the rule *)

val print_event_kind :
  ?env:Environment.t -> Format.formatter -> event_kind -> unit
val print_event_kind_dot_annot :
  Environment.t -> Format.formatter -> event_kind -> unit

type refined_event =
  event_kind *
    Instantiation.concrete Instantiation.event *
      unit Mods.simulation_info
type refined_obs =
  event_kind *
    Instantiation.concrete Instantiation.test list *
      unit Mods.simulation_info
type step =
  | Subs of int * int
  | Event of refined_event
  | Init of Instantiation.concrete Instantiation.action list
  | Obs of refined_obs
  | Dummy  of string

type t = step list

val dummy_step : string -> step
val subs_step : int -> int -> step

val step_is_obs : step -> bool
val step_is_init : step -> bool
val step_is_subs : step -> bool
val step_is_event : step -> bool
val has_creation_of_step: step -> bool

val tests_of_step :
  step -> Instantiation.concrete Instantiation.test list
val actions_of_step :
  step ->
  (Instantiation.concrete Instantiation.action list *
     (Instantiation.concrete Instantiation.site *
	Instantiation.concrete Instantiation.binding_state) list)
val simulation_info_of_step: step -> unit Mods.simulation_info option
val creation_of_actions :
  ('a -> 'b) -> 'a Instantiation.action list -> 'b list
val creation_of_step : step -> int list

val print_step:
  ?compact:bool -> ?env:Environment.t -> Format.formatter -> step -> unit
