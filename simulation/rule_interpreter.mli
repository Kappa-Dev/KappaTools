(**Graph rewriting module*)

type t (**Abstract graph*)

(** {6 Initialisation} *)

val empty : has_tracking:bool -> Environment.t -> t

(** {6 algebraic expression computation} *)
(** [get_alg] is by default [Environment.get_alg] but it is not hard
wired because perturbations can redefined alg_expr.*)

val value_alg :
  get_alg:(int -> Alg_expr.t) -> Mods.Counter.t -> t -> Alg_expr.t -> Nbr.t

val value_bool :
  get_alg:(int -> Alg_expr.t) -> Mods.Counter.t -> t ->
  Alg_expr.t Ast.bool_expr -> bool

(** {6 Core} *)

val apply_rule :
  get_alg:(int -> Alg_expr.t) -> Connected_component.Env.t -> Mods.Counter.t
  -> t -> Causal.event_kind -> Primitives.elementary_rule -> t option
(** Returns the graph obtained by applying the rule. [None] means
null_event *)

val force_rule :
  get_alg:(int -> Alg_expr.t) -> Connected_component.Env.t -> Mods.Counter.t
  -> t -> Causal.event_kind -> Primitives.elementary_rule ->
  (t * Connected_component.Matching.t list option)
(** Apply the rule for sure if it is possible. Try [apply_rule] but in
case of null_event, it computes the exact injections of the left hand
side to do apply the rule and returns the remaining exact injections. *)

val update_outdated_activities :
  get_alg:(int -> Alg_expr.t) -> (int -> float -> 'a -> unit) ->
  Environment.t -> Mods.Counter.t -> t -> 'a -> t
(** Resynchronize the state after a rule application.

As long as you don't use any algebraic variable (that include don't
pick a rule randomly), you can apply several rules in row before
resynchronizing. (This is what initial state and perturbations do.) *)

val print : Environment.t -> Format.formatter -> t -> unit

(** {6 Stories} *)

val add_tracked :
  Connected_component.t -> Causal.event_kind ->
  Instantiation.abstract Instantiation.test list ->
  t -> t
val remove_tracked : Connected_component.t -> t -> t
val generate_stories : Format.formatter -> Environment.t -> t -> unit

(** {6 Debugging} *)

val print_injections :
  ?sigs:Signature.s -> Format.formatter ->
  ValMap.tree Connected_component.Map.t -> unit
val debug_print : Format.formatter -> t -> unit
