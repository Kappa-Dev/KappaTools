(**Graph rewriting module*)

type t (**Abstract graph*)

type result = Clash | Corrected | Success of (int option * t)
(** the int option is the distance between patterns in unimolecular
    instances of rule if you've asked for them *)

(** {6 Initialisation} *)

val empty :
  ?story_compression:((bool * bool * bool) * bool) ->
  store_distances:bool -> Random.State.t -> Environment.t -> t

(** {6 algebraic expression computation} *)
(** [get_alg] is by default [Environment.get_alg] but it is not hard
wired because perturbations can redefined alg_expr.*)

val value_alg :
  get_alg:(int -> Alg_expr.t) -> Counter.t -> t -> Alg_expr.t -> Nbr.t

val value_bool :
  get_alg:(int -> Alg_expr.t) -> Counter.t -> t ->
  (Pattern.id array list,int) Alg_expr.bool_expr -> bool

(** {6 Core} *)

val apply_rule :
  ?rule_id:int -> get_alg:(int -> Alg_expr.t) -> Environment.t ->
  Pattern.Set.t -> Counter.t ->
  t -> Trace.event_kind -> Primitives.elementary_rule -> result
(** Returns the graph obtained by applying the rule.
 [rule_id] is mandatory if the rule has an unary rate.*)

val apply_unary_rule :
  rule_id:int -> get_alg:(int -> Alg_expr.t) -> Environment.t ->
  Pattern.Set.t -> Counter.t ->
  t -> Trace.event_kind -> Primitives.elementary_rule -> result
(** Returns the graph obtained by applying the rule.
    [rule_id] is mandatory if the rule has an unary rate.*)

val force_rule :
  get_alg:(int -> Alg_expr.t) -> Environment.t -> Pattern.Set.t ->
  Counter.t -> t -> Trace.event_kind -> Primitives.elementary_rule -> t
(** Apply the rule for sure if it is possible. Try [apply_rule] but in
case of null_event, it computes the exact injections of the left hand
side to do apply the rule and returns the remaining exact injections. *)

val adjust_rule_instances :
  rule_id:int -> get_alg:(int -> Alg_expr.t) -> (int -> int -> float -> unit) ->
  Environment.t -> Counter.t -> t -> Primitives.elementary_rule -> t

val adjust_unary_rule_instances :
  rule_id:int -> get_alg:(int -> Alg_expr.t) -> (int -> int -> float -> unit) ->
  Environment.t -> Counter.t -> t -> Primitives.elementary_rule -> t

val incorporate_extra_pattern : Pattern.Env.t -> t -> Pattern.id -> t

val extra_outdated_var : int -> t -> t
val update_outdated_activities :
  get_alg:(int -> Alg_expr.t) -> (int -> int -> float -> unit) ->
  Environment.t -> Counter.t -> t -> t
(** Resynchronize the state after a rule application.

It takes the function to store the new activities as an argument whose
 signature is [store rule_id syntactic_rule_id new_activity].

As long as you don't use any algebraic variable (that include don't
pick a rule randomly), you can apply several rules in row before
resynchronizing. (This is what initial state and perturbations do.) *)

val snapshot: Environment.t -> Counter.t -> string -> t -> Data.snapshot

val print : Environment.t -> Format.formatter -> t -> unit

val get_random_state : t -> Random.State.t

(** {6 Stories} *)

val add_tracked :
  Pattern.id array -> Trace.event_kind ->
  Instantiation.abstract Instantiation.test list ->
  t -> t
val remove_tracked : Pattern.id array -> t -> t
val generate_stories : t -> (((bool*bool*bool)*bool)*Trace.t) option

(** {6 Debugging} *)

val print_injections :
  ?domain:Pattern.Env.t -> Format.formatter ->
  Mods.IntSet.t Pattern.Map.t -> unit
val debug_print : Format.formatter -> t -> unit

(** {6 Internals } *)
val apply_negative_transformation :
  (Instantiation.concrete Instantiation.site) list *
  Mods.IntSet.t Mods.IntMap.t Pattern.Map.t * Edges.t ->
  Instantiation.concrete Primitives.Transformation.t ->
  (Instantiation.concrete Instantiation.site) list *
  Mods.IntSet.t Mods.IntMap.t Pattern.Map.t * Edges.t
val apply_positive_transformation :
  Signature.s ->
  (Pattern.Matching.t * int Mods.IntMap.t) *
  (Instantiation.concrete Instantiation.site) list *
  Mods.IntSet.t Mods.IntMap.t Pattern.Map.t * Edges.t ->
  Instantiation.abstract Primitives.Transformation.t ->
  ((Pattern.Matching.t * int Mods.IntMap.t) *
   (Instantiation.concrete Instantiation.site) list *
   Mods.IntSet.t Mods.IntMap.t Pattern.Map.t * Edges.t) *
  Instantiation.concrete Primitives.Transformation.t
