(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(**Graph rewriting module*)

module Make (Instances:Instances_sig.S) : sig
  type t (**Abstract graph*)

  type result = Clash | Corrected | Blocked | Success of t
  (** Clash means rectangular approximation failure
      Corrected means molecular ambiguity failure *)

  (** {6 Initialisation} *)

  val empty :
    with_trace:bool -> Random.State.t -> Model.t -> Counter.t -> t

  (** {6 algebraic expression computation} *)
  (** [get_alg] is by default [Model.get_alg] but it is not hard
      wired because perturbations can redefined alg_expr.*)

  val value_alg : Counter.t -> t -> Primitives.alg_expr -> Nbr.t

  val value_bool :
    Counter.t -> t -> (Pattern.id array list,int) Alg_expr.bool -> bool

  val activity : t -> float

  val get_edges : t -> Edges.t

  (** {6 Core} *)

  val apply_given_rule :
    outputs:(Data.t -> unit) ->
    ?rule_id:int -> Model.t -> Counter.t -> t -> Trace.event_kind ->
    Primitives.elementary_rule -> result
  (** Returns the graph obtained by applying the rule.
      [rule_id] is mandatory if the rule has an unary rate.*)

  val apply_rule :
    outputs:(Data.t -> unit) ->
    ?maxConsecutiveBlocked:int -> maxConsecutiveClash:int ->
    Model.t -> Counter.t -> t -> int option * bool * t
  (** [apply_rule ~outputs ~maxConsecutiveClash ?is_blocked model counter st]
      Returns [(corresponding_syntactic_rule, is_final_step, new_state)].
      [is_final_step] is determined by the counter.
      [corresponding_syntactic_rule] is equal to None if and only if
      a null event occured *)

  val force_rule :
    outputs:(Data.t -> unit) ->
    Model.t -> Counter.t -> t -> Trace.event_kind ->
    ?rule_id:int ->  Primitives.elementary_rule -> t option
  (** Apply the rule for sure if it is possible. Try [apply_rule] but in
      case of null_event, it computes the exact injections of the left hand
      side to do apply the rule and returns the remaining exact injections. *)

  val incorporate_extra_pattern : Pattern.Env.t -> t -> Pattern.id -> t

  val overwrite_var : int -> Counter.t -> t -> Primitives.alg_expr -> t
  val update_outdated_activities :
    (int -> float -> float -> unit) ->
    Model.t -> Counter.t -> t -> int list -> (t * int list)
  (** Resynchronize the state after a rule application.

      It takes the function to store the new activities as an argument whose
      signature is [store rule_id syntactic_rule_id new_activity].

      As long as you don't use any algebraic variable (that include don't
      pick a rule randomly), you can apply several rules in row before
      resynchronizing. (This is what initial state does.)

      takes the list of perturbations to be tried and returns it updated *)

  val snapshot: Model.t -> Counter.t -> string -> t -> Data.snapshot

  val print : Model.t -> Format.formatter -> t -> unit

  val get_random_state : t -> Random.State.t

  val update_edges_from_actions :
    outputs:(Data.t -> unit) -> Signature.s -> Counter.t -> Pattern.Env.t ->
    t -> Instantiation.concrete Instantiation.action list *
         Instantiation.concrete Instantiation.site list -> t

  val send_instances_message : Instances.message -> t -> t

  (** {6 Blocking events} *)

  type event_predicate =
    int option -> Matching.t ->
    (Instantiation.concrete Instantiation.test) list ->
    (Instantiation.concrete Instantiation.action) list ->
    bool

  val set_events_to_block : event_predicate option -> t -> t

  (** {6 Stories} *)

  val add_tracked :
    Pattern.id array -> string ->
    Instantiation.abstract Instantiation.test list list -> t -> t
  val remove_tracked : Pattern.id array -> string option -> t -> t

  (** {6 Species} *)

  val add_tracked_species :
    Pattern.id array -> string ->
    Instantiation.abstract Instantiation.test list list -> t -> t
  val remove_tracked_species : string -> t -> t

  (** {6 Debugging} *)

  type stats = { mixture_stats : Edges.stats }

  val stats : t -> stats
  val print_stats : Format.formatter -> t -> unit

  val debug_print : Format.formatter -> t -> unit

  (** {6 Internals } *)
  val apply_negative_transformation :
    (int,unit) Hashtbl.t ->
    (Instantiation.concrete Instantiation.site) list * Instances.t * Edges.t ->
    Instantiation.concrete Primitives.Transformation.t ->
    (Instantiation.concrete Instantiation.site) list * Instances.t * Edges.t
  val apply_positive_transformation :
    Signature.s -> (int,unit) Hashtbl.t ->
    (Matching.t * int Mods.IntMap.t) *
    (Instantiation.concrete Instantiation.site) list *
    Instances.t * Edges.t ->
    Instantiation.abstract Primitives.Transformation.t ->
    ((Matching.t * int Mods.IntMap.t) *
     (Instantiation.concrete Instantiation.site) list *
     Instances.t * Edges.t) *
    Instantiation.concrete Primitives.Transformation.t
  val apply_concrete_positive_transformation :
    Signature.s -> (int,unit) Hashtbl.t -> Instances.t * Edges.t ->
    Instantiation.concrete Primitives.Transformation.t ->
    Instances.t * Edges.t

val obs_from_transformations :
  Pattern.Env.t -> Edges.t ->
  Instantiation.concrete Primitives.Transformation.t list ->
  (Pattern.id * Agent.t) list * Operator.DepSet.t
(** [obs_from_transformations domain state transformations]
    @return [(obs, deps)] *)

val add_outdated_dependencies : Operator.DepSet.t -> t -> t

val debug_print_instances : Format.formatter -> t -> unit

end
