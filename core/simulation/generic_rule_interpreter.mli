(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(**Graph rewriting module*)

module Make (Instances : Instances_sig.S) : sig
  type t
  (**Abstract graph*)

  type instance

  type result =
    | Clash
    | Corrected
    | Blocked
    | Success of t
        (** Clash means rectangular approximation failure
      Corrected means molecular ambiguity failure *)

  (** {2 Initialisation} *)

  val empty :
    outputs:(Data.t -> unit) ->
    with_trace:bool ->
    Random.State.t ->
    Model.t ->
    Counter.t ->
    t

  (** {2 algebraic expression computation} *)

  (** [get_alg] is by default [Model.get_alg] but it is not hard
      wired because perturbations can redefined alg_expr.*)

  val value_alg : Counter.t -> t -> Primitives.alg_expr -> Nbr.t

  val value_bool :
    Counter.t -> t -> (Pattern.id array list, int) Alg_expr.bool -> bool

  val activity : t -> float
  val get_edges : t -> Edges.t

  (** {2 Core} *)

  val apply_given_rule :
    debugMode:bool ->
    outputs:(Data.t -> unit) ->
    ?rule_id:int ->
    Model.t ->
    Counter.t ->
    t ->
    Trace.event_kind ->
    Primitives.elementary_rule ->
    result
  (** Returns the graph obtained by applying the rule.
      [rule_id] is mandatory if the rule has an unary rate.*)

  val pick_an_instance : debugMode:bool -> Kappa_terms.Model.t -> t -> instance
  val is_correct_instance : Model.t -> t -> instance -> bool

  val apply_instance :
    debugMode:bool ->
    outputs:(Data.t -> unit) ->
    ?maxConsecutiveBlocked:int ->
    maxConsecutiveClash:int ->
    Model.t ->
    Counter.t ->
    t ->
    instance ->
    int option * bool * t
  (** [apply_rule ~outputs ~maxConsecutiveClash ?is_blocked model counter st]
      Returns [(corresponding_syntactic_rule, is_final_step, new_state)].
      [is_final_step] is determined by the counter.
      [corresponding_syntactic_rule] is equal to None if and only if
      a null event occured *)

  val force_rule :
    debugMode:bool ->
    outputs:(Data.t -> unit) ->
    Model.t ->
    Counter.t ->
    t ->
    Trace.event_kind ->
    ?rule_id:int ->
    Primitives.elementary_rule ->
    t option
  (** Apply the rule for sure if it is possible. Try [apply_rule] but in
      case of null_event, it computes the exact injections of the left hand
      side to do apply the rule and returns the remaining exact injections. *)

  val incorporate_extra_pattern :
    debugMode:bool -> Pattern.Env.t -> t -> Pattern.id -> t

  val overwrite_var : int -> Counter.t -> t -> Primitives.alg_expr -> t

  val update_outdated_activities :
    debugMode:bool ->
    (int -> float -> float -> unit) ->
    Model.t ->
    Counter.t ->
    t ->
    int list ->
    t * int list
  (** Resynchronize the state after a rule application.

      It takes the function to store the new activities as an argument whose
      signature is [store rule_id syntactic_rule_id new_activity].

      As long as you don't use any algebraic variable (that include don't
      pick a rule randomly), you can apply several rules in row before
      resynchronizing. (This is what initial state does.)

      takes the list of perturbations to be tried and returns it updated *)

  val snapshot :
    debugMode:bool -> raw:bool -> Model.t -> Counter.t -> t -> Data.snapshot

  val print : Model.t -> Format.formatter -> t -> unit
  val get_random_state : t -> Random.State.t

  val update_edges_from_actions :
    debugMode:bool ->
    outputs:(Data.t -> unit) ->
    Signature.s ->
    Counter.t ->
    Pattern.Env.t ->
    t ->
    Instantiation.concrete Instantiation.action list
    * Instantiation.concrete Instantiation.site list ->
    t

  val send_instances_message : Instances.message -> t -> t

  (** {2 Blocking events} *)

  type event_predicate =
    int option ->
    Matching.t ->
    Instantiation.concrete Instantiation.test list ->
    Instantiation.concrete Instantiation.action list ->
    bool

  val set_events_to_block : event_predicate option -> t -> t

  (** {2 Stories} *)

  val add_tracked :
    outputs:(Data.t -> unit) ->
    Pattern.id array ->
    string ->
    Instantiation.abstract Instantiation.test list list ->
    t ->
    t

  val remove_tracked : Pattern.id array -> string option -> t -> t

  (** {2 Species} *)

  val add_tracked_species :
    Pattern.id array ->
    string ->
    Instantiation.abstract Instantiation.test list list ->
    t ->
    t

  val remove_tracked_species : string -> t -> t

  (** {2 Debugging} *)

  type stats = { mixture_stats: Edges.stats }

  val stats : t -> stats
  val print_stats : Format.formatter -> t -> unit
  val debug_print : Format.formatter -> t -> unit

  val apply_negative_transformation :
    ?mod_connectivity_store:Roots.mod_ccs_cache ->
    Instances.t ->
    Instantiation.concrete Instantiation.site list * Edges.t ->
    Instantiation.concrete Primitives.Transformation.t ->
    Instantiation.concrete Instantiation.site list * Edges.t
  (** {2 Internals } *)

  val apply_positive_transformation :
    debugMode:bool ->
    Signature.s ->
    ?mod_connectivity_store:Roots.mod_ccs_cache ->
    Instances.t ->
    (Matching.t * int Mods.IntMap.t)
    * Instantiation.concrete Instantiation.site list
    * Edges.t ->
    Instantiation.abstract Primitives.Transformation.t ->
    ((Matching.t * int Mods.IntMap.t)
    * Instantiation.concrete Instantiation.site list
    * Edges.t)
    * Instantiation.concrete Primitives.Transformation.t

  val apply_concrete_positive_transformation :
    Signature.s ->
    ?mod_connectivity_store:Roots.mod_ccs_cache ->
    Instances.t ->
    Edges.t ->
    Instantiation.concrete Primitives.Transformation.t ->
    Edges.t

  val obs_from_transformations :
    debugMode:bool ->
    Pattern.Env.t ->
    Edges.t ->
    Instantiation.concrete Primitives.Transformation.t list ->
    (Pattern.id * Agent.t) list * Operator.DepSet.t
  (** [obs_from_transformations domain state transformations]
    @return [(obs, deps)] *)

  val add_outdated_dependencies : Operator.DepSet.t -> t -> t
  val debug_print_instances : Format.formatter -> t -> unit
end
