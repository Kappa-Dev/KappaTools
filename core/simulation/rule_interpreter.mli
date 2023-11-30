type t
type instance
type result = Clash | Corrected | Blocked | Success of t

val value_alg : Counter.t -> t -> Primitives.alg_expr -> Nbr.t

val empty :
  outputs:(Data.t -> unit) ->
  with_trace:bool ->
  Random.State.t ->
  Kappa_terms.Model.t ->
  Counter.t ->
  t

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

val update_outdated_activities :
  debugMode:bool ->
  (int -> float -> float -> unit) ->
  Model.t ->
  Counter.t ->
  t ->
  int list ->
  t * int list

val overwrite_var : int -> Counter.t -> t -> Primitives.alg_expr -> t

val snapshot :
  debugMode:bool -> raw:bool -> Model.t -> Counter.t -> t -> Data.snapshot

val add_tracked :
  outputs:(Data.t -> unit) ->
  Pattern.id array ->
  string ->
  Instantiation.abstract Instantiation.test list list ->
  t ->
  t

val remove_tracked : Pattern.id array -> string option -> t -> t

val add_tracked_species :
  Pattern.id array ->
  string ->
  Instantiation.abstract Instantiation.test list list ->
  t ->
  t

val remove_tracked_species : string -> t -> t

val value_bool :
  Counter.t -> t -> (Pattern.id array list, int) Alg_expr.bool -> bool

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

val incorporate_extra_pattern :
  debugMode:bool -> Pattern.Env.t -> t -> Pattern.id -> t

val activity : t -> float

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

val apply_concrete_positive_transformation :
  Signature.s ->
  ?mod_connectivity_store:Roots.mod_ccs_cache ->
  Instances.t ->
  Edges.t ->
  Instantiation.concrete Primitives.Transformation.t ->
  Edges.t

val print : Model.t -> Format.formatter -> t -> unit
val pick_an_instance : debugMode:bool -> Kappa_terms.Model.t -> t -> instance
val is_correct_instance : Kappa_terms.Model.t -> t -> instance -> bool
val get_random_state : t -> Random.State.t

val obs_from_transformations :
  debugMode:bool ->
  Kappa_terms.Pattern.Env.t ->
  Kappa_mixtures.Edges.t ->
  Instantiation.concrete Primitives.Transformation.t list ->
  (Kappa_terms.Pattern.id * Instantiation.concrete) list
  * Kappa_generic_toolset.Operator.DepSet.t

val print_stats : Format.formatter -> t -> unit

val apply_negative_transformation :
  ?mod_connectivity_store:Roots.mod_ccs_cache ->
  Instances.t ->
  Agent.t Instantiation.site list * Kappa_mixtures.Edges.t ->
  Agent.t Primitives.Transformation.t ->
  Agent.t Instantiation.site list * Kappa_mixtures.Edges.t

val apply_positive_transformation :
  debugMode:bool ->
  Kappa_mixtures.Signature.s ->
  ?mod_connectivity_store:Roots.mod_ccs_cache ->
  Instances.t ->
  (Kappa_terms.Matching.t * int Kappa_generic_toolset.Mods.IntMap.t)
  * Agent.t Instantiation.site list
  * Kappa_mixtures.Edges.t ->
  Instantiation.abstract Primitives.Transformation.t ->
  ((Kappa_terms.Matching.t * int Kappa_generic_toolset.Mods.IntMap.t)
  * Agent.t Instantiation.site list
  * Kappa_mixtures.Edges.t)
  * Agent.t Primitives.Transformation.t
