(** fold_over_orbit positions sigma sigma_inv sigma_raw sigma_raw_inv f rule init iterates the function f over the orbit of rule by a subgroup of transformations.
sigma and sigma_inv apply a transformation and its inverse to tested/modified/deleted agents.
sigma_raw and sigma_raw_inv apply a transformatio and its inverse to created agents.
These four functions operate by side-effects (LKappa agents are imperatif).
The subgroup is defined by these transformations on agents and the list of  agents positions, where the transformation may be applied.  Positions starts at 0 with the agents that may be tested/modified/deleted, followed by the ones that may be created.
At the end of the itteration, the rule has the same value as before the iteration.*)

val fold_over_orbit:
    int list ->
    (LKappa.rule_agent -> unit) ->
    (LKappa.rule_agent -> unit) ->
    (Raw_mixture.agent -> unit) ->
    (Raw_mixture.agent -> unit) ->
    (LKappa.rule -> 'a -> 'a) ->
    LKappa.rule ->
    'a ->
    'a
