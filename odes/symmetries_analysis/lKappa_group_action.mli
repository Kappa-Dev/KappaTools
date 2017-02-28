(** check_orbit_internal_state_permutation ~agent_type ~site1 ~site2 rule
    ~correct rates cache ~counter to_be_checked
    will visit the orbit of rule when swapping the internal states of site1 and site2 in agents of type agent_type;
    rates contains the rate of each rule (stored according to their hash)
    to be divided by the respecive integer in correct.
    counter is an array of 0 (after the call, the array is reset to 0.
    to_be_checked is an array of boolean: false means that there is no rule corresponding to this hash, or that this rule is already in an orbit;
     true, means that there is a rule corresponding to this hash, and it does not belong to a visited orbit *)

val check_orbit_internal_state_permutation:
  agent_type:int ->
  site1:int ->
  site2:int ->
  LKappa.rule ->
  correct:(int array) -> (*what i have to divide to get gamma *)
  ('a, 'b) Alg_expr.e Locality.annot Rule_modes.RuleModeMap.t array ->
  LKappa_auto.cache ->
  counter:(int array) -> (*counter the number of array of orbit*)
  bool array -> (LKappa_auto.cache * int array * bool array) * bool

(**
   check_orbit_binding_state_permutation ~agent_type ~site1 ~site2 rule
    ~correct rates cache ~counter to_be_checked
    will visit the orbit of rule when swapping the binding states of site1 and site2 in agents of type agent_type;
    rates contains the rate of each rule (stored according to their hash)
    to be divided by the respecive integer in correct.
    counter is an array of 0 (after the call, the array is reset to 0.
    to_be_checked is an array of boolean: false means that there is no rule corresponding to this hash, or that this rule is already in an orbit;
     true, means that there is a rule corresponding to this hash, and it does not belong to a visited orbit *)
val check_orbit_binding_state_permutation:
  agent_type:int ->
  site1:int ->
  site2:int ->
  LKappa.rule ->
  correct:(int array) ->
  ('a, 'b) Alg_expr.e Locality.annot Rule_modes.RuleModeMap.t array ->
  LKappa_auto.cache ->
  counter:(int array) ->
  bool array -> (LKappa_auto.cache * int array * bool array) * bool
