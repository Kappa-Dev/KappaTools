let normalize_internal_states_in_raw_mixture signature cache symmetries cc =
  match
    Raw_mixture_extra.pattern_to_raw_mixture signature cc
  with
  | Some (raw_mixture, unspec) ->
    let raw_mixture =
      Raw_mixture_group_action.normalize_internal_states_in_raw_mixture
        symmetries raw_mixture
    in
    let a,b,_ =
      Raw_mixture_extra.raw_mixture_to_pattern cache raw_mixture unspec
    in
    a,b
  | None ->
    cache, cc
