let normalize ?parameters signature rule_cache cache symmetries cc =
  match
    Raw_mixture_extra.species_to_raw_mixture ?parameters signature cc
  with
  | Some (raw_mixture, unspec) ->
    let rule_cache, raw_mixture =
      Raw_mixture_group_action.normalize rule_cache symmetries
        raw_mixture
    in
    let a, b, _ =
      Raw_mixture_extra.raw_mixture_to_species
        ?parameters ~signature cache raw_mixture unspec
    in
    rule_cache, a, b
  | None -> rule_cache, cache, cc
