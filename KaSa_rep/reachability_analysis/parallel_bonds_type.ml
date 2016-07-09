(* Time-stamp: <Jul 02 2016> *)
module PairAgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
                  (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

(*parallel*)
module PairAgentsSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_rule_id *  Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))


(* add an abstract value for a tuple *)
let add_value parameters error x value store_result =
  let error, old_value =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs parameters error x store_result
    with
    | error, None -> error, Usual_domains.Undefined
    | error, Some v -> error, v
  in
  let new_value = Usual_domains.lub old_value value in
  if compare new_value old_value = 0
  then
    error, store_result
  else
    let (agent,site,site',_,_),(agent'',site'',site''',_,_) =
      x
    in
    let () =
      if Remanent_parameters.get_dump_reachability_analysis_parallel parameters
      then
        match value
        with
        | Usual_domains.Val true ->
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameters)
              "When the agent %s has its site %s bound to the site %s of a %s, and its site %s bound to the site %s of a %s, then both instances of %s are the same"
              (Ckappa_sig.string_of_agent_name agent)
              (Ckappa_sig.string_of_site_name site)
              (Ckappa_sig.string_of_site_name site''')
              (Ckappa_sig.string_of_agent_name agent'')
              (Ckappa_sig.string_of_site_name site')
              (Ckappa_sig.string_of_site_name site''')
              (Ckappa_sig.string_of_agent_name agent'')
              (Ckappa_sig.string_of_agent_name agent'')
          in Loggers.print_newline (Remanent_parameters.get_logger parameters)
        | Usual_domains.Val false ->
          let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameters)
              "When the agent %s has its site %s bound to the site %s of a %s, and its site %s bound to the site %s of a %s, then both instances of %s are the different"
              (Ckappa_sig.string_of_agent_name agent)
              (Ckappa_sig.string_of_site_name site)
              (Ckappa_sig.string_of_site_name site''')
              (Ckappa_sig.string_of_agent_name agent'')
              (Ckappa_sig.string_of_site_name site')
              (Ckappa_sig.string_of_site_name site''')
              (Ckappa_sig.string_of_agent_name agent'')
              (Ckappa_sig.string_of_agent_name agent'')
          in Loggers.print_newline (Remanent_parameters.get_logger parameters)
        | Usual_domains.Undefined | Usual_domains.Any -> ()
    in
    let error, store_result =
      PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameters error x new_value store_result
    in
    error, store_result

let project (a,b,c,d,e,f) = (b,c,d,e,f)
let project2 (x,y) = (project x,project y)
let add_value_from_refined_tuple parameters error x =
  add_value parameters error (project2 x)
