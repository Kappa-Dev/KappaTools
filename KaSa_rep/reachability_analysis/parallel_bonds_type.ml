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

let convert_tuple parameters error kappa_handler tuple =
  let (agent,site,site',_,_),(agent'',site'',site''',_,_) =
    tuple
  in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site'' in
  let error, site''' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site''' in
  let error, agent'' = Handler.translate_agent parameters error kappa_handler agent'' in
  error, (agent,site,site',agent'',site'',site''')

let convert_refined_tuple parameters error kappa_handler tuple =
  let (agent_id,agent,site,site',_,_),(agent_id',agent'',site'',site''',_,_) =
    tuple
  in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, agent = Handler.translate_agent parameters error kappa_handler agent in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site'' in
  let error, site''' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site''' in
  let error, agent'' = Handler.translate_agent parameters error kappa_handler agent'' in
  error, (Ckappa_sig.string_of_agent_id agent_id, agent,site,site',Ckappa_sig.string_of_agent_id agent_id',agent'',site'',site''')
let print_parallel_constraint ?prefix:(prefix="") ?final_resul:(final_result=false)
    ?dump_any:(dump_any=false) parameters error kappa_handler tuple value =
  let modalite = if final_result then "are necessarily" else "may be" in
  let error, (agent,site,site',agent'',site'',site''') =
    convert_tuple parameters error kappa_handler tuple
  in
  let () =
    match value
    with
    | Usual_domains.Val true ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "When the agent %s has its site %s bound to the site %s of a %s, and its site %s bound to the site %s of a %s, then both instances of %s %s the same"
          agent site site'' agent'' site' site''' agent'' agent'' modalite
      in Loggers.print_newline (Remanent_parameters.get_logger parameters)
    | Usual_domains.Val false ->
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "When the agent %s has its site %s bound to the site %s of a %s, and its site %s bound to the site %s of a %s, then both instances of %s %s  different"
          agent site site'' agent'' site' site''' agent'' agent'' modalite
      in Loggers.print_newline (Remanent_parameters.get_logger parameters)
    | Usual_domains.Undefined -> ()
    | Usual_domains.Any ->
      if dump_any then
        let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameters)
            "When the agent %s has its site %s bound to the site %s of a %s, and its site %s bound to the site %s of a %s, then both instances of %s may be  different or not"
            agent site site'' agent'' site' site''' agent'' agent''
        in Loggers.print_newline (Remanent_parameters.get_logger parameters)
  in error

(* add an abstract value for a tuple *)
let add_value parameters error kappa_handler x value store_result =
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
    let error =
      if Remanent_parameters.get_dump_reachability_analysis_diff parameters
      then
        print_parallel_constraint ~dump_any:true parameters error kappa_handler x value
      else error
    in
    let error, store_result =
      PairAgentSitesStates_map_and_set.Map.add_or_overwrite parameters error x new_value store_result
    in
    error, store_result

let project (_,b,c,d,e,f) = (b,c,d,e,f)
let project2 (x,y) = (project x,project y)
let add_value_from_refined_tuple parameters error kappa_handler x =
  add_value parameters error kappa_handler (project2 x)


let swap_sites_in_tuple (a,b,s,s',st,st') = (a,b,s',s,st',st)

let add_symmetric_tuple_pair f parameter error (x,y) remanent =
  let x' = swap_sites_in_tuple x in
  let y' = swap_sites_in_tuple y in
  List.fold_left
    (fun (error, remanent) t ->
       f
         parameter error t remanent
      )
      (error, remanent)
      [x,y;(*y,x;*)x',y';(*y',x'*)]
