(* Time-stamp: <Jul 02 2016> *)
module PairAgentsSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module PairAgentSiteState_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
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
           (Ckappa_sig.c_agent_id * Ckappa_sig.c_agent_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state)
         let compare = compare
         let print _ _ = ()
       end))

module AgentsSitesStates_map_and_set =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t =
           (Ckappa_sig.c_rule_id *  Ckappa_sig.c_agent_id *
            Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state)
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
  error, (Ckappa_sig.string_of_agent_id agent_id, agent,site,site',
          Ckappa_sig.string_of_agent_id agent_id',agent'',site'',site''')

let print_parallel_constraint
    ?verbose:(verbose=true)
    ?sparse:(sparse=false)
    ?final_resul:(final_result=false)
    ?dump_any:(dump_any=false) parameters error kappa_handler tuple value =
  let modalite = if final_result then "are necessarily" else "may be" in
  let prefix = Remanent_parameters.get_prefix parameters in
  let error, (string_agent,string_site,string_site',string_agent'',string_site'',string_site''') =
    convert_tuple parameters error kappa_handler tuple
  in
  let (agent,site,site',_,_),(agent'',site'',site''',_,_) =
    tuple
  in
  let t_precondition = Ckappa_backend.Ckappa_backend.empty in
  let error, agent_id, t_precondition =
    Ckappa_backend.Ckappa_backend.add_agent
      parameters error kappa_handler
      agent t_precondition in
  let error, t_precondition =
    Ckappa_backend.Ckappa_backend.add_bond_type
      parameters error kappa_handler
      agent_id site agent'' site'' t_precondition
  in
  let error, t_precondition =
    Ckappa_backend.Ckappa_backend.add_bond_type
      parameters error kappa_handler
      agent_id site' agent'' site''' t_precondition
  in
  let error, agent_id'', t_same =
    Ckappa_backend.Ckappa_backend.add_agent
      parameters error kappa_handler
      agent'' t_precondition
  in
  let error, t_same =
    Ckappa_backend.Ckappa_backend.add_bond
      parameters error kappa_handler
      agent_id site agent_id'' site'' t_same
  in


  let error, t_same =
    Ckappa_backend.Ckappa_backend.add_bond
      parameters error kappa_handler
      agent_id site' agent_id'' site''' t_same
  in
  if sparse && compare site site' > 0
  then error
  else
    let error =
      match value
      with
      | Usual_domains.Val true ->
        begin
          match Remanent_parameters.get_backend_mode parameters
          with
          | Remanent_parameters_sig.Kappa
          | Remanent_parameters_sig.Raw ->
            begin
              let error =
                if verbose
                then
                  let error =
                    Ckappa_backend.Ckappa_backend.print
                      (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                      t_precondition
                  in
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters) " => "
                  in
                  error
                else
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_prefix parameters)
                  in error
              in
              let error =
                Ckappa_backend.Ckappa_backend.print
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  t_same
              in
              let () =
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              in
              error
            end
          | Remanent_parameters_sig.Natural_language ->
            if verbose then
              let () =
                Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "%sWhen the agent %s has its site %s bound to the site %s of a %s, \
                   and its site %s bound to the site %s of a %s, then both instances of %s %s the same."
                  prefix string_agent string_site string_site'' string_agent'' string_site' string_site''' string_agent'' string_agent'' modalite in error
            else
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%s" (Remanent_parameters.get_prefix parameters)
              in
              let error =
                Ckappa_backend.Ckappa_backend.print
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  t_same
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error
        end
      | Usual_domains.Val false ->
        begin
          match Remanent_parameters.get_backend_mode parameters
          with
          | Remanent_parameters_sig.Kappa
          | Remanent_parameters_sig.Raw ->
            begin
              let (agent,site,site',_,_),(agent'',site'',site''',_,_) =
                tuple
              in
              let t = Ckappa_backend.Ckappa_backend.empty in
              let error, agent_id, t =
                Ckappa_backend.Ckappa_backend.add_agent
                  parameters error kappa_handler
                  agent t in
              let error, t =
                Ckappa_backend.Ckappa_backend.add_bond_type
                  parameters error kappa_handler
                  agent_id site agent'' site'' t
              in
              let error, t =
                Ckappa_backend.Ckappa_backend.add_bond_type
                  parameters error kappa_handler
                  agent_id site' agent'' site''' t
              in
              let error, agent_id'', t' =
                Ckappa_backend.Ckappa_backend.add_agent
                  parameters error kappa_handler
                  agent'' t
              in
              let error, t' =
                Ckappa_backend.Ckappa_backend.add_bond
                  parameters error kappa_handler
                  agent_id site agent_id'' site'' t'
              in
              let error, agent_id''', t' =
                Ckappa_backend.Ckappa_backend.add_agent
                  parameters error kappa_handler
                  agent'' t'
              in
              let error, t' =
                Ckappa_backend.Ckappa_backend.add_bond
                  parameters error kappa_handler
                  agent_id site' agent_id''' site''' t'
              in
              let error =
                if verbose then
                  let error =
                    Ckappa_backend.Ckappa_backend.print
                      (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                      t
                  in
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters) " => "
                  in error
                else
                  let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_prefix parameters)
                  in error
              in
              let error =
                Ckappa_backend.Ckappa_backend.print
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  t'
              in
              let () =
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              in
              error
            end
          | Remanent_parameters_sig.Natural_language ->
            let () =
              if verbose then
                Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "%sWhen the agent %s has its site %s bound to the site %s of a %s, \
                   and its site %s bound to the site %s of a %s, then both instances of %s %s  different."
                  prefix string_agent string_site string_site'' string_agent'' string_site' string_site''' string_agent'' string_agent'' modalite
              else
                Loggers.fprintf (Remanent_parameters.get_logger parameters)
                  "%s%s(%s!1,%s!2),%s(%s!1),%s(%s!2)"
                  prefix string_agent string_site string_site' string_agent'' string_site'' string_agent string_site'''
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in error
        end
      | Usual_domains.Undefined -> error
      | Usual_domains.Any ->
        let () =
          if dump_any then
            let () =
              if verbose then
                match Remanent_parameters.get_backend_mode parameters
                with
                | Remanent_parameters_sig.Kappa
                | Remanent_parameters_sig.Raw -> ()
                | Remanent_parameters_sig.Natural_language ->
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    "%sWhen the agent %s has its site %s bound to the site %s of a %s, \
                     and its site %s bound to the site %s of a %s, then both instances of %s may be  different or not."
                    prefix string_agent string_site string_site'' string_agent'' string_site' string_site''' string_agent'' string_agent''
              else
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "%s%s(%s!1,%s!2),%s(%s!1,%s!2)"
                    prefix string_agent string_site string_site'' string_agent'' string_site' string_site'''
                in
                let () =
                  Loggers.print_newline (Remanent_parameters.get_logger parameters)
                in
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "%s%s(%s!1,%s!2),%s(%s!1),%s(%s!2)"
                    prefix string_agent string_site string_site'' string_agent'' string_site' string_agent string_site'''
                in
                ()
            in
            Loggers.print_newline
              (Remanent_parameters.get_logger parameters)
        in error
    in error

(* add an abstract value for a tuple *)
let add_value parameters error kappa_handler x value store_result =
  let error, old_value =
    match
      PairAgentSitesStates_map_and_set.Map.find_option_without_logs
        parameters error x store_result
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
        let parameters = Remanent_parameters.update_prefix parameters "         " in
        print_parallel_constraint
          ~verbose:false
          ~dump_any:true parameters error kappa_handler x value
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
