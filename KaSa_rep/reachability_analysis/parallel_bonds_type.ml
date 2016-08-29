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
           Ckappa_sig.c_agent_id * ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state) *
           (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name *
            Ckappa_sig.c_site_name * Ckappa_sig.c_state * Ckappa_sig.c_state))
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
  let error, agent =
    Handler.translate_agent
      ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site'' in
  let error, site''' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site''' in
  let error, agent'' =
    Handler.translate_agent
      ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent''
  in
  error, (agent,site,site',agent'',site'',site''')

let convert_refined_tuple parameters error kappa_handler tuple =
  let (agent_id,agent,site,site',_,_),(agent_id',agent'',site'',site''',_,_) =
    tuple
  in
  let error, site = Handler.string_of_site_contact_map parameters error kappa_handler agent site in
  let error, site' = Handler.string_of_site_contact_map parameters error kappa_handler agent site' in
  let error, agent =
    Handler.translate_agent
      ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent
  in
  let error, site'' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site'' in
  let error, site''' = Handler.string_of_site_contact_map parameters error kappa_handler agent'' site''' in
  let error, agent'' =
    Handler.translate_agent
      ~message:"unknown agent type" ~ml_pos:(Some __POS__)
      parameters error kappa_handler agent''
  in
  error, (Ckappa_sig.string_of_agent_id agent_id, agent,site,site',
          Ckappa_sig.string_of_agent_id agent_id',agent'',site'',site''')

let cons_opt a l =
  match a with None -> l
             | Some a -> a::l

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
  let error, t_same_self =
    if agent = agent'' && site<>site'' && site'<>site'''
    then
      let error, t_same_self =
        Ckappa_backend.Ckappa_backend.add_bond
          parameters error kappa_handler
          agent_id site agent_id site'' t_precondition
      in
      error, Some t_same_self
    else
      error, None
  in
  let error, agent_id'', t_same =
    Ckappa_backend.Ckappa_backend.add_agent
      parameters error kappa_handler
      agent'' t_precondition
  in
  let error, t_distinct_self1 =
    if agent = agent'' &&
       site <> site'' && site <> site' && site' <> site''
    then
      let error, t_distinct_self1 =
        Ckappa_backend.Ckappa_backend.add_bond
          parameters error kappa_handler
          agent_id site' agent_id'' site'''  t_same
      in
      let error, t_distinct_self1 =
        Ckappa_backend.Ckappa_backend.add_bond
          parameters error kappa_handler
          agent_id site agent_id site'' t_distinct_self1
      in
      error, Some t_distinct_self1
    else
      error, None
  in
  let error, t_same =
    Ckappa_backend.Ckappa_backend.add_bond
      parameters error kappa_handler
      agent_id site agent_id'' site'' t_same
  in
  let error, t_distinct_self2 =
    if agent = agent'' &&
       site' <> site''' && site' <> site && site<>site'''
    then
      let error, t_distinct_self2 =
        Ckappa_backend.Ckappa_backend.add_bond
          parameters error kappa_handler
          agent_id site' agent_id site''' t_same
      in error, Some t_distinct_self2
    else
      error, None
  in
  let error, agent_id''', t_distinct =
    Ckappa_backend.Ckappa_backend.add_agent
      parameters error kappa_handler
      agent'' t_same
  in
  let error, t_distinct =
    Ckappa_backend.Ckappa_backend.add_bond
      parameters error kappa_handler
      agent_id site' agent_id''' site''' t_distinct
  in
  let error, t_same =
    Ckappa_backend.Ckappa_backend.add_bond
      parameters error kappa_handler
      agent_id site' agent_id'' site''' t_same
  in
  let list_same =
    t_same::(cons_opt t_same_self [])
  in
  let list_distinct =
    t_distinct::
    (cons_opt t_distinct_self1
       (cons_opt t_distinct_self2 []))
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
                    Loggers.fprintf
                      (Remanent_parameters.get_logger parameters) "%s" prefix
                  in error
              in
              let error = Ckappa_backend.Ckappa_backend.print_list
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  list_same
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
                  "%s" prefix
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
              let error =
                if verbose then
                  let error =
                    Ckappa_backend.Ckappa_backend.print
                      (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                      t_precondition
                  in
                  let () =
                    Loggers.fprintf (Remanent_parameters.get_logger parameters) " => "
                  in error
                else
                  let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_prefix parameters)
                  in error
              in
              let error =
                Ckappa_backend.Ckappa_backend.print_list
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  list_distinct
              in
              let () =
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              in
              error
            end
          | Remanent_parameters_sig.Natural_language ->
            let error =
              if verbose then
                let () =
                  Loggers.fprintf (Remanent_parameters.get_logger parameters)
                    "%sWhen the agent %s has its site %s bound to the site %s of a %s, \
                     and its site %s bound to the site %s of a %s, then both instances of %s %s  different."
                    prefix string_agent string_site string_site'' string_agent'' string_site' string_site''' string_agent'' string_agent'' modalite
                in error
              else
                let error =
                  Ckappa_backend.Ckappa_backend.print
                    (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                    t_distinct
                in error
            in
            let () =
              Loggers.print_newline (Remanent_parameters.get_logger parameters)
            in error
        end
      | Usual_domains.Undefined -> error
      | Usual_domains.Any ->
        let error =
          if dump_any then
            if verbose then
              let () =
                match
                  Remanent_parameters.get_backend_mode parameters
                with
                | Remanent_parameters_sig.Kappa
                | Remanent_parameters_sig.Raw -> ()
                | Remanent_parameters_sig.Natural_language ->
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    "%sWhen the agent %s has its site %s bound to the site %s of a %s, \
                     and its site %s bound to the site %s of a %s, then both instances of %s may be  different or not."
                    prefix string_agent string_site string_site'' string_agent'' string_site' string_site''' string_agent'' string_agent''
              in error
            else
              let error =
                Ckappa_backend.Ckappa_backend.print
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  t_same
              in
              let () =
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              in
              let error =
                Ckappa_backend.Ckappa_backend.print
                  (Remanent_parameters.get_logger parameters) parameters error kappa_handler
                  t_distinct
              in
              let () =
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              in
              error
          else
            error
        in
        let () =
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
let get_id ((a,_,_,_,_,_),_) = a
let get_tuple (a,b) = project a,project b

let project2 = snd

let add_value_from_refined_tuple parameters error kappa_handler x =
  add_value parameters error kappa_handler (project2 x)

let swap_sites_in_tuple (a,b,s,s',st,st') = (a,b,s',s,st',st)

let add_symmetric_tuple_pair f parameter error (x,y) remanent =
  let x' = swap_sites_in_tuple x in
  let y' = swap_sites_in_tuple y in
  List.fold_left
    (fun (error, remanent) t ->
       f
         parameter error (get_id t,get_tuple t) remanent
    )
    (error, remanent)
    [x,y;(*y,x;*)x',y';(*y',x'*)]
