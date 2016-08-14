module Ckappa_backend =
struct

  type agent_id = Ckappa_sig.c_agent_id
  type bond_index = int

  type t =
    {
      views:
        (Ckappa_sig.c_agent_name *
         ((Ckappa_sig.c_state * Ckappa_sig.c_state))
           Ckappa_sig.Site_map_and_set.Map.t) Ckappa_sig.Agent_id_map_and_set.Map.t ;
      fresh_agent_id: agent_id ;
      fresh_bond_id: bond_index ;
      bonds:
        bond_index
          Ckappa_sig.Site_map_and_set.Map.t Ckappa_sig.Agent_id_map_and_set.Map.t;
      stabilized:
        (string *
         (string option * string option)
           Wrapped_modules.LoggedStringMap.t)
          Ckappa_sig.Agent_id_map_and_set.Map.t
          option
    }

  let empty =
    {
      views =  Ckappa_sig.Agent_id_map_and_set.Map.empty ;
      fresh_agent_id = Ckappa_sig.dummy_agent_id ;
      fresh_bond_id = 1 ;
      bonds = Ckappa_sig.Agent_id_map_and_set.Map.empty ;
      stabilized = None
    }

  let add_state_interv
      parameter kappa_handler
      agent agent_string agent_type
      site state (error, string_map)
    =
    error, string_map

  let add_bond_index
      parameter kappa_handler
      agent agent_string agent_type
      site bond (error, string_map)
    =
    error, string_map

  let stabilize parameter error kappa_handler t =
    match t.stabilized with
    | Some _ -> error, t
    | None ->
      let error, string_map =
        Ckappa_sig.Agent_id_map_and_set.Map.fold2
          parameter
          error
          (fun parameter error agent_id (agent_type,state_map) agent_map ->
             let error, agent_string =
               Handler.translate_agent
                 parameter error kappa_handler (agent_type:Ckappa_sig.c_agent_name)
             in
             let error, (agent_string', site_map) =
               Ckappa_sig.Agent_id_map_and_set.Map.find_default_without_logs
                 parameter error
                 (agent_string,
                  Wrapped_modules.LoggedStringMap.empty)
                 agent_id
                 agent_map
             in
             let error,() =
               if agent_string = agent_string'
               then
                 error,()
               else
                 Exception.warn
                   parameter error __POS__
                   ~message:"inconsistent agent type"
                   Exit
                   ()
             in
             let error, site_map =
               Ckappa_sig.Site_map_and_set.Map.fold
                 (add_state_interv parameter kappa_handler agent_id agent_string agent_type)
                 state_map
                 (error, site_map)
             in
             Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
               parameter error
               agent_id
               (agent_string, site_map)
               agent_map)
          (fun parameter error agent_id bond_map agent_map ->
             Exception.warn
               parameter error __POS__ Exit agent_map)
          (fun parameter error agent_id (agent_type,state_map) bond_map agent_map ->
             let error, agent_string =
               Handler.translate_agent parameter error kappa_handler agent_type
             in
             let error, (agent_string', site_map) =
               Ckappa_sig.Agent_id_map_and_set.Map.find_default_without_logs
                 parameter error
                 (agent_string,
                  Wrapped_modules.LoggedStringMap.empty)
                 agent_id
                 agent_map
             in
             let error,() =
               if agent_string = agent_string'
               then
                 error,()
               else
                 Exception.warn
                   parameter error __POS__
                   ~message:"inconsistent agent type"
                   Exit
                   ()
             in
             let error, site_map =
               Ckappa_sig.Site_map_and_set.Map.fold2
                 parameter error
                 (fun parameter error site state map ->
                    add_state_interv parameter kappa_handler
                      agent_id agent_string agent_type site state (error, map))
                 (fun parameter error site bond map ->
                    add_bond_index parameter kappa_handler
                      agent_id agent_string agent_type site bond
                      (error, map))
                 (fun parameter error site _ bond map ->
                    add_bond_index parameter kappa_handler
                      agent_id agent_string agent_type site bond
                      (error, map))
                 state_map
                 bond_map
                 site_map
             in
             Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
               parameter error
               agent_id (agent_string, site_map) agent_map)
          t.views
          t.bonds
          Ckappa_sig.Agent_id_map_and_set.Map.empty
      in
      error,
      {
        t with stabilized = Some string_map
      }

  let unstabilize  t =
    {
      t with stabilized = None
    }

  let check_stability new_t old_t =
    if old_t == new_t
    then
      new_t
    else
      {new_t with stabilized = None}

  let add_agent parameter error kappa_handler agent_type t =
    let error', _ =
      Handler.translate_agent parameter error kappa_handler agent_type
    in
    let error =
      Exception.check_point
        Exception.warn parameter error error' __POS__
        ~message:"unknown agent type"
        Exit
    in
    let agent_id = t.fresh_agent_id in
    let error', views =
      Ckappa_sig.Agent_id_map_and_set.Map.add
        parameter error
        agent_id
        (agent_type, Ckappa_sig.Site_map_and_set.Map.empty)
        t.views
    in
    let error =
      Exception.check_point
        Exception.warn parameter error error' __POS__
        ~message:"this agent id is already used"
        Exit
    in
    error, t.fresh_agent_id,
    check_stability
      {t
       with
        fresh_agent_id = Ckappa_sig.next_agent_id t.fresh_agent_id ;
        views = views} t

  let max_state_index a b =
    if Ckappa_sig.compare_state_index a b <= 0
    then b else a
  let min_state_index a b =
    if Ckappa_sig.compare_state_index a b <= 0
    then a else b

  let add_state_interv parameter error kappa_handler agent_id site
      state_min state_max t =
    let error, agent_op =
      Ckappa_sig.Agent_id_map_and_set.Map.find_option
        parameter error
        agent_id
        t.views
    in
    match agent_op with
    | None ->
      Exception.warn
        parameter error __POS__
        ~message:"unknown agent type"
        Exit t
    | Some (agent_type, map) ->
      begin
        let error', _ =
          Handler.translate_site parameter error kappa_handler agent_type site
        in
        let error =
          Exception.check_point
            Exception.warn parameter error error' __POS__
            ~message:"undefined site"
            Exit
        in
        let error', _ =
          Handler.translate_state parameter error kappa_handler agent_type site state_min
        in
        let error =
          Exception.check_point
            Exception.warn
            parameter error error' __POS__
            ~message:"undefined site state"
            Exit
        in
        let error', _ =
          Handler.translate_state parameter error kappa_handler agent_type site state_max
        in
        let error =
          Exception.check_point
            Exception.warn
            parameter error error' __POS__
            ~message:"undefined site state"
            Exit
        in
        let error, old_asso =
          Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
            parameter error
            site map
        in
        let error, new_map =
          match old_asso with
          | None ->
            Ckappa_sig.Site_map_and_set.Map.add
              parameter error
              site (state_min,state_max)
              map
          | Some (old_min, old_max) ->
            if Ckappa_sig.compare_state_index state_min old_max  <= 0
            || Ckappa_sig.compare_state_index state_min old_max <= 0
            then
              let new_min = max_state_index state_min old_min in
              let new_max = min_state_index state_max old_max in
              if new_min = old_min && new_max = old_max
              then
                error, map
              else
                Ckappa_sig.Site_map_and_set.Map.overwrite
                  parameter error
                  site (new_min,new_max)
                  map
            else
              Exception.warn
                parameter error __POS__
                ~message:"incompatible states"
                Exit map
        in
        let error, views =
          Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
            parameter error
            agent_id (agent_type, new_map)
            t.views
        in
        error,
        check_stability
          {t with views = views} t
      end

  let add_state parameter error kappa_handler agent_id site
      state t =
    add_state_interv parameter error kappa_handler agent_id site
      state state t

  let add_bond _parameter error _kappa_handler
      _agent_id _site _agent_id' _site' t =
    error, t
  let add_bound_to_unknown
      _parameter error _kappa_handler
      _agent_id _site t =
    error, t
  let to_json parameter error kappa_handler t =
    error, `Assoc []

  let print _logger _parameter error _kappa_handler _t  = error


end

let main () = ()
