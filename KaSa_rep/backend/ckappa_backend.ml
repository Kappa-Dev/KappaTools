module Ckappa_backend =
struct

  type agent_id = Ckappa_sig.c_agent_id
  type bond_index = int

  type binding_state =
    | Free | Wildcard | Bound_to_unknown
    | Binding_type of Ckappa_sig.agent_name * Ckappa_sig.site_name
    | Bound_to of bond_index

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
      string_version:
        (string *
         (string option * binding_state option)
           Wrapped_modules.LoggedStringMap.t)
          Ckappa_sig.Agent_id_map_and_set.Map.t
    }

  let empty =
    {
      views =  Ckappa_sig.Agent_id_map_and_set.Map.empty ;
      fresh_agent_id = Ckappa_sig.dummy_agent_id ;
      fresh_bond_id = 1 ;
      bonds = Ckappa_sig.Agent_id_map_and_set.Map.empty ;
      string_version = Ckappa_sig.Agent_id_map_and_set.Map.empty ;
    }

  let add_agent parameter error kappa_handler agent_type t =
    let error', agent_string =
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
    let error', string_version =
      Ckappa_sig.Agent_id_map_and_set.Map.add
        parameter error
        agent_id
        (agent_string, Wrapped_modules.LoggedStringMap.empty)
        t.string_version
    in
    let error =
      Exception.check_point
        Exception.warn parameter error error' __POS__
        ~message:"this agent id is already used"
        Exit
    in
    error, t.fresh_agent_id,
    {t
     with
      fresh_agent_id = Ckappa_sig.next_agent_id t.fresh_agent_id ;
      views = views ;
      string_version = string_version }

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
        let error', site_string =
          Handler.string_of_site_contact_map
            parameter error kappa_handler agent_type site
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
        let error, (new_map, range_opt) =
          match old_asso with
          | None ->
            let error , map =
              Ckappa_sig.Site_map_and_set.Map.add
                parameter error
                site (state_min,state_max)
                map
            in
            error, (map, Some (state_min, state_max))
          | Some (old_min, old_max) ->
            if Ckappa_sig.compare_state_index state_min old_max  <= 0
            || Ckappa_sig.compare_state_index state_min old_max <= 0
            then
              let new_min = max_state_index state_min old_min in
              let new_max = min_state_index state_max old_max in
              if new_min = old_min && new_max = old_max
              then
                error, (map, None)
              else
                let error', map =
                  Ckappa_sig.Site_map_and_set.Map.overwrite
                    parameter error
                    site (new_min,new_max)
                    map
                in
                let error =
                  Exception.check_point
                    Exception.warn
                    parameter error error' __POS__ Exit
                in
                error, (map, Some (new_min, new_max))
            else
              Exception.warn
                parameter error __POS__
                ~message:"incompatible states"
                Exit (map, None)
        in
        let error, views =
          Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
            parameter error
            agent_id (agent_type, new_map)
            t.views
        in
        match range_opt
        with
        | None -> error, t
        | Some (state_min,state_max) ->
          let error, is_binding_site =
            Handler.is_binding_site parameter error kappa_handler
              agent_type site
          in
          let error,
              (internal_state_string_opt, binding_state_opt)
            =
            match state_min = state_max, is_binding_site with
            | true, true ->
              if state_min = Ckappa_sig.dummy_state_index
              then
                error, (None, Some Free)
              else
                let error, triple_opt =
                  Handler.dual
                    parameter error kappa_handler
                    agent_type site state_min
                in
                let error, (agent_type', site') =
                  match triple_opt with
                  | None ->
                    Exception.warn
                      parameter error __POS__
                      Exit
                      (Ckappa_sig.dummy_agent_name, Ckappa_sig.dummy_site_name)
                  | Some (agent_type', site', _) ->
                    error, (agent_type', site')
                in
                let error, agent_string' =
                  Handler.translate_agent
                    parameter error kappa_handler
                    agent_type'
                in
                let error, site_string' =
                  Handler.string_of_site_contact_map
                    parameter error kappa_handler
                    agent_type' site'
                in
                error,
                (None, Some (Binding_type (agent_string',site_string')))
            | true, false ->
              let error, state_string =
                Handler.string_of_state
                  parameter error kappa_handler
                  agent_type site state_min
              in
              error, (Some state_string, None)
            | false, true ->
              if state_min = Ckappa_sig.dummy_state_index
              then error, (None, Some Wildcard)
              else error, (None, Some Bound_to_unknown)
            | false,false ->
              begin
                Exception.warn
                  parameter error __POS__
                  Exit (None, None)
              end
          in
          let error, (agent_string, sitemap) =
            Ckappa_sig.Agent_id_map_and_set.Map.find_default
              parameter error
              ("",Wrapped_modules.LoggedStringMap.empty)
              agent_id
              t.string_version
          in
          let error, (old_state,old_binding) =
            Wrapped_modules.LoggedStringMap.find_default_without_logs
              parameter error
              (None,None)
              site_string
              sitemap
          in
          let new_internal_state =
            match internal_state_string_opt with
            | None -> old_state
            | Some x -> Some ("~"^x)
          in
          let error, new_binding_state =
            match
              internal_state_string_opt, binding_state_opt, old_binding
            with
            | Some _, None, None ->
              let error, bool =
                Handler.has_a_binding_state parameter error kappa_handler agent_type site
              in
              if bool then
                error, Some Wildcard
              else
                error, old_binding
            | _, Some x, _  -> error, binding_state_opt
            | _, None, _ -> error, old_binding
          in
          let error, sitemap =
            Wrapped_modules.LoggedStringMap.add_or_overwrite
              parameter error
              site_string
              (new_internal_state,new_binding_state)
              sitemap
          in
          let error', string_version =  Ckappa_sig.Agent_id_map_and_set.Map.overwrite
              parameter error
              agent_id
              (agent_string, sitemap)
              t.string_version
          in
          let error =
            Exception.check_point
              Exception.warn
              parameter error error' __POS__ Exit
          in
          error,
          {
            t
            with views = views ;
                 string_version = string_version
          }
  end

let add_state parameter error kappa_handler agent_id site
    state t =
  add_state_interv parameter error kappa_handler agent_id site
    state state t

let add_bound_to_unknown
    parameter error kappa_handler
    agent_id site t =
  let error, (agent_type, _) =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default
      parameter error
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.Site_map_and_set.Map.empty)
      agent_id
      t.views
  in
  let error, state_max =
    Handler.last_state_of_site
      parameter error kappa_handler
      agent_type site
  in
  add_state_interv parameter error kappa_handler agent_id site
    Ckappa_sig.dummy_state_index_1 state_max t

let add_bond_to parameter error kappa_handler agent_id site bond_id t =
  let error, (agent_type, _) =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default
      parameter error
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.Site_map_and_set.Map.empty)
      agent_id
      t.views
  in
  let error, old_site_map =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default_without_logs
      parameter error
      Ckappa_sig.Site_map_and_set.Map.empty
      agent_id
      t.bonds
  in
  let error, new_site_map =
    Ckappa_sig.Site_map_and_set.Map.add
      parameter error
      site
      bond_id
      old_site_map
  in
  let error', new_bonds =
    Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
      parameter error
      agent_id
      new_site_map
      t.bonds
  in
  let error =
    Exception.check_point
      Exception.warn
      parameter error error' __POS__ Exit
  in
  let error, (agent_string, old_site_map) =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default
      parameter error
      ("",Wrapped_modules.LoggedStringMap.empty)
      agent_id
      t.string_version
  in
  let error, site_string =
    Handler.string_of_site_contact_map parameter error kappa_handler agent_type site
  in
  let error, (old_internal, old_binding) =
    Wrapped_modules.LoggedStringMap.find_default_without_logs
      parameter error
      (None,None)
      site_string
      old_site_map
  in
  let error, new_site_map =
    Wrapped_modules.LoggedStringMap.add_or_overwrite
      parameter error
      site_string
      (None, Some (Bound_to bond_id))
      old_site_map
  in
  let error', string_version =
    Ckappa_sig.Agent_id_map_and_set.Map.overwrite
      parameter error
      agent_id
      (agent_string, new_site_map)
      t.string_version
  in
  let error =
    Exception.check_point
      Exception.warn
      parameter error error' __POS__ Exit
  in
  error,
  {t with bonds = new_bonds ; string_version = string_version}

let add_bond_type
    parameter error kappa_handler
    agent_id site agent_name' site' t =
  let error, (agent_type, _) =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default
      parameter error
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.Site_map_and_set.Map.empty)
      agent_id
      t.views
  in
  let error, state_id =
    Handler.id_of_binding_type
      parameter error kappa_handler
      agent_type site agent_name' site'
  in
  add_state parameter error kappa_handler
    agent_id site state_id t

let add_bond
    parameter error kappa_handler
    agent_id site agent_id' site' t =
  let bond_id = t.fresh_bond_id in
  let error_ref = error in
  let error, (agent_type, _) =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default
      parameter error
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.Site_map_and_set.Map.empty)
      agent_id
      t.views
  in
  let error, (agent_type', _) =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default
      parameter error
      (Ckappa_sig.dummy_agent_name, Ckappa_sig.Site_map_and_set.Map.empty)
      agent_id'
      t.views
  in
  let error, state_dic =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
      parameter error
      (agent_type,site)
      kappa_handler.Cckappa_sig.states_dic
  in
  let error, state_dic' =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
      parameter error
      (agent_type',site')
      kappa_handler.Cckappa_sig.states_dic
  in
  let error, state_id =
    Handler.id_of_binding_type
      parameter error kappa_handler
      agent_type site agent_type' site'
  in
  let error, state_id' =
    Handler.id_of_binding_type
      parameter error kappa_handler
      agent_type' site' agent_type site
  in
  let error, t =
    add_state
      parameter error kappa_handler agent_id site state_id t
  in
  let error, t =
    add_state
      parameter error kappa_handler agent_id' site' state_id' t
  in
  if error == error_ref
  then
    let error, t =
      add_bond_to parameter error kappa_handler agent_id site bond_id t
    in
    let error, t =
      add_bond_to parameter error kappa_handler agent_id' site' bond_id t
    in
    error,
    {t with fresh_bond_id = t.fresh_bond_id +1 }
  else
    Exception.warn
      parameter error __POS__
      ~message:"incompatible binding states"
      Exit t


let to_json parameter error kappa_handler t =
  error, `Assoc []

let print logger parameter error kappa_handler t  =
  let _bool =
    Ckappa_sig.Agent_id_map_and_set.Map.fold
      (fun _ (agent_string, site_map) bool ->
         let () =
           if bool then
             Loggers.fprintf logger " , "
         in
         let () = Loggers.fprintf logger "%s(" agent_string in
         let _bool =
           Wrapped_modules.LoggedStringMap.fold
             (fun site_string (internal,binding) bool ->
                let () =
                  if bool then Loggers.fprintf logger ","
                in
                let () = Loggers.fprintf logger "%s" site_string in
                let () =
                  match internal with
                  | None -> ()
                  | Some s -> Loggers.fprintf logger "%s" s
                in
                let () =
                  match binding with
                  | None | Some Free -> ()
                  | Some Wildcard -> Loggers.fprintf logger "?"
                  | Some Bound_to_unknown -> Loggers.fprintf logger "!_"
                  | Some (Bound_to int) -> Loggers.fprintf logger "!%i" int
                  | Some (Binding_type (ag,st)) ->
                    Loggers.fprintf logger "!%s.%s" ag st
                in
                true)
             site_map
             false
         in
         let () = Loggers.fprintf logger ")" in
         true
      )
      t.string_version
      false
  in error
end
