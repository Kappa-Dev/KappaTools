module Ckappa_backend =
struct

  type agent_id = Ckappa_sig.c_agent_id
  type bond_index = int

  type binding_state =
    | Free | Wildcard | Bound_to_unknown
    | Binding_type of Ckappa_sig.agent_name * Ckappa_sig.site_name
    | Bound_to of bond_index

  let agent = "agent"
  let site = "site"
  let free = ""
  let wildcard = "?"
  let bound = "!_"
  let bond_id = "bond id"
  let bound_to = "bound to"
  let binding_type = "binding type"
  let int_of_bond_index (a:bond_index) : int = a
  let bond_index_of_int (a:int) : bond_index = a

  let binding_state_to_json =
    function
    | Free -> `Assoc [free, `Null]
    | Wildcard -> `Assoc [wildcard, `Null]
    | Bound_to_unknown -> `Assoc [bound_to, `Null]
    | (Bound_to i) ->
      `Assoc [bond_id, JsonUtil.of_int (int_of_bond_index i)]
    | (Binding_type (agent_name, site_name)) ->
      `Assoc [binding_type,
              JsonUtil.of_pair ~lab1:agent ~lab2:site
                JsonUtil.of_string
                JsonUtil.of_string
                (agent_name, site_name)]

  let binding_state_of_json =
    function
    | `Assoc [s, `Null] when s = free -> Free
    | `Assoc [s, `Null] when s = wildcard -> Wildcard
    | `Assoc [s, `Null] when s = bound_to -> Bound_to_unknown
    | `Assoc [s, j] when s = bond_id ->
      let i =
        JsonUtil.to_int ~error_msg:"wrong binding id" j
      in
      let bond_index = bond_index_of_int i in
      Bound_to bond_index
    | `Assoc [s, j] when s = binding_type ->
      let (agent_name, site_name) =
        JsonUtil.to_pair
          ~lab1:agent ~lab2:site ~error_msg:"binding type"
          (JsonUtil.to_string ~error_msg:"agent name") (JsonUtil.to_string ~error_msg:"site name")
          j
      in
      Binding_type (agent_name, site_name)
    | x -> raise
             (Yojson.Basic.Util.Type_error ("wrong binding state",x))

  type agent =
    (string *
     (string option * binding_state option)
       Wrapped_modules.LoggedStringMap.t)

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
          Ckappa_sig.Site_map_and_set.Map.t
          Ckappa_sig.Agent_id_map_and_set.Map.t;
      string_version:
        agent
          Ckappa_sig.Agent_id_map_and_set.Map.t
    }

  type string_version = agent list

  let get_string_version t = t.string_version
  let set_string_version s_v t =
    {t with string_version = s_v}

  let empty =
    {
      views =  Ckappa_sig.Agent_id_map_and_set.Map.empty ;
      fresh_agent_id = Ckappa_sig.dummy_agent_id ;
      fresh_bond_id = 1 ;
      bonds = Ckappa_sig.Agent_id_map_and_set.Map.empty ;
      string_version = Ckappa_sig.Agent_id_map_and_set.Map.empty ;
    }

  let add_agent parameter error kappa_handler agent_type t =
    let error, agent_string =
      Handler.translate_agent
        ~message:"unknown agent type" ~ml_pos:(Some __POS__) parameter error kappa_handler agent_type
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

  let has_a_binding_state parameter error kappa_handler agent_type site =
    let error,site =
      Handler.translate_site parameter error kappa_handler agent_type site
    in
    match site with
    | Ckappa_sig.Internal s ->
      let new_site = Ckappa_sig.Binding s in
      let error, dic_opt =
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get
          parameter error agent_type kappa_handler.Cckappa_sig.sites
      in
      begin
        match dic_opt with
        | None ->
          Exception.warn parameter error __POS__ Exit false
        | Some dic ->
          Ckappa_sig.Dictionary_of_sites.member
            parameter error new_site dic
      end
    | Ckappa_sig.Binding _ ->
      Exception.warn parameter error __POS__ Exit false

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
        let error, site_string =
          Handler.string_of_site_contact_map
            ~ml_pos:(Some __POS__)
            ~message:"undefined site"
            parameter error kappa_handler agent_type site
        in
        let error, _ =
          Handler.translate_state
            ~ml_pos:(Some __POS__)
            ~message:"undefined site state"
            parameter error kappa_handler
            agent_type site state_min
        in
        let error, _ =
          Handler.translate_state
            ~ml_pos:(Some __POS__)
            ~message:"undefined site state"
            parameter error kappa_handler
            agent_type site state_max
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
              let new_min = Cckappa_sig.max_state_index state_min old_min in
              let new_max = Cckappa_sig.min_state_index state_max old_max in
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
        match range_opt with
        | None -> error, t
        | Some (state_min,state_max) ->
          let error, is_binding_site =
            Handler.is_binding_site parameter error kappa_handler
              agent_type site
          in
          let error,
              (internal_state_string_opt, binding_state_opt) =
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
                    ~message:"unknown agent type"
                    ~ml_pos:(Some __POS__)
                    parameter error kappa_handler
                    agent_type'
                in
                (*let error, site_string' =
                  Handler.translate_binding_type parameter error kappa_handler
                    agent_type' site'
                in*)
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
            | Some x -> Some x
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
          let error', string_version =
            Ckappa_sig.Agent_id_map_and_set.Map.overwrite
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
    let error, (old_internal, _old_binding) =
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
        (old_internal, Some (Bound_to bond_id))
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

  let print_agent logger parameter error agent_string site_map bool =
    let () =
      if bool then
        Loggers.fprintf logger "%s"
          (Remanent_parameters.get_site_sep_comma_symbol parameter)
    in
    let () = Loggers.fprintf logger "%s%s" agent_string
        (Remanent_parameters.get_agent_open_symbol parameter)
    in
    let _ =
      Wrapped_modules.LoggedStringMap.fold
        (fun site_string (internal,binding) bool ->
           let () =
             if bool then
               Loggers.fprintf logger
                 "%s"
                 (Remanent_parameters.get_site_sep_comma_symbol parameter)
           in
           let () = Loggers.fprintf logger "%s" site_string in
           let () =
             match internal with
             | None -> ()
             | Some s ->
               Loggers.fprintf
                 logger "%s%s%s%s"
                 (Remanent_parameters.get_open_internal_state parameter)
                 (Remanent_parameters.get_internal_state_symbol parameter)
                 s
                 (Remanent_parameters.get_close_internal_state parameter)
           in
           let () =
             match binding with
             | None | Some Free ->
               Loggers.fprintf logger
                 "%s%s%s"
                 (Remanent_parameters.get_open_binding_state parameter)
                 (Remanent_parameters.get_free_symbol parameter)
                 (Remanent_parameters.get_close_binding_state parameter)
             | Some Wildcard ->
               Loggers.fprintf logger "%s%s%s"
                 (Remanent_parameters.get_open_binding_state parameter)
                 (Remanent_parameters.get_link_to_any parameter)
                 (Remanent_parameters.get_close_binding_state parameter)
             | Some Bound_to_unknown ->
               Loggers.fprintf logger "%s%s%s"
                 (Remanent_parameters.get_open_binding_state parameter)
                 (Remanent_parameters.get_link_to_some parameter)
                 (Remanent_parameters.get_close_binding_state parameter)
             | Some (Bound_to int) ->
               Loggers.fprintf logger "%s%s%i%s"
                 (Remanent_parameters.get_open_binding_state parameter)
                 (Remanent_parameters.get_bound_symbol parameter)
                 int
                 (Remanent_parameters.get_close_binding_state parameter)
             | Some (Binding_type (agent_name,site_name)) ->
               let binding_type_symbol =
                 Remanent_parameters.get_at_symbol parameter
               in
               let binding =
                 Public_data.string_of_binding_type
                   ~binding_type_symbol ~agent_name ~site_name
               in
               Loggers.fprintf logger
                 "%s%s%s%s"
                 (Remanent_parameters.get_open_binding_state parameter)
                 (Remanent_parameters.get_bound_symbol parameter)
                 binding
                 (Remanent_parameters.get_close_binding_state parameter)
           in
           true
        ) site_map false
    in
    let () =
      Loggers.fprintf logger
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameter)
    in
    error

  let print logger parameter error kappa_handler t  =
    let error,_ =
      Ckappa_sig.Agent_id_map_and_set.Map.fold
        (fun _ (agent_string, site_map) (error,bool) ->
           let error =
             print_agent logger parameter error
               agent_string site_map bool
           in
           error, true
        )
        t.string_version
        (error, false)
    in error

  (***************************************************************************)

  let print_list logger parameter error kappa_handler list =
    match list with
    | [] -> error
    | [a] -> print logger parameter error kappa_handler a
    | _::_ ->
      begin
        let () = Loggers.fprintf logger "%s "
            (Remanent_parameters.get_open_binding_state parameter)
        in
        let error,_ =
          List.fold_left
            (fun (error, bool) pattern ->
               let () =
                 if bool then
                   Loggers.fprintf logger " v "
               in
               print logger parameter error kappa_handler pattern,true)
            (error, false)
            list in
        let () = Loggers.fprintf logger " %s"
            (Remanent_parameters.get_close_binding_state parameter)
        in
        error
      end
end
