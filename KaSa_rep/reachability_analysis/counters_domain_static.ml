let add_relation parameters error
    ~agent_name ~source ~target array
  =
  let error, old_set =
    match
      Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.unsafe_get parameters error (agent_name,source) array
    with
    | error, None -> error, Ckappa_sig.Site_map_and_set.Set.empty
    | error, Some a -> error, a
  in
  let error, new_set =
    Ckappa_sig.Site_map_and_set.Set.add parameters error
      target old_set
  in
  Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.set parameters error (agent_name,source) new_set array

let add_dependence parameters error
    ~agent_name ~site ~counter ~packs ~backward_dependences =
  let error, packs =
    add_relation
      parameters error
      ~agent_name ~source:counter ~target:site packs
  in
  let error, backward_dependences =
    add_relation
      parameters error
      ~agent_name~source:site ~target:counter
      backward_dependences
  in
  error, (packs, backward_dependences)

let compute_packs parameters error handler compil =
  let error, packs =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.create parameters error (0,0)
  in
  let error, backward_dependences =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.create parameters error (0,0)
  in
  let error, (packs, backward_dependences) =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameters
      error
      (fun parameters error rule_id rule (packs, backward_dependences)  ->
         let rule = rule.Cckappa_sig.e_rule_c_rule in
         let actions = rule.Cckappa_sig.actions.Cckappa_sig.translate_counters in
         let error, agents_with_counters =
           List.fold_left
             (fun
               (error, map) (site_address,_)
               ->
                 let error, is_counter =
                   Handler.is_counter
                     parameters error handler site_address.Cckappa_sig.agent_type site_address.Cckappa_sig.site
                 in
                 if is_counter (* the site is a counter *)
                 then
                   let ag_id = site_address.Cckappa_sig.agent_index in
                   let error, old =
                     Ckappa_sig.Agent_id_map_and_set.Map.find_default_without_logs
                     parameters error []
                     ag_id
                     map
                   in
                   Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
                     parameters error ag_id (site_address.Cckappa_sig.site::old)
                     map

                 else
                   error, map
             )
             (error, Ckappa_sig.Agent_id_map_and_set.Map.empty)
             actions
         in
         let error, (packs, backward_dependences)  =
           Ckappa_sig.Agent_id_map_and_set.Map.fold
             (fun
               ag list_of_counters
               (error, (packs, backward_dependences))
               ->
                 match
                   Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
                     parameters error
                     ag
                     rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
                 with
                 | error, None -> error, (packs, backward_dependences)
                 | error, Some a ->
                   begin
                     match a with
                     | Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
                     | Cckappa_sig.Unknown_agent _ ->
                       error, (packs, backward_dependences)
                     | Cckappa_sig.Agent ag ->
                       let agent_name = ag.Cckappa_sig.agent_name in
                       Ckappa_sig.Site_map_and_set.Map.fold
                         (fun site _ (error, (packs, backward_dependences)) ->
                            List.fold_left
                              (fun (error, (packs, backward_dependences)) counter ->
                                 add_dependence
                                   parameters error
                                   ~agent_name ~site ~counter ~packs ~backward_dependences)
                              (error, (packs, backward_dependences))
                              list_of_counters)
                         ag.Cckappa_sig.agent_interface
                         (error, (packs, backward_dependences))
                   end
             )
             agents_with_counters
             (error, (packs, backward_dependences))
         in
         error, (packs, backward_dependences)
      ) compil.Cckappa_sig.rules (packs, backward_dependences)
  in
  error, packs
