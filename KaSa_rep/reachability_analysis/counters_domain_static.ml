let compute_packs parameters error compil =
  let packs =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.create parameters error (0,0)
  in
  let error, packs =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameters
      error
      (fun parameters error rule_id rule pack  ->
         let rule = rule.Cckappa_sig.e_rule_c_rule in
         let actions = rule.Cckappa_sig.actions.Cckappa_sig.translate_counters in
         let error, agents_with_counters =
           List.fold_left
             (fun
               (error, map) (site_address,_)
               ->
                 let ag_id = site_address.Cckappa_sig.agent_index in
                 let error, old =
                   Ckappa_sig.Agent_id_map_and_set.Map.find_default_without_logs
                     parameters error []
                     ag_id
                     map
                 in
                 Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
                   parameters error ag_id (site_address::old)
                   map
             )
             (error, Ckappa_sig.Agent_id_map_and_set.Map.empty)
             actions
         in
         error, packs
      ) compil.Cckappa_sig.rules packs
  in
  error, packs
