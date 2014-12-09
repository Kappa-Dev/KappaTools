 (**
  * influence_map.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: March the 10th of 2011
  * Last modification: October the 5th of 2014
  * 
  * Compute the influence relations between rules and sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Influence_map") message exn (fun () -> default) 

let local_trace = true
                                                     
let generic_add fold2_common diag parameters error handler n a b c = 
  fold2_common
     parameters 
     error 
     (fun parameters error _ a b map -> 
        Int_storage.Quick_Nearly_inf_Imperatif.fold 
           parameters 
           error 
           (fun parameters error rule a map -> 
               Int_storage.Quick_Nearly_inf_Imperatif.fold 
                    parameters 
                    error 
                    (fun parameters error rule' a' map -> 
                        let rule' = n + rule' in 
                        if not diag && rule = rule' then 
                          (error,map) 
                        else 
                        let key = rule,rule' in 
                        let error,old = Quark_type.Int2Set_and_map.find_map_option parameters error key map in                        
                        let old = 
                            match old with 
                              | None -> Quark_type.Labels.empty_couple  
                              | Some old -> old 
                        in 
                      let error,couple = Quark_type.Labels.add_couple parameters error a a' old in
                      Quark_type.Int2Set_and_map.add_map parameters error key couple map)
                  b 
                  map)
                a
                map)
        a b c 

let compute_influence_map parameters error handler quark_maps nrules = 
  let wake_up_map = Quark_type.Int2Set_and_map.empty_map in 
  let inhibition_map  = Quark_type.Int2Set_and_map.empty_map in 
  let error,wake_up_map = 
    generic_add 
      Quark_type.AgentMap.fold2_common
      true
      parameters 
      error 
      handler 
      0 
      quark_maps.Quark_type.agent_modif_plus 
      quark_maps.Quark_type.agent_test 
      wake_up_map
  in
  let error,wake_up_map = 
    generic_add 
      Quark_type.AgentMap.fold2_common
      true
      parameters 
      error 
      handler 
      nrules 
      quark_maps.Quark_type.agent_modif_plus 
      quark_maps.Quark_type.agent_var_plus
      wake_up_map
  in
   let error,inhibition_map = 
    generic_add 
      Quark_type.AgentMap.fold2_common
      true
      parameters 
      error 
      handler 
      nrules 
      quark_maps.Quark_type.agent_modif_plus 
      quark_maps.Quark_type.agent_var_minus
      inhibition_map 
  in
  let error,wake_up_map = 
    generic_add 
      Quark_type.SiteMap.fold2_common
      true
      parameters 
      error 
      handler
      0 
      quark_maps.Quark_type.site_modif_plus 
      quark_maps.Quark_type.site_test
      wake_up_map
  in
  let error,wake_up_map = 
    generic_add 
      Quark_type.SiteMap.fold2_common
      true
      parameters 
      error 
      handler
      nrules
      quark_maps.Quark_type.site_modif_plus 
      quark_maps.Quark_type.site_var_plus
      wake_up_map
  in
  let error,inhibition_map = 
    generic_add 
      Quark_type.SiteMap.fold2_common
      true
      parameters 
      error 
      handler
      nrules
      quark_maps.Quark_type.site_modif_plus 
      quark_maps.Quark_type.site_var_minus
      inhibition_map 
  in
  let error,inhibition_map = 
    generic_add 
      Quark_type.AgentMap.fold2_common
      false
      parameters 
      error 
      handler 
      0
      quark_maps.Quark_type.agent_modif_minus 
      quark_maps.Quark_type.agent_test
      inhibition_map
  in
  let error,inhibition_map = 
    generic_add 
      Quark_type.SiteMap.fold2_common
      false
      parameters 
      error 
      handler 
      0
      quark_maps.Quark_type.site_modif_minus 
      quark_maps.Quark_type.site_test
      inhibition_map
  in
  let error,inhibition_map = 
    generic_add 
      Quark_type.AgentMap.fold2_common
      false
      parameters 
      error 
      handler 
      nrules
      quark_maps.Quark_type.agent_modif_minus 
      quark_maps.Quark_type.agent_var_plus
      inhibition_map
  in
  let error,inhibition_map = 
    generic_add 
      Quark_type.SiteMap.fold2_common
      false
      parameters 
      error 
      handler 
      nrules
      quark_maps.Quark_type.site_modif_minus 
      quark_maps.Quark_type.site_var_plus
      inhibition_map
  in
   let error,wake_up_map = 
    generic_add 
      Quark_type.AgentMap.fold2_common
      false
      parameters 
      error 
      handler 
      nrules
      quark_maps.Quark_type.agent_modif_minus 
      quark_maps.Quark_type.agent_var_minus
      wake_up_map
  in
  let error,wake_up_map = 
    generic_add 
      Quark_type.SiteMap.fold2_common
      false
      parameters 
      error 
      handler 
      nrules
      quark_maps.Quark_type.site_modif_minus 
      quark_maps.Quark_type.site_var_minus
      wake_up_map
  in
    error,wake_up_map,inhibition_map  
  
