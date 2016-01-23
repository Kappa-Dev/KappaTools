 (**
  * list_tokens.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: 2011, the 17th of January
  * Last modification: 2015, the 17th of February
  * *
  * Number agents, sites, states in ckappa represenations
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "List_tokens") message exn (fun () -> default)


module Int_Set_and_Map = SetMap.Make (struct type t = int let compare = compare end)

let local_trace = false

let empty_site_list = { Ckappa_sig.used = [] ; Ckappa_sig.declared = [] ; Ckappa_sig.creation = []}

let empty_agent_specification =
  {
    Ckappa_sig.binding_sites_usage = empty_site_list;
    Ckappa_sig.marked_sites_usage = empty_site_list
        }

let empty_handler parameters error =
  let error,int_constraints =  Int_storage.Nearly_inf_Imperatif.create parameters error 0 in
  let error,sites = Int_storage.Nearly_inf_Imperatif.create parameters error 0 in
  let error,states_dic = Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.create parameters error (0,0) in
  let error,dual = Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.create parameters error (0,(0,0)) in
  error,
  {
    Cckappa_sig.nvars = 0 ;
    Cckappa_sig.nagents = 0 ;
    Cckappa_sig.nrules = 0 ;
    Cckappa_sig.agents_dic = Ckappa_sig.Dictionary_of_agents.init ();
    Cckappa_sig.interface_constraints = int_constraints;
    Cckappa_sig.sites = sites;
    Cckappa_sig.states_dic = states_dic;
    Cckappa_sig.dual = dual
  }


let create_binding_state_dictionary parameters error =
  let dic = Cckappa_sig.Dictionary_of_States.init () in
  let error,output= Cckappa_sig.Dictionary_of_States.allocate parameters error Misc_sa.compare_unit (Ckappa_sig.Binding Cckappa_sig.Free) () Misc_sa.const_unit dic in
  match output
  with
    | None -> error,dic
    | Some (_,_,_,x) -> error,x

let create_internal_state_dictionary parameters error =
  let dic = Cckappa_sig.Dictionary_of_States.init () in
  error,dic


let declare_agent parameters error handler agent_name =
  let agents_dic = handler.Cckappa_sig.agents_dic in
  let error,(bool,output) = Ckappa_sig.Dictionary_of_agents.allocate_bool parameters error Misc_sa.compare_unit agent_name () Misc_sa.const_unit agents_dic in
  match output with
    | None -> warn parameters error (Some "line 61") Exit (handler,0)
    | Some (k,_,_,dic) ->
     if bool
      then
        let error,int_constraints = Int_storage.Nearly_inf_Imperatif.set parameters error k empty_agent_specification handler.Cckappa_sig.interface_constraints in
        let error,sites = Int_storage.Nearly_inf_Imperatif.set parameters error k (Ckappa_sig.Dictionary_of_sites.init ()) handler.Cckappa_sig.sites in
        error,
          ({handler with
            Cckappa_sig.nrules = 0 ;
            Cckappa_sig.nagents = max k handler.Cckappa_sig.nagents;
            Cckappa_sig.agents_dic = dic ;
            Cckappa_sig.interface_constraints = int_constraints;
            Cckappa_sig.sites = sites;
           },k)
      else
        error,(handler,k)

let declare_site create parameters make_site make_state (error,handler) agent_id (site_name:Ckappa_sig.site_name) list =
  let site = make_site site_name in
  let states_dic = handler.Cckappa_sig.states_dic in
  let error,sites = Int_storage.Nearly_inf_Imperatif.get parameters error agent_id handler.Cckappa_sig.sites in
   match sites with
    | None  -> warn parameters error (Some "line 87") Exit (handler,[],0)
    | Some sites ->
         let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit site () Misc_sa.const_unit sites in
        begin
           match output with
           | None -> warn parameters error (Some "line 92") Exit (handler,[],0)
           | Some (k,_,_,sites) ->
               let error,(states_dic,dic_states,handler) =
               if bool
               then
                let error,dic_states = create parameters error in
                let error,states_dic =  Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.set parameters error (agent_id,k) dic_states states_dic in
                let error,new_sites = Int_storage.Nearly_inf_Imperatif.set parameters error agent_id sites handler.Cckappa_sig.sites in
                 error,(states_dic,dic_states,{handler with Cckappa_sig.sites = new_sites})
               else
                 match Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_id,k) states_dic
                 with
                   | error,None ->
                     let error,dic = create parameters error in
                      warn parameters error (Some "line 106") Exit (states_dic,dic,handler)
                   | error,Some u ->
                     let error,dic_states =
                       match Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_id,k) states_dic
                       with
                         | error,None ->
                             let error,dic_states = (create parameters error) in
                               warn parameters error (Some "line 111") Exit dic_states
                         | error,Some dic_states -> error,dic_states
                      in
                       error,(states_dic,dic_states,handler)
              in
              let error,(new_dic_states,l,bool) =
                List.fold_left
                  (fun (error,(dic_states,l,bool)) internal ->
                      let state = make_state internal in
                      let error,(bool2,output) =  Cckappa_sig.Dictionary_of_States.allocate_bool parameters error Misc_sa.compare_unit state () Misc_sa.const_unit dic_states in
                      begin
                        match output
                        with
                          | None ->
                              warn parameters error (Some "line 126") Exit (dic_states,l,bool2)
                          | Some (state_id,_,_,dic) ->
                              let l = (agent_id,k,state_id)::l in
                              if bool2
                              then
                                 error,(dic,l,bool2)
                              else
                            error,(dic_states,l,bool)
                      end)
                  (error,(dic_states,[],bool))
                  list
              in

              begin
              if bool
              then
                let error,states_dic = Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.set parameters error (agent_id,k) new_dic_states states_dic in
                let error,handler =   error,{handler with Cckappa_sig.states_dic = states_dic} in
                  error,(handler,l,k)
              else
                  error,(handler,l,k)
              end
        end

let declare_site_with_internal_states parameters =
  declare_site
    create_internal_state_dictionary
    parameters
    (fun x -> Ckappa_sig.Internal x)
    (fun x -> Ckappa_sig.Internal x)

let declare_site_with_binding_states parameters =
  declare_site
    create_binding_state_dictionary
    parameters
    (fun x -> Ckappa_sig.Binding x)
    (fun x -> Ckappa_sig.Binding x)

let declare_dual parameter error handler ag site state ag' site' state' =
  let dual = handler.Cckappa_sig.dual in
  let error,dual = Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.set parameter error (ag,(site,state)) (ag',site',state') dual in
  let error,dual = Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.set parameter error (ag',(site',state')) (ag,site,state) dual in
    error,{handler with Cckappa_sig.dual = dual}

let scan_agent parameters (error,handler) agent =
  let error,(handler,ag_id) = declare_agent parameters error handler agent.Ckappa_sig.ag_nme in
  let rec aux error interface handler =
    match interface
    with
    | Ckappa_sig.EMPTY_INTF -> error,handler
    | Ckappa_sig.PORT_SEP (port,interface) ->
      let site_name = port.Ckappa_sig.port_nme in
      let error,handler =
          match port.Ckappa_sig.port_int
          with
            | [] -> error,handler
            | list ->
               begin
                 let error,(handler,_,c) = declare_site_with_internal_states parameters (error,handler) ag_id  site_name list in
                   error,handler
               end
      in
      let error,handler =
        match port.Ckappa_sig.port_lnk
        with
          | Ckappa_sig.FREE | Ckappa_sig.LNK_ANY _  -> error,handler
                            | Ckappa_sig.LNK_VALUE (_,agent',site',_,_)
                            | Ckappa_sig.LNK_TYPE ((agent',_),(site',_)) ->
               ( let error,(handler,ag_id')   = declare_agent parameters error handler agent' in
                 let error,(handler,_,site_id) = declare_site_with_binding_states parameters (error,handler) ag_id site_name [] in
		 let error,(handler,_,site_id') = declare_site_with_binding_states parameters (error,handler) ag_id' site' [] in
                 let error,(handler,l1,site_id)  = declare_site_with_binding_states parameters (error,handler) ag_id site_name [Cckappa_sig.Lnk_type (ag_id',site_id')] in
                 let error,(handler,l2,site_id') = declare_site_with_binding_states parameters (error,handler) ag_id' site' [Cckappa_sig.Lnk_type (ag_id,site_id)] in
                 let error,handler =
                   begin
                     match l1,l2
                     with
                       | [agent_id1,site_id1,state_id1],[agent_id2,site_id2,state_id2] ->
                           declare_dual parameters error handler agent_id1 site_id1 state_id1 agent_id2 site_id2 state_id2
                       | _ -> warn parameters error (Some "line 204") Exit handler
                   end
                 in
                   error,handler)
          | Ckappa_sig.LNK_SOME _ ->
              let error,(handler,_,site_id) = declare_site_with_binding_states parameters (error,handler) ag_id site_name [] in
            error,handler
       in aux error interface handler
in aux error agent.Ckappa_sig.ag_intf handler

let rec scan_mixture parameters remanent mixture =
  match mixture with
    | Ckappa_sig.EMPTY_MIX -> remanent
    | Ckappa_sig.SKIP mixture -> scan_mixture parameters remanent mixture
    | Ckappa_sig.COMMA(agent,mixture) | Ckappa_sig.DOT(_,agent,mixture) | Ckappa_sig.PLUS(_,agent,mixture) ->
        let remanent = scan_agent parameters remanent agent in
         scan_mixture parameters remanent mixture

let scan_token parameters remanent alg = (*TO DO*)
  let error,remanent = remanent in
  let error,remanent = warn parameters error (Some "line 221, scan_token is not implemented yet") Exit remanent in
  error,remanent

let rec scan_alg parameters remanent alg = (*TO DO*)
  remanent

let scan_initial_states parameters =
  List.fold_left
    (fun remanent (_,init_t) ->
      match
	init_t
      with
      | Ast.INIT_MIX((alg,pos),(mixture,_pos')) ->
	let remanent = scan_mixture parameters remanent mixture in
          scan_alg parameters remanent alg
      | Ast.INIT_TOK ((alg,pos),tok) ->
	let remanent = scan_token parameters remanent tok in
	 scan_alg parameters remanent alg)

let scan_declarations parameters  =
  List.fold_left
    (fun remanent a -> scan_agent parameters remanent a)

let scan_observables scan_mixt parameters remanent variable = (*TODO*)
  remanent

let scan_perts scan_mixt parameters =
  List.fold_left
    (fun remanent ((_,m,_),_) ->
     List.fold_left
       (fun remanent m ->
        match m with
        | (Ast.INTRO (_,(m,_)) | Ast.DELETE(_,(m,_)) | Ast.CFLOWMIX (_,(m,_))) ->
	   scan_mixt parameters remanent m
        | Ast.UPDATE _ | Ast.STOP _ | Ast.SNAPSHOT _ | Ast.PLOTENTRY
	| Ast.UPDATE_TOK _ | Ast.PRINT _ | Ast.CFLOWLABEL _
	| Ast.FLUXOFF _ | Ast.FLUX _ -> remanent
       ) remanent m)

let scan_rules scan_mixt parameters a b =
  let _ =
    if Remanent_parameters.get_trace parameters
    then
      let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "Scan rules!" in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameters)  in
      ()
  in
    List.fold_left
      (fun remanent (_,((_,rule),_)) -> scan_mixture parameters (scan_mixt parameters remanent rule.Ckappa_sig.lhs) rule.Ckappa_sig.rhs)
      a b

let scan_compil parameters error compil =
  let parameters = Remanent_parameters.set_trace parameters (local_trace || (Remanent_parameters.get_trace parameters)) in
  let also_explore_tested_agents = Remanent_parameters.lexical_analysis_of_tested_only_patterns parameters in
  let scan_tested_mixture =
    if also_explore_tested_agents
    then scan_mixture
    else (fun parameters remanent mixture -> remanent)
  in
  let remanent = empty_handler parameters error in
  let remanent = scan_initial_states parameters remanent compil.Ast.init in
  let remanent = scan_declarations parameters remanent compil.Ast.signatures  in
  let remanent = scan_observables scan_tested_mixture parameters remanent compil.Ast.observables in
  let remanent = scan_perts scan_tested_mixture parameters remanent compil.Ast.perturbations in
  let remanent = scan_rules scan_tested_mixture parameters remanent compil.Ast.rules in
  remanent

