(**
  * preprocess.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 12/08/2010
  * Last modification: 04/02/2015
  * * 
  * Translation from kASim ast to OpenKappa internal representations, and linkage
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All riGhoghts reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Preprocess") message exn (fun () -> default) 
  

let local_trace = true  

let empty_agent handler error = 
 let error,interface = Int_storage.Quick_Nearly_inf_Imperatif.create handler error 0 in 
   error,{
    Cckappa_sig.agent_kasim_id = 0  ; 
    Cckappa_sig.agent_name = 0  ;
    Cckappa_sig.agent_interface =  interface ;
    Cckappa_sig.agent_position = Location.dummy
         }
  
let empty_mixture handler error = 
  let error,views = Int_storage.Quick_Nearly_inf_Imperatif.create handler error 0 in 
  let error,bonds = Int_storage.Quick_Nearly_inf_Imperatif.create handler error 0 in 
  error, 
    { 
    Cckappa_sig.views=  views ;
    Cckappa_sig.bonds= bonds;        
    Cckappa_sig.plus=[];
    Cckappa_sig.dot=[];
    Cckappa_sig.c_mixture = Ckappa_sig.EMPTY_MIX}

let empty_pos = ("",0,0)

let empty_rule handler error  = 
  let error,empty_lhs = empty_mixture handler error in 
  let error,empty_rhs = empty_mixture handler error in
  let error,empty_direct = Int_storage.Quick_Nearly_inf_Imperatif.create handler error 0 in 
  let error,empty_reverse = Int_storage.Quick_Nearly_inf_Imperatif.create handler error 0 in 
  error,{
      Cckappa_sig.prefix   = 0 ;
      Cckappa_sig.delta    = 0 ; 
    Cckappa_sig.rule_lhs = empty_lhs ; 
    Cckappa_sig.rule_arrow = Ast.RAR  ;
    Cckappa_sig.rule_rhs = empty_rhs ; 
    diff_direct = empty_direct ; 
    diff_reverse = empty_reverse ; 
    actions = Cckappa_sig.empty_actions
        }

let empty_e_rule handler error = 
    let error,rule = empty_rule handler error in 
    {
      Cckappa_sig.e_rule_label= None ;
      Cckappa_sig.e_rule_label_dot = None ;
      Cckappa_sig.e_rule_initial_direction = Ckappa_sig.Direct ;
      Cckappa_sig.e_rule_rule = 
	{
	  Ckappa_sig.prefix = 0;
	  Ckappa_sig.delta  = 0;
	  Ckappa_sig.lhs = Ckappa_sig.EMPTY_MIX ; 
	  Ckappa_sig.arrow = Ast.RAR (*empty_pos*);
	  Ckappa_sig.rhs = Ckappa_sig.EMPTY_MIX; 
	  Ckappa_sig.k_def = Location.dummy_annot (Ast.CONST (Nbr.F 0.));
       (*       Ckappa_sig.k_un_radius = None ; *)
	  Ckappa_sig.k_un = None};
      Cckappa_sig.e_rule_c_rule = rule }

let rename_rule_rlhs handler error id_agent tab =
  let error,agent = 
    Misc_sa.unsome 
      (Int_storage.Quick_Nearly_inf_Imperatif.get handler error id_agent tab)
      (fun error -> warn handler error (Some "line 51") Exit Cckappa_sig.Ghost) 
  in 
    match agent with 
    | Cckappa_sig.Unknown_agent _ | Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _ -> warn handler error (Some "line 51") Exit 0
    | Cckappa_sig.Agent ag -> error,ag.Cckappa_sig.agent_kasim_id 
  
let rename_rule_rhs handler error id_agent rule = rename_rule_rlhs handler error id_agent rule.Cckappa_sig.rule_rhs.Cckappa_sig.views 
let rename_rule_lhs handler error id_agent rule = rename_rule_rlhs handler error id_agent rule.Cckappa_sig.rule_lhs.Cckappa_sig.views 
    
  
let length_mixture mixture = 
  let rec aux mixture size = 
    match mixture with 
      | Ckappa_sig.EMPTY_MIX -> size
      | Ckappa_sig.COMMA(_,mixture) | Ckappa_sig.DOT(_,_,mixture) | Ckappa_sig.PLUS(_,_,mixture) | Ckappa_sig.SKIP(mixture)-> aux mixture (size+1)
  in aux mixture 0 
    
let add_bond parameters error i id_agent agent site id_agent' agent' site' bond_list = 
  let error,old = 
    match Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get parameters error id_agent bond_list 
    with 
    | _,None -> error,Cckappa_sig.Site_map_and_set.Map.empty
    | _,Some i -> error,i 
  in 
  let error',updated = Cckappa_sig.Site_map_and_set.Map.add parameters error site {Cckappa_sig.agent_index = id_agent' ; Cckappa_sig.agent_type = agent' ; Cckappa_sig.site = site'} old in
  let error = Exception.check warn parameters error error' (Some "line 106, add_bond") Exit  in 
  Int_storage.Quick_Nearly_inf_Imperatif.set parameters error id_agent updated bond_list
  
let translate_agent_sig parameters error handler agent kasim_id = 
  let error,(bool,output) = Ckappa_sig.Dictionary_of_agents.allocate_bool parameters error Misc_sa.compare_unit agent.Ckappa_sig.ag_nme () Misc_sa.const_unit handler.Cckappa_sig.agents_dic in  
  let error,agent_name = 
    match bool,output with
      | _ , None  | true, _  -> warn parameters error (Some "line 45") Exit 0
      | _ , Some (i,_,_,_) ->  
          error,i
  in 
  let error,site_dic = 
    match Int_storage.Nearly_inf_Imperatif.get parameters error agent_name handler.Cckappa_sig.sites with 
      | error,None -> 
          warn parameters error (Some "line 52") Exit (Ckappa_sig.Dictionary_of_sites.init ()) 
      | error,Some i -> error,i 
  in 
  let error,c_interface = error, Cckappa_sig.Site_map_and_set.Map.empty in
  let rec aux interface error c_interface =
      match interface with 
      | Ckappa_sig.EMPTY_INTF -> error,c_interface 
      | Ckappa_sig.PORT_SEP(port,interface) ->
        let error,c_interface =
          match port.Ckappa_sig.port_int with 
            | []
               -> error,c_interface
            | list -> 
              let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Internal port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
              let error,site_name = 
                match bool,output with
                  | _ , None  | true, _  -> warn parameters error (Some ("line 123"^agent.Ckappa_sig.ag_nme^" "^port.Ckappa_sig.port_nme)) Exit 0
                  | _ , Some (i,_,_,_) ->  
                error,i
              in 
              let error,state_dic = 
                Misc_sa.unsome (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_name,site_name) handler.Cckappa_sig.states_dic)
                               (fun error ->  warn parameters error (Some "line 129") Exit (Cckappa_sig.Dictionary_of_States.init ()))
              in               
              let error,internal_list = 
                  List.fold_left 
                    (fun (error,internal_list) state -> 
                      let error,(bool,output) = Cckappa_sig.Dictionary_of_States.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Internal state) () Misc_sa.const_unit state_dic in
                      let error,internal  =
                        match bool,output with
                          | _ , None  | true, _  -> warn parameters error (Some "line 137") Exit 0
                          | _ , Some (i,_,_,_) ->  
                              error,i
                      in
                      error,internal::internal_list)
                    (error,[]) list
              in
	      let error',c_interface = 
                Cckappa_sig.Site_map_and_set.Map.add
		  parameters
		  error 
		  site_name 
                    { 
                      Cckappa_sig.site_name = site_name ;
                      Cckappa_sig.site_position = Location.dummy ; (*port.Ckappa_sig.port_pos ;*)
                      Cckappa_sig.site_state = internal_list ;
                      Cckappa_sig.site_free = port.Ckappa_sig.port_free 
                    } c_interface
	      in
	      let error = Exception.check warn parameters error error' (Some "line 170, translate_agent_sig") Exit  in 
	      error,c_interface 
        in 
        let error,c_interface =
          match port.Ckappa_sig.port_lnk with 
            | Ckappa_sig.LNK_ANY _ -> warn parameters error (Some "line 107") Exit c_interface 
            | Ckappa_sig.FREE ->     
              begin
              let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
                match bool,output with
                  | _ , None  | true, _  -> error,c_interface 
                  | _ , Some (site_name,_,_,_) ->
		     let error',c_interface =
                       Cckappa_sig.Site_map_and_set.Map.add
                            parameters error site_name 
                            { 
                               Cckappa_sig.site_name = site_name ;
                               Cckappa_sig.site_position = Location.dummy ;
                               Cckappa_sig.site_state = [0] ;
                               Cckappa_sig.site_free = port.Ckappa_sig.port_free
                            }
                            c_interface
		     in
		     let error = Exception.check warn parameters error error' (Some "line 192") Exit  in
		     error,c_interface 
	   
              end
          | Ckappa_sig.LNK_SOME pos -> warn parameters error (Some "line 124") Exit c_interface 
          | Ckappa_sig.LNK_VALUE (i,agent',site',id_agent',pos) ->
              warn parameters error (Some "line 126") Exit c_interface 
          | Ckappa_sig.LNK_TYPE (agent',site')  ->
              warn parameters error (Some "line 128") Exit c_interface 
        in aux interface error c_interface
 in 
 let error,c_interface = aux agent.Ckappa_sig.ag_intf error c_interface in  
    error,
    ({
      Cckappa_sig.agent_kasim_id = kasim_id ;
      Cckappa_sig.agent_name = agent_name ; 
      Cckappa_sig.agent_interface = c_interface ; 
      Cckappa_sig.agent_position = Location.dummy ;
     }:Cckappa_sig.agent_sig)

let translate_view parameters error handler k kasim_id agent bond_list question_marks = 
  let error,(bool,output) = Ckappa_sig.Dictionary_of_agents.allocate_bool parameters error Misc_sa.compare_unit agent.Ckappa_sig.ag_nme () Misc_sa.const_unit handler.Cckappa_sig.agents_dic in  
  match bool,output with
  | _ , None ->
     let error,ag = warn parameters error (Some "line 143") Exit (Cckappa_sig.Unknown_agent (agent.Ckappa_sig.ag_nme,kasim_id)) in
     error,bond_list,question_marks,ag
  | true,_ -> error, bond_list,question_marks,Cckappa_sig.Unknown_agent (agent.Ckappa_sig.ag_nme,kasim_id)
  | _ , Some (agent_name,_,_,_) ->  
     begin   
       let error,site_dic = 
	 match Int_storage.Nearly_inf_Imperatif.get parameters error agent_name handler.Cckappa_sig.sites with 
	 | error,None -> 
            warn parameters error (Some "line 150") Exit (Ckappa_sig.Dictionary_of_sites.init ()) 
	 | error,Some i -> error,i 
       in 
       let error,c_interface = error, Cckappa_sig.Site_map_and_set.Map.empty in
       let rec aux interface error bond_list c_interface question_marks dead_sites dead_state_sites dead_link_sites =
	 match interface with 
	 | Ckappa_sig.EMPTY_INTF -> error,bond_list,c_interface,question_marks,dead_sites,dead_state_sites,dead_link_sites 
	 | Ckappa_sig.PORT_SEP(port,interface) ->
            let error,(c_interface,dead_sites,dead_states_sites)  =
              match port.Ckappa_sig.port_int with 
              | [] -> error,(c_interface,dead_sites,dead_state_sites)
              | [state] -> 
		 begin 
		   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Internal port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
		   match bool,output with
		   | _, None -> warn parameters error (Some ("Preprocess line 227"^agent.Ckappa_sig.ag_nme^" "^port.Ckappa_sig.port_nme)) Exit (c_interface,dead_sites,dead_state_sites) 
		   | true, _ ->
		      let error,dead_sites = Cckappa_sig.KaSim_Site_map_and_set.Set.add parameters error (Ckappa_sig.Internal port.Ckappa_sig.port_nme) dead_sites in 
		      error,(c_interface,dead_sites,dead_state_sites)
		   | _,Some (site_name,_,_,_) ->
		      begin 
			let error,state_dic = 
			  Misc_sa.unsome 
			    (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_name,site_name) handler.Cckappa_sig.states_dic)
			    (fun error -> warn parameters error (Some ("line 224"^agent.Ckappa_sig.ag_nme^" "^port.Ckappa_sig.port_nme)) Exit (Cckappa_sig.Dictionary_of_States.init ()))
			in 
			let error,(bool,output) = Cckappa_sig.Dictionary_of_States.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Internal state) () Misc_sa.const_unit state_dic in
			match bool,output with
			| _ , None  | true, _  ->
				       let error',dead_state_list =
				       Cckappa_sig.Site_map_and_set.Map.add parameters error site_name (Ckappa_sig.Internal state) dead_state_sites
				       in 
				       Exception.check warn parameters error error' (Some "line 254, a site even dead should occur only once in an interface") Exit,
				       (c_interface,dead_sites,dead_state_sites)
			| _ , Some (internal,_,_,_) ->  
			   let error',c_interface =
			     Cckappa_sig.Site_map_and_set.Map.add
			       parameters
			       error
			       site_name 
			       {
				 Cckappa_sig.site_name = site_name ;
				 Cckappa_sig.site_position = Location.dummy ;
				 Cckappa_sig.site_free = None ; 
				 Cckappa_sig.site_state = 
				   {
				     Cckappa_sig.min = (internal:int) ; 
				     Cckappa_sig.max = internal
				   };
			       } c_interface in
			   let error = Exception.check warn parameters error error' (Some "line 272, a site should occur only once in an interface") Exit  in
			   error,(c_interface,dead_sites,dead_state_sites)
		      end 
		 end 
	      | _ -> warn parameters error (Some "line 199") Exit (c_interface,dead_sites,dead_state_sites)
	    in 
            let error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites) =
              match port.Ckappa_sig.port_lnk with 
              | Ckappa_sig.LNK_ANY pos -> 
		 begin
                   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
		   begin
                     match bool,output with
                     | true,_  -> error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites) (* OK if question marks in a site that is never bound *)
		     | _,None  -> warn parameters error (Some "line 266") Exit (c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		     | _ , Some (site_name,_,_,_) ->
			
			let error,state_dic = 
			  Misc_sa.unsome 
			    (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_name,site_name) handler.Cckappa_sig.states_dic)
			    (fun error -> warn parameters error (Some "Preprocess line 272") Exit (Cckappa_sig.Dictionary_of_States.init ()))
			in                    
			let error,max = Cckappa_sig.Dictionary_of_States.last_entry parameters error state_dic in 
			let error',c_interface =
			  Cckappa_sig.Site_map_and_set.Map.add
			    parameters
			    error
			    site_name 
			    { 
                              Cckappa_sig.site_name = site_name ;
                              Cckappa_sig.site_free = port.Ckappa_sig.port_free; 
                              Cckappa_sig.site_position = Location.dummy ;
                              Cckappa_sig.site_state = {Cckappa_sig.min = min 0 max; Cckappa_sig.max = max}
			    } 
			    c_interface 
			in
			let error = Exception.check warn parameters error error' (Some "line 192") Exit  in
			error,(c_interface,bond_list,(k,site_name)::question_marks,dead_sites,dead_link_sites)
		   end 
		 end
	      | Ckappa_sig.FREE ->     
		 begin
		   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
                   match bool,output with
                   | _ , None  | true, _  -> error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites) 
                   | _ , Some (site_name,_,_,_) ->  
                      let error',c_interface =
			Cckappa_sig.Site_map_and_set.Map.add
                          parameters
			  error
			  site_name 
                          {			       
                            Cckappa_sig.site_name = site_name ;
                            Cckappa_sig.site_position = Location.dummy ;
                            Cckappa_sig.site_free = port.Ckappa_sig.port_free ; 
                            Cckappa_sig.site_state = {Cckappa_sig.min = 0 ; Cckappa_sig.max = 0 } 
                          }
                          c_interface
                      in
		      let error = Exception.check warn parameters error error' (Some "line 192") Exit  in
		      error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		 end
              | Ckappa_sig.LNK_SOME pos -> 
		 begin
                   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
                   match bool,output with
                   | _ , None ->  
		      warn parameters error (Some ("Preprocess line 333"^","^(Location.to_string pos)^", this site cannot be bound, "^agent.Ckappa_sig.ag_nme^" "^port.Ckappa_sig.port_nme)) Exit (c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		   | true, _  ->
		      let error,dead_sites = Cckappa_sig.KaSim_Site_map_and_set.Set.add parameters error (Ckappa_sig.Binding port.Ckappa_sig.port_nme) dead_sites in 
		      error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		   | _ , Some (site_name,_,_,_) ->  
                      begin 
			match 
			  (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_name,site_name) handler.Cckappa_sig.states_dic)
			with 
			| error,None ->
			   let error',dead_link_sites =
			     Cckappa_sig.Site_map_and_set.Map.add parameters error site_name port.Ckappa_sig.port_lnk dead_link_sites
			   in 
			   Exception.check warn parameters error error' (Some "line 350, a site even dead should occur only once in an interface") Exit,
			   (c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
			| error,Some state_dic -> 
			   let error,max = Cckappa_sig.Dictionary_of_States.last_entry parameters error state_dic in 
			   if max = 0
			   then
			     let error',dead_link_sites =
			       Cckappa_sig.Site_map_and_set.Map.add parameters error site_name port.Ckappa_sig.port_lnk dead_link_sites
			     in 
			     Exception.check warn parameters error error' (Some "line 357, a site even dead should occur only once in an interface") Exit,
			     (c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
			   else 
			     let error',c_interface =
			       Cckappa_sig.Site_map_and_set.Map.add
				 parameters
				 error
				 site_name 
				 { 
				   Cckappa_sig.site_name = site_name ;
				   Cckappa_sig.site_free = port.Ckappa_sig.port_free; 
				   Cckappa_sig.site_position = Location.dummy ;
				   Cckappa_sig.site_state = {Cckappa_sig.min = min 1 max; Cckappa_sig.max = max}
				 } 
				 c_interface 
			     in
			     let error = Exception.check warn parameters error error' (Some "line 192") Exit  in
			     error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		      end
		 end 
	      | Ckappa_sig.LNK_VALUE (id_agent',agent',site',i,pos) ->
		 begin
		   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
                   match bool,output with
                   | _ , None  -> warn parameters error (Some ("Preprocess line 341"^","^(Location.to_string pos)^", this site cannot be bound, "^agent.Ckappa_sig.ag_nme^" "^port.Ckappa_sig.port_nme)) Exit (c_interface,bond_list,question_marks,dead_sites,dead_link_sites)						
		   | true, _ ->
		      let error,dead_sites = Cckappa_sig.KaSim_Site_map_and_set.Set.add parameters error (Ckappa_sig.Binding port.Ckappa_sig.port_nme) dead_sites in 
		      error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		   | _ , Some (site_name,_,_,_) ->  
                      begin 
			match
			  (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_name,site_name) handler.Cckappa_sig.states_dic)
			with
			| error,None ->
			   let error',dead_link_sites =
			     Cckappa_sig.Site_map_and_set.Map.add parameters error site_name port.Ckappa_sig.port_lnk dead_link_sites
			   in 
			   Exception.check warn parameters error error' (Some "line 395, a site even dead should occur only once in an interface") Exit,
			   (c_interface,bond_list,question_marks,dead_sites,dead_link_sites)		 
			| error,Some state_dic -> 
			   begin
			     let error,(bool,output) = Ckappa_sig.Dictionary_of_agents.allocate_bool parameters error Misc_sa.compare_unit agent' () Misc_sa.const_unit handler.Cckappa_sig.agents_dic in  
			     let error,agent_name' = 
			       match bool,output with
			       | _ , None  | true, _  -> warn parameters error (Some "line 285") Exit 0
			       | _ , Some (i,_,_,_) -> error,i
			     in
			     let error,site_dic' = 
			       match Int_storage.Nearly_inf_Imperatif.get parameters error agent_name' handler.Cckappa_sig.sites with 
			       | error,None -> 
				  warn parameters error (Some "line 291") Exit (Ckappa_sig.Dictionary_of_sites.init ()) 
			       | error,Some i -> 
				  error,i 
			     in 
			     let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding site') () Misc_sa.const_unit site_dic' in
			     let error,site_name' = 
			       match bool,output with
			       | _ , None  | true, _  -> warn parameters error (Some "line 298") Exit 0
			       | _ , Some (i,_,_,_) ->  
				  error,i
			     in 
			     let error,bond_list = add_bond parameters error i k agent_name site_name id_agent' agent_name' site_name' bond_list in
			     let state = Cckappa_sig.Lnk_type (agent_name',site_name') in  
			     let error,(bool,output) = Cckappa_sig.Dictionary_of_States.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding state) () Misc_sa.const_unit state_dic in
			     let error,c_interface = 
			       match bool,output with
			       | _ , None | true, _ -> 
					     warn parameters error (Some "line 308") Exit c_interface  
			       | _ ,Some (i,_,_,_) -> 
				  let error',c_interface =
				    Cckappa_sig.Site_map_and_set.Map.add
				      parameters
				      error 
				      site_name 
				      { 
					Cckappa_sig.site_free = port.Ckappa_sig.port_free; 
					Cckappa_sig.site_name = site_name ;
					Cckappa_sig.site_position = Location.dummy ;
					Cckappa_sig.site_state = {Cckappa_sig.min = i; Cckappa_sig.max = i}
				      } 
				      c_interface 
				  in
				  let error = Exception.check warn parameters error error' (Some "line 416") Exit in
				  error,c_interface 
			     in
			     error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites)			      
			   end
		      end
		 end   
	      | Ckappa_sig.LNK_TYPE (agent',site')  ->                          
		 begin
		   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding port.Ckappa_sig.port_nme) () Misc_sa.const_unit site_dic in
		   let error,site_name = 
                     match bool,output with
                     | _ , None  | true, _  -> warn parameters error (Some "line 264") Exit 0
                     | _ , Some (i,_,_,_) ->  
			error,i
		   in  
		   let error,(bool,output) = Ckappa_sig.Dictionary_of_agents.allocate_bool parameters error Misc_sa.compare_unit  (fst agent')  () Misc_sa.const_unit handler.Cckappa_sig.agents_dic in  
		   let error,agent_name' = 
                     match bool,output with
                     | _ , None  | true, _  -> 
				    warn parameters error (Some "line 349") Exit 0
                     | _ , Some (i,_,_,_) -> error,i
		   in
		   let error,site_dic' = 
                     match Int_storage.Nearly_inf_Imperatif.get parameters error agent_name' handler.Cckappa_sig.sites with 
                     | error,None -> 
                        warn parameters error (Some "line 355") Exit (Ckappa_sig.Dictionary_of_sites.init ()) 
                     | error,Some i -> error,i 
		   in 
                   let error,(bool,output) = Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding (fst site')) () Misc_sa.const_unit site_dic' in
		   let error,site_name' = 
                     match bool,output with
                     | _ , None  | true, _  -> 
				    warn parameters error (Some "line 298") Exit 0
                     | _ , Some (i,_,_,_) ->  
			error,i
		   in 
		   let state = Cckappa_sig.Lnk_type (agent_name',site_name') in  
		   let error,state_dic = 
                     Misc_sa.unsome 
                       (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameters error (agent_name,site_name) handler.Cckappa_sig.states_dic)
                       (fun error -> warn parameters error (Some ("line 394"^(string_of_int agent_name')^(string_of_int site_name'))) Exit (Cckappa_sig.Dictionary_of_States.init ()))
		   in 
		   
		   let error,(bool,output) = Cckappa_sig.Dictionary_of_States.allocate_bool parameters error Misc_sa.compare_unit (Ckappa_sig.Binding state) () Misc_sa.const_unit state_dic in
		   let error,c_interface = 
                     match bool,output with
                     | _ , None | true, _ -> warn parameters error (Some "line 369") Exit c_interface  
                     | _ ,Some (i,_,_,_) -> 
			let error',c_interface = 
			  Cckappa_sig.Site_map_and_set.Map.add
			    parameters
			    error
			    site_name 
			    { 
                              Cckappa_sig.site_free = port.Ckappa_sig.port_free; 
                              Cckappa_sig.site_name = site_name ;
                              Cckappa_sig.site_position = Location.dummy ;
                              Cckappa_sig.site_state = {Cckappa_sig.min = i; Cckappa_sig.max = i}
			    } 
			    c_interface
			in
			let error = Exception.check warn parameters error error' (Some "line 192") Exit  in
			error,c_interface
		   in 
		   error,(c_interface,bond_list,question_marks,dead_sites,dead_link_sites)
		 end
		   
            in aux interface error bond_list c_interface question_marks dead_sites dead_state_sites dead_link_sites 
       in
       let deadsites =  Cckappa_sig.KaSim_Site_map_and_set.Set.empty in
       let deadstate = Cckappa_sig.Site_map_and_set.Map.empty in
       let deadlink = Cckappa_sig.Site_map_and_set.Map.empty in
       let error,bond_list,c_interface,question_marks,dead_sites,dead_state_sites,dead_link_sites = aux agent.Ckappa_sig.ag_intf error bond_list c_interface question_marks deadsites deadstate deadlink in  
       error,bond_list,question_marks, 
       if deadlink == dead_link_sites && deadstate==dead_state_sites && deadsites == 	dead_sites				
       then Cckappa_sig.Agent 
              {
		Cckappa_sig.agent_kasim_id = kasim_id ;
		Cckappa_sig.agent_name = agent_name ; 
		Cckappa_sig.agent_interface = c_interface ; 
		Cckappa_sig.agent_position = Location.dummy ;
              }
       else
	 Cckappa_sig.Dead_agent
	   ( {
               Cckappa_sig.agent_kasim_id = kasim_id ;
               Cckappa_sig.agent_name = agent_name ; 
               Cckappa_sig.agent_interface = c_interface ; 
               Cckappa_sig.agent_position = Location.dummy ;
	     },
	     dead_sites,
	     dead_state_sites,
	     dead_link_sites)
     end 

let translate_mixture parameters error handler mixture = 
	 let size = length_mixture mixture in 
	 let rec aux mixture error k kasim_id bond_list questionmarks dot_list plus_list array = 
    match mixture with 
      | Ckappa_sig.EMPTY_MIX -> error,bond_list,questionmarks,dot_list,plus_list,array
      | Ckappa_sig.COMMA(agent,mixture) -> 
         let error,bond_list,questionmarks,view = translate_view parameters error handler k kasim_id agent bond_list questionmarks in 
         let error,array = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k view array in 
          aux mixture error (k+1) (kasim_id+1) bond_list questionmarks dot_list plus_list array 
      | Ckappa_sig.DOT(id,agent,mixture) -> 
         let dot_list = (k,id)::dot_list in 
         let error,bond_list,questionmarks,view = translate_view parameters error handler k kasim_id agent bond_list questionmarks in 
         let error,array = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k view array in 
          aux mixture error (k+1) (kasim_id+1) bond_list questionmarks dot_list plus_list array 
      | Ckappa_sig.PLUS(id,agent,mixture) -> 
         let plus_list = (k,id)::plus_list in 
         let error,bond_list,questionmarks,view = translate_view parameters error handler k kasim_id agent bond_list questionmarks in 
         let error,array = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k view array in 
          aux mixture error (k+1) (kasim_id+1) bond_list questionmarks dot_list plus_list array 
      | Ckappa_sig.SKIP(mixture) -> 
          let error,array = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k Cckappa_sig.Ghost array in 
            aux mixture error (k+1) kasim_id bond_list questionmarks dot_list plus_list array 
  in 
  let error,array = Int_storage.Quick_Nearly_inf_Imperatif.create parameters error size  in
  let error,bonds = Int_storage.Quick_Nearly_inf_Imperatif.create parameters error size in 
  let error,bond_list,questionmarks,dot_list,plus_list,array = aux mixture error 0 0 bonds [] [] [] array in 
  error,
    {
      Cckappa_sig.views = array ; 
      Cckappa_sig.dot = dot_list ;
      Cckappa_sig.plus = plus_list ; 
      Cckappa_sig.bonds = bond_list ;
      Cckappa_sig.c_mixture = mixture },questionmarks
  
 let clean_agent = Cckappa_sig.map_agent (fun _ -> ())
   
 let clean_agent2 map = 
   let l = 
     Cckappa_sig.Site_map_and_set.Map.fold
       (fun i _ l  -> i::l)
       map.Cckappa_sig.agent_interface 
       []
   in 
   l 

 let set_bound_sites parameters error k ag set = 
   Cckappa_sig.Site_map_and_set.Map.fold
     (fun site state (error,set) -> 
      if state.Cckappa_sig.site_free = Some true 
            then error,set 
            else
	      let error',set = 
		Cckappa_sig.Address_map_and_set.Set.add
		  parameters
		  error 
		  {
		    Cckappa_sig.agent_index = k;
		    Cckappa_sig.agent_type = ag.Cckappa_sig.agent_name;
		    Cckappa_sig.site = site}
		  set
	      in
     	      let error = Exception.check warn parameters error error' (Some "line 192") Exit  in
	      error,set)
     ag.Cckappa_sig.agent_interface
     (error,set)

 let set_released_sites parameters error k ag ag' set = 
   Cckappa_sig.Site_map_and_set.Map.fold2 parameters error
     (fun parameters error site state _ ->
      warn parameters error (Some "line 514") Exit set)
     (fun parameters error site state set ->
      if state.Cckappa_sig.site_free = Some true
       then 
	 let error',set =
	   Cckappa_sig.Address_map_and_set.Set.add
	     parameters error 
	     {
               Cckappa_sig.agent_index = k;
               Cckappa_sig.agent_type = ag.Cckappa_sig.agent_name;
               Cckappa_sig.site = site} set
	 in
	 let error = Exception.check warn parameters error error' (Some "line 576") Exit  in
	 error,set
       else error,set)
     (fun parameters error site state state' set ->
       if state.Cckappa_sig.site_free  = state'.Cckappa_sig.site_free
       || state.Cckappa_sig.site_free = Some true
       then error,set
       else
	 let error',set =
	   Cckappa_sig.Address_map_and_set.Set.add
	      parameters
	      error 
              {
		Cckappa_sig.agent_index = k;
		Cckappa_sig.agent_type = ag.Cckappa_sig.agent_name;
		Cckappa_sig.site = site} set
	 in
	 let error = Exception.check warn parameters error error' (Some "line 593") Exit in
	 error,set)    
     ag.Cckappa_sig.agent_interface ag'.Cckappa_sig.agent_interface set

 let equ_port s1 s2 = 
   s1.Cckappa_sig.site_name = s2.Cckappa_sig.site_name 
   && s1.Cckappa_sig.site_free = s2.Cckappa_sig.site_free 
     && s1.Cckappa_sig.site_state = s2.Cckappa_sig.site_state 

 let clean_question_marks parameters error l mixture =
   let rec aux error l views = 
     match
       l  
     with
     | [] -> error,views
     | (k,s)::t ->
	let error,mixture =
	  let error,agent = Int_storage.Quick_Nearly_inf_Imperatif.get parameters error k views in
	  let error,agent =
	    match agent
	    with
	    | Some Cckappa_sig.Unknown_agent _ | None | Some Cckappa_sig.Ghost -> warn parameters error (Some ("Preprocess line 557"^", question marks should not appear on the rhs or in introduction")) Exit (Cckappa_sig.Ghost)  
	    | Some Cckappa_sig.Dead_agent (ag,set,l,l') ->
	       let error',interface = Cckappa_sig.Site_map_and_set.Map.remove parameters error s ag.Cckappa_sig.agent_interface in
	       let error = Exception.check warn parameters error error' (Some "line 617") Exit in 
	       error,Cckappa_sig.Dead_agent ({ag with Cckappa_sig.agent_interface = interface},set,l,l')
	    | Some Cckappa_sig.Agent ag ->
	       let error',interface = Cckappa_sig.Site_map_and_set.Map.remove parameters error s ag.Cckappa_sig.agent_interface in
	       let error = Exception.check warn parameters error error' (Some "line 617") Exit in 
	       error,Cckappa_sig.Agent {ag with Cckappa_sig.agent_interface = interface}
	  in 
          let error,views = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k agent views in 
	  error,views
	in
	aux error t views
   in
   let error,views = aux error l mixture.Cckappa_sig.views in 
   error,{mixture with Cckappa_sig.views = views}

 let filter parameters error l mixture =
   let views = mixture.Cckappa_sig.views in 
   let rec aux error l output = 
     match
       l
     with
     | [] -> error,output
     | (k,s)::t ->
	let error,agent = Int_storage.Quick_Nearly_inf_Imperatif.get parameters error k views in
	let error,keep =
	  match agent
	  with
	  | None -> warn parameters error (Some "line 581, missing agent") Exit false
	  | Some Cckappa_sig.Ghost -> error,true
	  | Some Cckappa_sig.Unknown_agent _
	  | Some Cckappa_sig.Dead_agent _ -> warn parameters error (Some "line 642, there should be no dead agent in rhs") Exit false 
	  | Some Cckappa_sig.Agent _ -> error,false
	in
	aux error t (if keep then ((k,s)::output) else output)
   in aux error l []
	  
  let translate_rule parameters error handler rule = 
   let label,((direction,rule),position) = rule in 
   let error,c_rule_lhs,question_marks_l = translate_mixture parameters error handler rule.Ckappa_sig.lhs in 
   let error,c_rule_rhs,question_marks_r = translate_mixture parameters error handler rule.Ckappa_sig.rhs in
   let error,c_rule_lhs = clean_question_marks parameters error question_marks_r c_rule_lhs in (* remove ? in the lhs when they occur in the rhs (according to the BNF, they have to occur in the lhs as well *)
   let error,filtered_question_marks_l = filter parameters error question_marks_l c_rule_rhs in 
   let error,c_rule_lhs = clean_question_marks parameters error filtered_question_marks_l c_rule_lhs in (* remove ? that occur in the lhs in degraded agent *)
   let error,c_rule_rhs = clean_question_marks parameters error question_marks_r c_rule_rhs in (* remove ? that occurs in the rhs *)
   let error,size = Int_storage.Quick_Nearly_inf_Imperatif.dimension error c_rule_lhs.Cckappa_sig.views in 
   let error,direct = Int_storage.Quick_Nearly_inf_Imperatif.create parameters error size in 
   let error,reverse = Int_storage.Quick_Nearly_inf_Imperatif.create parameters error size in 
   let actions = Cckappa_sig.empty_actions in 
   let half_release_set = Cckappa_sig.Address_map_and_set.Set.empty in
   let full_release_set = Cckappa_sig.Address_map_and_set.Set.empty in
   let rec aux_agent k (error,(direct,reverse,actions,half_release_set,full_release_set,dead)) = 
     if k>=size then (error,(direct,reverse,actions,half_release_set,full_release_set,dead)) 
     else 
       begin 
         let error,lhsk = Int_storage.Quick_Nearly_inf_Imperatif.get parameters error k c_rule_lhs.Cckappa_sig.views in  
         let error,rhsk = Int_storage.Quick_Nearly_inf_Imperatif.get parameters error k c_rule_rhs.Cckappa_sig.views in 
	 let error,(direct,reverse,actions,half_release_set,agent_type,lbondk,rbondk,dead) = 
	   match lhsk,rhsk with 
	   | Some Cckappa_sig.Unknown_agent _, Some Cckappa_sig.Ghost -> (* suppression of a dead agent *)
	      error, (
                       direct,
                       reverse,
                       actions,
                       half_release_set,
                       None,
                       Cckappa_sig.Site_map_and_set.Map.empty,
		       Cckappa_sig.Site_map_and_set.Map.empty,
		       true
	     )		       
	   | Some Cckappa_sig.Agent lagk, Some Cckappa_sig.Ghost
	   | Some Cckappa_sig.Dead_agent (lagk,_,_,_), Some Cckappa_sig.Ghost -> (*suppression*)
	      begin
                let agent_type = lagk.Cckappa_sig.agent_name in 
                let error,reverse = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k (Cckappa_sig.upgrade_some_interface lagk) reverse in
                let error,lbondk = Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get parameters error k c_rule_lhs.Cckappa_sig.bonds in  
                let lbondk = 
                  match lbondk with 
                  | None ->  Cckappa_sig.Site_map_and_set.Map.empty
                  | Some a -> a 
                in 
                let rbondk = Cckappa_sig.Site_map_and_set.Map.empty in
                let error,half_release_set = set_bound_sites parameters error k lagk half_release_set in  
                let actions =  
                  {actions 
                  with Cckappa_sig.remove = (k,clean_agent lagk,[])::actions.Cckappa_sig.remove} 
                in 
                error, (
                  direct,
                  reverse,
                  actions,
                       half_release_set,
                       Some agent_type,
                       lbondk,
                       rbondk,
		       dead || (match lhsk with Some Cckappa_sig.Dead_agent _ -> true | _ -> false) 
                )
	      end
	   | Some Cckappa_sig.Ghost, Some Cckappa_sig.Agent ragk -> (*creation*)
	      begin 
		let agent_type = ragk.Cckappa_sig.agent_name in 
		let error,direct = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k (Cckappa_sig.upgrade_some_interface ragk) direct in  
		let error,rbondk = Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get parameters error k c_rule_rhs.Cckappa_sig.bonds in  
		let rbondk  = 
                  match rbondk with 
                  | None  -> Cckappa_sig.Site_map_and_set.Map.empty
                  | Some a -> a 
		in 
		let lbondk = Cckappa_sig.Site_map_and_set.Map.empty in
		error,
		(
                  direct,
                  reverse,
                  {
		    actions 
                  with Cckappa_sig.creation = (k,ragk.Cckappa_sig.agent_name)::actions.Cckappa_sig.creation
                  },
                  half_release_set,
                  Some agent_type,
                  lbondk, 
                  rbondk,
		  dead
		)
	      end 
	   | Some Cckappa_sig.Agent lagk,Some Cckappa_sig.Agent ragk 
	   | Some Cckappa_sig.Dead_agent (lagk,_,_,_), Some Cckappa_sig.Agent ragk -> (* TO DO check what happen if one site is dead *)
	      let agent_type = lagk.Cckappa_sig.agent_name in 
	      let error',ldiff,rdiff = Cckappa_sig.Site_map_and_set.Map.diff_pred parameters error equ_port lagk.Cckappa_sig.agent_interface ragk.Cckappa_sig.agent_interface in 
	      let error = Exception.check warn parameters error error' (Some "line 617") Exit in 
	      let error,lbondk = Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get parameters error k c_rule_lhs.Cckappa_sig.bonds in  
	      let error,rbondk = Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get parameters error k c_rule_rhs.Cckappa_sig.bonds in  
	      let lbondk = 
                match lbondk with 
                | None -> Cckappa_sig.Site_map_and_set.Map.empty
                | Some a -> a 
	      in
	      let rbondk = 
		match rbondk with 
		| None -> Cckappa_sig.Site_map_and_set.Map.empty
		| Some a -> a 
	      in
	      let error,direct = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k (Cckappa_sig.upgrade_interface lagk rdiff) direct in 
	      let error,reverse = Int_storage.Quick_Nearly_inf_Imperatif.set parameters error k (Cckappa_sig.upgrade_interface ragk ldiff) reverse in 
	      let error,half_release_set = set_released_sites parameters error k lagk ragk half_release_set in                 
	      error,(direct,reverse,actions,half_release_set,Some agent_type,lbondk,rbondk,
		    dead || (match lhsk with Some Cckappa_sig.Dead_agent _ -> true | _ -> false))
	   | Some Cckappa_sig.Unknown_agent _, _ 
	   | _, Some Cckappa_sig.Unknown_agent _ 
	   | Some Cckappa_sig.Ghost,Some Cckappa_sig.Ghost
	   | _,Some Cckappa_sig.Dead_agent _ | None,_ | _,None -> 
							 (print_int k;print_newline ();warn parameters error (Some "line 542") Exit (direct,reverse,actions,half_release_set,None,Cckappa_sig.Site_map_and_set.Map.empty,Cckappa_sig.Site_map_and_set.Map.empty,true))
         in 
         let error',bond_l,bond_r = Cckappa_sig.Site_map_and_set.Map.diff parameters error lbondk rbondk in
	 let error = Exception.check warn parameters error error' (Some "line 746") Exit in 
         let release = actions.Cckappa_sig.release in 
         let error,(full_release_set,release) =
	   match agent_type
	   with
	     None -> error,(full_release_set,release)
	   | Some agent_type -> 
	      Cckappa_sig.Site_map_and_set.Map.fold
		(fun site target (error,(full_release_set,release)) -> 
                 let source = Cckappa_sig.build_address k agent_type site in 
                 let error',full_release_set = Cckappa_sig.Address_map_and_set.Set.add parameters error source full_release_set in 
		 let error = Exception.check warn parameters error error' (Some "line 753") Exit in 
                 let release = 
                   if compare source target < 0 
                     then (source,target)::release
                   else release
                 in 
                 (error,(full_release_set,release)))
		bond_l
		(error,(full_release_set,release))
         in 
         let bind = actions.Cckappa_sig.bind in 
         let error,bind = 
	   match agent_type
	   with
	     None -> error,bind
	   | Some agent_type -> 
	      Cckappa_sig.Site_map_and_set.Map.fold
		(fun site target (error,bind) -> 
		 let source = Cckappa_sig.build_address k agent_type site in 
		 let bind = 
                   if compare source target < 0 
                   then (source,target)::bind
                   else bind
		 in 
		 (error,bind))
		bond_r
		(error,bind)
         in 
         let actions = {actions with Cckappa_sig.release = release ; Cckappa_sig.bind = bind } in  
         aux_agent (k+1) (error,(direct,reverse,actions,half_release_set,full_release_set,dead))
       end
   in
   let error,(direct,reverse,actions,half_release_set,full_release_set,dead) = aux_agent 0 (error,(direct,reverse,actions,half_release_set,full_release_set,false)) in
   let error',half_release_set = Cckappa_sig.Address_map_and_set.Set.minus parameters error half_release_set full_release_set in
   let error = Exception.check warn parameters error error' (Some "line 753") Exit in                  
   let list = Cckappa_sig.Address_map_and_set.Set.elements half_release_set in
   let error,list = 
     List.fold_left 
       (fun (error,list) add -> 
         let error,ag = Int_storage.Quick_Nearly_inf_Imperatif.get parameters error add.Cckappa_sig.agent_index c_rule_lhs.Cckappa_sig.views in  
         match ag 
         with 
         | None | Some Cckappa_sig.Ghost -> warn parameters error (Some "line 623") Exit ((add,None)::list)
	 | Some (Cckappa_sig.Unknown_agent _) -> error,list 
	 | Some (Cckappa_sig.Dead_agent (ag,_,_,l')) ->
            let interface = ag.Cckappa_sig.agent_interface in
	    begin
	      match Cckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters error add.Cckappa_sig.site interface with
	      | error,None ->
		 begin
		   match Cckappa_sig.Site_map_and_set.Map.find_option_without_logs parameters error add.Cckappa_sig.site l'
		   with
		   | error,None ->
		     begin 
		       Exception.warn parameters error
				      (Some "Preprocess") (Some "line 829, dead site")
				      Not_found (fun () -> (add,None)::list)
		     end
		   | error,Some x ->
	    	      error,(add,None)::list
		 end
	      | error',Some state ->
		 Exception.check warn parameters error error' (Some "line 799") Exit,
		 (add,Some state.Cckappa_sig.site_state)::list
	    end
	 | Some (Cckappa_sig.Agent ag) ->
            let interface = ag.Cckappa_sig.agent_interface in
	    match Cckappa_sig.Site_map_and_set.Map.find_option parameters error add.Cckappa_sig.site interface with
	    | error',None ->
	       Exception.warn parameters (Exception.check warn parameters error error' (Some "line 799") Exit)
			      (Some "Preprocess") (Some "find_map, line 405")
			      Not_found (fun () -> (add,None)::list)
            | error',Some state ->
	       Exception.check warn parameters error error' (Some "line 799") Exit,
	       (add,Some state.Cckappa_sig.site_state)::list)
       (error,[])
       (List.rev list) 
   in 
   let actions = {actions with Cckappa_sig.half_break = list} in 
   let error,label_dot = 
     match 
       label 
     with 
     | None -> error,None 
     | Some (string,pos) -> 
       let error,s = Tools_kasa.make_id_compatible_with_dot_format parameters error string in
       error,Some(s,pos)
   in 
   error, 
    ({Cckappa_sig.e_rule_label = label;
      Cckappa_sig.e_rule_label_dot = label_dot;
      Cckappa_sig.e_rule_initial_direction = direction; 
      Cckappa_sig.e_rule_rule = rule;
      Cckappa_sig.e_rule_c_rule = 
        { Cckappa_sig.prefix = rule.Ckappa_sig.prefix;
	  Cckappa_sig.delta = rule.Ckappa_sig.delta;
	  Cckappa_sig.rule_lhs = c_rule_lhs ; 
          Cckappa_sig.rule_arrow = rule.Ckappa_sig.arrow ; 
          Cckappa_sig.rule_rhs =  c_rule_rhs ;        
          Cckappa_sig.actions = actions;
          Cckappa_sig.diff_direct = direct ; 
          Cckappa_sig.diff_reverse = reverse ; 
        }
   })
   
 let refine_removal_action parameters error handler (i,ag,l) = 
   let l_documented = List.sort compare (clean_agent2 ag) in 
   let error,l_undocumented = 
     Handler.complementary_interface parameters error handler ag.Cckappa_sig.agent_name l_documented
   in 
     (error,(i,ag,l_undocumented))
        
 let refine_rule parameters error handler rule = 
   let error,removal_actions = 
     List.fold_left
       (fun (error,l) act -> 
          let error,act' = refine_removal_action parameters error handler act in 
           error,act'::l)
       (error,[])
       (List.rev rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.actions.Cckappa_sig.remove) 
   in 
   error,
   {rule 
       with Cckappa_sig.e_rule_c_rule = 
         {rule.Cckappa_sig.e_rule_c_rule 
           with Cckappa_sig.actions = 
               {rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.actions 
                 with Cckappa_sig.remove = removal_actions}}}
        
let lift_forbidding_question_marks parameters handler = 
  (fun error x ->
   let a,b,c = translate_mixture parameters error handler  x
   in a,b)
    
let lift_allowing_question_marks parameters handler =
  (fun error x ->
   let a,b,c = translate_mixture parameters error handler x in
   clean_question_marks parameters a c b) 

let translate_init parameters error handler init =  
   let (a,init_t) = init in
   match 
     init_t
   with Ast.INIT_MIX((alg,pos),(mixture,_pos')) -> 
     let error,c_alg = Prepreprocess.alg_map (lift_allowing_question_marks parameters handler) error alg in 
     let error,c_mixture,_ = translate_mixture parameters error handler mixture in 
   error,
   {Cckappa_sig.e_init_factor = alg ; 
    Cckappa_sig.e_init_c_factor = c_alg ;
    Cckappa_sig.e_init_mixture = mixture ;
    Cckappa_sig.e_init_c_mixture = c_mixture ;
    Cckappa_sig.e_init_string_pos = a} 
   | Ast.INIT_TOK _ -> (*TO DO*)
     let error,dft = Cckappa_sig.dummy_init parameters error in 
     warn parameters error (Some "line 710, token are not supported yet") Exit dft 

let alg_with_pos_map = Prepreprocess.map_with_pos Prepreprocess.alg_map

let translate_var parameters error handler (a,b) =
  let error,b' = alg_with_pos_map (lift_allowing_question_marks parameters handler) error b in 
   let error,a_dot = Tools_kasa.make_id_compatible_with_dot_format parameters error (fst a) in 
   error,
	  {
	    Cckappa_sig.e_id = fst a; 
	    Cckappa_sig.e_id_dot = a_dot;
	    Cckappa_sig.c_variable = fst b ;
	    Cckappa_sig.e_variable = (a,b')}
   

 let translate_obs parameters error handler (a,b) = 
   let error,a' = Prepreprocess.alg_map (lift_allowing_question_marks parameters handler) error a in 
   error,(a',b)
     
 let bool_with_pos_map = Prepreprocess.map_with_pos Prepreprocess.bool_map

 let bool_with_pos_with_option_map = Prepreprocess.with_option_map bool_with_pos_map

 let translate_perturb parameters error handler ((bool1,modif,bool2),pos2) = 
   let error,bool1' = bool_with_pos_map (lift_allowing_question_marks parameters handler) error bool1 in 
   let error,modif' = 
     List.fold_left 
       (fun (error,l) elt -> 
	let error,elt' = Prepreprocess.modif_map (lift_forbidding_question_marks parameters handler) (lift_allowing_question_marks parameters handler) error elt in 
	 error,elt'::l)
       (error,[]) (List.rev modif) 
   in 
   let error,bool2' = bool_with_pos_with_option_map (lift_allowing_question_marks parameters handler) error bool2 in 
   error,((bool1',modif',bool2'),pos2)

 let translate_c_compil parameters error handler compil = 
   let error,c_signatures = 
     List.fold_left 
       (fun (error,list) agent -> 
         let error,ag = 
           translate_agent_sig 
             parameters 
             error 
             handler 
             agent 
             0 
         in 
         error,(ag::list))
       (error,[]) compil.Ast.signatures in 
   let error,c_variables =  
     List.fold_left 
       (fun (error,list) var -> 
         let error,var = translate_var parameters error handler var in 
         error,(var::list))
       (error,[]) compil.Ast.variables 
   in 
   let error,c_rules = 
     List.fold_left 
       (fun (error,list) rule -> 
         let error,c_rule = translate_rule parameters error handler rule in 
         error,(c_rule::list))
       (error,[]) 
       compil.Ast.rules 
   in 
   let error,c_observables = 
     List.fold_left 
       (fun (error,list) obs -> 
         let error,c_obs = translate_obs parameters error handler obs in 
         error,c_obs::list)
       (error,[]) compil.Ast.observables  
   in 
   let error,c_inits = 
     List.fold_left 
       (fun (error,list) init -> 
         let error,c_init = translate_init parameters error handler init in 
         error,c_init::list)
       (error,[]) compil.Ast.init
   in 
   let error,c_perturbations = 
     List.fold_left 
       (fun (error,list) perturb -> 
         let error,c_perturb = translate_perturb parameters error handler perturb in 
         error,c_perturb::list)
       (error,[]) compil.Ast.perturbations  
   in 
   
   let error,c_rules = 
     List.fold_left 
       (fun (error,list) rule -> 
         let error,c_rule = refine_rule parameters error handler rule in 
         error,(c_rule::list))
       (error,[]) (List.rev c_rules) 
    in 
   let n_vars = List.length c_variables in 
   let error,c_variables = Misc_sa.array_of_list Int_storage.Nearly_inf_Imperatif.create Int_storage.Nearly_inf_Imperatif.set parameters error (List.rev c_variables) in   
   let error,c_signatures = Misc_sa.array_of_list Int_storage.Nearly_inf_Imperatif.create Int_storage.Nearly_inf_Imperatif.set parameters error (List.rev c_signatures) in  
   let n_rules = List.length c_rules in 
   let error,c_rules = Misc_sa.array_of_list Int_storage.Nearly_inf_Imperatif.create Int_storage.Nearly_inf_Imperatif.set parameters error (List.rev c_rules) in  
   let error,c_observables = Misc_sa.array_of_list Int_storage.Nearly_inf_Imperatif.create Int_storage.Nearly_inf_Imperatif.set parameters error (List.rev c_observables) in   
   let error,c_inits = Misc_sa.array_of_list Int_storage.Nearly_inf_Imperatif.create Int_storage.Nearly_inf_Imperatif.set parameters error (List.rev c_inits) in  
   let error,c_perturbations = Misc_sa.array_of_list Int_storage.Nearly_inf_Imperatif.create Int_storage.Nearly_inf_Imperatif.set parameters error (List.rev c_perturbations) in  
 
   error,
   {handler with Cckappa_sig.nrules = n_rules ; Cckappa_sig.nvars = n_vars }, 
   {
    Cckappa_sig.variables = c_variables ;       
    Cckappa_sig.signatures = c_signatures ; 
    Cckappa_sig.rules = c_rules ;
    Cckappa_sig.observables = c_observables ; 
    Cckappa_sig.init = c_inits ;
    Cckappa_sig.perturbations = c_perturbations } 
    
   
  
