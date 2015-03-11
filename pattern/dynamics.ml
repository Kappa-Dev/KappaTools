open Mods
open ExceptionDefn
open Primitives

let compute_causal lhs rhs script env =
  let open Primitives.Causality in
  let causal_map = (*adding tests for all sites mentionned in the left hand side --including existential site*)
    IntMap.fold
      (fun id ag causal_map ->
       let causal_map,bool =
	 Mixture.fold_interface
	   (fun site_id (int,lnk) (causal_map,bool) ->
	    if site_id <> 0 then
              let c = create (int <> None) (lnk <> Mixture.WLD) in
	      PortMap.add (KEPT id,site_id) c causal_map,true
	    else causal_map,bool)
	   ag (causal_map,false)
       in
       (* we put a TEsT on the existential site only if nothing else is tested *)
       PortMap.add (KEPT id,0) (create false (not bool)) causal_map)
      (Mixture.agents lhs) PortMap.empty
  in
  let add_causal p f map =
    let c' = try PortMap.find p map with Not_found -> create false false in
    PortMap.add p (f c') map
  in
  let causal_map,_ =
    List.fold_left
      (fun (causal_map,bool) action ->
       match action with
       | BND (p1, p2) ->
	  add_causal p2 add_link_modif (add_causal p1 add_link_modif causal_map),true
       | FREE (p1,side_effect_free) ->
	  if side_effect_free then
	    begin
	      match p1 with
	      | (KEPT id, site_id) ->
		 begin
		   match Mixture.follow (id,site_id) lhs with
		   | Some (id',site_id') ->
		      let p2 = (KEPT id',site_id') in
		      add_causal p2 add_link_modif
				 (add_causal p1 add_link_modif causal_map),true
		   | None -> invalid_arg "Dynamics.Compute_causal"
		 end
	      | (FRESH _, _) -> invalid_arg "Dynamics.Compute_causal"
	    end
	  else
	    add_causal p1 add_link_modif causal_map,true
       | MOD (p,_) -> add_causal p add_internal_modif causal_map,true
       | DEL ag_id ->
	  Mixture.fold_interface
	    (fun site_id (int,_lnk) (causal_map,bool) ->
	     let causal_map =
	       match int with
		 Some _ -> add_causal (KEPT ag_id,site_id) add_internal_modif causal_map | None -> causal_map
	     in
	     add_causal (KEPT ag_id,site_id) add_link_modif causal_map,true
	    )
	    (Mixture.agent_of_id ag_id lhs) (causal_map,bool)
       | ADD (ag_id,name_id) -> (*Interface might be partial!*)
	  let arity = Signature.arity env.Environment.signatures name_id in
	  let site_id = ref 0 in
	  let p_causal = ref causal_map in
	  while !site_id < arity do
	    p_causal :=
	      begin
		match Environment.default_state name_id !site_id env with
		| None -> !p_causal
		| Some i ->
                   add_causal (FRESH ag_id,!site_id) add_internal_modif !p_causal
	      end ;
	    p_causal := add_causal (FRESH ag_id,!site_id) add_link_modif !p_causal ;
	    site_id := !site_id + 1 ;
	  done ;
	  !p_causal,if arity=0 then false else true
      ) (causal_map,false) script
  in causal_map

let compute_causal_init (((node_id,agent_name),interface),_) env =
  let open Primitives.Causality in
  List.fold_left
    (fun causal_map (site_id,(int,lnk)) ->
     let c =
       (match int with
	| Some i -> add_internal_modif
	| None -> fun x -> x) (add_link_modif (create false false))
     in
     (* I do not know whether the site can bear an internal state *)
     (* TO DO: improve *)
     if site_id <> 0 then
       Mods.Int2Map.add (node_id,site_id) c causal_map
     else causal_map
    )
    (Mods.Int2Map.add (node_id,0) (add_link_modif (create false false))
		      Mods.Int2Map.empty)
    interface

let compute_causal_obs lhs = compute_causal lhs lhs []

let diff pos m0 m1 env =
  let add_map id site_type map =
    let set = try IdMap.find id map with Not_found -> Int2Set.empty in
    IdMap.add id (Int2Set.add site_type set) map
  in
  (*let side_effect = ref false in*)
  let pp_warning pr = warning ~pos pr in
  let compile_error pos msg = raise (ExceptionDefn.Malformed_Decl (msg,pos)) in
  let id_preserving ag1 ag2 =
    (*check whether ag2 can be the residual of ag1 for (same name)*)
    Mixture.name ag1 = Mixture.name ag2
  in
  let prefix, deleted, add_index =
    IntMap.fold
      (fun id ag0 (prefix, deleted, ind) ->
       if deleted = [] then
	 try
	   let ag1 = IntMap.find id (Mixture.agents m1) in
	   if id_preserving ag0 ag1 then (id:: prefix, deleted, ind)
	   else (prefix, id:: deleted, if id<ind then id else ind)
	 with Not_found -> (prefix, id:: deleted, ind)
       (*id is bigger than the max id in m1 so id is deleted*)
       else (prefix,id::deleted,ind)
      )
      (Mixture.agents m0) ([],[], IntMap.size (Mixture.agents m0))
  in
  let added =
    IntMap.fold
      (fun id ag1 added ->
       if id < add_index then added else (id:: added)
      )	(Mixture.agents m1) []
  in
  let balance = (List.length deleted, List.length prefix, List.length added)
  and instructions = (*adding deletion instructions*)
    List.fold_left
      (fun inst id ->
       (*side_effect := true ;*)
       (DEL id):: inst
      )
      [] deleted
  in
  let instructions = (*adding creation instructions*)
    List.fold_left
      (fun instructions id ->
       let ag = Mixture.agent_of_id id m1 in
       let name_id = Mixture.name ag in
       ADD(id, name_id)::instructions
      )
      instructions added
  in
  let instructions,modif_sites = (*adding connections of new agents if partner has a lower id*)
    List.fold_left
      (fun (inst,idmap) id ->
       let ag = Mixture.agent_of_id id m1 in
       let name_id = Mixture.name ag in
       let sign = Environment.get_sig name_id env in
       let modif_sites =
	 Signature.fold
	   (fun site_id _ idmap ->
	    add_map (FRESH id) (site_id,0) (add_map (FRESH id) (site_id,1) idmap)
	   )
	   sign idmap
       in
       let inst =
	 Mixture.fold_interface
	   (fun site_id (int, lnk) inst ->
	    let def_int =
	      Environment.default_state (Mixture.name ag) site_id env in
	    let inst =
	      match (def_int, int) with
	      | (None, None) -> inst
	      | (Some i, None) -> inst (*DCDW: default will be assumed*)
	      | (Some i, Some j) -> (MOD((FRESH id, site_id), j))::inst
	      | (None, Some k) ->
		 compile_error pos "This rule is adding an agent that is not supposed to have an internal state"
	    in
	    match lnk with
	    | Mixture.WLD ->
	       compile_error pos "This rule is adding an agent that is not fully described (wild card link)"
	    | Mixture.FREE -> inst
	    | Mixture.TYPE _ ->
	       compile_error pos "This rule is adding an agent that is not fully described (link type)"
	    | Mixture.BND ->
	       let opt = Mixture.follow (id, site_id) m1 in
	       match opt with
	       | None ->
		  compile_error pos "This rule is adding an agent that is not fully described (semi-lnk)"
	       | Some (i,x) ->
		  let bnd_i =
		    if List.mem i added then (FRESH i) else (KEPT i)
		  in
		  if i < id || (i = id && x < site_id) then
		    BND((bnd_i, x),(FRESH id, site_id)):: inst
		  else  inst
	   )
	   ag inst
       in
       (inst,modif_sites)
      )
      (instructions,IdMap.empty) added
  in
  let instructions,modif_sites =
    List.fold_left
      (fun (inst,idmap) id -> (*adding link and internal state modifications for agents conserved by the rule*)
       let ag, ag' = (Mixture.agent_of_id id m0, Mixture.agent_of_id id m1) in
       let ag_name f = Environment.print_agent env f (Mixture.name ag) in
       let interface' = Mixture.interface ag' in
       (*folding on ag's interface: *)
       (* problem when a site is not mentionned at all in ag but is used in ag'
          --ie modif with no test*)
       let sign = Environment.get_sig (Mixture.name ag) env in
       let interface = Mixture.interface ag in
       let interface = Signature.fold
			 (fun site_id _ interface ->
			  if IntMap.mem site_id interface then interface
			  else IntMap.add site_id (None,Mixture.WLD) interface)
			 sign interface in
       IntMap.fold
	 (fun site_id (int_state, lnk_state) (inst,idmap) ->
	  let site_name f = Environment.print_site env (Mixture.name ag) f site_id in
	  let int_state', lnk_state' =
	    try IntMap.find site_id interface' with
	    | Not_found -> (None,Mixture.WLD) (*site is not mentioned in the right hand side*)
	  in
	  let inst,idmap =
	    match (int_state, int_state') with
	    | (Some i, Some j) ->
	       if i = j then (inst,idmap)
	       else
		 let inst = (MOD ((KEPT id, site_id), j))::inst
		 and idmap = add_map (KEPT id) (site_id,0) idmap
		 in
		 (inst,idmap)
	    | (Some _, None) ->
	       compile_error
		 pos
		 (Format.asprintf "The internal state of agent '%t', site '%t' on the right hand side is underspecified" ag_name site_name)
	    | (None, Some j) ->
	       let _ =
		 pp_warning
		   (fun f ->
		    Format.fprintf
		      f "internal state of site '%t' of agent '%t' is modified although it is left unpecified in the left hand side"
		      site_name ag_name
		   )
	       in
	       let inst = (MOD ((KEPT id, site_id), j))::inst
	       and idmap = add_map (KEPT id) (site_id,0) idmap
	       in
	       (inst,idmap)
	    | (None, None) -> (inst,idmap)
	  in
	  match lnk_state, lnk_state' with
	  | (Mixture.BND, Mixture.FREE | Mixture.TYPE _, Mixture.FREE) ->
	     (*connected -> disconnected*)
	     let opt = Mixture.follow (id, site_id) m0 in
	     begin
	       match opt with
	       | Some (id', site_id') -> (*generating a FREE instruction only for the smallest port*)
		  let kept = List.exists (fun id -> id=id') prefix
		  (*try let _ = (Mixture.agent_of_id id' m1) in true with Not_found -> false*)
		  in
		  let apply_anyway = (*generate the FREE instruction without testing whether a FREE instruction will be generated for (id',site_id')*)
		    if not kept then true
		    else
		      match Mixture.follow (id',site_id') m1 with
		      | None -> false
		      | Some _ -> true
		  in
		  let idmap =
		    if kept then
		      add_map (KEPT id) (site_id,1) (add_map (KEPT id') (site_id',1) idmap) 
		    else
		      add_map (KEPT id) (site_id,1) idmap
		  and inst =
		    (*generating only one FREE instruction when id'<=id or when (id',site_id') is still bound in rhs*)
		    if apply_anyway || id'< id || (id'= id && site_id'< site_id)
		    then (FREE (((KEPT id), site_id),true)):: inst
		    else inst
		  in
		  (inst,idmap)
	       | None -> (*breaking a semi link so generate a FREE instruction*)
		  let inst = (FREE (((KEPT id), site_id),false)):: inst
		  and idmap = add_map (KEPT id) (site_id,1) idmap
		  in
		  (*side_effect := true ;*)
		  let _ =
		    pp_warning
		      (fun f ->
		       Format.fprintf
			 f "breaking a semi-link on site '%t' will induce a side effect"
			 site_name
		      )
		  in
		  (inst,idmap)
	     end
	  | (Mixture.BND, Mixture.BND | Mixture.TYPE _, Mixture.BND) -> (*connected -> connected*)
             let opt = Mixture.follow (id, site_id) m0 in
	     let opt' =  Mixture.follow (id, site_id) m1 in
	     let common_update inst idmap id1 i1 =
	       let id1' =
		 if List.exists (fun id -> id=id1) prefix
		 then (KEPT id1)
		 else (FRESH id1) in
	       let idmap' =
		 add_map (KEPT id) (site_id,1) (add_map id1' (i1,1) idmap) in
	       let inst =
		 if id1 < id || (id1 = id && i1 < site_id)
		 (*generating an instruction only for one of both sites*)
		 then BND((KEPT id, site_id), (id1',i1)):: inst
		 else inst in
	       (inst,idmap')
	     in
	     begin
	       match (opt, opt') with
	       | (None, Some (id1', i1')) -> (*sub-case: semi-link -> connected*)
		  let () =
		    pp_warning
		      (fun f ->
		       Format.fprintf
			 f
			 "link state of site '%t' of agent '%t' is changed although it is a semi-link in the left hand side"
			 site_name ag_name
		      ) in
		  common_update inst idmap id1' i1'
	       | (Some (id1, i1), Some (id1', i1')) -> (*sub-case: connected -> connected*)
		  let kept_id1 = List.exists (fun id -> id=id1) prefix in
                  if kept_id1 && id1=id1' && i1=i1' then (inst,idmap)
                  else
		    let () =
		      pp_warning
			(fun f ->
			 Format.fprintf
			   f
			   "rule induces a link permutation on site '%t' of agent '%t'"
			   site_name ag_name
			) in
		    (*modifed sites*)
		    (*it might be that id1 is not preserved by the reaction!*)
		    let idmap = if kept_id1
				then add_map (KEPT id1) (i1,1) idmap
				else idmap in
		    common_update inst idmap id1' i1'
	       | (Some (id1, i1), None) ->
		  (*sub-case: connected -> semi-link*)
		  compile_error
		    pos (Format.asprintf
			   "The link status of agent '%t', site '%t' on the right hand side is underspecified"
			   ag_name site_name)
	       | (None, None) -> (*sub-case: semi-link -> semi-link*)
		  (inst,idmap) (*nothing to be done*)
	     end
	  | (Mixture.FREE, Mixture.BND) -> (*free -> connected*)
	     begin
	       let opt' = Mixture.follow (id, site_id) m1 in
	       match opt' with
	       | None -> (*sub-case: free -> semi-link*)
		  compile_error
		    pos (Format.asprintf
			   "The link status of agent '%t', site '%t' on the right hand side is inconsistent"
			   ag_name site_name)
	       | Some (id', i') -> (*sub-case: free -> connected*)
		  (*no warning*)
		  (*modif sites*)
		  let id'' = if List.exists (fun id -> id=id') prefix
			     then KEPT id'
			     else FRESH id' in
		  let idmap' = add_map (KEPT id) (site_id,1) (add_map id'' (i',1) idmap) 
		  in
		  if (id'< id) || (id'= id && i'< site_id) then
		    let inst = BND((KEPT id, site_id), (id'', i')):: inst in
		    (inst,idmap')
		  else
		    (inst,idmap')
	     end
	  | (Mixture.FREE, Mixture.FREE | Mixture.WLD, Mixture.WLD) ->
	     (*free -> free or wildcard -> wildcard*) (inst,idmap)
	  | (Mixture.TYPE (sid,nme),Mixture.TYPE(sid',nme')) ->
	     if sid=sid' && nme=nme' then (inst,idmap)
	     else compile_error
		    pos (Format.asprintf
			   "The link status of agent '%t', site '%t' on the right hand side is inconsistent"
					ag_name site_name)
	  | Mixture.WLD, Mixture.FREE ->  (*wildcard -> free*)
	     let _ =
	       pp_warning
		 (fun f ->
		  Format.fprintf
		    f "application of this rule will induce a null event when applied to an agent '%t' that is free on '%t'"
		    ag_name site_name
		 )
	     in
	     let inst = (FREE ((KEPT id, site_id),false))::inst
	     and idmap = add_map (KEPT id) (site_id,1) idmap
	     in
	     ((*side_effect := true ;*)
	       (inst,idmap))
	  | Mixture.WLD, Mixture.BND ->  (*wildcard -> connected*)
	     let opt' = Mixture.follow (id, site_id) m1 in
	     begin
	       match opt' with
	       | None ->
		  compile_error
		    pos (Format.asprintf
			   "The link status of agent '%t', site '%t' on the right hand side is inconsistent"
			   ag_name site_name)
	       | Some (id', i') ->
		  (*warning*)
		  let () =
		    pp_warning
		      (fun f ->
		       Format.fprintf
			 f "site '%t' of agent '%t' is bound in the right hand side although it is unspecified in the left hand side"
			 site_name ag_name
		      )
		  in
		  (*modif sites*)
		  let id'' = if List.exists (fun id -> id=id') prefix then KEPT id' else FRESH id' in
		  let idmap' = add_map (KEPT id) (site_id,1) (add_map id'' (i',1) idmap) in
		  (*instruction*)
		  if (id'< id) || (id'= id && i'< site_id) then
		    let inst = BND((KEPT id, site_id), (id'', i')):: inst
		    in
		    ((*side_effect:= true;*)
		      (inst,idmap'))
		  else (inst,idmap')
	     end
	  | _,(Mixture.WLD | Mixture.TYPE _) -> (*connected,free -> wildcard*)
	     compile_error
	       pos (Format.asprintf
		      "The link status of agent '%t', site '%t' on the right hand side is underspecified"
		      ag_name site_name)
	 )
	 interface (inst,idmap)
      )
      (instructions,modif_sites) prefix
  in
  let sort inst inst' =
    let weight i =
      match i with
      | ADD _ -> 0 (*adding new agents has biggest priority*)
      | DEL _ -> 4 (*deleting agents last to minimize side effects*)
      | MOD _ -> 1 (*whatev*)
      | BND _ -> 3 (*freeing links before creating new ones*)
      | FREE _ -> 2
    in
    compare (weight inst) (weight inst')
  in
  ((List.fast_sort sort instructions),balance,added,modif_sites (*,!side_effect*))
(*List.rev instructions, balance, added, modif_sites,!side_effect*)

let site_defined site_id ag is_added env =
  try
    let (int,lnk) = IntMap.find site_id (Mixture.interface ag) in
    match int with
    | Some _ -> Some (int,lnk)
    | None ->
       match lnk with
       | Mixture.WLD -> None
       | Mixture.BND | Mixture.TYPE _ | Mixture.FREE -> Some (int,lnk)
  with Not_found ->
    if not is_added then None
    else
      try Some (Environment.default_state (Mixture.name ag) site_id env,
		Mixture.FREE)
      with Not_found -> invalid_arg "Mixture.site_defined: invariant violation"


let rec superpose todo_list lhs rhs map already_done added codomain env =
  match todo_list with
  | [] -> map
  | (lhs_id,rhs_id)::tl ->
     let map = IntMap.add lhs_id rhs_id map in (*attempt to map lhs_id to rhs_id*)
     let lhs_ag = Mixture.agent_of_id lhs_id lhs
     and rhs_ag = Mixture.agent_of_id rhs_id rhs
     in
     let todo_list,already_done =
       if not (Mixture.name lhs_ag = Mixture.name rhs_ag) then raise False
       else
	 Mixture.fold_interface
	   (fun site_id (int,lnk) (todo,already_done) ->
	    let opt = site_defined site_id rhs_ag (IntSet.mem rhs_id added) env in
	    match opt with
	    | None -> (todo,already_done) (*site_id is not in the agent in the rhs*)
	    | Some (int',lnk') ->
	       let compatible =
		 match (int,int') with
		 | (Some i,Some i') -> i=i'
		 | (None,_) -> true
		 | (_,None) -> true
	       in
	       if not compatible then raise False
	       else
		 match (lnk,lnk') with
		 | (Mixture.BND,Mixture.BND) ->
		    begin
		      let opt = Mixture.follow (lhs_id,site_id) lhs
		      and opt' = Mixture.follow (rhs_id,site_id) rhs in
		      match opt with
		      | None -> (*semi-link*) (todo,already_done)
		      | Some (lhs_id',site_id') ->
			 match opt' with
			 | None -> (todo,already_done)
			 | Some (rhs_id',site_id'') ->
			    if site_id'=site_id'' then
			      if Int2Set.mem (lhs_id',rhs_id') already_done
			      then (todo,already_done)
			      else ((lhs_id',rhs_id')::todo,
				    Int2Set.add (lhs_id',rhs_id') already_done)
			    else raise False
		    end
		 | (Mixture.BND,Mixture.TYPE (site_j,name_id)) ->
		    begin
		      let opt = Mixture.follow (lhs_id,site_id) lhs in
		      match opt with
		      | None -> (todo,already_done) (*semi-link < link_type*)
		      | Some (lhs_id',site_id') ->
			 let ag' = Mixture.agent_of_id lhs_id' lhs in
			 let name = Mixture.name ag' in
			 if (name = name_id) && (site_id' = site_j)
			 then (todo,already_done)
			 else raise False
		    end
		 | (Mixture.TYPE (site_j,name_id),Mixture.BND) ->
		    begin
		      let opt = Mixture.follow (rhs_id,site_id) rhs in
		      match opt with
		      | None -> (todo,already_done) (*semi-link < link_type*)
		      | Some (rhs_id',site_id') ->
			 let ag' = Mixture.agent_of_id rhs_id' rhs in
			 let name = Mixture.name ag' in
			 if (name = name_id) && (site_id' = site_j)
			 then (todo,already_done)
			 else raise False
		    end
		 | (Mixture.TYPE (site_j,name_id),Mixture.TYPE (site_j',name_id')) ->
		    if (name_id = name_id') && (site_j = site_j')
		    then (todo,already_done)
		    else raise False
		 | (Mixture.FREE,Mixture.FREE)
		 | (Mixture.WLD,_) | (_,Mixture.WLD) -> (todo,already_done)
		 | (Mixture.FREE | Mixture.TYPE _ | Mixture.BND), _ ->
		    raise False
	   ) lhs_ag (tl,already_done)
     in
     superpose todo_list lhs rhs map (*IntMap.add lhs_id rhs_id map*) already_done added (IntSet.add rhs_id codomain) env

let enable r mix env =
  let unify pat1 pat2 (root,modif_sites) glueings already_done =
    let root_ag =
      try Mixture.agent_of_id root pat1
      with Not_found ->
	invalid_arg
	  (Format.asprintf "Dynamics.enable: agent %d not found in %a"
			  root (Kappa_printer.mixture true env) pat1) in
    let name_id_root = Mixture.name root_ag in
    let candidates = (*agent id in lhs --all cc-- that have the name name_id_root*)
      let cpt = ref 0
      and candidates = ref IntSet.empty
      in
      while !cpt < Mixture.arity pat2 do
	candidates := IntSet.union
			!candidates
			(Mixture.ids_of_name name_id_root !cpt pat2);
	cpt := !cpt+1
      done ;
      !candidates
    in
    IntSet.fold
      (fun lhs_ag_id (glueings,already_done) ->
       if Int2Set.exists
	    (*checking that lhs contains --ie tests-- a site that is modified by rhs*) 
	    (fun (site_id,t) ->
	     let ag = Mixture.agent_of_id lhs_ag_id pat2 in
	     let opt = site_defined site_id ag false env in
	     match opt with
	     | Some (int,lnk) ->
		begin
		  if t=0 (*int-modified*) then
		    match int with Some _ -> true | None -> false
		  else
		    match lnk with Mixture.WLD -> false
				 | (Mixture.BND | Mixture.TYPE _
				    | Mixture.FREE) -> true
		end
	     | None -> false
	    ) modif_sites
       then
	 let opt =
	   try
	     Some (superpose [(lhs_ag_id,root)] pat2 pat1 IntMap.empty
			     Int2Set.empty r.added IntSet.empty env)
	   with False -> None
	 in
	 match opt with
	 | Some map -> (*map: id_mix -> id_rule*)
	    begin
	      let opt = IntMap.root map in
	      match opt with
	      | None -> invalid_arg "Dynamics.enable: empty map"
	      | Some (i,j) ->
		 if Int2Set.mem (i,j) already_done then (glueings,already_done)
		 else
		   try
		     let already_done,_ =
		       IntMap.fold
			 (fun i j (set,comap) ->
			  let opt = try Some (IntMap.find j comap)
				    with Not_found -> None in
			  match opt with
			  | None ->
			     (Int2Set.add (i,j) set, IntMap.add j i comap)
			  | Some i' ->
			     if i=i' then (Int2Set.add (i,j) set,comap)
			     else (Debug.tag_if_debug "Glueing is not injective, discarding" ;
				   raise False)
			 ) map (already_done,IntMap.empty)
		     in
		     (map::glueings,already_done)
		   with False -> (glueings,already_done)
	    end
	 | None -> (glueings,already_done)
       else (glueings,already_done)
      ) candidates (glueings,already_done)
  in
  (*end sub function unify*)
  begin
    let idmap = r.modif_sites in
    let glueings,_ =
      IdMap.fold
	(fun id set (glueings,already_done) ->
	 match id with
	 | FRESH i | KEPT i -> unify r.rhs mix (i,set) glueings already_done
	) idmap ([],Int2Set.empty)
    in
    glueings
  end

let to_kappa r env =
  try Environment.rule_of_num r.r_id env
  with Not_found ->
    Format.asprintf "%a->%a" (Kappa_printer.mixture false env) r.lhs
		   (Kappa_printer.mixture false env) r.rhs

let dump err_fmt r env =
  let () = Format.pp_open_vbox err_fmt 0 in
  let pr_r f =
    Format.fprintf f "%a->%a" (Kappa_printer.mixture false env) r.lhs
		   (Kappa_printer.mixture false env) r.rhs in
  let name = to_kappa r env in
  Format.fprintf err_fmt "****Rule '%s' [%t]****@," name pr_r;
  let () = Format.pp_open_hvbox err_fmt 0 in
  let () = IntMap.iter
	     (fun id ag ->
	      Mixture.fold_interface
		(fun site_id _ _ ->
		 let c = PortMap.find (KEPT id,site_id) r.pre_causal in
		 Format.fprintf err_fmt  "#%d.%d=%d@ " id site_id
				(Primitives.Causality.to_int c)
		) ag ()
	     ) (Mixture.agents r.lhs) in
  let () = Format.pp_close_box err_fmt () in
  let () = Format.pp_print_cut err_fmt () in
  let dump_script script =
    let id_of_port (id, s) =
      match id with
      | KEPT i -> (string_of_int i)^"."^(string_of_int s)
      | FRESH i ->
	 (string_of_int ((fun (deleted,kept,_) -> deleted +kept+ i) r.balance))
	 ^"."^(string_of_int s)
    in
    List.iter
      (fun action ->
       match action with
       | BND (p, p') ->
	  Format.fprintf err_fmt "BND (#%s,#%s)@," (id_of_port p) (id_of_port p')
       | FREE (p,b) ->
	  if b then Format.fprintf err_fmt "FREE #%s@," (id_of_port p)
	  else Format.fprintf err_fmt "FREE* #%s@," (id_of_port p)
       | MOD (p, i) ->
	  Format.fprintf err_fmt "SET #%s to state %d@," (id_of_port p) i
       | DEL i ->
	  Format.fprintf err_fmt "DEL #%d@," i
       | ADD (i, name) ->
	  let sign = Environment.get_sig name env in
	  Format.fprintf err_fmt "ADD %a%a with identifier #%d@,"
			 (Environment.print_agent env) name Signature.print sign
			 ((fun (deleted,kept,_) -> deleted +kept+ i) r.balance)
      )
      script
  in
  Format.fprintf err_fmt "Apply %s@," (to_kappa r env) ;
  dump_script r.script ;
  Format.fprintf err_fmt "if pattern %d is matched@," (Mixture.get_id r.lhs) ;
  Format.fprintf err_fmt "Modif sites: @[[%a]@]@,"
		 (Pp.set IdMap.bindings Pp.comma
			 (fun f (id,s) ->
			  Format.fprintf f "%i -> @[{%a}@]"
					 (match id with FRESH i | KEPT i -> i)
					 (Pp.set Int2Set.elements Pp.comma
						 (fun f (x,y) -> Format.fprintf f "(%i,%i)" x y))
					 s))
		 r.modif_sites;
  match r.cc_impact with
  | None -> Format.fprintf err_fmt "No CC impact@,"
  | Some (con,dis,se) ->
     Format.fprintf err_fmt
       "@[<v>%a%a%a@]@,"
       (Pp.set IntMap.bindings (fun f -> Format.pp_print_cut f ())
	       (fun f (i,s) -> Format.fprintf
				 f "@[CC[%d] and CCs {%a} in the left hand side will merge@]"
				 i (Pp.set IntSet.elements Pp.comma
					   Format.pp_print_int) s))
       con
       (Pp.set IntMap.bindings (fun f -> Format.pp_print_cut f ())
	       (fun f (i,s) -> Format.fprintf
				 f "@[CC[%d] and CCs {%a} in the rhs are freshly disconnected@]"
				 i (Pp.set IntSet.elements Pp.comma
					   Format.pp_print_int) s))
       dis
       (Pp.set IntMap.bindings (fun f -> Format.pp_print_cut f ())
	       (fun f (i,s) -> Format.fprintf
				 f "@[agent #%d might have side effect disconnection on sites {%a}@]"
				 i (Pp.set IntSet.elements Pp.comma
					   Format.pp_print_int) s))
       se;
     let () = Format.pp_close_box err_fmt () in
     Format.pp_print_newline err_fmt ()
