open Graph
open Mods
open Tools
open Mixture
open ExceptionDefn

let compatible_info (int_state,lnk_state) ag i = 
		let ok = 
			match int_state with
				| None -> true
				| state -> (state = Node.internal_state (ag,i))
		in
			if not ok then false
			else 
				match lnk_state with
					| Node.WLD -> true
					| Node.BND -> Node.is_bound (ag,i)
					| Node.FREE -> not (Node.is_bound (ag,i))
					| Node.TYPE (site_id,nme) -> Node.is_bound ~with_type:(site_id,nme) (ag,i)


(*SHOULD BE OPTIMAL, WILL BE USED IN EVENT LOOP*)
(*returns (Some component_injection) if mix anchored at id_agent has an injection in sg at root id_node*)
(*NOTE: can be used to store (id_agent,id_node) as a representant of the whole component injection, in which case checking additional edges is not necessary*)
let component ?(check_additional_edges=true) ?(already_done=Int2Set.empty) embedding id_agent (sg,id_node) mix = 
	let rec reco queue part_embedding port_map =
		match queue with
			| [] -> (part_embedding,port_map)
			| (a_i,n_j)::tl ->
				if IntMap.mem n_j port_map then raise False (*Partial embedding would not be injective on nodes*)
				else
				if Int2Set.mem (a_i,n_j) already_done then raise False (*to avoid adding twice the same embedding*)
				else
					let node_j = try SiteGraph.node_of_id sg n_j with Not_found -> invalid_arg "Matching.component: not a valid node address" 
					and ag_i = try Mixture.agent_of_id a_i mix with Not_found -> invalid_arg "Matching.component: not a valid agent identifier"
					in
						if not (Node.name node_j = Mixture.name ag_i) then raise False
						else 
							let queue',port_list_j = 
								Mixture.fold_interface 
								(fun site_id (int_opt,lnk_opt) (queue,port_list_j) ->
									let port_list_j = Node.test (node_j,site_id) (int_opt,lnk_opt) port_list_j (*may raise False if test fails*)
									in 
										(*following the spanning tree rooted at id_agent*)
										let next_to_match = Mixture.follow_in_spanning_tree id_agent (a_i,site_id) mix 
										in
											match next_to_match with
												| None -> (queue,port_list_j)
												| Some (a_i',s) ->
													let opt = Node.follow (node_j,site_id) in
														match opt with
															| Some (node_j',s') -> (*no need to add s' in the port_list see remark 29/06*)
																	let n_j' = Node.get_address node_j' in 
																		if (s'=s) then ((a_i',n_j')::queue,port_list_j)
																		else raise False
															| None -> raise False 
								)
								ag_i (tl,[])
							in
								reco queue' (Injection.add a_i n_j part_embedding) (IntMap.add n_j port_list_j port_map)
	in
		try
			let partial_embedding,port_map = reco [(id_agent,id_node)] embedding IntMap.empty 
			in
				let port_map = (*checking edges not in the spanning tree only if unsure of the embedding*)
					if not check_additional_edges then port_map
					else
							Int2Map.fold 
							(fun (a_i,s_i) (b_j,s_j) port_map ->
								let opt = try Some (Injection.find a_i partial_embedding) with Not_found -> None in
									match opt with
										| None -> invalid_arg "Matching.component: invariant violation"
										| Some n_i -> 
											let node_i = try SiteGraph.node_of_id sg n_i with Not_found -> invalid_arg "Matching.component: not a valid node address"
											in 
												let (int_opt,lnk_opt) = IntMap.find s_i (Mixture.interface (Mixture.agent_of_id a_i mix))
												and port_list_i = try IntMap.find n_i port_map with Not_found -> invalid_arg "Matching.component" 
												in
													let port_list_i = Node.test (node_i,s_i) (int_opt,lnk_opt) port_list_i (*may raise False*)
													in
														let port_map = IntMap.add n_i port_list_i port_map in
															let opt = Node.follow (node_i,s_i) in
															match opt with
																| None -> raise False
																| Some (node_x,s_x) ->
																	begin
																		try
																			let n_j = Injection.find b_j partial_embedding in
																				let node_j = try SiteGraph.node_of_id sg n_j with Not_found -> invalid_arg "Matching.component: not a valid node address"
																				in 
																					let (int_opt,lnk_opt) = IntMap.find s_j (Mixture.interface (Mixture.agent_of_id b_j mix))
																					in
																						let _ = (*no need to modify port_map*) 
																							Node.test (node_j,s_j) (int_opt,lnk_opt) [] (*may raise False*)
																						in
																							let n_x = try Node.get_address node_x with Not_found -> invalid_arg "Matching.component: not allocated" in
																							if (n_j = n_x) && (s_j = s_x) then port_map (*ok*)
																							else raise False
																		with Not_found -> raise False
																	end
							) (Mixture.internal_edges id_agent mix) port_map  
							(*parital_embedding is total if no exception was raised in the iter loop above*)
				in
					Some (partial_embedding,port_map)
		with False -> None

(*Given id_agent -> id_node as partial embedding, returns a complete embedding of mix in sg*)
let complete_embedding (id_agent,mix) (id_node,sg) = 
	let embedding = Injection.empty (IntMap.size (Mixture.agents mix)) (Mixture.get_id mix,0) in
	component ~check_additional_edges:false embedding id_agent (sg,id_node) mix
				