(**Module that manages connectedness of injection images --for dot and plus operators*)
open Mods
open Graph
open State
open Tools
open Dynamics
open ExceptionDefn

(**updating non local mixtures after application of a linking rule using embedding [embedding_info]*)
let update_intra_in_components r embedding_info state counter env =
	if !Parameter.debugModeOn then Debug.tag "Looking for side effect update of non local rules..." ;
	let components = 
		match embedding_info.components with
			| Some map -> map
			| None -> invalid_arg "NonLocal.update_intra_in_components: component not computed"
	in
	
	(*case one tries to gather intra redexes in each components -- nb this may lead to false intras but that will be rejected upon rule application*)
	let tmp_extend part_inj inj =
		let _,cc_id = Injection.get_coordinate inj in
		if IntMap.mem cc_id part_inj then failwith "Invariant violation"
		else
			IntMap.add cc_id inj part_inj
	in
	
	let search_elements graph component extensions env =
		let ext,modified = 
			IntSet.fold
			(fun u_id (extensions,modified) ->
				if !Parameter.debugModeOn then (Printf.printf "looking for a piece of intra on lifts of node %d\n" u_id) ;
				let u = try SiteGraph.node_of_id graph u_id with Not_found -> invalid_arg "NonLocal.search_elements"
				in
				let _,lifts = Node.get_lifts u 0 in (*BUG here site 0 is not always "_"*)
				(if !Parameter.safeModeOn then let str = Environment.site_of_id (Node.name u) 0 env in if str <> "_" then failwith "Invariant violation in NonLocal.search_element") ;
				LiftSet.fold
				(fun inj (extensions,modified) ->
					if Injection.is_trashed inj then (extensions,modified) (*injection should not be already invalid...*)
					else
						let (mix_id,cc_id) = Injection.get_coordinate inj in
						
						if not (Environment.is_nl_rule mix_id env) then 
							begin
								if !Parameter.debugModeOn then Printf.printf "a lift points to rule %d but it is a local one\n" mix_id ;
								(extensions,modified)
							end
						else
							let inj_map = try IntMap.find mix_id extensions with Not_found -> IntMap.empty in
							let inj_list = try IntMap.find cc_id inj_map with Not_found -> [] in
							let inj_map' = IntMap.add cc_id (inj::inj_list) inj_map in
							(IntMap.add mix_id inj_map' extensions,true)
				) lifts (extensions,modified)
			) component (extensions,false)
		in
		if modified then (Some ext)
		else None
	in
	
	(*reusing components that were computed to check that the rule was indeed binary*)
	let con_map = match r.Dynamics.cc_impact with None -> invalid_arg "State.nl_pos_upd: cc_impact is not initialized"  | Some (map,_,_) -> map in
	
	let extensions,found = (*r_id -> cc_id -> part_inj_list*)
		IntMap.fold 
		(fun eq cc_set (extensions,found) ->
			if !Parameter.debugModeOn then
				Debug.tag (Printf.sprintf "CCs %s are merged by the rule and CC[%d] is the representative" (string_of_set string_of_int IntSet.fold cc_set) eq) ;
			IntSet.fold 
			(fun cc_i (extensions,found) ->
				let root = 
					match Mixture.root_of_cc r.lhs cc_i with Some r -> IntMap.find r embedding_info.map | None -> invalid_arg "State.nl_positive_update" 
				in
				let _ = 
					if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Exploring into image of CC[%d] computed during rule %d application" cc_i r.r_id)
	 			in
				let component_i = 
					try IntMap.find root components with 
						| Not_found -> 
							(Debug.tag 
							(Printf.sprintf "root %d (= phi(%d)) not found in %s" root (match Mixture.root_of_cc r.lhs cc_i with Some r -> r | None -> -1)
								(Tools.string_of_map string_of_int (Tools.string_of_set string_of_int IntSet.fold) 
								IntMap.fold 
								components
								)
							); invalid_arg "nl_pos_upd")
				in
				if !Parameter.debugModeOn then Debug.tag (Tools.string_of_set string_of_int IntSet.fold component_i) ;
				let opt = search_elements state.graph component_i extensions env
				in
				match opt with
					| Some ext -> (ext,found+1) 
					| None -> (extensions,found) (*no partial intra was found in cc_i *)
			) cc_set (extensions,found) 
		) con_map (IntMap.empty,0)
	in
	if found < 2 then 
		(if !Parameter.debugModeOn then Debug.tag "Potential new intras are not shared between merged cc and cannot be new, skipping"; state)
	else
		IntMap.fold 
		(fun r_id cc_map state ->
			if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Trying to find intra(s) for rule [%d]" r_id) ;
			
			let lhs = kappa_of_id r_id state in
			try
				let intras_for_r_id,cc = 
					IntMap.fold
					(fun cc_id injs_list (new_intras,cpt) ->
						if cpt <> cc_id then raise (Break cc_id)
						else
							let new_intras = 
								List.fold_left
								(fun cont part_prod_inj ->
									let l = 
										List.fold_left 
										(fun cont inj ->
											(tmp_extend part_prod_inj inj)::cont
										) cont injs_list
									in
									l (*@cont*)
								) [] new_intras
							in
							(new_intras,cpt+1)
					) cc_map ([IntMap.empty],0)
				in
				
				if cc <> Mixture.arity lhs then 
					(if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "One CC of rule [%d] has no candidate for intra, aborting" r_id); state)
				else
					List.fold_left  (*nl_injections : (InjProdHeap.t option) array*)
					(fun state injprod_map -> 
						let injprod_hp = match state.nl_injections.(r_id) with
							| Some hp -> hp
							| None -> InjProdHeap.create !Parameter.defaultHeapSize 
						in
						let ip = match InjProdHeap.next_alloc injprod_hp with None -> InjProduct.create (Mixture.arity lhs) r_id | Some ip -> ip
						in
						let ip = IntMap.fold (fun cc_id inj injprod -> InjProduct.add cc_id inj injprod ; injprod) injprod_map ip
						in
						try
							let injprod_hp = InjProdHeap.alloc ~check:true ip injprod_hp in
							state.nl_injections.(r_id) <- Some injprod_hp ;
							update_activity state r.r_id r_id counter env ;
							state
						with
							| InjProdHeap.Is_present -> state
					) state intras_for_r_id
		with 
		| Break cc_id ->
			 (if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "CC[%d] of rule [%d] has no candidate for intra, aborting" cc_id r_id); state)
		) extensions state
		
(**[update_rooted_intras injection_list state counter env] tries to form new intras using injections in [injection_list] as one component*)
let rec update_rooted_intras new_injs state counter env = 
	match new_injs with 
		| [] -> state
		| injection::tl ->
			let (mix_id,cc_id) = Injection.get_coordinate injection in (*mix_id is the id of a non local lhs by invariant*)
			
			(*one should look into cc(root_injection) whether some nodes have a lift to (mix_id,cc_id') with cc_id <> cc_id'*)
			let (a_0,u_0) = match Injection.root_image injection with None -> invalid_arg "NonLocal.complete_injections" | Some p -> p
			in
			(*if activity of mix_id is null then there can be no intra*)
			if not (is_complete mix_id state) then update_rooted_intras tl state counter env
			else
				
				(***NOT EFFICIENT BECAUSE LIFTSET WILL BE TRAVERSED ONCE MORE, neighborhood FUNCTION SHOULD DO THIS***)
				let predicate = 
					fun node -> 
						let _,liftset = Node.get_lifts node 0 in
						LiftSet.exists (fun inj -> let (mix_id',cc_id') = Injection.get_coordinate inj in (mix_id' = mix_id) && (cc_id <> cc_id') ) liftset
				in   
				let (_,d_map,components,_) = SiteGraph.neighborhood ~filter_elements:predicate state.graph u_0 (-1) in
				
				(*components contains nodes whose name is the root of at least one complemetary injection*)
				let candidate_map = 
					IntSet.fold 
					(fun u_i map -> 
						let node = SiteGraph.node_of_id state.graph u_i in
						let _,lifts = Node.get_lifts node 0 in
						LiftSet.fold 
						(fun inj map -> 
							let mix_id',cc_id' = Injection.get_coordinate inj in
							if (mix_id' = mix_id) && (cc_id' <> cc_id) then
								let l_part_inj = try IntMap.find cc_id' map with Not_found -> [] in
								IntMap.add cc_id' (inj::l_part_inj) map
							else map
						) lifts map
					) components IntMap.empty
				in 
				if !Parameter.debugModeOn then
					begin
						Printf.printf "Trying to extend (%d,%d) : %s with:\n" mix_id cc_id (Injection.to_string injection) ;
						IntMap.iter 
						(fun cc_id' inj_list -> 
							Printf.printf "(%d,%d):%s\n" mix_id cc_id' 
							(Tools.string_of_list Injection.string_of_coord inj_list)
						) candidate_map 
					end ;
				if IntMap.size candidate_map < (Mixture.arity (kappa_of_id mix_id state)) - 1 then 
					update_rooted_intras tl state counter env
				else
					let new_intras = 
						IntMap.fold (*folding on candidate map*)
						(fun cc_id ext_injs new_intras ->
							
							List.fold_left 
							(fun cont inj_map ->
								let ext_cont = 
									List.fold_left 
									(fun cont inj_cc ->
										(IntMap.add cc_id inj_cc inj_map)::cont
									) (*[]*) cont ext_injs
								in
								ext_cont (*@ cont*)
							) [] new_intras
							
						) candidate_map [IntMap.add cc_id injection IntMap.empty]
					in
										
					if !Parameter.debugModeOn then
						List.iter (fun injmap -> Debug.tag ("new_intras: "^(string_of_map string_of_int Injection.string_of_coord IntMap.fold injmap))) new_intras ;
					
					let injprod_hp = match state.nl_injections.(mix_id) with None -> InjProdHeap.create !Parameter.defaultHeapSize | Some hp -> hp in
					let mix = kappa_of_id mix_id state in
					let injprod_hp = 
						List.fold_left
						(fun injprod_hp intra_map ->
							if IntMap.size intra_map < (Mixture.arity mix) then injprod_hp (*intra is not complete*)
							else
								let injprod = match InjProdHeap.next_alloc injprod_hp with None -> InjProduct.create (Mixture.arity mix) mix_id | Some ip -> ip 
								in
								let injprod,new_roots = 
									IntMap.fold 
									(fun cc_id inj (injprod,new_roots) ->
										let root = (function Some (_,j) -> j | None -> invalid_arg "") (Injection.root_image inj) in
										InjProduct.add cc_id inj injprod ;
										(injprod, IntMap.add cc_id root new_roots)
									) intra_map (injprod,IntMap.empty)
								in
								let opt = 
									try 
										Some (
											InjProduct.fold_left 
											(fun cod inj -> 
												let _,cod' = Injection.codomain inj (IntMap.empty,cod) in cod'
											) IntSet.empty injprod
											) 
									with Injection.Clashing -> None
								in
								if opt = None then injprod_hp 
								else
								try
								let injprod_hp = InjProdHeap.alloc ~check:true injprod injprod_hp in
									injprod_hp
								with InjProdHeap.Is_present -> 
									if !Parameter.debugModeOn then Debug.tag "Intra already added, skipping" ;
									injprod_hp 
						) injprod_hp new_intras
					in
					state.nl_injections.(mix_id) <- Some injprod_hp ;
					update_activity state (-1) mix_id counter env ;
					update_rooted_intras tl state counter env

let initialize_embeddings state counter env = 
	SiteGraph.fold 
	(fun id u state ->
		let name = Node.name u in
		if Environment.is_nl_root name env then (*node is potentially the root of an intra rule*) 
		let _,lifts = Node.get_lifts u 0 in
		LiftSet.fold 
		(fun inj state -> 
			let (mix_id,_) = Injection.get_coordinate inj in
			if Environment.is_nl_rule mix_id env then
				update_rooted_intras [inj] state counter env
			else
				state
		) lifts state 
		else 
			state
	) state.graph state
	


let positive_update r embedding_t new_injs state counter env = 
	(*Step 1 : For each new embedding phi, adding all new intras of the form <phi,psi> *)
	let state = update_rooted_intras new_injs state counter env in
	
	(*Step 2 : Non local positive update *)
	(*If rule is potentially breaking up some connected component this should wake up silenced rules*)
	begin
		match r.Dynamics.cc_impact with 
			| None -> (if !Parameter.debugModeOn then Debug.tag "Rule cannot decrease connectedness no need to update silenced rules") 
			| Some _ -> (*should be more precise here*)
				if IntSet.is_empty state.silenced then (if !Parameter.debugModeOn then Debug.tag "No silenced rule, skipping")
				else
	 				IntSet.fold
					(fun id _ ->
					if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "Updating silenced rule %d" id) ; 
					update_activity state r.Dynamics.r_id id counter env ;
					state.silenced <- IntSet.remove id state.silenced ;
					) state.silenced () 
	end ;
		
	(*If rule is potentially merging two connected components this should trigger a positive update of non local rules*)
	begin
		match r.Dynamics.cc_impact with
			| None -> 
				(if !Parameter.debugModeOn then 
					Debug.tag "No possible side effect update of unary rules because applied rule cannot increase connectedness" ;
				state
				)
			| Some (con_map,_,_) ->
				if IntMap.is_empty con_map then state
				else
					begin
						match embedding_t with
							| CONNEX _ -> 
								(if !Parameter.debugModeOn then 
									Debug.tag "No possible side effect update of unary rules because a unary instance was applied"; 
								state
								)
							| DISJOINT e | AMBIGUOUS e -> (*one may need to compute connected components if they are not present in e, as in the AMBIGUOUS case*)
								update_intra_in_components r e state counter env 
					end
	end 
	
	