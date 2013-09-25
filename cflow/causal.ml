open Mods
open Dynamics
open Graph
open State
open LargeArray
open Printf

type atom_state = FREE | BND of int*int | INT of int | UNDEF
type event_kind = OBS of int | RULE of int | INIT of int | PERT of int
type atom = 
	{ 
	causal_impact : int ; (*(1) tested (2) modified, (3) tested + modified*) 
	eid:int ; (*event identifier*)
	kind:event_kind ;
(*	observation: string list*)
	}

type attribute = atom list (*vertical sequence of atoms*)
type grid = 
  {
    flow: (int*int*int,attribute) Hashtbl.t ;  (*(n_i,s_i,q_i) -> att_i with n_i: node_id, s_i: site_id, q_i: link (1) or internal state (0) *)
    pid_to_init: (int*int*int,int) Hashtbl.t ;
    obs: int list ; 
    weak_list: int list ; 
    init_tbl: (int,Mods.IntSet.t) Hashtbl.t;(*decreasing*)
    init_to_eidmax: (int,int) Hashtbl.t;
  } 
type config = 
  {
    events: atom IntMap.t ; 
    prec_1: IntSet.t IntMap.t ; 
    conflict : IntSet.t IntMap.t ; 
    top : IntSet.t}
type enriched_grid = 
    { 
      config:config;
      ids:(int * int * int) list ;
      depth:int;
      prec_star: int list array ; (*decreasing*)
      depth_of_event: int Mods.IntMap.t ;
      size:int;
    }

let empty_config = {events=IntMap.empty ; conflict = IntMap.empty ; prec_1 = IntMap.empty ; top = IntSet.empty}
let is i c = (i land c = i)

let empty_grid () = 
{
  flow = Hashtbl.create !Parameter.defaultExtArraySize ; 
  pid_to_init = Hashtbl.create !Parameter.defaultExtArraySize ; 
  obs = [] ; 
  weak_list = [];
  init_tbl = Hashtbl.create !Parameter.defaultExtArraySize ; 
  init_to_eidmax = Hashtbl.create !Parameter.defaultExtArraySize 
}



let build_subs l = 
  snd 
    (  List.fold_left 
         (fun (n,map) a -> 
           let n=n+1 in 
           (n,IntMap.add a n map))
         (0,IntMap.empty) 
         l)

let submap subs l m default = 
  List.fold_left 
    (fun m' a -> 
      let new_a = IntMap.find a subs in 
      IntMap.add new_a (
      try 
        IntMap.find a m
      with 
      | Not_found -> 
        begin 
          match default with None -> raise Not_found | Some a -> a
        end 
     ) m')
    IntMap.empty l 

let add_init_pid eid pid grid = 
  let eid_init = Hashtbl.find grid.pid_to_init pid in 
  let old = 
    try
      Hashtbl.find grid.init_tbl eid 
    with 
      Not_found -> Mods.IntSet.empty
  in 
  let _ = Hashtbl.replace grid.init_tbl eid (Mods.IntSet.add eid_init old) in 
  let _ = Hashtbl.replace grid.init_to_eidmax eid_init eid in 
  grid 

let subset subs l s = 
  List.fold_left 
    (fun s' a -> if IntSet.mem a s then IntSet.add (IntMap.find a subs) s' else s')
    IntSet.empty l 

let subconfig_with_subs subs config l = 
  {events = submap subs l config.events None ;
   prec_1 = submap subs l config.prec_1 (Some IntSet.empty);
   conflict = submap subs l config.conflict (Some IntSet.empty);
   top = subset subs l config.top}
let subenriched_grid_with_subs subs  grid l = 
  let depth_of_event = submap subs  l grid.depth_of_event (Some 0) in 
  let depth = IntMap.fold (fun _ -> max) depth_of_event 0 in 
  {grid with 
    config = subconfig_with_subs subs  grid.config l ;
    depth_of_event = depth_of_event ;
    depth = depth ;
      size = List.length l ;
  }

let subconfig config l = subconfig_with_subs (build_subs l) config l 
let subenriched_grid grid l = subenriched_grid_with_subs (build_subs l) grid l

let add_obs_eid eid grid = {grid with obs = eid::(grid.obs)}

let grid_find (node_id,site_id,quark) grid = Hashtbl.find grid.flow (node_id,site_id,quark)

let is_empty_grid grid = (Hashtbl.length grid.flow = 0)

let grid_add quark eid (attribute:attribute) grid = 
  let _ = 
    try 
      let _ = Hashtbl.find grid.flow quark in ()
    with 
    | _ -> Hashtbl.add grid.pid_to_init quark eid  
  in 
  Hashtbl.replace grid.flow quark attribute ;
  grid
		
let impact q c = 
	if q = 1 (*link*) 
	then 
		if (is _LINK_TESTED c) && (is _LINK_MODIF c) then 3 
		else 
			if (is _LINK_MODIF c) then 2 else 1
	else (*internal state*)
		if (is _INTERNAL_TESTED c) && (is _INTERNAL_MODIF c) then 3 
		else 
			if (is _INTERNAL_MODIF c) then 2 else 1

let last_event attribute = 
	match attribute with
		| [] -> None
		| a::_ -> (Some a.eid)

(*adds atom a to attribute att. Collapses last atom if if bears the same id as a --in the case of a non atomic action*)
let push (a:atom) (att:atom list) = 
	match att with
		| [] -> [a]
		| a'::att' -> 
			if a'.eid = a.eid then a::att' (*if rule has multiple effect on the same attribute, only the last one is recorded*) 
			else
				a::att
				 

let add (node_id,site_id) c grid event_number kind obs =
  (* make  this function more compact *)
	(*adding a link modification*)
	let grid = 
		if (is _LINK_TESTED c) || (is _LINK_MODIF c) then
			let att = try grid_find (node_id,site_id,1) grid with Not_found -> [] in
			let att = push {causal_impact = impact 1 c ; eid = event_number ; kind = kind (*; observation = obs*)} att
			in
                        let grid = 
			  grid_add (node_id,site_id,1) event_number att grid
                        in 
                        let grid = 
                          add_init_pid event_number (node_id,site_id,1) grid 
                        in 
                        grid
		else
			grid 
	in
	if (is _INTERNAL_TESTED c) || (is _INTERNAL_MODIF c) then
	    (*adding an internal state modification*)
	  let att = try grid_find (node_id,site_id,0) grid with Not_found -> [] in
	  let att = push {causal_impact = impact 0 c ; eid = event_number ; kind = kind (*; observation = obs*)} att
	  in
          let grid = 
	    grid_add (node_id,site_id,0) event_number att grid
          in 
           let grid = 
            add_init_pid event_number (node_id,site_id,0) grid 
          in 
          grid 
	else 
	  grid

          
(**side_effect Int2Set.t: pairs (agents,ports) that have been freed as a side effect --via a DEL or a FREE action*)
(*NB no internal state modif as side effect*)
let store_is_weak is_weak eid grid = 
  if is_weak 
  then 
    {grid 
     with weak_list = eid::(grid.weak_list)}
  else 
    grid 

let record ?decorate_with rule side_effects (embedding,fresh_map) is_weak event_number grid env = 
	
	let pre_causal = rule.Dynamics.pre_causal
	and r_id = rule.Dynamics.r_id
	and obs = match decorate_with with None -> [] | Some l -> (List.rev_map (fun (id,_) -> Environment.kappa_of_num id env) (List.rev l))
	in
  	let kind = RULE r_id in
	
	let im embedding fresh_map id =
		match id with
			| FRESH j -> IntMap.find j fresh_map
			| KEPT j -> IntMap.find j embedding
	in
        let grid = store_is_weak is_weak event_number grid in 
	let grid =  
		(*adding side-effect free modifications and tests*)
		let grid = 
			Id2Map.fold
			(fun (id,site_id) c grid ->
				let node_id = im embedding fresh_map id in
				add (node_id,site_id) c grid event_number kind obs 
			) pre_causal grid
		in
		(*adding side effects modifications*)
		Int2Set.fold 
		(fun (node_id,site_id) grid -> 
                  add (node_id,site_id) (_LINK_TESTED lor _LINK_MODIF) grid event_number kind obs) 
		side_effects grid
	in
	grid

let record_obs side_effects ((r_id,state,embedding,_),test) is_weak event_number grid env = 
  let grid = add_obs_eid event_number grid in 
  let grid = store_is_weak is_weak event_number grid in 
  let im embedding id =
    match id with
      | FRESH j -> raise (Invalid_argument "Causal.record_obs")
      | KEPT j -> IntMap.find j embedding
  in
  let causal = Dynamics.compute_causal_obs state env in 
  (*adding tests*)
  let grid = 
    Id2Map.fold 
      (fun (id,site_id) c grid ->
	let node_id = im embedding id in
	add (node_id,site_id) c grid event_number (OBS r_id) []
      ) 
      causal  
      grid
  in
  let grid =  
    (*adding side effects modifications*)
    Int2Set.fold 
      (fun (node_id,site_id) grid -> 
        add (node_id,site_id) (_LINK_TESTED lor _LINK_MODIF) grid event_number (OBS r_id) [])
      side_effects grid
  in
  grid

let record_init init is_weak event_number grid env = 
(*  if !Parameter.showIntroEvents  
  then *)
    (*adding tests*)
    let (((node_id,agent_name),interface),_) = init in  
    let causal = Dynamics.compute_causal_init init env in 
    let grid = store_is_weak is_weak event_number grid in 
    let grid = 
    Mods.Int2Map.fold 
      (fun (node_id,site_id) c grid -> 
	add 
          (node_id,site_id) 
          c (*(_INTERNAL_MODIF lor _INTERNAL_TESTED lor _LINK_TESTED lor _LINK_MODIF) (* HACK, TO DO CLEANER *)*) grid event_number (INIT agent_name) []
      ) causal grid 
    in
    grid
(*  else 
    grid *)

let add_pred eid atom config = 
	let events = IntMap.add atom.eid atom config.events
	in
	let pred_set = try IntMap.find eid config.prec_1 with Not_found -> IntSet.empty in
	let prec_1 = IntMap.add eid (IntSet.add atom.eid pred_set) config.prec_1 in
	{config with prec_1 = prec_1 ; events = events}

let add_conflict eid atom config =
	let events = IntMap.add atom.eid atom config.events in
	let cflct_set = try IntMap.find eid config.conflict with Not_found -> IntSet.empty in
	let cflct = IntMap.add eid (IntSet.add atom.eid cflct_set) config.conflict in
	{config with conflict = cflct ; events = events }

let rec parse_attribute last_modif last_tested attribute config = 
	match attribute with
		| [] -> config
		| atom::att -> 
			begin
				let events = IntMap.add atom.eid atom config.events
				and prec_1 = let preds = try IntMap.find atom.eid config.prec_1 with Not_found -> IntSet.empty in IntMap.add atom.eid preds config.prec_1
				in
				let config = {config with events =  events ; prec_1 = prec_1} in
				(*atom has a modification*)
				if (atom.causal_impact = 2) || (atom.causal_impact = 3) then 
					let config = 
						List.fold_left (fun config pred_id -> add_pred pred_id atom config) config last_tested 
					in
					let tested = if (atom.causal_impact = 1) || (atom.causal_impact = 3) then [atom.eid] else []
					in
					parse_attribute (Some atom.eid) tested att config 
				else 
					(* atom is a pure test*) 
					let config = 
						match last_modif with 
							| None -> config
							| Some eid -> add_conflict eid atom config (*adding conflict with last modification*) 
					in
					parse_attribute last_modif (atom.eid::last_tested) att config
			end

let cut attribute_ids grid =
	let rec build_config attribute_ids cfg =
		match attribute_ids with
			| [] -> cfg
			| (node_i,site_i,type_i)::tl ->
				let attribute = try grid_find (node_i,site_i,type_i) grid with Not_found -> invalid_arg "Causal.cut"
				in
				let cfg =
					match attribute with
						| [] -> cfg
						| atom::att -> 
							let events = IntMap.add atom.eid atom cfg.events 
						        and prec_1 = let preds = try IntMap.find atom.eid cfg.prec_1 with Not_found -> IntSet.empty in IntMap.add atom.eid preds cfg.prec_1
	                                                and top = IntSet.add atom.eid cfg.top
							in 
							let tested = if (atom.causal_impact = 1) || (atom.causal_impact = 3) then [atom.eid] else []
							and modif =  if (atom.causal_impact = 2) || (atom.causal_impact = 3) then Some atom.eid else None 
							in
							parse_attribute modif tested att {cfg with prec_1 = prec_1 ; events = events ; top = top} 
				in
				build_config tl cfg
	in
	build_config attribute_ids empty_config

let string_of_atom atom = 
	let imp_str = match atom.causal_impact with 1 -> "o" | 2 -> "x" | 3 -> "%" | _ -> invalid_arg "Causal.string_of_atom" in
	Printf.sprintf "%s_%d" imp_str atom.eid
		
let dump grid fic state env = 
	let d = open_out ((Filename.chop_extension fic)^".txt") in
	Hashtbl.fold 
	(fun (n_id,s_id,q) att _ ->
		let q_name = "#"^(string_of_int n_id)^"."^(string_of_int s_id)^(if q=0 then "~" else "!") in
		let att_ls =
			List.fold_left
			(fun ls atom -> LongString.concat (string_of_atom atom) ls) 
			LongString.empty (List.rev att)
		in
		Printf.fprintf d "%s:" q_name ; 
		LongString.printf d att_ls ;
		Printf.fprintf d "\n"
	) grid.flow () ;
	close_out d
	
let label env state e = 
  match e with
    | OBS mix_id -> Environment.kappa_of_num mix_id env
    | PERT p_id -> Environment.pert_of_num p_id env
    | RULE r_id -> Dynamics.to_kappa (State.rule_of_id r_id state) env
    | INIT agent -> "Intro "^(Environment.name agent env)

let ids_of_grid grid = Hashtbl.fold (fun key _ l -> key::l) grid.flow []
let config_of_grid = cut 


(*let prec_star_of_config_old  config = 
  let rec prec_closure config todo already_done closure =
    if IntSet.is_empty todo then closure
    else
      let eid = IntSet.choose todo in
      let todo' = IntSet.remove eid todo in
      if IntSet.mem eid already_done 
      then 
        prec_closure config todo' already_done closure 
      else 
        let prec = try IntMap.find eid config.prec_1 with Not_found -> IntSet.empty
        in
      prec_closure config (IntSet.union todo' prec) (IntSet.add eid already_done) (IntSet.union prec closure)
  in
  IntMap.fold 
    (fun eid kind prec_star -> 
      let set = prec_closure config (IntSet.singleton eid) IntSet.empty IntSet.empty
      in
      IntMap.add eid set prec_star
    ) config.events IntMap.empty *)

let prec_star_of_config config_closure config to_keep init_to_eidmax weak_events init = 
  let a = Graph_closure.closure config_closure config.prec_1 to_keep  init_to_eidmax weak_events init in 
  Graph_closure.A.map fst a 

let depth_and_size_of_event config =
  IntMap.fold 
    (fun eid prec_eids (emap,size,depth) ->
      let d = 
	IntSet.fold 
	  (fun eid' d ->
	    let d' = try IntMap.find eid' emap with Not_found -> 0
	    in
	    max (d'+1) d
	  ) prec_eids 0
      in
      IntMap.add eid d emap,size+1,d 
    ) config.prec_1 (IntMap.empty,0,0)


let enrich_grid config_closure grid = 
  let keep_l = List.fold_left (fun a b -> IntSet.add b a) IntSet.empty grid.obs in 
  let to_keep = 
    (fun i -> IntSet.mem i keep_l)
  in 
  let ids = ids_of_grid grid  in 
  let config = config_of_grid ids grid in 
  let max_key = List.fold_left max 0 grid.weak_list  in 
  let tbl = Graph_closure.A.create (max_key+1) false in 
  let _ = 
    List.iter 
      (fun i -> Graph_closure.A.set tbl i true)
      grid.weak_list 
  in 
  let weak_fun i = 
    try 
      Graph_closure.A.get tbl i 
    with _ -> false 
  in 
  let init_fun i = 
    try 
      List.rev (Mods.IntSet.elements (Mods.IntSet.remove i (Hashtbl.find grid.init_tbl i)))
    with 
      _ -> []
  in 
  let init_to_eid_max i = 
    try 
      Hashtbl.find grid.init_to_eidmax i
    with 
      Not_found -> 0 
  in 
  let prec_star= prec_star_of_config config_closure config to_keep init_to_eid_max weak_fun init_fun in
  let depth_of_event,size,depth = depth_and_size_of_event config in  
  { 
    config = config ; 
    ids = ids ;
    size = size ; 
    prec_star = prec_star ; 
    depth = depth ; 
    depth_of_event = depth_of_event 
  }

let dot_of_grid profiling fic enriched_grid state env = 
	(*dump grid fic state env ; *)
	let t = Sys.time () in 
        let config = enriched_grid.config in 
        let prec_star = enriched_grid.prec_star in 
        let depth_of_event = enriched_grid.depth_of_event in 
	let label = label env state in 
	let sorted_events = 
		IntMap.fold 
		(fun eid d dmap ->
			let set = try IntMap.find d dmap with Not_found -> IntSet.empty
			in
			IntMap.add d (IntSet.add eid set) dmap
		) depth_of_event IntMap.empty
	in
	let desc = open_out fic
	in
	let _ = Parameter.add_out_desc desc in 
        let _ = profiling desc in
	fprintf desc "digraph G{\n ranksep=.5 ; \n" ;
	IntMap.iter
	(fun d eids_at_d ->
		fprintf desc "{ rank = same ; \"%d\" [shape=plaintext] ; " d ;
		IntSet.iter 
		(fun eid ->
			let atom = IntMap.find eid config.events in
			if eid = 0 then () else
			match atom.kind  with 
      	| RULE _  -> fprintf desc "node_%d [label=\"%s\", shape=invhouse, style=filled, fillcolor = lightblue] ;\n" eid (label atom.kind) 
        | OBS _  ->  fprintf desc "node_%d [label =\"%s\", style=filled, fillcolor=red] ;\n" eid (label atom.kind) 
        | INIT _  ->
	    if !Parameter.showIntroEvents then fprintf desc "node_%d [label =\"%s\", shape=house, style=filled, fillcolor=green] ;\n" eid (label atom.kind)
	| _ -> invalid_arg "Event type not handled"
	(*		List.iter (fun obs -> fprintf desc "obs_%d [label =\"%s\", style=filled, fillcolor=red] ;\n node_%d -> obs_%d [arrowhead=vee];\n" eid obs eid eid) atom.observation ;*) 
		) eids_at_d ;
		fprintf desc "}\n" ;
	) sorted_events ;
	let cpt = ref 0 in
	while !cpt < (IntMap.size sorted_events) do
		if !cpt+1 < IntMap.size sorted_events then (fprintf desc "\"%d\" -> \"%d\" [style=\"invis\"]; \n" !cpt (!cpt+1)) ;
		cpt := !cpt + 1
	done ; 
	IntMap.iter
	(fun eid pred_set ->
		if eid = 0 then () 
		else 
			IntSet.iter
			(fun eid' ->
				if eid' = 0 then () 
				else
					if !Parameter.showIntroEvents then
						fprintf desc "node_%d -> node_%d\n" eid' eid
					else
						let atom = IntMap.find eid' config.events in
						match atom.kind with
							| INIT _ -> ()
							| _ -> fprintf desc "node_%d -> node_%d\n" eid' eid
			) pred_set
	) config.prec_1 ;
	IntMap.iter
	(fun eid cflct_set ->
		if eid = 0 then () 
		else
		  let prec = try prec_star.(eid) with _ -> [] in 
                  let _ = 
                    IntSet.fold_inv 
                      (fun eid' prec -> 
                        let bool,prec = 
                          let rec aux prec = 
                            match 
                              prec
                            with 
                            | []   -> false,prec
                            | h::t -> 
                              begin
                                if h=eid'
                                then true,t
                                else if h>eid'
                                then aux t
                                else false,prec 
                              end 
                          in aux prec 
                        in 
                        let _ = 
                          if bool then ()
                        else 
			    fprintf desc "node_%d -> node_%d [style=dotted, arrowhead = tee] \n" eid eid'
                        in 
                        prec
		       ) cflct_set prec 
                  in () 
	) config.conflict ;
	fprintf desc "}\n" ;
        fprintf desc "/*\n Dot generation time: %f\n*/" (Sys.time () -. t) ; 
        close_out desc 

(*story_list:[(key_i,list_i)] et list_i:[(grid,_,sim_info option)...] et sim_info:{with story_id:int story_time: float ; story_event: int}*)
let pretty_print config_closure compression_type label story_list state env =	
  let n = List.length story_list in 
  let _ = 
    if compression_type = "" 
    then
      Debug.tag (Printf.sprintf "\n+ Pretty printing %d flow%s" n (if n>1 then "s" else ""))
    else 
      Debug.tag (Printf.sprintf "\n+ Pretty printing %d %scompressed flow%s" n label (if n>1 then "s" else ""))
  in
  let story_list = 
    List.map (fun (x,y) -> enrich_grid config_closure x,y) story_list 
  in 
  let _ =
    List.fold_left 
      (fun cpt (enriched_config,stories) -> 
	let av_t,ids,n =
	  List.fold_left 
	    (fun (av_t,ids,n) info_opt -> 
	      match info_opt with
		| Some info -> 
                  (av_t +. info.story_time,info.story_id::ids,n+1)
		| None -> invalid_arg "Causal.pretty_print"
	    )(0.,[],0) (List.rev stories) 
	in
	let profiling = 
	  (fun desc -> 
	    Printf.fprintf desc "/* Compression of %d causal flows obtained in average at %E t.u */\n" n (av_t/.(float_of_int n)) ;
	    Printf.fprintf desc "/* Compressed causal flows were: %s */\n" (Tools.string_of_list string_of_int ids) ;
	  )
	in
	let fic = (Filename.chop_extension (!(Parameter.cflowFileName)))^compression_type^"_"^(string_of_int cpt)^".dot" in
	dot_of_grid profiling fic enriched_config state env ;
	cpt+1
      ) 0 story_list
  in
  let fic = (Filename.chop_extension (!(Parameter.cflowFileName)))^compression_type^"Summary.dat" in 
  let desc = open_out fic in 
  let _ = fprintf desc "#id\tE\tT\t\tdepth\tsize\t\n" in 
  let _ = 
    List.fold_left 
      (fun cpt (enriched_config,story) -> 
        let depth = enriched_config.depth in 
        let size = enriched_config.size in 
        let _ = 
          List.iter 
            (fun story -> 
              match story 
              with 
                | None -> invalid_arg "Causal.pretty_print"
                | Some info -> 
                  let time = info.story_time in 
                  let event = info.story_event in 
                  fprintf desc "%i\t%i\t%E\t%i\t%i\t\n" cpt event time depth size
            )
            story 
        in 
        cpt+1)
      0 story_list 
  in 
  let _ = close_out desc in 
  ()
	  
let print_stat parameter handler enriched_grid = 
  let size = Array.length enriched_grid.prec_star  in 
  let rec aux k n_step longest_story n_nonempty length_sum length_square_sum = 
    if k>=size 
    then (n_step,longest_story,n_nonempty,length_sum,length_square_sum)
    else 
      let cc = List.length (Array.get enriched_grid.prec_star k) in
      aux 
        (k+1) 
        (n_step+1)
        (max longest_story cc)
        (if cc>0 then n_nonempty+1 else n_nonempty)
        (length_sum+cc)
        (length_square_sum+cc*cc)
  in 
  let n_step,longest_story,n_nonempty,length_sum,length_square_sum = aux 0 0 0 0 0 0 in 
  let _ = Debug.tag "" in 
  let _ = Debug.tag "Stats:" in 
  let _ = Debug.tag (" number of step   : "^(string_of_int n_step)) in 
(*  let _ = Debug.tag (" number of stories: "^(string_of_int n_nonempty)) in *)
  let _ = Debug.tag (" longest story    : "^(string_of_int longest_story)) in 
  let _ = Debug.tag (" average length   : "^(string_of_float ((float_of_int length_sum)/.(float_of_int n_nonempty)))) in 
  let _ = Debug.tag (" geometric mean   : "^(string_of_float (sqrt ((float_of_int length_square_sum)/.(float_of_int n_nonempty))))) in 
  let _ = Debug.tag "" in 
  ()
