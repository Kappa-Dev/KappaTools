open Mods

type event_kind =
  | OBS of string
  | RULE of int
  | INIT of int list (* the agents *)
  | PERT of string (* the rule *)

type quark_lists = {
  site_tested : (int * int) list;
  site_modified : (int * int) list;
  internal_state_tested : (int * int) list;
  internal_state_modified : (int * int) list;
}

let atom_tested = 1
let atom_modified = 2
let atom_testedmodified = 3
type atom =
    {
      causal_impact : int ; (*(1) tested (2) modified, (3) tested + modified*)
      eid:int ; (*event identifier*)
      kind:event_kind ;
(*      observation: string list*)
    }

type attribute = atom list (*vertical sequence of atoms*)
type grid =
    {
      flow: (int*int*int,attribute) Hashtbl.t ;
      (*(n_i,s_i,q_i) -> att_i with n_i: node_id, s_i: site_id, q_i:
      link (1) or internal state (0) *)
      pid_to_init: (int*int*int,int) Hashtbl.t ;
      obs: int list ;
      init_tbl: (int,Mods.IntSet.t) Hashtbl.t;(*decreasing*)
      init_to_eidmax: (int,int) Hashtbl.t;
    }
type config =
  {
    events_kind: event_kind IntMap.t ;
    prec_1: IntSet.t IntMap.t ;
    conflict : IntSet.t IntMap.t ;
  }
type enriched_grid =
    {
      config:config;
      depth:int;
      prec_star: (int list array * Graph_closure.order); (*decreasing*)
      depth_of_event: int Mods.IntMap.t ;
      size:int;
    }

let empty_config =
  { events_kind=IntMap.empty; conflict = IntMap.empty; prec_1 = IntMap.empty }

let debug_print_event_kind f = function
  | OBS i -> Format.fprintf f "OBS(%s)" i
  | RULE i -> Format.fprintf f "RULE(%i)" i
  | INIT l ->
     Format.fprintf f "INIT(%a)" (Pp.list Pp.comma Format.pp_print_int) l
  | PERT s -> Format.fprintf f "PERT(%s)" s

let debug_print_causal f i =
  Format.pp_print_string
    f (if i = atom_tested then "tested"
       else if i = atom_modified then "modified"
       else if i = atom_tested lor atom_modified then "tested&modified"
       else "CAUSAL IMPACT UNDEFINED")

let debug_print_atom f a =
  Format.fprintf f "{#%i: %a %a}" a.eid debug_print_causal a.causal_impact
		 debug_print_event_kind a.kind

let debug_print_grid f g =
  let () =
    Format.fprintf f "@[<v>Flow:@," in
  let () = Hashtbl.iter
	     (fun (node_id,site_id,q) l ->
	      Format.fprintf
		f "@[<2>%i.%i%s:@,%a@]@," node_id site_id
		(if q = 0 then "~" else if q = 1 then "" else "UNDEFINED")
		(Pp.list Pp.space debug_print_atom) l)
	     g.flow in
  Format.fprintf f "@]@."

let empty_grid () =
  {
    flow = Hashtbl.create !Parameter.defaultExtArraySize ;
    pid_to_init = Hashtbl.create !Parameter.defaultExtArraySize ;
    obs = [] ;
    init_tbl = Hashtbl.create !Parameter.defaultExtArraySize ;
    init_to_eidmax = Hashtbl.create !Parameter.defaultExtArraySize
  }

let build_subs l =
  snd (List.fold_left
	 (fun (n,map) a -> (succ n,IntMap.add a n map)) (1,IntMap.empty) l)

let submap subs l m default =
  List.fold_left
    (fun m' a ->
      match IntMap.find_option a subs with
      | None -> raise Not_found
      | Some new_a -> 
	IntMap.add new_a (
	  match IntMap.find_option a m with
	  | Some x -> x
	  | None ->
	    match default with None -> raise Not_found | Some a -> a
	) m')
    IntMap.empty l

let add_init_pid eid pid grid =
  let eid_init = Hashtbl.find grid.pid_to_init pid in
  let old =
    try Hashtbl.find grid.init_tbl eid
    with Not_found -> Mods.IntSet.empty
  in
  let () = Hashtbl.replace grid.init_tbl eid (Mods.IntSet.add eid_init old) in
  let () = Hashtbl.replace grid.init_to_eidmax eid_init eid in
  grid

let subset subs l s =
  List.fold_left
    (fun s' a ->
     if IntSet.mem a s
     then IntSet.add
	    (match IntMap.find_option a subs with
	     | Some i -> i
	     | None -> raise Not_found)
	    s'
     else s')
    IntSet.empty l

let subconfig_with_subs subs config l =
  {
    events_kind = submap subs l config.events_kind None ;
    prec_1 = submap subs l config.prec_1 (Some IntSet.empty);
    conflict = submap subs l config.conflict (Some IntSet.empty);
  }
let subenriched_grid_with_subs subs  grid l =
  let depth_of_event = submap subs  l grid.depth_of_event (Some 0) in
  let depth = IntMap.fold (fun _ -> max) depth_of_event 0 in
  {
    prec_star = grid.prec_star;
    config = subconfig_with_subs subs grid.config l ;
    depth_of_event = depth_of_event ;
    depth = depth ;
    size = List.length l ;
  }

let subconfig config l = subconfig_with_subs (build_subs l) config l
let subenriched_grid grid l = subenriched_grid_with_subs (build_subs l) grid l

let add_obs_eid eid grid = {grid with obs = eid::(grid.obs)}

let grid_find (node_id,site_id,quark) grid =
  Hashtbl.find grid.flow (node_id,site_id,quark)

let is_empty_grid grid = (Hashtbl.length grid.flow = 0)

let grid_add quark eid (attribute:attribute) grid =
  let () =
    if not (Hashtbl.mem grid.flow quark)
    then Hashtbl.add grid.pid_to_init quark eid in
  Hashtbl.replace grid.flow quark attribute ;
  grid

let last_event attribute =
  match attribute with
  | [] -> None
  | a::_ -> (Some a.eid)

(*adds atom a to attribute att.
 Collapses last atom if if bears the same id as a --in the case of a non atomic action*)
let push (a:atom) (att:atom list) =
  match att with
  | [] -> [a]
  | a'::att' ->
     if a'.eid = a.eid then
       let () = assert (a'.kind = a.kind) in
       { a' with causal_impact = a.causal_impact lor a'.causal_impact }::att'
     else a::att

(**side_effect Int2Set.t: pairs (agents,ports) that have been freed as a side effect --via a DEL or a FREE action*)
(*NB no internal state modif as side effect*)


(*let impact is_link c =
  if is_link then
    if Primitives.Causality.is_link_modif c then
      if Primitives.Causality.is_link_tested c then 3 else 2
    else 1
  else (*internal state*)
    if Primitives.Causality.is_internal_modif c then
      if Primitives.Causality.is_internal_tested c then 3 else 2
    else 1
 *)
let add ((node_id,_),site_id) is_link va grid event_number kind =
  let q = if is_link then 1 else 0 in
  let att =
    try grid_find (node_id,site_id,q) grid
    with Not_found -> [] in
  let att =
    push {causal_impact = va ; eid = event_number ;
	  kind = kind (*; observation = obs*)} att in
  let grid = grid_add (node_id,site_id,q) event_number att grid in
  add_init_pid event_number (node_id,site_id,q) grid

let add_actions env grid event_number kind actions =
  let rec aux grid = function
    | [] -> grid
    | Instantiation.Mod_internal (site,_) :: q ->
       aux (add site false atom_modified grid event_number kind) q
    | (Instantiation.Bind (site1,site2)
      | Instantiation.Bind_to (site1,site2)) :: q ->
       let grid' = add site2 true atom_modified grid event_number kind in
       aux (add site1 true atom_modified grid' event_number kind) q
    | Instantiation.Free site :: q ->
       aux (add site true atom_modified grid event_number kind) q
    | (Instantiation.Create ((_,na as ag),_)
      | Instantiation.Remove (_,na as ag)) :: q ->
       let sigs = Environment.signatures env in
       let ag_intf = Signature.get sigs na in
       let grid = add (ag,-1) true atom_modified grid event_number kind in
       let grid =
	 Signature.fold
	   (fun site _ grid ->
	    let grid' =
	      match Signature.default_internal_state na site sigs with
	      | None -> grid
	      | Some _ -> add (ag,site) false atom_modified grid event_number kind in
	    add (ag,site) true atom_modified grid' event_number kind)
	   ag_intf grid in
       aux grid q
  in aux grid actions

let add_tests grid event_number kind tests =
  let rec aux grid = function
    | [] -> grid
    | Instantiation.Is_Here ag :: q ->
       aux (add (ag,-1) true atom_tested grid event_number kind) q
    | Instantiation.Has_Internal (site,_) :: q ->
       aux (add site false atom_tested grid event_number kind) q
    | (Instantiation.Is_Free site
      | Instantiation.Is_Bound site
      | Instantiation.Has_Binding_type (site,_)) :: q ->
       aux (add site true atom_tested grid event_number kind) q
    | Instantiation.Is_Bound_to (site1,site2) :: q ->
       let grid' = add site2 true atom_tested grid event_number kind in
       aux (add site1 true atom_tested grid' event_number kind) q
  in aux grid tests

let record (kind,(tests,(actions,_,side_effects)))
	   event_number env grid =
  let grid = add_tests grid event_number kind tests in
  let grid = add_actions env grid event_number kind actions in
  List.fold_left
    (fun grid site -> add site true atom_modified grid event_number kind) grid side_effects

let record_obs (kind,tests,_) side_effects event_number grid =
  let grid = add_obs_eid event_number grid in
  let grid = add_tests grid event_number kind tests in
  List.fold_left
    (fun grid site -> add site true atom_modified grid event_number kind) grid side_effects

let record_init (lbl,actions) event_number env grid =
  (* if !Parameter.showIntroEvents then *)
  (*adding tests*)
  add_actions env grid event_number (INIT lbl)  actions

let add_pred eid atom config =
  let events_kind = IntMap.add atom.eid atom.kind config.events_kind in
  let pred_set =
    IntMap.find_default IntSet.empty eid config.prec_1 in
  let prec_1 = IntMap.add eid (IntSet.add atom.eid pred_set) config.prec_1 in
  {config with prec_1 = prec_1 ; events_kind = events_kind}

let add_conflict eid atom config =
  let events_kind = IntMap.add atom.eid atom.kind config.events_kind in
  let cflct_set =
    IntMap.find_default IntSet.empty eid config.conflict in
  let cflct = IntMap.add eid (IntSet.add atom.eid cflct_set) config.conflict in
  {config with conflict = cflct ; events_kind = events_kind }

let rec parse_attribute last_modif last_tested attribute config =
  match attribute with
  | [] -> config
  | atom::att ->
     let events_kind = IntMap.add atom.eid atom.kind config.events_kind in
     let prec_1 =
       let preds = IntMap.find_default IntSet.empty atom.eid config.prec_1 in
       IntMap.add atom.eid preds config.prec_1 in
     let config = {config with events_kind =  events_kind ; prec_1 = prec_1} in
     (*atom has a modification*)
     if (atom.causal_impact = atom_modified) || (atom.causal_impact = 3) then
       let config =
	 List.fold_left (fun config pred_id -> add_pred pred_id atom config)
			config last_tested in
       let tested =
	 if (atom.causal_impact = atom_tested)
	    ||(atom.causal_impact = atom_tested lor atom_modified)
	 then [atom.eid] else [] in
       parse_attribute (Some atom.eid) tested att config
     else
       (* atom is a pure test*)
       let config =
	 match last_modif with
	 | None -> config
	 | Some eid ->
	    add_conflict eid atom config (*adding conflict with last modification*) 
       in
       parse_attribute last_modif (atom.eid::last_tested) att config

let cut attribute_ids grid =
  let rec build_config attribute_ids cfg =
    match attribute_ids with
    | [] -> cfg
    | (node_i,site_i,type_i)::tl ->
       let attribute =
	 try grid_find (node_i,site_i,type_i) grid
	 with Not_found -> invalid_arg "Causal.cut" in
       let cfg =
	 match attribute with
	 | [] -> cfg
	 | atom::att ->
	    let events_kind = IntMap.add atom.eid atom.kind cfg.events_kind in
	    let prec_1 =
	      let preds =
		IntMap.find_default IntSet.empty atom.eid cfg.prec_1 in
	      IntMap.add atom.eid preds cfg.prec_1 in
	    let tested =
	      if (atom.causal_impact = atom_tested) || (atom.causal_impact = 3)
	      then [atom.eid] else [] in
	    let modif =
	      if (atom.causal_impact = atom_modified) || (atom.causal_impact = 3)
	      then Some atom.eid else None in
	    parse_attribute
	      modif tested att
	      {cfg with prec_1 = prec_1 ; events_kind = events_kind}
       in build_config tl cfg
  in
  build_config attribute_ids empty_config

let pp_atom f atom =
  let imp_str = match atom.causal_impact with
      1 -> "o" | 2 -> "x" | 3 -> "%"
      | _ -> invalid_arg "Causal.string_of_atom" in
  Format.fprintf f "%s_%d" imp_str atom.eid

let dump grid fic =
  let d_chan = open_out ((Filename.chop_extension fic)^".txt") in
  let d = Format.formatter_of_out_channel d_chan in
  let () = Format.pp_open_vbox d 0 in
  Hashtbl.fold
    (fun (n_id,s_id,q) att _ ->
     Format.fprintf d "#%i.%i%c:%a@," n_id s_id (if q=0 then '~' else '!')
		    (Pp.list Pp.empty pp_atom) att
    ) grid.flow () ;
  let () = Format.fprintf d "@]@." in
  close_out d_chan

let label ?env = function
  | OBS name -> name
  | PERT s -> s
  | RULE r_id -> Format.asprintf "%a" (Environment.print_ast_rule ?env) r_id
  | INIT s ->
     Format.asprintf
       "Intro @[<h>%a@]" (Pp.list Pp.comma (Environment.print_agent ?env)) s

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

let prec_star_of_config = Graph_closure.closure
			   
let depth_and_size_of_event config =
  IntMap.fold
    (fun eid prec_eids (emap,_) ->
      let d =
	IntSet.fold
	  (fun eid' d ->
	   let d' = IntMap.find_default 0 eid' emap in
	   max (d'+1) d
	  ) prec_eids 0
      in
      IntMap.add eid d emap,d
    ) config.prec_1 (IntMap.empty,0)

let enrich_grid err_fmt config_closure grid =
  let keep_l =
    List.fold_left (fun a b -> IntSet.add b a) IntSet.empty grid.obs in
  let to_keep i = IntSet.mem i keep_l in
  let ids = ids_of_grid grid  in
  let config = config_of_grid ids grid in
  let init_to_eid_max i =
    try Hashtbl.find grid.init_to_eidmax i
    with Not_found -> 0 in
  let prec_star = prec_star_of_config
		    err_fmt config_closure config.prec_1 to_keep
		    init_to_eid_max in
  let depth_of_event,depth = depth_and_size_of_event config in
  {
    config = config ;
    prec_star = prec_star ;
    depth = depth ;
    depth_of_event = depth_of_event ;
    size = IntMap.size config.prec_1 ;
  }

let dot_of_grid profiling env enriched_grid form =
  let t = Sys.time () in
  let config = enriched_grid.config in
  let prec_star = enriched_grid.prec_star in
  let depth_of_event = enriched_grid.depth_of_event in
  let sorted_events =
    IntMap.fold
      (fun eid d dmap ->
       let set = IntMap.find_default IntSet.empty d dmap in
       IntMap.add d (IntSet.add eid set) dmap
      ) depth_of_event IntMap.empty in
  Format.fprintf form "@[<v>%t@,digraph G{@, ranksep=.5 ;@," profiling;
  IntMap.iter
    (fun d eids_at_d ->
     Format.fprintf form "@[<hv>{ rank = same ; \"%d\" [shape=plaintext] ;@," d;
     IntSet.iter
       (fun eid ->
	match IntMap.find_option eid config.events_kind with
	| None -> raise Not_found
	| Some atom_kind ->
	   if eid <> 0 then
	     match atom_kind  with
	     | RULE _  ->
		Format.fprintf
		  form
		  "node_%d [label=\"%s\", shape=%s, style=%s, fillcolor = %s] ;@,"
		  eid (label ~env atom_kind) "invhouse" "filled" "lightblue"
	     | OBS _  ->
		Format.fprintf
		  form "node_%d [label=\"%s\", style=filled, fillcolor=red] ;@,"
		  eid (label ~env atom_kind)
	     | INIT _ ->
		if !Parameter.showIntroEvents then
		  Format.fprintf
		    form
		    "node_%d [label=\"%s\", shape=%s, style=%s, fillcolor=%s] ;@,"
		    eid (label ~env atom_kind) "house" "filled" "green"
	     | PERT _ ->
		Format.fprintf
		  form
		  "node_%d [label=\"%s\", shape=%s, style=%s, fillcolor = %s] ;@,"
		  eid (label ~env atom_kind) "invhouse" "filled" "green"
       (* List.iter (fun obs -> fprintf desc "obs_%d [label =\"%s\", style=filled, fillcolor=red] ;\n node_%d -> obs_%d [arrowhead=vee];\n" eid obs eid eid) atom.observation ;*)
       ) eids_at_d ;
     Format.fprintf form "}@]@," ;
    ) sorted_events ;
  let cpt = ref 0 in
  while !cpt+1 < (IntMap.size sorted_events) do
    Format.fprintf form "\"%d\" -> \"%d\" [style=\"invis\"];@," !cpt (!cpt+1);
    cpt := !cpt + 1
  done ;
  IntMap.iter
    (fun eid pred_set ->
     if eid <> 0 then
       IntSet.iter
	 (fun eid' ->
	  if eid' = 0 then ()
	  else
	    if !Parameter.showIntroEvents then
	      Format.fprintf form "node_%d -> node_%d@," eid' eid
	    else
	      match IntMap.find_option eid' config.events_kind with
	      | None -> raise Not_found
	      | Some atom_kind ->
		 match atom_kind with
		 | INIT _ -> ()
		 | PERT _ | RULE _ | OBS _ -> Format.fprintf form "node_%d -> node_%d@," eid' eid
	 ) pred_set
    ) config.prec_1 ;
  IntMap.iter
    (fun eid cflct_set ->
     if eid <> 0 then
       let prec = try (fst prec_star).(eid) with _ -> [] in
       let _ =
         IntSet.fold_inv
           (fun eid' prec ->
            let bool,prec =
	      let rec aux prec =
                match prec with
                | []   -> true,prec
                | h::t ->
                   if h=eid' then false,t else
		     if h>eid' then aux t else true,prec
	      in aux prec in
            let () =
	      if bool then
		Format.fprintf
		  form "node_%d -> node_%d [style=dotted, arrowhead = tee]@ "
		  eid eid'
            in
            prec
	   ) cflct_set prec
       in ()
    ) config.conflict ;
  Format.fprintf form "}@," ;
  Format.fprintf form "/*@, Dot generation time: %f@,*/@]@." (Sys.time () -. t)

let js_of_grid env enriched_grid f =
  let () = Format.fprintf f "// Create a new directed graph@," in
  let () =
    Format.fprintf f "var g = new dagreD3.graphlib.Graph().setGraph({});@," in

  let () = Pp.set
	     ~trailing:Pp.space IntMap.bindings Pp.space
	     (fun f (eid,atom_kind) ->
	      Format.fprintf
		f "g.setNode(%i, { label: \"%s\", style: \"fill: %s\" });"
		eid (label ~env atom_kind)
		(match atom_kind with
		 | OBS _ -> "#f77"
		 | (INIT _ | PERT _) -> "#7f7"
		 | RULE _ -> "#77f"))
	     f enriched_grid.config.events_kind in
  let () =
    Pp.set
      IntMap.bindings Pp.empty
      (fun f (eid,set) ->
       Pp.set IntSet.elements ~trailing:Pp.space Pp.space
	      (fun f eid' -> Format.fprintf f "g.setEdge(%i,%i,{});" eid' eid)
	      f set)
      f enriched_grid.config.prec_1 in

  let () = Format.fprintf
	     f "var svg = d3.select(\"svg\"),inner = svg.select(\"g\");@," in

  let () = Format.fprintf
	     f "// Set up zoom support@," in
  let () = Format.fprintf
	     f "var zoom = d3.behavior.zoom().on(\"zoom\", function() {@," in
  let () = Format.fprintf
	     f "inner.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" +@," in
  let () = Format.fprintf
	     f "\"scale(\" + d3.event.scale + \")\");@,});@,svg.call(zoom);" in
  let () = Format.fprintf
	     f "// Create the renderer@, var render = new dagreD3.render();@," in
  let () = Format.fprintf
	     f "// Run the renderer. This is what draws the final graph.@," in
  let () = Format.fprintf f "render(inner, g);@," in

  let () = Format.fprintf f "// Center the graph@,var initialScale = 0.75;@," in
  let () = Format.fprintf f "zoom@," in
  let () = Format.fprintf
	     f ".translate([(svg.attr(\"width\") - g.graph().width * initialScale) / 2, 20])@," in
  let () = Format.fprintf f ".scale(initialScale)@,.event(svg);@," in
  Format.fprintf f "svg.attr('height', g.graph().height * initialScale + 40);"

let html_of_grid profiling compression_type cpt env enriched_grid =
  let title f = Format.fprintf
		  f "%s compressed story number %i" compression_type cpt in
  Pp_html.graph_page
    title ["http://d3js.org/d3.v3.min.js";
	   "http://cpettitt.github.io/project/dagre-d3/latest/dagre-d3.min.js"]
    (fun f ->
     let () = Format.fprintf f "@[<v 2><style>@," in
     let () =
       Format.fprintf f "dt {float: left; clear: left; width: 20em;}@," in
     let () =
       Format.fprintf f "dd {font-weight: bold; margin: 0 0 0 21em;}@," in
     let () = Format.fprintf f ".node rect {stroke: #333; fill: #fff;}@," in
     let () =
       Format.fprintf
	 f ".edgePath path {stroke: #333; fill: #333; stroke-width: 1.5px;}" in
     Format.fprintf f "@]@,</style>")
    (fun f ->
     let () = Format.fprintf f "<p>@[%t@]</p>@," profiling in
     Format.fprintf
       f "@[<v 2><script>@,%t@]@,</script>"
       (js_of_grid env enriched_grid))

(*story_list:[(key_i,list_i)] et list_i:[(grid,_,sim_info option)...]
 et sim_info:{with story_id:int story_time: float ; story_event: int}*)
let pretty_print err_fmt env config_closure compression_type label story_list =
  let n = List.length story_list in
  let () =
    if compression_type = "" then
      Format.fprintf err_fmt "+ Pretty printing %d flow%s@."
		     n (if n>1 then "s" else "")
    else
      Format.fprintf err_fmt "+ Pretty printing %d %scompressed flow%s@."
		     n label (if n>1 then "s" else "")
  in
  let compression_type =
    if compression_type = "" then "none" else compression_type in
  let story_list =
    List.map (fun (x,y) -> enrich_grid err_fmt config_closure x,y) story_list in
  let _ =
    List.fold_left
      (fun cpt (enriched_config,stories) ->
       let av_t,ids,n =
	 List.fold_left
	   (fun (av_t,ids,n) info ->
	    (av_t +. info.story_time,info.story_id::ids,n+1)
	   )
	   (0.,[],0) (List.rev stories)
       in
       let enriched_config' =
	 if !Parameter.reduceCflows
	 then {enriched_config with
		config = {enriched_config.config with
			   prec_1 = Graph_closure.reduction
				      enriched_config.config.prec_1}}
	 else enriched_config in
        let () =   (*dump grid fic state env ; *)
	  if !Parameter.dotCflows then
	    let profiling desc =
	      Format.fprintf
		desc "/* @[Compression of %d causal flows" n;
	      Format.fprintf
		desc "@ obtained in average at %E t.u@] */@,"
		(av_t/.(float_of_int n)) ;
	      Format.fprintf
		desc "@[/* Compressed causal flows were:@ [%a] */@]"
		(Pp.list (fun f -> Format.fprintf f ";@,")
			 Format.pp_print_int) ids
	    in
	    Kappa_files.with_cflow_file
	      [compression_type;string_of_int cpt] "dot"
	      (dot_of_grid profiling env enriched_config')
	  else
	    let profiling desc =
	      Format.fprintf
		desc
		"@[<v 2><dl>@,<dt>Compression of</dt><dd>%d causal flows</dd>"n;
	      Format.fprintf
		desc "@,<dt>obtained in average at</dt><dd>%E t.u</dd>@,"
		(av_t/.(float_of_int n)) ;
	      Format.fprintf desc "<dt>Compressed causal flows were:</dt>";
	      Format.fprintf
		desc "@ <dd>[@[%a@]]</dd>@]@,</dl>"
		(Pp.list (fun f -> Format.fprintf f ";@,")
			 Format.pp_print_int) ids;
       in

	    Kappa_files.with_cflow_file
	      [compression_type;string_of_int cpt] "html"
	      (html_of_grid
		 profiling compression_type cpt env enriched_config') in
	cpt+1
      ) 0 story_list
  in
  Kappa_files.with_cflow_file
    [compression_type;"Summary"] "dat"
    (fun form ->
     let () = Format.fprintf form "@[<v>#id\tE\tT\t\tdepth\tsize\t@," in
     let () =
       Pp.listi Pp.empty
		(fun cpt f (enriched_config,story) ->
		 let depth = enriched_config.depth in
		 let size = enriched_config.size in
		 List.iter
		   (fun info  ->
		    let time = info.story_time in
		    let event = info.story_event in
		    Format.fprintf f "%i\t%i\t%E\t%i\t%i\t@,"
				   cpt event time depth size
		   ) story) form story_list in
     Format.fprintf form "@]@?")

let print_stat f parameter handler enriched_grid =
  let count_obs =
    match
      snd enriched_grid.prec_star
    with
      Graph_closure.Increasing_with_last_event -> (fun x -> x)
    | Graph_closure.Decreasing_without_last_event -> succ
  in 
  let size = Array.length (fst enriched_grid.prec_star) in
  let rec aux k n_step longest_story n_nonempty length_sum length_square_sum =
    if k>=size
    then (n_step,longest_story,n_nonempty,length_sum,length_square_sum)
    else
      let cc = List.length (Array.get (fst enriched_grid.prec_star) k) in
      let cc' = if cc>0 then count_obs cc else cc in 
      aux (k+1) (n_step+1) (max longest_story  cc')
        (if cc>0 then n_nonempty+1 else n_nonempty)
        (length_sum+cc') (length_square_sum+cc'*cc') in
  let n_step,longest_story,n_nonempty,length_sum,length_square_sum =
    aux 0 0 0 0 0 0 in
  let () = Format.fprintf f  "@[<v>Stats:@," in
  let () = Format.fprintf f " number of step   : %i@," n_step in
  let () = Format.fprintf f " longest story    : %i@," longest_story in
  let () = Format.fprintf f " average length   : %F@,"
			  (float length_sum /. float n_nonempty) in
  let () = Format.fprintf f " geometric mean   : %F@,"
			  (sqrt (float length_square_sum /. float n_nonempty)) in
  Format.fprintf f "@]@."
