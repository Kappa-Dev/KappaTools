type quark_lists = {
  site_tested: (int * int) list;
  site_modified: (int * int) list;
  internal_state_tested: (int * int) list;
  internal_state_modified: (int * int) list;
}

type event_kind = OBS of string | EVENT of Trace.event_kind

let atom_tested = 1
let atom_modified = 2
let atom_testedmodified = 3

type atom = {
  causal_impact: int; (*(1) tested (2) modified, (3) tested + modified*)
  eid: int; (*event identifier*)
  kind: event_kind; (*observation: string list*)
}

type attribute = atom list (*vertical sequence of atoms*)

type grid = {
  flow: (int * int * int, attribute) Hashtbl.t;
  (*(n_i,s_i,q_i) -> att_i with n_i: node_id, s_i: site_id, q_i:
    link (1) or internal state (0) *)
  pid_to_init: (int * int * int, int) Hashtbl.t;
  obs: int list;
  init_tbl: (int, Mods.IntSet.t) Hashtbl.t; (*decreasing*)
  init_to_eidmax: (int, int) Hashtbl.t;
}

type config = {
  events_kind: event_kind Mods.IntMap.t;
  prec_1: Mods.IntSet.t Mods.IntMap.t;
  conflict: Mods.IntSet.t Mods.IntMap.t;
}

type enriched_grid = {
  config: config;
  depth: int;
  prec_star: int list array * Graph_closure.order; (*decreasing*)
  depth_of_event: int Mods.IntMap.t;
  size: int;
}

type formatCflow = Dot | Html | Json

let empty_config =
  {
    events_kind = Mods.IntMap.empty;
    conflict = Mods.IntMap.empty;
    prec_1 = Mods.IntMap.empty;
  }

let print_event_kind ?env f = function
  | EVENT e -> Trace.print_event_kind ?env f e
  | OBS i ->
    (match env with
    | None -> Format.fprintf f "OBS(%s)" i
    | Some _ -> Format.pp_print_string f i)

let debug_print_causal f i =
  Format.pp_print_string f
    (if i = atom_tested then
       "tested"
     else if i = atom_modified then
       "modified"
     else if i = atom_tested lor atom_modified then
       "tested&modified"
     else
       "CAUSAL IMPACT UNDEFINED")

let debug_print_atom f a =
  Format.fprintf f "{#%i: %a %a}" a.eid debug_print_causal a.causal_impact
    (print_event_kind ?env:None)
    a.kind

let debug_print_grid f g =
  let () = Format.fprintf f "@[<v>Flow:@," in
  let () =
    Hashtbl.iter
      (fun (node_id, site_id, q) l ->
        Format.fprintf f "@[<2>%i.%i%s:@,%a@]@," node_id site_id
          (if q = 0 then
             "~"
           else if q = 1 then
             ""
           else
             "UNDEFINED")
          (Pp.list Pp.space debug_print_atom)
          l)
      g.flow
  in
  Format.fprintf f "@]@."

let empty_grid () =
  {
    flow = Hashtbl.create !Parameter.defaultExtArraySize;
    pid_to_init = Hashtbl.create !Parameter.defaultExtArraySize;
    obs = [];
    init_tbl = Hashtbl.create !Parameter.defaultExtArraySize;
    init_to_eidmax = Hashtbl.create !Parameter.defaultExtArraySize;
  }

let build_subs l =
  snd
    (List.fold_left
       (fun (n, map) a -> succ n, Mods.IntMap.add a n map)
       (1, Mods.IntMap.empty) l)

let submap subs l m default =
  List.fold_left
    (fun m' a ->
      match Mods.IntMap.find_option a subs with
      | None -> raise Not_found
      | Some new_a ->
        Mods.IntMap.add new_a
          (match Mods.IntMap.find_option a m with
          | Some x -> x
          | None ->
            (match default with
            | None -> raise Not_found
            | Some a -> a))
          m')
    Mods.IntMap.empty l

let add_init_pid eid pid grid =
  let eid_init = Hashtbl.find grid.pid_to_init pid in
  let old =
    try Hashtbl.find grid.init_tbl eid with Not_found -> Mods.IntSet.empty
  in
  let () = Hashtbl.replace grid.init_tbl eid (Mods.IntSet.add eid_init old) in
  let () = Hashtbl.replace grid.init_to_eidmax eid_init eid in
  grid

let _subset subs l s =
  List.fold_left
    (fun s' a ->
      if Mods.IntSet.mem a s then
        Mods.IntSet.add
          (match Mods.IntMap.find_option a subs with
          | Some i -> i
          | None -> raise Not_found)
          s'
      else
        s')
    Mods.IntSet.empty l

let subconfig_with_subs subs config l =
  {
    events_kind = submap subs l config.events_kind None;
    prec_1 = submap subs l config.prec_1 (Some Mods.IntSet.empty);
    conflict = submap subs l config.conflict (Some Mods.IntSet.empty);
  }

let subenriched_grid_with_subs subs grid l =
  let depth_of_event = submap subs l grid.depth_of_event (Some 0) in
  let depth = Mods.IntMap.fold (fun _ -> max) depth_of_event 0 in
  {
    prec_star = grid.prec_star;
    config = subconfig_with_subs subs grid.config l;
    depth_of_event;
    depth;
    size = List.length l;
  }

let _subconfig config l = subconfig_with_subs (build_subs l) config l
let _subenriched_grid grid l = subenriched_grid_with_subs (build_subs l) grid l
let add_obs_eid eid grid = { grid with obs = eid :: grid.obs }

let grid_find (node_id, site_id, quark) grid =
  Hashtbl.find grid.flow (node_id, site_id, quark)

let _is_empty_grid grid = Hashtbl.length grid.flow = 0

let grid_add quark eid (attribute : attribute) grid =
  let () =
    if not (Hashtbl.mem grid.flow quark) then
      Hashtbl.add grid.pid_to_init quark eid
  in
  Hashtbl.replace grid.flow quark attribute;
  grid

let _last_event attribute =
  match attribute with
  | [] -> None
  | a :: _ -> Some a.eid

(*adds atom a to attribute att.
  Collapses last atom if if bears the same id as a --in the case of a non atomic action*)
let push (a : atom) (att : atom list) =
  match att with
  | [] -> [ a ]
  | a' :: att' ->
    if a'.eid = a.eid then (
      let () = assert (a'.kind = a.kind) in
      { a' with causal_impact = a.causal_impact lor a'.causal_impact } :: att'
    ) else
      a :: att

(**side_effect Int2Set.t: pairs (agents,ports) that have been freed as a side effect --via a DEL or a FREE action*)
(*NB no internal state modif as side effect*)

(*let impact is_link c =
  if is_link then
    if Primitives.Causality.is_link_modif c then
      if Primitives.Causality.is_link_tested c then atom_testedmodified else atom_modified
    else atom_tested
  else (*internal state*)
    if Primitives.Causality.is_internal_modif c then
      if Primitives.Causality.is_internal_tested c then atom_testedmodified else atom_modified
    else atom_tested
*)
let add ((node_id, _), site_id) is_link va grid event_number kind =
  let q =
    if is_link then
      atom_tested
    else
      0
  in
  let att = try grid_find (node_id, site_id, q) grid with Not_found -> [] in
  let att =
    push
      { causal_impact = va; eid = event_number; kind (*; observation = obs*) }
      att
  in
  let grid = grid_add (node_id, site_id, q) event_number att grid in
  add_init_pid event_number (node_id, site_id, q) grid

let add_actions env grid event_number kind actions =
  let rec aux grid = function
    | [] -> grid
    | Instantiation.Mod_internal (site, _) :: q ->
      aux (add site false atom_modified grid event_number kind) q
    | Instantiation.Bind (site1, site2) :: q ->
      let grid' = add site2 true atom_modified grid event_number kind in
      aux (add site1 true atom_modified grid' event_number kind) q
    | Instantiation.Bind_to (site1, _) :: q ->
      aux (add site1 true atom_modified grid event_number kind) q
    | Instantiation.Free site :: q ->
      aux (add site true atom_modified grid event_number kind) q
    | ( Instantiation.Create (((_, na) as ag), _)
      | Instantiation.Remove ((_, na) as ag) )
      :: q ->
      let sigs = Model.signatures env in
      let ag_intf = Signature.get sigs na in
      let grid = add (ag, -1) true atom_modified grid event_number kind in
      let grid =
        Signature.fold
          (fun site _ grid ->
            let grid' =
              match Signature.default_internal_state na site sigs with
              | None -> grid
              | Some _ ->
                add (ag, site) false atom_modified grid event_number kind
            in
            add (ag, site) true atom_modified grid' event_number kind)
          grid ag_intf
      in
      aux grid q
  in
  aux grid actions

let add_tests grid event_number kind tests =
  List.fold_left
    (List.fold_left (fun grid -> function
       | Instantiation.Is_Here ag ->
         add (ag, -1) true atom_tested grid event_number kind
       | Instantiation.Has_Internal (site, _) ->
         add site false atom_tested grid event_number kind
       | Instantiation.Is_Free site
       | Instantiation.Is_Bound site
       | Instantiation.Has_Binding_type (site, _) ->
         add site true atom_tested grid event_number kind
       | Instantiation.Is_Bound_to (site1, site2) ->
         let grid' = add site2 true atom_tested grid event_number kind in
         add site1 true atom_tested grid' event_number kind))
    grid tests

let record (kind, event, _) event_number env grid =
  let grid =
    add_tests grid event_number (EVENT kind) event.Instantiation.tests
  in
  let grid =
    add_tests grid event_number (EVENT kind)
      [ event.Instantiation.connectivity_tests ]
  in
  let grid =
    add_actions env grid event_number (EVENT kind) event.Instantiation.actions
  in
  List.fold_left
    (fun grid site ->
      add
        (fst site, -1)
        true atom_tested
        (add site true atom_modified grid event_number (EVENT kind))
        event_number (EVENT kind))
    grid event.Instantiation.side_effects_dst

let record_obs (kind, tests, _) side_effects event_number grid =
  let grid = add_obs_eid event_number grid in
  let grid = add_tests grid event_number (OBS kind) tests in
  List.fold_left
    (fun grid site -> add site true atom_modified grid event_number (OBS kind))
    grid side_effects

let record_init (lbl, actions) event_number env grid =
  add_actions env grid event_number (EVENT (Trace.INIT lbl)) actions

let add_pred eid atom config =
  let events_kind = Mods.IntMap.add atom.eid atom.kind config.events_kind in
  let pred_set = Mods.IntMap.find_default Mods.IntSet.empty eid config.prec_1 in
  let prec_1 =
    Mods.IntMap.add eid (Mods.IntSet.add atom.eid pred_set) config.prec_1
  in
  { config with prec_1; events_kind }

let add_conflict eid atom config =
  let events_kind = Mods.IntMap.add atom.eid atom.kind config.events_kind in
  let cflct_set =
    Mods.IntMap.find_default Mods.IntSet.empty eid config.conflict
  in
  let cflct =
    Mods.IntMap.add eid (Mods.IntSet.add atom.eid cflct_set) config.conflict
  in
  { config with conflict = cflct; events_kind }

let rec parse_attribute last_modif last_tested attribute config =
  match attribute with
  | [] -> config
  | atom :: att ->
    let events_kind = Mods.IntMap.add atom.eid atom.kind config.events_kind in
    let prec_1 =
      let preds =
        Mods.IntMap.find_default Mods.IntSet.empty atom.eid config.prec_1
      in
      Mods.IntMap.add atom.eid preds config.prec_1
    in
    let config = { config with events_kind; prec_1 } in
    (*atom has a modification*)
    if
      atom.causal_impact = atom_modified
      || atom.causal_impact = atom_testedmodified
    then (
      let config =
        List.fold_left
          (fun config pred_id -> add_pred pred_id atom config)
          config last_tested
      in
      let tested =
        if
          atom.causal_impact = atom_tested
          || atom.causal_impact = atom_tested lor atom_modified
        then
          [ atom.eid ]
        else
          []
      in
      parse_attribute (Some atom.eid) tested att config
    ) else (
      (* atom is a pure test*)
      let config =
        match last_modif with
        | None -> config
        | Some eid ->
          add_conflict eid atom
            config (*adding conflict with last modification*)
      in
      parse_attribute last_modif (atom.eid :: last_tested) att config
    )

let cut ?(with_reduction = true) parameter handler log_info error attribute_ids
    grid =
  let error, log_info =
    StoryProfiling.StoryStats.add_event parameter error
      StoryProfiling.Build_configuration None log_info
  in
  let rec build_config attribute_ids cfg =
    match attribute_ids with
    | [] -> cfg
    | (node_i, site_i, type_i) :: tl ->
      let attribute =
        try grid_find (node_i, site_i, type_i) grid
        with Not_found -> invalid_arg "Causal.cut"
      in
      let cfg =
        match attribute with
        | [] -> cfg
        | atom :: att ->
          let events_kind =
            Mods.IntMap.add atom.eid atom.kind cfg.events_kind
          in
          let prec_1 =
            let preds =
              Mods.IntMap.find_default Mods.IntSet.empty atom.eid cfg.prec_1
            in
            Mods.IntMap.add atom.eid preds cfg.prec_1
          in
          let tested =
            if
              atom.causal_impact = atom_tested
              || atom.causal_impact = atom_testedmodified
            then
              [ atom.eid ]
            else
              []
          in
          let modif =
            if
              atom.causal_impact = atom_modified
              || atom.causal_impact = atom_testedmodified
            then
              Some atom.eid
            else
              None
          in
          parse_attribute modif tested att { cfg with prec_1; events_kind }
      in
      build_config tl cfg
  in
  let cfg = build_config attribute_ids empty_config in
  let error, log_info =
    StoryProfiling.StoryStats.close_event parameter error
      StoryProfiling.Build_configuration None log_info
  in
  let error, log_info, reduction =
    if with_reduction then
      Graph_closure.reduction parameter handler log_info error cfg.prec_1
    else
      error, log_info, cfg.prec_1
  in
  error, log_info, { cfg with prec_1 = reduction }

let pp_atom f atom =
  let imp_str =
    match atom.causal_impact with
    | 1 -> "o"
    | 2 -> "x"
    | 3 -> "%"
    | _ -> invalid_arg "Causal.string_of_atom"
  in
  Format.fprintf f "%s_%d" imp_str atom.eid

let _dump grid fic =
  let d_chan = Kappa_files.open_out (Filename.chop_extension fic ^ ".txt") in
  let d = Format.formatter_of_out_channel d_chan in
  let () = Format.pp_open_vbox d 0 in
  Hashtbl.fold
    (fun (n_id, s_id, q) att _ ->
      Format.fprintf d "#%i.%i%c:%a@," n_id s_id
        (if q = 0 then
           '~'
         else
           '!')
        (Pp.list Pp.empty pp_atom) att)
    grid.flow ();
  let () = Format.fprintf d "@]@." in
  close_out d_chan

let ids_of_grid grid = Hashtbl.fold (fun key _ l -> key :: l) grid.flow []
let config_of_grid = cut

(*let prec_star_of_config_old  config =
  let rec prec_closure config todo already_done closure =
    if Mods.IntSet.is_empty todo then closure
    else
      let eid = Mods.IntSet.choose todo in
      let todo' = Mods.IntSet.remove eid todo in
      if Mods.IntSet.mem eid already_done
      then
        prec_closure config todo' already_done closure
      else
        let prec = try IntMap.find eid config.prec_1 with Not_found -> Mods.IntSet.empty
        in
      prec_closure config (IntSet.union todo' prec) (IntSet.add eid already_done) (IntSet.union prec closure)
  in
  IntMap.fold
    (fun eid kind prec_star ->
      let set = prec_closure config (IntSet.singleton eid) Mods.IntSet.empty Mods.IntSet.empty
      in
      IntMap.add eid set prec_star
    ) config.events IntMap.empty *)

let prec_star_of_config = Graph_closure.closure

let depth_and_size_of_event config =
  Mods.IntMap.fold
    (fun eid prec_eids (emap, _) ->
      let d =
        Mods.IntSet.fold
          (fun eid' d ->
            let d' = Mods.IntMap.find_default 0 eid' emap in
            max (d' + 1) d)
          prec_eids 0
      in
      Mods.IntMap.add eid d emap, d)
    config.prec_1 (Mods.IntMap.empty, 0)

let enrich_grid parameter handler log_info error config_closure grid =
  let keep_l =
    List.fold_left (fun a b -> Mods.IntSet.add b a) Mods.IntSet.empty grid.obs
  in
  let to_keep i = Mods.IntSet.mem i keep_l in
  let ids = ids_of_grid grid in
  let error, log_info, config =
    config_of_grid parameter handler log_info error ids grid
  in
  let error, log_info, prec_star =
    prec_star_of_config parameter handler log_info error
      (Some StoryProfiling.Transitive_closure) config_closure config.prec_1
      to_keep
  in
  let depth_of_event, depth = depth_and_size_of_event config in
  ( error,
    log_info,
    {
      config;
      prec_star;
      depth;
      depth_of_event;
      size = Mods.IntMap.size config.prec_1;
    } )

let fold_over_causal_past_of_obs parameter handler log_info error config_closure
    grid f a =
  let keep_l =
    List.fold_left (fun a b -> Mods.IntSet.add b a) Mods.IntSet.empty grid.obs
  in
  let to_keep i = Mods.IntSet.mem i keep_l in
  let ids = ids_of_grid grid in
  let error, log_info =
    StoryProfiling.StoryStats.add_event parameter error
      StoryProfiling.Build_configuration None log_info
  in
  let error, log_info, config =
    config_of_grid ~with_reduction:false parameter handler log_info error ids
      grid
  in
  let error, log_info =
    StoryProfiling.StoryStats.close_event parameter error
      StoryProfiling.Build_configuration None log_info
  in
  Graph_closure.closure_bottom_up_with_fold parameter handler log_info error
    (Some StoryProfiling.Collect_traces) config_closure config.prec_1 to_keep f
    a

let print_event_kind_dot_annot env f = function
  | OBS name ->
    Format.fprintf f "[label=\"%s\", style=filled, fillcolor=red]" name
  | EVENT e -> Trace.print_event_kind_dot_annot env f e

let dot_of_grid profiling env enriched_grid form =
  let t = Sys.time () in
  let config = enriched_grid.config in
  let prec_star = enriched_grid.prec_star in
  let depth_of_event = enriched_grid.depth_of_event in
  let sorted_events =
    Mods.IntMap.fold
      (fun eid d dmap ->
        let set = Mods.IntMap.find_default Mods.IntSet.empty d dmap in
        Mods.IntMap.add d (Mods.IntSet.add eid set) dmap)
      depth_of_event Mods.IntMap.empty
  in
  Format.fprintf form "@[<v>%t@,digraph G{@, ranksep=.5 ;@," profiling;
  Mods.IntMap.iter
    (fun d eids_at_d ->
      Format.fprintf form "@[<hv>{ rank = same ; \"%d\" [shape=plaintext] ;@," d;
      Mods.IntSet.iter
        (fun eid ->
          match Mods.IntMap.find_option eid config.events_kind with
          | None -> raise Not_found
          | Some atom_kind ->
            if eid <> 0 then
              Format.fprintf form "node_%d %a ;@," eid
                (print_event_kind_dot_annot env)
                atom_kind
          (* List.iter (fun obs -> fprintf desc "obs_%d [label =\"%s\", style=filled, fillcolor=red] ;\n node_%d -> obs_%d [arrowhead=vee];\n" eid obs eid eid) atom.observation ;*))
        eids_at_d;
      Format.fprintf form "}@]@,")
    sorted_events;
  let cpt = ref 0 in
  while !cpt + 1 < Mods.IntMap.size sorted_events do
    Format.fprintf form "\"%d\" -> \"%d\" [style=\"invis\"];@," !cpt (!cpt + 1);
    cpt := !cpt + 1
  done;
  Mods.IntMap.iter
    (fun eid pred_set ->
      if eid <> 0 then
        Mods.IntSet.iter
          (fun eid' ->
            if eid' = 0 then
              ()
            else
              Format.fprintf form "node_%d -> node_%d@," eid' eid)
          pred_set)
    config.prec_1;
  Mods.IntMap.iter
    (fun eid cflct_set ->
      if eid <> 0 then (
        let prec = try (fst prec_star).(eid) with _ -> [] in
        let _ =
          Mods.IntSet.fold_inv
            (fun eid' prec ->
              let bool, prec =
                let rec aux prec =
                  match prec with
                  | [] -> true, prec
                  | h :: t ->
                    if h = eid' then
                      false, t
                    else if h > eid' then
                      aux t
                    else
                      true, prec
                in
                aux prec
              in
              let () =
                if bool then
                  Format.fprintf form
                    "node_%d -> node_%d [style=dotted, arrowhead = tee]@ " eid
                    eid'
              in
              prec)
            cflct_set prec
        in
        ()
      ))
    config.conflict;
  Format.fprintf form "}@,";
  Format.fprintf form "/*@, Dot generation time: %f@,*/@]@." (Sys.time () -. t)

let js_of_grid env enriched_grid f =
  let () = Format.fprintf f "// Create a new directed graph@," in
  let () =
    Format.fprintf f "var g = new dagreD3.graphlib.Graph().setGraph({});@,"
  in

  let () =
    Pp.set ~trailing:Pp.space Mods.IntMap.bindings Pp.space
      (fun f (eid, atom_kind) ->
        Format.fprintf f
          "g.setNode(%i, { label: \"%a\", style: \"fill: %s\" });" eid
          (print_event_kind ~env) atom_kind
          (match atom_kind with
          | OBS _ -> "#f77"
          | EVENT (Trace.INIT _ | Trace.PERT _) -> "#7f7"
          | EVENT (Trace.RULE _) -> "#77f"))
      f enriched_grid.config.events_kind
  in
  let () =
    Pp.set Mods.IntMap.bindings Pp.empty
      (fun f (eid, set) ->
        Pp.set Mods.IntSet.elements ~trailing:Pp.space Pp.space
          (fun f eid' -> Format.fprintf f "g.setEdge(%i,%i,{});" eid' eid)
          f set)
      f enriched_grid.config.prec_1
  in

  let () =
    Format.fprintf f "var svg = d3.select(\"svg\"),inner = svg.select(\"g\");@,"
  in

  let () = Format.fprintf f "// Set up zoom support@," in
  let () =
    Format.fprintf f "var zoom = d3.behavior.zoom().on(\"zoom\", function() {@,"
  in
  let () =
    Format.fprintf f
      "inner.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" +@,"
  in
  let () =
    Format.fprintf f
      "\"scale(\" + d3.event.scale + \")\");@,});@,svg.call(zoom);"
  in
  let () =
    Format.fprintf f
      "// Create the renderer@, var render = new dagreD3.render();@,"
  in
  let () =
    Format.fprintf f
      "// Run the renderer. This is what draws the final graph.@,"
  in
  let () = Format.fprintf f "render(inner, g);@," in

  let () = Format.fprintf f "// Center the graph@,var initialScale = 0.75;@," in
  let () = Format.fprintf f "zoom@," in
  let () =
    Format.fprintf f
      ".translate([(svg.attr(\"width\") - g.graph().width * initialScale) / 2, \
       20])@,"
  in
  let () = Format.fprintf f ".scale(initialScale)@,.event(svg);@," in
  Format.fprintf f "svg.attr('height', g.graph().height * initialScale + 40);"

let html_of_grid profiling compression_type cpt env enriched_grid =
  let title f =
    Format.fprintf f "%s compressed story number %i" compression_type cpt
  in
  Pp_html.graph_page title
    [
      "http://d3js.org/d3.v3.min.js";
      "http://cpettitt.github.io/project/dagre-d3/latest/dagre-d3.min.js";
    ]
    (fun f ->
      let () = Format.fprintf f "@[<v 2><style>@," in
      let () =
        Format.fprintf f "dt {float: left; clear: left; width: 20em;}@,"
      in
      let () =
        Format.fprintf f "dd {font-weight: bold; margin: 0 0 0 21em;}@,"
      in
      let () = Format.fprintf f ".node rect {stroke: #333; fill: #fff;}@," in
      let () =
        Format.fprintf f
          ".edgePath path {stroke: #333; fill: #333; stroke-width: 1.5px;}"
      in
      Format.fprintf f "@]@,</style>")
    (fun f ->
      let () = Format.fprintf f "<svg width=\"960\"><g></g></svg>@," in
      let () = Format.fprintf f "<p>@[%t@]</p>@," profiling in
      Format.fprintf f "@[<v 2><script>@,%t@]@,</script>"
        (js_of_grid env enriched_grid))

let check_create_quarks aid sites quarks =
  List.for_all
    (fun (site, internal) ->
      match internal with
      | Some _ ->
        List.mem (atom_modified, (aid, site, 0)) quarks
        && List.mem (atom_modified, (aid, site, 1)) quarks
      | None -> List.mem (atom_modified, (aid, site, 1)) quarks)
    sites

let check_modified_quarks ((aid, _), site) modif quarks =
  List.exists
    (fun (c, (n, s, m)) ->
      (c = atom_modified || c = atom_testedmodified)
      && n = aid && s = site && m = modif)
    quarks

let check_tested_quarks ((aid, _), site) modif quarks =
  List.exists
    (fun (c, (n, s, m)) ->
      (c = atom_tested || c = atom_testedmodified)
      && n = aid && s = site && m = modif)
    quarks

let check_event_quarks actions tests quarks =
  List.for_all
    (function
      | Instantiation.Create ((aid, _), sites) ->
        check_create_quarks aid sites quarks
      | Instantiation.Free asite -> check_modified_quarks asite 1 quarks
      | Instantiation.Bind_to (asite1, asite2)
      | Instantiation.Bind (asite1, asite2) ->
        check_modified_quarks asite1 1 quarks
        && check_modified_quarks asite2 1 quarks
      | Instantiation.Mod_internal (asite, _) ->
        check_modified_quarks asite 0 quarks
      | Instantiation.Remove (aid, _) ->
        List.exists
          (fun (c, (n, _, _)) ->
            (c = atom_modified || c = atom_testedmodified) && n = aid)
          quarks)
    actions
  && List.for_all
       (List.for_all (function
         | Instantiation.Is_Here (aid, _) ->
           List.exists
             (fun (c, (n, _, _)) ->
               (c = atom_tested || c = atom_testedmodified) && n = aid)
             quarks
         | Instantiation.Has_Internal (asite, _) ->
           check_tested_quarks asite 0 quarks
         | Instantiation.Is_Free asite
         | Instantiation.Is_Bound asite
         | Instantiation.Has_Binding_type (asite, _) ->
           check_tested_quarks asite 1 quarks
         | Instantiation.Is_Bound_to (asite1, asite2) ->
           check_tested_quarks asite1 1 quarks
           && check_tested_quarks asite2 1 quarks))
       tests

let log_event id quarks event_kind steps =
  match event_kind with
  | EVENT (Trace.INIT _) ->
    let stp =
      List.find
        (function
          | Trace.Init actions ->
            List.for_all
              (function
                | Instantiation.Create ((aid, _), sites) ->
                  check_create_quarks aid sites quarks
                | Instantiation.Free _ | Instantiation.Bind_to _
                | Instantiation.Bind _ | Instantiation.Mod_internal _ ->
                  true
                | Instantiation.Remove _ ->
                  raise
                    (ExceptionDefn.Internal_Error
                       (Locality.dummy_annot
                          "init event has actions not allowed")))
              actions
          | Trace.Rule _ | Trace.Pert _ | Trace.Obs _ | Trace.Subs _
          | Trace.Dummy _ ->
            false)
        steps
    in
    `List [ `Int id; Trace.step_to_yojson stp ]
  | EVENT (Trace.RULE rid) ->
    let stp =
      List.find
        (function
          | Trace.Rule (rid', e, _) ->
            rid = rid'
            && check_event_quarks e.Instantiation.actions
                 (e.Instantiation.connectivity_tests :: e.Instantiation.tests)
                 quarks
          | Trace.Pert _ | Trace.Obs _ | Trace.Subs _ | Trace.Dummy _
          | Trace.Init _ ->
            false)
        steps
    in
    `List [ `Int id; Trace.step_to_yojson stp ]
  | OBS _ ->
    let stp =
      List.find
        (function
          | Trace.Obs _ -> true
          | Trace.Subs _ | Trace.Dummy _ | Trace.Init _ | Trace.Rule _
          | Trace.Pert _ ->
            false)
        steps
    in
    `List [ `Int id; Trace.step_to_yojson stp ]
  | EVENT (Trace.PERT pert) ->
    let stp =
      List.find
        (function
          | Trace.Pert (pert', e, _) ->
            pert = pert'
            && check_event_quarks e.Instantiation.actions
                 (e.Instantiation.connectivity_tests :: e.Instantiation.tests)
                 quarks
          | Trace.Rule _ | Trace.Obs _ | Trace.Subs _ | Trace.Dummy _
          | Trace.Init _ ->
            false)
        steps
    in
    `List [ `Int id; Trace.step_to_yojson stp ]

let json_of_grid enriched_grid grid_story steps =
  let config = enriched_grid.config in
  let prec_star = enriched_grid.prec_star in
  let depth_of_event = enriched_grid.depth_of_event in
  let tbl = Hashtbl.create !Parameter.defaultExtArraySize in
  let () =
    Hashtbl.iter
      (fun quark att_ls ->
        List.iter
          (fun atom -> Hashtbl.add tbl atom.eid (atom.causal_impact, quark))
          att_ls)
      grid_story.flow
  in
  let sorted_events =
    Mods.IntMap.fold
      (fun eid d dmap ->
        let set = Mods.IntMap.find_default Mods.IntSet.empty d dmap in
        Mods.IntMap.add d (Mods.IntSet.add eid set) dmap)
      depth_of_event Mods.IntMap.empty
  in
  let node_to_json eid =
    match Mods.IntMap.find_option eid config.events_kind with
    | None -> raise Not_found
    | Some atom_kind ->
      if eid <> 0 then (
        let quarks = Hashtbl.find_all tbl eid in
        log_event eid quarks atom_kind steps
      ) else
        `Null
  in
  let nodes_to_list =
    Mods.IntMap.fold
      (fun _ eids_at_d ls ->
        let ls'' = Mods.IntSet.fold (fun eid ls' -> eid :: ls') eids_at_d [] in
        ls'' @ ls)
      sorted_events []
  in
  let nodes_to_json = JsonUtil.of_list node_to_json nodes_to_list in
  let edge_to_json (eid, eid') = `Assoc [ "from", `Int eid'; "to", `Int eid ] in
  let prec_edges =
    Mods.IntMap.fold
      (fun eid pred_set ls ->
        if eid <> 0 then (
          let ls'' =
            Mods.IntSet.fold
              (fun eid' ls' ->
                if eid' = 0 then
                  ls'
                else
                  (eid, eid') :: ls')
              pred_set []
          in
          ls'' @ ls
        ) else
          ls)
      config.prec_1 []
  in
  let prec_edges_to_json = JsonUtil.of_list edge_to_json prec_edges in
  let confl_edges =
    Mods.IntMap.fold
      (fun eid cflct_set ls ->
        if eid <> 0 then (
          let prec = try (fst prec_star).(eid) with _ -> [] in
          let _, ls' =
            Mods.IntSet.fold_inv
              (fun eid' (prec, ls') ->
                let bool, prec =
                  let rec aux prec =
                    match prec with
                    | [] -> true, prec
                    | h :: t ->
                      if h = eid' then
                        false, t
                      else if h > eid' then
                        aux t
                      else
                        true, prec
                  in
                  aux prec
                in
                let ls'' =
                  if bool then
                    (eid, eid') :: ls'
                  else
                    ls'
                in
                prec, ls'')
              cflct_set (prec, ls)
          in
          ls'
        ) else
          ls)
      config.conflict []
  in
  let confl_edges_to_json = JsonUtil.of_list edge_to_json confl_edges in
  (`Assoc
     [
       "nodes", nodes_to_json;
       "cause", prec_edges_to_json;
       "inhibit", confl_edges_to_json;
     ]
    : Yojson.Basic.t)

(*story_list:[(key_i,list_i)] et list_i:[(grid,_,sim_info option)...]
  et sim_info:{with story_id:int story_time: float ; story_event: int}*)
let pretty_print ~dotFormat parameter handler log_info error env config_closure
    compression_type label grid_list =
  match
    Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
  with
  | None -> error, log_info
  | Some err_fmt ->
    let n = List.length grid_list in
    let () =
      if compression_type = "" then
        Format.fprintf err_fmt "+ Pretty printing %d flow%s@." n
          (if n > 1 then
             "s"
           else
             "")
      else
        Format.fprintf err_fmt "+ Pretty printing %d %scompressed flow%s@." n
          label
          (if n > 1 then
             "s"
           else
             "")
    in
    let compression_type =
      if compression_type = "" then
        "none"
      else
        compression_type
    in
    let error, log_info, story_list =
      List.fold_left
        (fun (error, log_info, list) (z, x, y) ->
          let error, log_info, x =
            enrich_grid parameter handler log_info error config_closure x
          in
          error, log_info, (z, x, y) :: list)
        (error, log_info, []) grid_list
    in
    let story_list = List.rev story_list in
    let _ =
      List.fold_left
        (fun cpt (steps, enriched_config, stories) ->
          let av_t, ids, n =
            List.fold_left
              (fun (av_t, ids, n) info ->
                ( av_t +. info.Trace.Simulation_info.story_time,
                  info.Trace.Simulation_info.story_id :: ids,
                  n + 1 ))
              (0., [], 0) (List.rev stories)
          in
          let () =
            (*dump grid fic state env ; *)
            let () =
              if dotFormat = Json then (
                let _, grid_story, _ = List.nth grid_list cpt in
                Kappa_files.with_cflow_file
                  [ compression_type; string_of_int cpt ]
                  ".json"
                  (fun f ->
                    Format.fprintf f "%s@."
                      (Yojson.Basic.to_string
                         (json_of_grid enriched_config grid_story steps)))
              )
            in
            match dotFormat with
            | Dot | Json ->
              let profiling desc =
                Format.fprintf desc "/* @[Compression of %d causal flows" n;
                Format.fprintf desc "@ obtained in average at %E t.u@] */@,"
                  (av_t /. float_of_int n);
                Format.fprintf desc
                  "@[/* Compressed causal flows were:@ [%a] */@]"
                  (Pp.list
                     (fun f -> Format.fprintf f ";@,")
                     Format.pp_print_int)
                  ids
              in
              Kappa_files.with_cflow_file
                [ compression_type; string_of_int cpt ]
                ".dot"
                (dot_of_grid profiling env enriched_config)
            | Html ->
              let profiling desc =
                Format.fprintf desc
                  "@[<v 2><dl>@,<dt>Compression of</dt><dd>%d causal flows</dd>"
                  n;
                Format.fprintf desc
                  "@,<dt>obtained in average at</dt><dd>%E t.u</dd>@,"
                  (av_t /. float_of_int n);
                Format.fprintf desc "<dt>Compressed causal flows were:</dt>";
                Format.fprintf desc "@ <dd>[@[%a@]]</dd>@]@,</dl>"
                  (Pp.list
                     (fun f -> Format.fprintf f ";@,")
                     Format.pp_print_int)
                  ids
              in
              Kappa_files.with_cflow_file
                [ compression_type; string_of_int cpt ]
                ".html"
                (html_of_grid profiling compression_type cpt env enriched_config)
          in
          cpt + 1)
        0 story_list
    in
    let () =
      match dotFormat with
      | Json ->
        Kappa_files.with_cflow_file [ compression_type; "env" ] ".json"
          (fun f ->
            Format.fprintf f "%s@."
              (Yojson.Basic.to_string (Model.to_yojson env)))
      | Dot | Html -> ()
    in
    let _ =
      Kappa_files.with_cflow_file [ compression_type; "Summary" ] ".dat"
        (fun form ->
          let () = Format.fprintf form "@[<v>#id\tE\tT\t\tdepth\tsize\t@," in
          let () =
            Pp.listi Pp.empty
              (fun cpt f (_, enriched_config, story) ->
                let depth = enriched_config.depth in
                let size = enriched_config.size in
                List.iter
                  (fun info ->
                    let time = info.Trace.Simulation_info.story_time in
                    let event = info.Trace.Simulation_info.story_event in
                    Format.fprintf f "%i\t%i\t%E\t%i\t%i\t@," cpt event time
                      depth size)
                  story)
              form story_list
          in
          Format.fprintf form "@]@?")
    in
    error, log_info

let print_stat f _parameter _handler enriched_grid =
  let count_obs =
    match snd enriched_grid.prec_star with
    | Graph_closure.Increasing_with_last_event -> fun x -> x
    | Graph_closure.Decreasing_without_last_event -> succ
  in
  let size = Array.length (fst enriched_grid.prec_star) in
  let rec aux k n_step longest_story n_nonempty length_sum length_square_sum =
    if k >= size then
      n_step, longest_story, n_nonempty, length_sum, length_square_sum
    else (
      let cc = List.length (Array.get (fst enriched_grid.prec_star) k) in
      let cc' =
        if cc > 0 then
          count_obs cc
        else
          cc
      in
      aux (k + 1) (n_step + 1) (max longest_story cc')
        (if cc > 0 then
           n_nonempty + 1
         else
           n_nonempty)
        (length_sum + cc')
        (length_square_sum + (cc' * cc'))
    )
  in
  let n_step, longest_story, n_nonempty, length_sum, length_square_sum =
    aux 0 0 0 0 0 0
  in
  let () = Format.fprintf f "@[<v>Stats:@," in
  let () = Format.fprintf f " number of step   : %i@," n_step in
  let () = Format.fprintf f " longest story    : %i@," longest_story in
  let () =
    Format.fprintf f " average length   : %.4g@,"
      (float length_sum /. float n_nonempty)
  in
  let () =
    Format.fprintf f " geometric mean   : %.4g@,"
      (sqrt (float length_square_sum /. float n_nonempty))
  in
  Format.fprintf f "@]@."
