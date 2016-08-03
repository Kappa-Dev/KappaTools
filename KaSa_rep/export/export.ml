(**
  * export.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: December, the 9th of 2014
  * Last modification: Time-stamp: <Aug 03 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Export")
    message exn (fun () -> default)

(*******************************************************************************)
(*module signatures*)
type state =
  (Domain_selection.Reachability_analysis.static_information,
   Domain_selection.Reachability_analysis.dynamic_information)
    Remanent_state.state
type contact_map = Remanent_state.contact_map
type ctmc_flow = Remanent_state.flow
type ode_flow = Ode_fragmentation_type.ode_frag
type c_compilation = Cckappa_sig.compil
type reachability_analysis =
  (Domain_selection.Reachability_analysis.static_information,
   Domain_selection.Reachability_analysis.dynamic_information)
    Remanent_state.reachability_result

type parameters = Remanent_parameters_sig.parameters
type errors = Exception.method_handler
type internal_contact_map = Remanent_state.internal_contact_map
type internal_influence_map =
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t *
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t
type handler = Cckappa_sig.kappa_handler

let string_of_influence_node x =
  match x with
  | Remanent_state.Rule i -> "Rule "^(string_of_int i)
  | Remanent_state.Var i -> "Var "^(string_of_int i)

let print_influence_map parameters influence_map =
  Loggers.fprintf (Remanent_parameters.get_logger parameters) "Influence map:" ;
  Loggers.print_newline (Remanent_parameters.get_logger parameters);
  Remanent_state.InfluenceNodeMap.iter
    (fun x y ->
       Remanent_state.InfluenceNodeMap.iter
         (fun y _labellist ->
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                " %s->%s"
                (string_of_influence_node x)
                (string_of_influence_node y)
            in
            let () =
              Loggers.print_newline
                (Remanent_parameters.get_logger parameters) in
            ())
         y)
    influence_map.Remanent_state.positive;
  Remanent_state.InfluenceNodeMap.iter
    (fun x y ->
       Remanent_state.InfluenceNodeMap.iter
         (fun y _labellist ->
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                " %s-|%s"
                (string_of_influence_node x) (string_of_influence_node y) in
            let () = Loggers.print_newline
                (Remanent_parameters.get_logger parameters) in
            ())
         y)
    influence_map.Remanent_state.negative;
  Loggers.print_newline
    (Remanent_parameters.get_logger parameters)

let print_contact_map parameters contact_map =
  Loggers.fprintf (Remanent_parameters.get_logger parameters)  "Contact map: ";
  Loggers.print_newline (Remanent_parameters.get_logger parameters) ;
  Mods.StringMap.iter
    (fun x ->
       Mods.StringMap.iter
         (fun y (l1,l2) ->
            if l1<>[]
            then
              begin
                let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s@%s: " x y in
                let _ = List.fold_left
                    (fun bool x ->
                       (if bool then
                          Loggers.fprintf (Remanent_parameters.get_logger parameters) ", ");
                       Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" x;
                       true)
                    false l1
                in
                Loggers.print_newline (Remanent_parameters.get_logger parameters)
              end
            else ();
            List.iter
              (fun (z,t) ->
                 Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s@%s--%s@%s" x y z t;
                 Loggers.print_newline (Remanent_parameters.get_logger parameters)
              ) l2
         )
    )
    contact_map


(*-------------------------------------------------------------------------------*)
(*operations of module signatures*)

let init ?compil ~called_from () =
  match
    compil
  with
  | Some compil ->
    let parameters = Remanent_parameters.get_parameters ~called_from () in
    let state = Remanent_state.create_state parameters (Remanent_state.Compil compil)
    in state
  | None ->
    begin
      match
        called_from
      with
      | Remanent_parameters_sig.Internalised
      | Remanent_parameters_sig.Server
      | Remanent_parameters_sig.KaSim
      | Remanent_parameters_sig.JS -> assert false
      | Remanent_parameters_sig.KaSa ->
        begin
          let errors = Exception.empty_error_handler in
          let errors,parameters,files  = Get_option.get_option errors in
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_full_version parameters) in
          let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
          let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (Remanent_parameters.get_launched_when_and_where parameters) in
          let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
          Remanent_state.create_state ~errors parameters (Remanent_state.Files files)
        end
    end

let get_parameters = Remanent_state.get_parameters
let set_parameters = Remanent_state.set_parameters
let set_errors = Remanent_state.set_errors
let get_errors = Remanent_state.get_errors

let title_only_in_kasa parameters =
  match
    Remanent_parameters.get_called_from parameters
  with
  | Remanent_parameters_sig.JS
  | Remanent_parameters_sig.Server
  | Remanent_parameters_sig.Internalised
  | Remanent_parameters_sig.KaSim -> false
  | Remanent_parameters_sig.KaSa -> true

let compute_show_title do_we_show_title log_title =
  (fun state ->
     let parameters = Remanent_state.get_parameters state in
     if do_we_show_title parameters
     then
       match log_title with
       | None -> ()
       | Some title ->
         let title =
           if title_only_in_kasa parameters
           then title^"..."
           else
             "+ "^title
         in
         let () =
           Loggers.fprintf
             (Remanent_parameters.get_logger parameters) "%s" title
         in
         Loggers.print_newline (Remanent_parameters.get_logger parameters)
     else
       ())

let get_gen
    ?debug_mode
    ?dump_result
    ?stack_title
    ?do_we_show_title
    ?log_title
    ?log_main_title
    ?log_prefix
    ?phase
    ?int
    ?dump
    get compute state =
  let debug_mode =
    match debug_mode with
    | None | Some false -> false
    | Some true -> true
  in
  let dump_result =
    match dump_result with
    | None | Some false -> false
    | Some true -> true
  in
  let dump =
    match dump with
    | None -> (fun state _output -> state)
    | Some f -> f
  in
  let do_we_show_title =
    match do_we_show_title with
    | None -> (fun _ -> true)
    | Some f -> f
  in
  match
    get state
  with
  | None ->
    let parameters = Remanent_state.get_parameters state in
    let parameters' =
      Remanent_parameters.update_call_stack
        parameters debug_mode stack_title
    in
    let parameters' =
      match log_prefix with
      | None -> parameters'
      | Some prefix -> Remanent_parameters.set_prefix parameters' prefix
    in
    let state = Remanent_state.set_parameters parameters' state in
    let () =
      match log_main_title with
      | None -> ()
      | Some title ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters) "%s" title
        in
        Loggers.print_newline (Remanent_parameters.get_logger parameters')
    in
    let show_title = compute_show_title do_we_show_title log_title in
    let state =
      match phase
      with
      | None -> state
      | Some phase -> Remanent_state.add_event phase int state
    in
    let state, output = compute show_title state in
    let state =
      match phase
      with
      | None -> state
      | Some phase -> Remanent_state.close_event phase int state
    in
    let state =
      if
        Remanent_parameters.get_trace parameters' || dump_result
      then
        dump state output
      else
        state
    in
    Remanent_state.set_parameters parameters state,
    output
  | Some a -> state, a

let lift_wo_handler f = (fun parameter error _handler x -> f parameter error x)
let flush_errors state =
  Remanent_state.set_errors Exception.empty_error_handler state

let compute_compilation show_title state =
  let compil =
    match Remanent_state.get_init state
    with
    | Remanent_state.Compil compil -> compil
    | Remanent_state.Files files ->
      let () = show_title state in
      List.fold_left (KappaLexer.compile Format.std_formatter) Ast.empty_compil files
  in
  let state = Remanent_state.set_compilation compil state in
  state, compil

let get_compilation =
  get_gen
    ~phase:StoryProfiling.KaSa_lexing
    Remanent_state.get_compilation
    compute_compilation

let compute_refined_compil show_title state =
  let state,compil = get_compilation state in
  let errors = Remanent_state.get_errors state in
  let parameters = Remanent_state.get_parameters state in
  let () = show_title state in
  let errors,refined_compil =
    Prepreprocess.translate_compil parameters errors compil
  in
  let state = Remanent_state.set_errors errors state in
  let state = Remanent_state.set_refined_compil refined_compil state in
  state, refined_compil

let get_refined_compil =
  get_gen
    ~debug_mode:Preprocess.local_trace
    ~stack_title:"Prepreprocess.translate_compil"
    ~phase:StoryProfiling.KaSim_compilation
    Remanent_state.get_refined_compil
    compute_refined_compil

let compute_prehandler show_title state =
  let state, refined_compil = get_refined_compil state in
  let parameters = Remanent_state.get_parameters state in
  let errors = Remanent_state.get_errors state in
  let () = show_title state in
  let errors, handler =
    List_tokens.scan_compil parameters errors refined_compil
  in
  let state = Remanent_state.set_errors errors state in
  let state = Remanent_state.set_handler handler state in
  state, handler

let lift_dump_parameter_error dump state output =
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error = dump parameters error output in
  Remanent_state.set_errors error state

let get_prehandler =
  get_gen
    ~debug_mode:List_tokens.local_trace
    ~dump_result:Print_handler.trace
    ~stack_title:"List_tokens.scan_compil"
    ~log_prefix:"Signature:"
    ~phase:StoryProfiling.KaSa_lexing
    Remanent_state.get_handler
    compute_prehandler
    ~dump:(lift_dump_parameter_error Print_handler.print_handler)

let compute_c_compilation_handler show_title state =
  let parameters = Remanent_state.get_parameters state in
  let state, refined_compil = get_refined_compil state in
  let state, handler = get_prehandler state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let error, handler, c_compil =
    Preprocess.translate_c_compil
      parameters error handler refined_compil
  in
  Remanent_state.set_errors
    error
    (Remanent_state.set_handler handler
       (Remanent_state.set_c_compil c_compil state)),
  (c_compil,handler)

let choose f show_title state =
  let state,pair = compute_c_compilation_handler show_title state in
  state,f pair

let get_c_compilation =
  get_gen
    ~debug_mode:List_tokens.local_trace
    ~stack_title:"Preprocess.translate_c_compil"
    ~log_prefix:"Compilation:"
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Compiling"
    ~phase:StoryProfiling.KaSa_linking
    Remanent_state.get_c_compil (choose fst)

let get_handler =
  get_gen
    ~debug_mode:List_tokens.local_trace
    ~stack_title:"Preprocess.translate_c_compil"
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Compiling"
    ~phase:StoryProfiling.KaSa_linking
    Remanent_state.get_handler (choose snd)

let dump_c_compil state c_compil =
  let state, handler = get_handler state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error = Print_cckappa.print_compil parameters error handler c_compil in
  let state = Remanent_state.set_errors error state in
  state

let compute_raw_internal_contact_map show_title state =
  let state, _ = get_compilation  state in
  let state, handler = get_handler state in
  let () = show_title state in
  let state, c_compil = get_c_compilation state in
  let parameters = Remanent_state.get_parameters state in
  let parameters = Remanent_parameters.update_prefix  parameters "Compilation:" in
  let error = Remanent_state.get_errors state in
  let error =
    if Remanent_parameters.get_trace parameters || Print_cckappa.trace
    then Print_cckappa.print_compil parameters error handler c_compil
    else error
  in
  let error, contact_map =
    Preprocess.export_contact_map parameters error handler
  in
  let state = Remanent_state.set_errors error state in
  Remanent_state.set_internal_contact_map Remanent_state.Low contact_map state,
  contact_map

let dump_raw_internal_contact_map state  handler =
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error = Print_handler.dot_of_contact_map parameters error handler in
  Remanent_state.set_errors error state

let get_raw_internal_contact_map  =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Generating the raw contact map"
    (*  ~dump:dump_raw_internal_contact_map *)
    (Remanent_state.get_internal_contact_map Remanent_state.Low)
    compute_raw_internal_contact_map

let compute_raw_contact_map show_title state =
  let sol        = ref Mods.StringMap.empty in
  let state, handler = get_prehandler state in
  let parameters = Remanent_state.get_parameters state in
  let error      = Remanent_state.get_errors state in
  let add_link (a,b) (c,d) sol =
    let sol_a = Mods.StringMap.find_default Mods.StringMap.empty a sol in
    let l,old = Mods.StringMap.find_default ([],[]) b sol_a in
    Mods.StringMap.add a (Mods.StringMap.add b (l,((c,d)::old)) sol_a) sol
  in
  (*----------------------------------------------------------------*)
  let add_internal_state (a,b) c sol =
    match c with
    | Ckappa_sig.Binding _ -> sol
    | Ckappa_sig.Internal state ->
      let sol_a = Mods.StringMap.find_default Mods.StringMap.empty a sol in
      let old,l = Mods.StringMap.find_default ([],[]) b sol_a in
      Mods.StringMap.add a (Mods.StringMap.add b (state::old,l) sol_a) sol
  in
  (*----------------------------------------------------------------*)
  let simplify_site site =
    match site with
    | Ckappa_sig.Binding site_name
    | Ckappa_sig.Internal site_name -> site_name
  in
  (*----------------------------------------------------------------*)
  let () = show_title state in
  let error =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.iter
      parameters error
      (fun parameters error (i,j) s  ->
         let error,ag =
           Handler.translate_agent parameters error handler i
         in
         let error,site =
           Handler.translate_site parameters error handler i j
         in
         let site = simplify_site site in
         let error =
           Ckappa_sig.Dictionary_of_States.iter
             parameters error
             (fun _parameters error _s state  () () ->
                let () =
                  sol := add_internal_state (ag,site) state (!sol)
                in
                error)
             s
         in
         error)
      handler.Cckappa_sig.states_dic
  in
  (*----------------------------------------------------------------*)
  let sol = !sol in
  let error, sol =
    Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
      parameters error
      (fun _parameters error (i, (j , _k)) (i', j', _k') sol ->
         let error, ag_i =
           Handler.translate_agent parameters error handler i
         in
         let error, site_j =
           Handler.translate_site parameters error handler i j
         in
         let site_j = simplify_site site_j in
         let error, ag_i' =
           Handler.translate_agent parameters error handler i'
         in
         let error, site_j' =
           Handler.translate_site parameters error handler i' j'
         in
         let site_j' = simplify_site site_j' in
         let sol = add_link (ag_i,site_j) (ag_i',site_j') sol in
         error, sol)
      handler.Cckappa_sig.dual sol
  in
  let sol =
    Mods.StringMap.map (Mods.StringMap.map (fun (l,x) -> List.rev l,x)) sol
  in
  Remanent_state.set_errors error
    (Remanent_state.set_contact_map Remanent_state.Low sol state),
  sol

let get_raw_contact_map =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Compute the contact map"
    (Remanent_state.get_contact_map Remanent_state.Low)
    compute_raw_contact_map


let convert_label a =
  if a<0 then Remanent_state.Side_effect (-(a+1))
  else Remanent_state.Direct a

let convert_id x nrules =
  if x<nrules
  then
    Remanent_state.Rule x
  else
    Remanent_state.Var (x-nrules)

let convert_half_influence_map influence nrules  =
  Ckappa_sig.PairRule_setmap.Map.fold
    (fun (x,y) list map ->
       let x = convert_id (int_of_string (Ckappa_sig.string_of_rule_id x)) nrules in
       let y = convert_id (int_of_string (Ckappa_sig.string_of_rule_id y)) nrules in
       let old =
         match
           Remanent_state.InfluenceNodeMap.find_option x map
         with
         | None -> Remanent_state.InfluenceNodeMap.empty
         | Some x -> x
       in
       let list =
         Quark_type.Labels.convert_label_set_couple list
       in
       let list =
         List.rev_map
           (fun (a,b) -> convert_label a,convert_label b)
           (List.rev list)
       in
       Remanent_state.InfluenceNodeMap.add x
         (Remanent_state.InfluenceNodeMap.add y list old)
         map
    )
    influence
    Remanent_state.InfluenceNodeMap.empty

let compute_quark_map show_title state =
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let error,quark_map =
    Quark.quarkify parameters error handler c_compil
  in
  let error =
    if
      (Remanent_parameters.get_trace parameters)
      || Print_quarks.trace
    then
      Print_quarks.print_quarks parameters error handler quark_map
    else
      error
  in
  Remanent_state.set_errors error
    (Remanent_state.set_quark_map quark_map state),
  quark_map

let get_quark_map =
  get_gen
    ~debug_mode:Quark.local_trace
    ~stack_title:"Quark.quarkify"
    ~log_prefix:"Quarks:"
    Remanent_state.get_quark_map
    compute_quark_map

let compute_raw_internal_influence_map show_title state =
  let parameters = Remanent_state.get_parameters state in
  let state, compil = get_c_compilation state in
  let state, quark_map = get_quark_map state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let nrules = Handler.nrules parameters error handler in
  let () = show_title state in
  let error,wake_up_map,inhibition_map =
    Influence_map.compute_influence_map parameters
      error handler quark_map nrules
  in
  let error =
    if
      (Remanent_parameters.get_trace parameters  || Print_quarks.trace)
      && Remanent_parameters.get_influence_map_accuracy_level parameters = Remanent_parameters_sig.Low
    then
      Print_quarks.print_wake_up_map
        parameters
        error
        handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels "\n"
        wake_up_map
    else error
  in
  let error =
    if
      (Remanent_parameters.get_trace parameters  || Print_quarks.trace)
      && Remanent_parameters.get_influence_map_accuracy_level parameters = Remanent_parameters_sig.Low
    then
      Print_quarks.print_inhibition_map
        parameters error handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels
        "\n"
        inhibition_map
    else error
  in
  let state =
    Remanent_state.set_internal_influence_map Remanent_state.Low
      (wake_up_map,inhibition_map)
      state
  in
  Remanent_state.set_errors error state,
  (wake_up_map, inhibition_map)

let get_raw_internal_influence_map =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_prefix:"Influence_map:"
    ~log_main_title:"Generating the raw influence map..."
    ~phase:(StoryProfiling.Internal_influence_map "raw")
    (Remanent_state.get_internal_influence_map Remanent_state.Low)
    compute_raw_internal_influence_map


module AgentProj =
  Map_wrapper.Proj
    (Ckappa_sig.Agent_map_and_set)
    (Map_wrapper.Make(Mods.StringSetMap))

module SiteProj =
  Map_wrapper.Proj
    (Ckappa_sig.Site_map_and_set)
    (Map_wrapper.Make(Mods.StringSetMap))

module StateProj =
  Map_wrapper.Proj
    (Ckappa_sig.State_map_and_set)
    (Map_wrapper.Make(Mods.StringSetMap))



let convert_contact_map show_title state  contact_map =
  let parameters = Remanent_state.get_parameters state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let error, contact_map =
    AgentProj.monadic_proj_map_i
      (fun parameters error (ag:Ckappa_sig.c_agent_name) ->
         (Handler.translate_agent parameters error handler ag:Exception.method_handler * Ckappa_sig.agent_name))
      parameters error
      Mods.StringMap.empty
      (fun parameters error _ ag sitemap->
         SiteProj.monadic_proj_map_i
           (fun parameters errors site ->
              let error, site = Handler.translate_site parameters errors handler ag (site:Ckappa_sig.c_site_name) in
              error, Handler.print_site_contact_map site)
           parameters error
           ([],[])
           (fun parameters error (list_a,list_b) site (list_a',list_b') ->
              let error, list_a'' =
                List.fold_left
                  (fun (error, list) state ->
                     let error, state = Handler.translate_state parameters error handler ag site state in
                     match state with
                     | Ckappa_sig.Internal state -> error, state::list
                     | Ckappa_sig.Binding _ ->
                       warn parameters error (Some "line 645") Exit list)
                  (error, list_a) (List.rev list_a')
              in
              let error, list_b'' =
                List.fold_left
                  (fun (error, list) (agent,site) ->
                     let error, ag = Handler.translate_agent parameters error handler agent in
                     let error, site = Handler.translate_site parameters error handler agent site in
                     let st = Handler.print_site_contact_map site in
                     error, (ag,st)::list)
                  (error, list_b) (List.rev list_b')
              in
              error, (list_a'', list_b''))
           sitemap)
      contact_map
  in
  Remanent_state.set_errors error state,
  contact_map


let convert_influence_map show_title state (wake_up_map, inhibition_map) =
  let parameters = Remanent_state.get_parameters state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let nrules = Handler.nrules parameters error handler in
  let state = Remanent_state.set_errors error state in
  let output =
    {
      Remanent_state.positive = convert_half_influence_map wake_up_map nrules ;
      Remanent_state.negative = convert_half_influence_map inhibition_map nrules ;
    }
  in
  let state =
    Remanent_state.set_influence_map
      Remanent_state.Low
      output
      state
  in
  state,
  output


let compute_intermediary_internal_influence_map show_title state =
  let state, handler = get_handler state in
  let state, compil = get_c_compilation state in
  let state,(wake_up_map,inhibition_map) =
    get_raw_internal_influence_map state
  in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let () = show_title state in
  let error,wake_up_map =
    Algebraic_construction.filter_influence
      parameters error handler compil wake_up_map true
  in
  let error,inhibition_map =
    Algebraic_construction.filter_influence
      parameters error handler compil inhibition_map false
  in
  let state =
    Remanent_state.set_internal_influence_map Remanent_state.Medium
      (wake_up_map,inhibition_map)
      state
  in
  let state = Remanent_state.set_errors error state in
  let state, handler = get_handler state in
  let error = Remanent_state.get_errors state in
  let nrules = Handler.nrules parameters error handler in
  let state =
    Remanent_state.set_influence_map Remanent_state.Medium
      {
        Remanent_state.positive = convert_half_influence_map wake_up_map nrules ;
        Remanent_state.negative = convert_half_influence_map inhibition_map nrules ;
      }
      state
  in
  let error =
    if Remanent_parameters.get_trace parameters || Print_quarks.trace
    then
      Print_quarks.print_wake_up_map
        parameters
        error
        handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels "\n"
        wake_up_map
    else error
  in
  let error =
    if
      Remanent_parameters.get_trace parameters
      || Print_quarks.trace
    then
      Print_quarks.print_inhibition_map
        parameters error handler
        compil
        Handler.print_rule_txt
        Handler.print_var_txt
        Handler.get_label_of_rule_txt
        Handler.get_label_of_var_txt
        Handler.print_labels
        "\n"
        inhibition_map
    else error
  in
  Remanent_state.set_errors error state, (wake_up_map, inhibition_map)

let get_map_gen
    (get: ?accuracy_level:Remanent_state.accuracy_level ->
     (Domain_selection.Reachability_analysis.static_information,
      Domain_selection.Reachability_analysis.dynamic_information) Remanent_state.state ->
     (Domain_selection.Reachability_analysis.static_information,
      Domain_selection.Reachability_analysis.dynamic_information) Remanent_state.state * 'a )
    convert ?accuracy_level:(accuracy_level=Remanent_state.Low)
    ?do_we_show_title:(do_we_show_title=(fun _ -> true))
    ?log_title
    state =
  let show_title =
    match log_title with
    | None -> (fun _ -> ())
    | Some log_title ->
      compute_show_title do_we_show_title (log_title accuracy_level)
  in
  let () = show_title state in
  let state, internal =
    get ~accuracy_level:accuracy_level state
  in
  convert (fun _ -> ()) state internal

let get_intermediary_internal_influence_map =
  get_gen
    ~log_prefix:"Influence_map:"
    ~log_title:"Refining the influence map"
    ~phase:(StoryProfiling.Internal_influence_map "medium")
    (Remanent_state.get_internal_influence_map Remanent_state.Medium)
    compute_intermediary_internal_influence_map



let get_internal_influence_map ?accuracy_level:(accuracy_level=Remanent_state.Low)
    state =
  match
    accuracy_level
  with
  | Remanent_state.Low ->
    get_raw_internal_influence_map state
  | Remanent_state.Medium | Remanent_state.High | Remanent_state.Full ->
    get_intermediary_internal_influence_map state

let get_influence_map =
  get_map_gen
    get_internal_influence_map
    convert_influence_map



let compute_reachability_result show_title state =
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let bdu_handler = Remanent_state.get_bdu_handler state in
  let log_info = Remanent_state.get_log_info state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error, log_info, static, dynamic =
    Domain_selection.Reachability_analysis.main
      parameters log_info error bdu_handler c_compil handler
  in
  let error, dynamic, state =
    Domain_selection.Reachability_analysis.export static dynamic
      error state in
  let state = Remanent_state.set_errors error state in
  let state = Remanent_state.set_log_info log_info state in
  let state = Remanent_state.set_bdu_handler bdu_handler state in
  let state = Remanent_state.set_reachability_result (static, dynamic) state in
  state, (static, dynamic)

let get_reachability_analysis =
  get_gen
    ~log_title:"Reachability analysis"
    (Remanent_state.get_reachability_result)
    compute_reachability_result


let compute_dead_rules _show_title state =
  let state,_ = get_reachability_analysis state in
  match
    Remanent_state.get_dead_rules state
  with
  | Some map -> state, map
  | None -> assert false


let get_dead_rules  =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Detecting which rules may be triggered during simulations"
    (*  ~dump:dump_raw_internal_contact_map *)
    Remanent_state.get_dead_rules
    compute_dead_rules


let compute_dead_agents _show_title state =
  let state,_ = get_reachability_analysis state in
  match
    Remanent_state.get_dead_agents state
  with
  | Some map -> state, map
  | None -> assert false


let get_dead_agents  =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Detecting which agents may occur during simulations"
    (*  ~dump:dump_raw_internal_contact_map *)
    Remanent_state.get_dead_agents
    compute_dead_agents


let compute_intermediary_internal_contact_map _show_title state =
  let state,_ = get_reachability_analysis state in
  match
    Remanent_state.get_internal_contact_map Remanent_state.Medium state
  with
  | Some map -> state, map
  | None -> assert false


let get_intermediary_internal_contact_map =
  get_gen
    ~do_we_show_title:title_only_in_kasa
    ~log_title:"Generating the intermediary contact map"
    (*  ~dump:dump_raw_internal_contact_map *)
    (Remanent_state.get_internal_contact_map Remanent_state.Medium)
    compute_intermediary_internal_contact_map

let get_internal_contact_map ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
  match
    accuracy_level
  with
  | Remanent_state.Low -> get_raw_internal_contact_map state
  | Remanent_state.Medium
  | Remanent_state.High
  | Remanent_state.Full -> get_intermediary_internal_contact_map state


let get_contact_map =
  get_map_gen
    ~do_we_show_title:(fun _ -> true)
    ~log_title:(fun x ->
        match
          x
        with
        | Remanent_state.Low ->
          Some "Compute the contact map"
        | Remanent_state.Medium
        | Remanent_state.High | Remanent_state.Full ->
          Some "Refine the contact map")
    get_internal_contact_map
    convert_contact_map

let compute_signature show_title state =
  let state,l = get_contact_map state in
  let () = show_title state in
  let l =
    Mods.StringMap.fold
      (fun a interface list ->
         (Location.dummy_annot a ,
          Mods.StringMap.fold
            (fun x (states,_binding) acc ->
               {
                 Ast.port_nme = Location.dummy_annot x ;
                 Ast.port_int =
                   List.rev_map
                     (fun s -> Location.dummy_annot s)
                     (List.rev states);
                 Ast.port_lnk = Location.dummy_annot Ast.FREE}::acc)
            interface [])::list)
      l [] in
  let signature = Signature.create l in
  Remanent_state.set_signature signature state,
  signature

let get_signature =
  get_gen
    Remanent_state.get_signature
    compute_signature

let compute_ctmc_flow show_title state =
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error, output = Stochastic_classes.stochastic_classes parameters error handler c_compil in
  Remanent_state.set_errors
    error
    (Remanent_state.set_ctmc_flow output state),
  output

let get_ctmc_flow =
  get_gen
    ~log_prefix:"Flow of information in the CTMC semantics:"
    ~log_title:"Flow of information in the CTMC semantcs:"
    Remanent_state.get_ctmc_flow
    compute_ctmc_flow

let compute_ode_flow show_title state =
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let () = show_title state in
  let parameters = Remanent_state.get_parameters state in
  let error = Remanent_state.get_errors state in
  let error, output = Ode_fragmentation_main.ode_fragmentation
      parameters error handler c_compil
  in
  Remanent_state.set_errors
    error
    (Remanent_state.set_ode_flow output state),
  output

let get_ode_flow =
  get_gen
    ~log_prefix:"Flow of information in the ODE semantics:"
    ~log_title:"Flow of information in the ODE semantcs:"
    Remanent_state.get_ode_flow
    compute_ode_flow

let find_most_precise map =
  match
    Remanent_state.AccuracyMap.max_key map
  with
  | None -> None
  | Some key ->
    Remanent_state.AccuracyMap.find_option key map

let get_most_accurate_contact_map state =
  let map = Remanent_state.get_contact_map_map state in
  find_most_precise map

let get_most_accurate_influence_map state =
  let map = Remanent_state.get_influence_map_map state in
  find_most_precise map

let dump_influence_map ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
  match
    Remanent_state.get_influence_map accuracy_level state
  with
  | None -> ()
  | Some influence_map ->
    print_influence_map (Remanent_state.get_parameters state) influence_map

let output_internal_influence_map ?loggers ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
  let parameters = get_parameters state in
  let state, influence_map = get_internal_influence_map ~accuracy_level state in
  let state, c_compil = get_c_compilation state in
  let state, handler = get_handler state in
  let error = get_errors state in
  let error =
    Print_quarks.dot_of_influence_map ?loggers parameters error handler c_compil influence_map
  in
  set_errors error state

let output_best_internal_influence_map state =
  let map = Remanent_state.get_internal_influence_map_map state in
  match
    Remanent_state.AccuracyMap.max_key map
  with
  | None -> state
  | Some accuracy_level ->
    output_internal_influence_map ~accuracy_level state

let dump_contact_map accuracy state =
  match
    Remanent_state.get_contact_map accuracy state
  with
  | None -> ()
  | Some contact_map ->
    print_contact_map (Remanent_state.get_parameters state) contact_map

let output_internal_contact_map ?loggers ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
  let parameters = Remanent_state.get_parameters state in
  let state, contact_map = get_internal_contact_map ~accuracy_level state in
  let state, handler = get_handler state in
  let error = get_errors state in
  let error = Preprocess.dot_of_contact_map ?loggers parameters error handler contact_map in
  set_errors error state

let dump_signature state =
  match
    Remanent_state.get_signature state
  with
  | None -> ()
  | Some _signature -> ()

let dump_errors state =
  Exception.print (Remanent_state.get_parameters state)
    (Remanent_state.get_errors state)

let dump_errors_light state =
  Exception.print_errors_light_for_kasim
    (Remanent_state.get_parameters state)
    (Remanent_state.get_errors state)
