(**
  * export.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: December, the 9th of 2014
  * Last modification: December, the 9th of 2014
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

module type Export =
sig

  val init:
    called_from:Remanent_parameters_sig.called_from ->
    ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil -> Remanent_state.state

  val flush_errors:
    Remanent_state.state -> Remanent_state.state

  val get_contact_map:
    ?accuracy_level:Remanent_state.accuracy_level ->
    Remanent_state.state -> Remanent_state.state * Remanent_state.contact_map

  val get_influence_map:
    ?accuracy_level:Remanent_state.accuracy_level ->
    Remanent_state.state -> Remanent_state.state * Remanent_state.influence_map

  val get_signature: Remanent_state.state -> Remanent_state.state * Signature.s

  val get_most_accurate_contact_map:
    Remanent_state.state -> Remanent_state.contact_map option

  val get_most_accurate_influence_map: Remanent_state.state -> Remanent_state.influence_map option

  val dump_influence_map: Remanent_state.accuracy_level -> Remanent_state.state -> unit
  val dump_contact_map: Remanent_state.accuracy_level -> Remanent_state.state -> unit
  val dump_signature: Remanent_state.state -> unit
  val dump_errors: Remanent_state.state -> unit
  val dump_errors_light: Remanent_state.state -> unit

end

(*******************************************************************************)
(*structures of module*)

module Export =
  (struct

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

    let init ~called_from compil =
      let parameters = Remanent_parameters.get_parameters ~called_from () in
      let state = Remanent_state.create_state parameters compil in
      let () =
        match called_from with
        | Remanent_parameters_sig.Internalised -> assert false
        | Remanent_parameters_sig.Server
        | Remanent_parameters_sig.KaSim
        | Remanent_parameters_sig.JS
        | Remanent_parameters_sig.KaSa -> ()
      in
      state

    let get_gen trace stack_title_option log_option prefix_option phase int get compute state =
      match
        get state
      with
      | None ->
        let parameters = Remanent_state.get_parameters state in
        let parameters' =
          Remanent_parameters.update_call_stack
            parameters trace stack_title_option
        in
        let parameters' =
          match prefix_option with
          | None -> parameters'
          | Some prefix -> Remanent_parameters.update_prefix parameters' prefix
        in
        let state = Remanent_state.set_parameters parameters' state in
        let () =
          match log_option with
          | None -> ()
          | Some title ->
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters) "%s" title
            in
            Loggers.print_newline (Remanent_parameters.get_logger parameters')
        in
        let state =
          match phase
          with
          | None -> state
          | Some phase -> Remanent_state.add_event phase int state
        in
        let state, output = compute state in
        let state =
          match phase
          with
          | None -> state
          | Some phase -> Remanent_state.close_event phase int state
        in
        Remanent_state.set_parameters parameters state,
        output
      | Some a -> state, a

    let flush_errors state =
      Remanent_state.set_errors Exception.empty_error_handler state

    let compute_refined_compil state =
      let compil = Remanent_state.get_compilation state in
      let errors = Remanent_state.get_errors state in
      let parameters = Remanent_state.get_parameters state in
      let errors,refined_compil =
        Prepreprocess.translate_compil parameters errors compil
      in
      let state = Remanent_state.set_errors errors state in
      let state = Remanent_state.set_refined_compil refined_compil state in
      state, refined_compil

    let get_refined_compil =
      get_gen
        Preprocess.local_trace
        (Some "Prepreprocess.translate_compil")
        None
        None
        (Some StoryProfiling.KaSim_compilation)
        None
        Remanent_state.get_refined_compil
        compute_refined_compil

    let compute_handler state =
      let state, refined_compil = get_refined_compil state in
      let parameters = Remanent_state.get_parameters state in
      let errors = Remanent_state.get_errors state in
      let errors, handler =
        List_tokens.scan_compil parameters errors refined_compil
      in
      let state = Remanent_state.set_errors errors state in
      let state = Remanent_state.set_handler handler state in
      state, handler

    let get_handler =
      get_gen
        List_tokens.local_trace
        (Some "List_tokens.scan_compil")
        None
        None
        (Some StoryProfiling.KaSa_lexing)
        None
        Remanent_state.get_handler compute_handler

    let compute_c_compilation state =
      let parameters = Remanent_state.get_parameters state in
      let state, refined_compil = get_refined_compil state in
      let state, handler = get_handler state in
      let error = Remanent_state.get_errors state in
      let error, handler, c_compil =
        Preprocess.translate_c_compil
          parameters error handler refined_compil
      in
      Remanent_state.set_errors
        error
        (Remanent_state.set_handler handler
           (Remanent_state.set_c_compil c_compil state)),
      c_compil

    let get_c_compilation =
      get_gen
      List_tokens.local_trace
      (Some "Preprocess.translate_c_compil")
      (Some "Compiling...")
      None
      (Some StoryProfiling.KaSa_linking)
      None
      Remanent_state.get_c_compil compute_c_compilation

    let compute_raw_contact_map state =
      let sol        = ref Mods.StringMap.empty in
      let state, handler = get_handler state in
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
      false
      None
      (Some "+ Compute the contact map")
      None
      None
      None
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

    let convert_influence_map influence nrules  =
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

    let compute_quark_map state =
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let state, handler = get_handler state in
      let state, c_compil = get_c_compilation state in
      let error,quark_map =
        Quark.quarkify parameters error handler c_compil
      in
      Remanent_state.set_errors error
        (Remanent_state.set_quark_map quark_map state),
      quark_map

    let get_quark_map =
      get_gen
        Quark.local_trace
        (Some "Quark.quarkify")
        (Some "Quarks:")
        None
        None
        None
        Remanent_state.get_quark_map
        compute_quark_map

    let compute_raw_internal_influence_map state =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let error = Remanent_state.get_errors state in
      let nrules = Handler.nrules parameters error handler in
      let state, quark_map = get_quark_map state in
      let error,wake_up_map,inhibition_map =
        Influence_map.compute_influence_map parameters
          error handler quark_map nrules
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
        true
        None
        (Some "Influence_map: (internal)")
        (Some "Generating the raw influence map (internal)...")
        (Some (StoryProfiling.Internal_influence_map "raw"))
        None
        (Remanent_state.get_internal_influence_map Remanent_state.Low)
        compute_raw_internal_influence_map

    let compute_raw_influence_map state =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let error = Remanent_state.get_errors state in
      let nrules = Handler.nrules parameters error handler in
      let state, (wake_up_map, inhibition_map) =
        get_raw_internal_influence_map state
      in
      let output =
        {
          Remanent_state.positive = convert_influence_map wake_up_map nrules ;
          Remanent_state.negative = convert_influence_map inhibition_map nrules ;
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

    let get_raw_influence_map =
      get_gen
      true
      None
      (Some "Influence_map:")
      (Some "Generating the raw influence map...")
      (Some (StoryProfiling.Influence_map "raw"))
      None
      (Remanent_state.get_influence_map Remanent_state.Low)
      compute_raw_influence_map


    let compute_intermediary_internal_influence_map state =
      let state, handler = get_handler state in
      let state, compil = get_c_compilation state in
      let state,(wake_up_map,inhibition_map) =
        get_raw_internal_influence_map state
      in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let nrules = Handler.nrules parameters error handler in
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
      let state =
        Remanent_state.set_influence_map Remanent_state.Medium
          {
            Remanent_state.positive = convert_influence_map wake_up_map nrules ;
            Remanent_state.negative = convert_influence_map inhibition_map nrules ;
          }
          state
      in
      let state = Remanent_state.set_errors error state in
      state, (wake_up_map, inhibition_map)

    let get_intermediary_internal_influence_map =
      get_gen
      true
      None
      (Some "Influence_map:")
      (Some "+refining the influence map")
      (Some (StoryProfiling.Internal_influence_map "medium"))
      None
        (Remanent_state.get_internal_influence_map Remanent_state.Medium)
        compute_intermediary_internal_influence_map

    let compute_intermediary_influence_map state =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let error = Remanent_state.get_errors state in
      let nrules = Handler.nrules parameters error handler in
      let state, (wake_up_map, inhibition_map) =
        get_intermediary_internal_influence_map state
      in
      let output =
        {
          Remanent_state.positive = convert_influence_map wake_up_map nrules ;
          Remanent_state.negative = convert_influence_map inhibition_map nrules ;
        }
      in
      let state =
        Remanent_state.set_influence_map
          Remanent_state.Medium
          output
          state
      in
      state,
      output

    let get_intermediary_influence_map =
      get_gen
      true
      None
      (Some "Influence_map:")
      (Some "+refining the influence map")
      (Some (StoryProfiling.Internal_influence_map "medium"))
      None
        (Remanent_state.get_influence_map Remanent_state.Medium)
        compute_intermediary_influence_map


    let get_contact_map ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
      match
        accuracy_level
      with
      | Remanent_state.Low
      | Remanent_state.Medium
      | Remanent_state.High
      | Remanent_state.Full -> get_raw_contact_map state

    let get_influence_map ?accuracy_level:(accuracy_level=Remanent_state.Low)
        state =
      match
        accuracy_level
      with
      | Remanent_state.Low ->
        get_raw_influence_map state
      | Remanent_state.Medium | Remanent_state.High | Remanent_state.Full ->
        get_intermediary_influence_map state


    let compute_signature state =
      let state,l = get_contact_map state in
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
        false
        None
        None
        None
        None
        None
        Remanent_state.get_signature
        compute_signature

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

    let dump_influence_map accuracy state =
      match
        Remanent_state.get_influence_map accuracy state
      with
      | None -> ()
      | Some influence_map ->
        print_influence_map (Remanent_state.get_parameters state) influence_map

    let dump_contact_map accuracy state =
      match
        Remanent_state.get_contact_map accuracy state
      with
      | None -> ()
      | Some contact_map ->
        print_contact_map (Remanent_state.get_parameters state) contact_map

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

  end:Export)
