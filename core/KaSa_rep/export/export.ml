(**
  * export.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: December, the 9th of 2014
  * Last modification: Time-stamp: <Mar 19 2020>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let warn parameters mh pos exn default =
  Exception.warn parameters mh pos exn default

(****************************************************************)
(*module signatures*)

module Export =
functor
  (Reachability : Analyzer.Analyzer)
  ->
  struct
    type state =
      ( Reachability.static_information,
        Reachability.dynamic_information )
      Remanent_state.state

    type contact_map = Public_data.contact_map
    type ctmc_flow = Remanent_state.flow
    type ode_flow = Ode_fragmentation_type.ode_frag
    type c_compilation = Cckappa_sig.compil

    type reachability_analysis =
      ( Reachability.static_information,
        Reachability.dynamic_information )
      Remanent_state.reachability_result

    type parameters = Remanent_parameters_sig.parameters
    type errors = Exception.method_handler
    type internal_contact_map = Remanent_state.internal_contact_map
    type internal_scc_decomposition = Remanent_state.internal_scc_decomposition
    type internal_influence_map = Remanent_state.internal_influence_map

    type bidirectional_influence_map =
      Remanent_state.bidirectional_influence_map

    type handler = Cckappa_sig.kappa_handler
    type internal_constraints_list = Remanent_state.internal_constraints_list

    module AgentProj =
      Map_wrapper.Proj
        (Ckappa_sig.Agent_map_and_set)
        (Map_wrapper.Make (Mods.StringSetMap))

    module SiteProj =
      Map_wrapper.Proj
        (Ckappa_sig.Site_map_and_set)
        (Map_wrapper.Make (Mods.StringSetMap))

    module StateProj =
      Map_wrapper.Proj
        (Ckappa_sig.State_map_and_set)
        (Map_wrapper.Make (Mods.StringSetMap))

    (******************************************************************)
    (*operations of module signatures*)

    let init ?compil ~called_from () =
      match compil with
      | Some compil ->
        let parameters = Remanent_parameters.get_parameters ~called_from () in
        let state =
          Remanent_state.create_state parameters (Remanent_state.Compil compil)
        in
        state
      | None ->
        (match called_from with
        | Remanent_parameters_sig.Internalised | Remanent_parameters_sig.Server
        | Remanent_parameters_sig.KaSim | Remanent_parameters_sig.KaSa ->
          let errors = Exception.empty_error_handler in
          let errors, parameters, files = Get_option.get_option errors in
          let log = Remanent_parameters.get_logger parameters in
          let _ =
            Loggers.fprintf log "%s"
              (Remanent_parameters.get_full_version parameters)
          in
          let () = Loggers.print_newline log in
          let _ =
            Loggers.fprintf log "%s"
              (Remanent_parameters.get_launched_when_and_where parameters)
          in
          let () = Loggers.print_newline log in
          Remanent_state.create_state ~errors parameters
            (Remanent_state.Files files))

    let get_parameters = Remanent_state.get_parameters
    let set_parameters = Remanent_state.set_parameters
    let set_errors = Remanent_state.set_errors
    let get_errors = Remanent_state.get_errors

    let title_only_in_kasa parameters =
      match Remanent_parameters.get_called_from parameters with
      | Remanent_parameters_sig.Server | Remanent_parameters_sig.Internalised
      | Remanent_parameters_sig.KaSim ->
        false
      | Remanent_parameters_sig.KaSa -> true

    let compute_show_title do_we_show_title log_title state =
      let parameters = Remanent_state.get_parameters state in
      if do_we_show_title parameters then (
        match log_title with
        | None -> ()
        | Some title ->
          let title =
            if title_only_in_kasa parameters then
              title ^ "..."
            else
              "+ " ^ title
          in
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%s" title
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
      ) else
        ()

    let get_gen ?debug_mode ?dump_result ?stack_title ?do_we_show_title
        ?log_title ?log_main_title ?log_prefix ?phase ?int ?dump get compute
        state =
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
        | None -> fun state _output -> state
        | Some f -> f
      in
      let do_we_show_title =
        match do_we_show_title with
        | None -> fun _ -> true
        | Some f -> f
      in
      (*------------------------------------------------------*)
      match get state with
      | None ->
        let parameters = Remanent_state.get_parameters state in
        let parameters' =
          Remanent_parameters.update_call_stack parameters debug_mode
            stack_title
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
                (Remanent_parameters.get_logger parameters)
                "%s" title
            in
            Loggers.print_newline (Remanent_parameters.get_logger parameters')
        in
        let show_title = compute_show_title do_we_show_title log_title in
        (*------------------------------------------------------*)
        let state =
          match phase with
          | None -> state
          | Some phase -> Remanent_state.add_event phase int state
        in
        let state, output = compute show_title state in
        (*------------------------------------------------------*)
        let state =
          match phase with
          | None -> state
          | Some phase -> Remanent_state.close_event phase int state
        in
        (*------------------------------------------------------*)
        let state =
          if Remanent_parameters.get_trace parameters' || dump_result then
            dump state output
          else
            state
        in
        Remanent_state.set_parameters parameters state, output
      | Some a -> state, a

    let lift_wo_handler f parameter error _handler x = f parameter error x

    let flush_errors state =
      Remanent_state.set_errors Exception.empty_error_handler state

    let compute_env_init show_title
        (state :
          ( Reachability.static_information,
            Reachability.dynamic_information )
          Remanent_state.state) =
      match Remanent_state.get_init state with
      | Remanent_state.Compil _ -> state, None, None, None
      | Remanent_state.Files files ->
        let () = show_title state in
        let cli = Run_cli_args.default in
        let syntax_version =
          Remanent_parameters.get_syntax_version
            (Remanent_state.get_parameters state)
        in
        let () = cli.Run_cli_args.syntaxVersion <- syntax_version in
        let () = cli.Run_cli_args.inputKappaFileNames <- files in
        let compilation_result : Cli_init.compilation_result =
          Cli_init.get_compilation
            ~warning:(fun ~pos:_ _msg -> ())
            ~debug_mode:false cli
        in
        let state =
          Remanent_state.set_init_state compilation_result.init_l
            (Remanent_state.set_env (Some compilation_result.env)
               (Remanent_state.set_contact_map_int
                  (Some compilation_result.contact_map) state))
        in
        ( state,
          Some (compilation_result.env : Model.t),
          Some compilation_result.init_l,
          Some compilation_result.contact_map )

    let compute_env show_title state =
      let state, env, _, _ = compute_env_init show_title state in
      state, env

    let get_env =
      get_gen ~phase:StoryProfiling.LKappa_signature Remanent_state.get_env
        compute_env

    let compute_init show_title state =
      let state, _, init, _ = compute_env_init show_title state in
      state, init

    let get_init =
      get_gen ~phase:StoryProfiling.LKappa_signature
        (fun x -> Some (Remanent_state.get_init_state x))
        compute_init

    (******************************************************************)
    (*compilation*)

    let compute_compilation show_title state =
      let parameters = get_parameters state in
      let syntax_version = Remanent_parameters.get_syntax_version parameters in
      let compil =
        match Remanent_state.get_init state with
        | Remanent_state.Compil compil -> compil
        | Remanent_state.Files files ->
          let () = show_title state in
          Cli_init.get_ast_from_list_of_files syntax_version files
      in
      let state = Remanent_state.set_compilation compil state in
      state, compil

    let get_compilation =
      get_gen ~phase:StoryProfiling.KaSa_lexing Remanent_state.get_compilation
        compute_compilation

    (******************************************************************)

    let compute_refined_compil show_title state =
      let state, compil = get_compilation state in
      let errors = Remanent_state.get_errors state in
      let parameters = Remanent_state.get_parameters state in
      let () = show_title state in
      let errors, refined_compil =
        Prepreprocess.translate_compil parameters errors compil
      in
      let state = Remanent_state.set_errors errors state in
      let state = Remanent_state.set_refined_compil refined_compil state in
      state, refined_compil

    let get_refined_compil =
      get_gen ~debug_mode:Preprocess.local_trace
        ~stack_title:"Prepreprocess.translate_compil"
        ~phase:StoryProfiling.KaSim_compilation
        Remanent_state.get_refined_compil compute_refined_compil

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
      get_gen ~debug_mode:List_tokens.local_trace
        ~dump_result:Print_handler.trace ~stack_title:"List_tokens.scan_compil"
        ~log_prefix:"Signature:" ~phase:StoryProfiling.KaSa_lexing
        Remanent_state.get_handler compute_prehandler
        ~dump:(lift_dump_parameter_error Print_handler.print_handler)

    let compute_c_compilation_handler show_title state =
      let parameters = Remanent_state.get_parameters state in
      let state, refined_compil = get_refined_compil state in
      let state, handler = get_prehandler state in
      let error = Remanent_state.get_errors state in
      let () = show_title state in
      let error, handler, c_compil =
        Preprocess.translate_c_compil parameters error handler refined_compil
      in
      ( Remanent_state.set_errors error
          (Remanent_state.set_handler handler
             (Remanent_state.set_c_compil c_compil state)),
        (c_compil, handler) )

    (******************************************************************)

    let choose f show_title state =
      let state, pair = compute_c_compilation_handler show_title state in
      state, f pair

    let get_c_compilation =
      get_gen ~debug_mode:List_tokens.local_trace
        ~stack_title:"Preprocess.translate_c_compil" ~log_prefix:"Compilation:"
        ~do_we_show_title:title_only_in_kasa ~log_title:"Compiling"
        ~phase:StoryProfiling.KaSa_linking Remanent_state.get_c_compil
        (choose fst)

    let get_handler =
      get_gen ~debug_mode:List_tokens.local_trace
        ~stack_title:"Preprocess.translate_c_compil"
        ~do_we_show_title:title_only_in_kasa ~log_title:"Compiling"
        ~phase:StoryProfiling.KaSa_linking Remanent_state.get_handler
        (choose snd)

    let simplify_site site =
      match site with
      | Ckappa_sig.Counter site_name
      | Ckappa_sig.Binding site_name
      | Ckappa_sig.Internal site_name ->
        site_name

    let translate_agent ~message ~ml_pos state agent =
      let state, handler = get_handler state in
      let error = get_errors state in
      let parameters = get_parameters state in
      let error, ag =
        Handler.translate_agent ~message ~ml_pos parameters error handler agent
      in
      let state = set_errors error state in
      state, ag

    let translate_site ~message ~ml_pos state agent site =
      let state, handler = get_handler state in
      let error = get_errors state in
      let parameters = get_parameters state in
      let error, site =
        Handler.translate_site ~message ~ml_pos parameters error handler agent
          site
      in
      let state = set_errors error state in
      state, site

    let translate_state ~message ~ml_pos state agent site prop =
      let state, handler = get_handler state in
      let error = get_errors state in
      let parameters = get_parameters state in
      let error, site =
        Handler.translate_state ~message ~ml_pos parameters error handler agent
          site prop
      in
      let state = set_errors error state in
      state, site

    let translate_and_simplify_site ~message ~ml_pos state agent site =
      let state, site = translate_site ~message ~ml_pos state agent site in
      state, simplify_site site

    let dump_c_compil state c_compil =
      let state, handler = get_handler state in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let error =
        Print_cckappa.print_compil parameters error handler c_compil
      in
      let state = Remanent_state.set_errors error state in
      state

    (******************************************************************)

    let rename_link parameters handler error map ((_, i), j) =
      let error, agent =
        Handler.translate_agent ~message:"unknown agent type"
          ~ml_pos:(Some __POS__) parameters error handler
          (Ckappa_sig.agent_name_of_int i)
      in
      let error, site =
        Handler.translate_site ~message:"unknown agent type"
          ~ml_pos:(Some __POS__) parameters error handler
          (Ckappa_sig.agent_name_of_int i)
          (Ckappa_sig.site_name_of_int j)
      in
      match Mods.String2Map.find_option (agent, simplify_site site) map with
      | None -> Exception.warn parameters error __POS__ Exit ((0, 0), 0)
      | Some (i, j) -> error, ((0, i), j)

    let rename_links parameter handler error map = function
      | User_graph.LINKS l ->
        let error', l' =
          List.fold_left
            (fun (error, l) link ->
              let error, link = rename_link parameter handler error map link in
              error, link :: l)
            (error, []) (List.rev l)
        in
        error', User_graph.LINKS l'
      | (WHATEVER | SOME | TYPE _) as x -> error, x

    let reindex parameters error handler list =
      let map, _ =
        Array.fold_left
          (fun (map, counter_ag) site_node ->
            let ag = site_node.User_graph.node_type in
            let interface = site_node.User_graph.node_sites in
            let map, _ =
              Array.fold_left
                (fun (map, counter_site) site ->
                  let site_name = site.User_graph.site_name in
                  ( Mods.String2Map.add (ag, site_name)
                      (counter_ag, counter_site) map,
                    counter_site + 1 ))
                (map, 0) interface
            in
            map, counter_ag + 1)
          (Mods.String2Map.empty, 0) list
      in
      Array.fold_right
        (fun site_node (error, agent_list) ->
          let error, interface =
            Array.fold_right
              (fun site (error, interface) ->
                let error, site =
                  match site.User_graph.site_type with
                  | User_graph.Counter _ -> error, site
                  | User_graph.Port p ->
                    let error, new_links =
                      rename_links parameters handler error map
                        p.User_graph.port_links
                    in
                    ( error,
                      {
                        site with
                        User_graph.site_type =
                          User_graph.Port
                            { p with User_graph.port_links = new_links };
                      } )
                in
                error, site :: interface)
              site_node.User_graph.node_sites (error, [])
          in
          let agent =
            { site_node with User_graph.node_sites = Array.of_list interface }
          in
          error, Some agent :: agent_list)
        list (error, [])

    (******************************************************************)

    let convert_label a =
      if a < 0 then
        Public_data.Side_effect (-(a + 1))
      else
        Public_data.Direct a

    let convert_id_short parameters error handler compiled id =
      Handler.convert_id_short parameters error handler compiled id

    let convert_id_refined parameters error handler compiled id =
      Handler.convert_id_refined parameters error handler compiled id

    (******************************************************************)
    (*quark map *)
    (******************************************************************)

    let compute_quark_map show_title state =
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let state, c_compil = get_c_compilation state in
      let state, handler = get_handler state in
      let () = show_title state in
      let error, quark_map = Quark.quarkify parameters error handler c_compil in
      let error =
        if Remanent_parameters.get_trace parameters || Print_quarks.trace then
          Print_quarks.print_quarks parameters error handler quark_map
        else
          error
      in
      ( Remanent_state.set_errors error
          (Remanent_state.set_quark_map quark_map state),
        quark_map )

    let get_quark_map =
      get_gen ~debug_mode:Quark.local_trace ~stack_title:"Quark.quarkify"
        ~log_prefix:"Quarks:" Remanent_state.get_quark_map compute_quark_map

    (******************************************************************)
    (*Reachability*)
    (******************************************************************)

    let compute_reachability_result show_title state =
      let state, c_compil = get_c_compilation state in
      let state, handler = get_handler state in
      let () = show_title state in
      let bdu_handler = Remanent_state.get_bdu_handler state in
      let log_info = Remanent_state.get_log_info state in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let error, log_info, static, dynamic =
        Reachability.main parameters log_info error bdu_handler c_compil handler
      in
      let error, dynamic, state =
        Reachability.export static dynamic error state
      in
      let state = Remanent_state.set_errors error state in
      let state = Remanent_state.set_log_info log_info state in
      let state = Remanent_state.set_bdu_handler bdu_handler state in
      let state =
        Remanent_state.set_reachability_result (static, dynamic) state
      in
      state, (static, dynamic)

    let get_reachability_analysis =
      get_gen ~log_title:"Reachability analysis"
        Remanent_state.get_reachability_result compute_reachability_result

    (******************************************************************)
    (*ODE flows*)
    (******************************************************************)

    let compute_ctmc_flow show_title state =
      let state, c_compil = get_c_compilation state in
      let state, handler = get_handler state in
      let () = show_title state in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let error, output =
        Stochastic_classes.stochastic_classes parameters error handler c_compil
      in
      ( Remanent_state.set_errors error
          (Remanent_state.set_ctmc_flow output state),
        output )

    let get_ctmc_flow =
      get_gen ~log_prefix:"Flow of information in the CTMC semantics:"
        ~log_title:"Flow of information in the CTMC semantcs:"
        Remanent_state.get_ctmc_flow compute_ctmc_flow

    let compute_ode_flow show_title state =
      let state, c_compil = get_c_compilation state in
      let state, handler = get_handler state in
      let () = show_title state in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let error, output =
        Ode_fragmentation.scan_rule_set parameters error handler c_compil
      in
      ( Remanent_state.set_errors error
          (Remanent_state.set_ode_flow output state),
        output )

    let get_ode_flow =
      get_gen ~log_prefix:"Flow of information in the ODE semantics:"
        ~log_title:"Flow of information in the ODE semantcs:"
        Remanent_state.get_ode_flow compute_ode_flow

    (******************************************************************)
    (*influence_map*)
    (******************************************************************)

    let compute_pos_of_rules_and_vars show_title state =
      let parameters = get_parameters state in
      let state, compil = get_c_compilation state in
      let state, handler = get_handler state in
      let error = get_errors state in
      let nrules = Handler.nrules parameters error handler in
      let nvars = Handler.nvars parameters error handler in
      let () = show_title state in
      let rec aux inc pos of_int lift n (error, l) =
        if n < 0 then
          error, l
        else (
          let error, p =
            pos parameters error handler compil (of_int (n + inc))
          in
          aux inc pos of_int lift (n - 1) (error, (lift n, p) :: l)
        )
      in
      let error, l =
        aux 0 Handler.pos_of_rule Ckappa_sig.rule_id_of_int
          (fun x -> Public_data.Rule x)
          (nrules - 1)
          (aux nrules Handler.pos_of_var Ckappa_sig.rule_id_of_int
             (fun x -> Public_data.Var x)
             (nvars - 1) (error, []))
      in
      let json = Public_data.pos_of_rules_and_vars_to_json l in
      let _ = Public_data.pos_of_rules_and_vars_of_json json in
      ( Remanent_state.set_errors error
          (Remanent_state.set_pos_of_rules_and_vars l state),
        l )

    let get_pos_of_rules_and_vars =
      get_gen ~log_prefix:"Summarize the position of rules and variables"
        ~log_title:"Summarize the position of rules and variables"
        Remanent_state.get_pos_of_rules_and_vars compute_pos_of_rules_and_vars

    let compute_raw_internal_influence_map show_title state =
      let parameters = Remanent_state.get_parameters state in
      let state, compil = get_c_compilation state in
      let state, quark_map = get_quark_map state in
      let state, handler = get_handler state in
      let error = Remanent_state.get_errors state in
      let nrules = Handler.nrules parameters error handler in
      let nvars = Handler.nvars parameters error handler in
      let nodes = Misc_sa.list_0_n (nrules + nvars - 1) in
      let nodes = List.rev_map Ckappa_sig.rule_id_of_int (List.rev nodes) in
      let () = show_title state in
      let error, wake_up_map, inhibition_map =
        Influence_map.compute_influence_map parameters error handler quark_map
          nrules
      in
      let error =
        if
          (Remanent_parameters.get_trace parameters || Print_quarks.trace)
          && Remanent_parameters.get_influence_map_accuracy_level parameters
             = Remanent_parameters_sig.Low
        then
          Print_quarks.print_wake_up_map parameters error handler compil
            Handler.print_rule_txt Handler.print_var_txt
            Handler.get_label_of_rule_txt Handler.get_label_of_var_txt
            Handler.print_labels "\n" wake_up_map
        else
          error
      in
      let error =
        if
          (Remanent_parameters.get_trace parameters || Print_quarks.trace)
          && Remanent_parameters.get_influence_map_accuracy_level parameters
             = Remanent_parameters_sig.Low
        then
          Print_quarks.print_inhibition_map parameters error handler compil
            Handler.print_rule_txt Handler.print_var_txt
            Handler.get_label_of_rule_txt Handler.get_label_of_var_txt
            Handler.print_labels "\n" inhibition_map
        else
          error
      in
      let state =
        Remanent_state.set_internal_influence_map Public_data.Low
          (nodes, wake_up_map, inhibition_map)
          state
      in
      Remanent_state.set_errors error state, (nodes, wake_up_map, inhibition_map)

    let get_raw_internal_influence_map =
      get_gen ~do_we_show_title:title_only_in_kasa ~log_prefix:"Influence_map:"
        ~log_main_title:"Generating the raw influence map..."
        ~phase:(StoryProfiling.Internal_influence_map "raw")
        (Remanent_state.get_internal_influence_map Public_data.Low)
        compute_raw_internal_influence_map

    let convert_half_influence_map parameters error handler compiled influence =
      Ckappa_sig.PairRule_setmap.Map.fold
        (fun (x, y) list (error, map) ->
          let error, x =
            Handler.convert_id_short parameters error handler compiled
              (Ckappa_sig.rule_id_of_int
                 (int_of_string (Ckappa_sig.string_of_rule_id x)))
          in
          let error, y =
            Handler.convert_id_short parameters error handler compiled
              (Ckappa_sig.rule_id_of_int
                 (int_of_string (Ckappa_sig.string_of_rule_id y)))
          in
          let old =
            match Public_data.InfluenceNodeMap.find_option x map with
            | None -> Public_data.InfluenceNodeMap.empty
            | Some x -> x
          in
          let list = Quark_type.Labels.convert_label_set_couple list in
          let list =
            List.rev_map
              (fun (a, b) -> convert_label a, convert_label b)
              (List.rev list)
          in
          ( error,
            Public_data.InfluenceNodeMap.add x
              (Public_data.InfluenceNodeMap.add y list old)
              map ))
        influence
        (error, Public_data.InfluenceNodeMap.empty)

    let extract_all_nodes_of_influence_map state (nodes, _, _) =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let state, compiled = get_c_compilation state in
      let error = Remanent_state.get_errors state in
      let _error, nodes =
        List.fold_left
          (fun (error, list) id ->
            let error, head =
              Handler.convert_id_refined parameters error handler compiled id
            in
            error, head :: list)
          (error, []) (List.rev nodes)
      in
      state, nodes

    let convert_influence_map state (nodes, wake_up_map, inhibition_map) =
      let state, nodes =
        extract_all_nodes_of_influence_map state
          (nodes, wake_up_map, inhibition_map)
      in
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let state, compiled = get_c_compilation state in
      let error = Remanent_state.get_errors state in
      let error, pos =
        convert_half_influence_map parameters error handler compiled wake_up_map
      in
      let error, neg =
        convert_half_influence_map parameters error handler compiled
          inhibition_map
      in
      let state = Remanent_state.set_errors error state in
      let output =
        {
          Public_data.nodes;
          Public_data.positive = pos;
          Public_data.negative = neg;
        }
      in
      state, output

    let compute_intermediary_internal_influence_map show_title state =
      let state, handler = get_handler state in
      let state, compil = get_c_compilation state in
      let state, (nodes, wake_up_map, inhibition_map) =
        get_raw_internal_influence_map state
      in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let state, _ = compute_pos_of_rules_and_vars (fun _ -> ()) state in
      let () = show_title state in
      let error, wake_up_map =
        Algebraic_construction.filter_influence parameters error handler compil
          wake_up_map true
      in
      let error, inhibition_map =
        Algebraic_construction.filter_influence parameters error handler compil
          inhibition_map false
      in
      let state =
        Remanent_state.set_internal_influence_map Public_data.Medium
          (nodes, wake_up_map, inhibition_map)
          state
      in
      let state, output =
        convert_influence_map state (nodes, wake_up_map, inhibition_map)
      in
      let state =
        Remanent_state.set_influence_map Public_data.Medium output state
      in
      let error =
        if Remanent_parameters.get_trace parameters || Print_quarks.trace then
          Print_quarks.print_wake_up_map parameters error handler compil
            Handler.print_rule_txt Handler.print_var_txt
            Handler.get_label_of_rule_txt Handler.get_label_of_var_txt
            Handler.print_labels "\n" wake_up_map
        else
          error
      in
      let error =
        if Remanent_parameters.get_trace parameters || Print_quarks.trace then
          Print_quarks.print_inhibition_map parameters error handler compil
            Handler.print_rule_txt Handler.print_var_txt
            Handler.get_label_of_rule_txt Handler.get_label_of_var_txt
            Handler.print_labels "\n" inhibition_map
        else
          error
      in
      Remanent_state.set_errors error state, (nodes, wake_up_map, inhibition_map)

    let get_intermediary_internal_influence_map =
      get_gen ~log_prefix:"Influence_map:"
        ~log_title:"Refining the influence map"
        ~phase:(StoryProfiling.Internal_influence_map "medium")
        (Remanent_state.get_internal_influence_map Public_data.Medium)
        compute_intermediary_internal_influence_map

    let string_of_influence_node rule var x =
      match x with
      | Public_data.Rule i ->
        "Rule " ^ rule i (*(string_of_int i.Public_data.rule_id)*)
      | Public_data.Var i -> "Var " ^ var i
    (*string_of_int i.Public_data.var_id)
                                        *)

    let string_of_short_influence_node =
      string_of_influence_node string_of_int string_of_int

    let string_of_refined_influence_node =
      string_of_influence_node
        (fun i -> string_of_int i.Public_data.rule_id)
        (fun i -> string_of_int i.Public_data.var_id)

    let print_influence_map parameters influence_map =
      let log = Remanent_parameters.get_logger parameters in
      Loggers.fprintf log "Influence map:";
      Loggers.print_newline log;
      Public_data.InfluenceNodeMap.iter
        (fun x y ->
          Public_data.InfluenceNodeMap.iter
            (fun y _labellist ->
              let () =
                Loggers.fprintf log " %s->%s"
                  (string_of_short_influence_node x)
                  (string_of_short_influence_node y)
              in
              let () = Loggers.print_newline log in
              ())
            y)
        influence_map.Public_data.positive;
      Public_data.InfluenceNodeMap.iter
        (fun x y ->
          Public_data.InfluenceNodeMap.iter
            (fun y _labellist ->
              let () =
                Loggers.fprintf log " %s-|%s"
                  (string_of_short_influence_node x)
                  (string_of_short_influence_node y)
              in
              let () = Loggers.print_newline log in
              ())
            y)
        influence_map.Public_data.negative;
      Loggers.print_newline log

    let query_inhibition_map influence_map r1 r2 =
      match
        Public_data.InfluenceNodeMap.find_option (Public_data.Rule r1)
          influence_map.Public_data.negative
      with
      | None -> []
      | Some map ->
        (match
           Public_data.InfluenceNodeMap.find_option (Public_data.Rule r2) map
         with
        | None -> []
        | Some l -> l)

    let find_most_precise map =
      match Public_data.AccuracyMap.max_key map with
      | None -> None
      | Some key -> Public_data.AccuracyMap.find_option key map

    let compute_high_res_internal_influence_map show_title state =
      let state, handler = get_handler state in
      let state, compil = get_c_compilation state in
      let state, (nodes, wake_up_map, inhibition_map) =
        get_intermediary_internal_influence_map state
      in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let state, (static, dynamic) = get_reachability_analysis state in
      let () = show_title state in
      let maybe_reachable static dynamic error =
        Reachability.maybe_reachable static dynamic error
          Analyzer_headers.Morphisms
      in
      let (error, dynamic), wake_up_map =
        Algebraic_construction.filter_influence_high maybe_reachable parameters
          handler error compil static dynamic wake_up_map true
      in
      let (error, dynamic), inhibition_map =
        Algebraic_construction.filter_influence_high maybe_reachable parameters
          handler error compil static dynamic inhibition_map false
      in
      let state = Remanent_state.set_errors error state in
      let state =
        Remanent_state.set_reachability_result (static, dynamic) state
      in
      let state =
        Remanent_state.set_internal_influence_map Public_data.High
          (nodes, wake_up_map, inhibition_map)
          state
      in
      let state, handler = get_handler state in
      let state, output =
        convert_influence_map state (nodes, wake_up_map, inhibition_map)
      in
      let state =
        Remanent_state.set_influence_map Public_data.High output state
      in
      let error = get_errors state in
      let error =
        if Remanent_parameters.get_trace parameters || Print_quarks.trace then
          Print_quarks.print_wake_up_map parameters error handler compil
            Handler.print_rule_txt Handler.print_var_txt
            Handler.get_label_of_rule_txt Handler.get_label_of_var_txt
            Handler.print_labels "\n" wake_up_map
        else
          error
      in
      let error =
        if Remanent_parameters.get_trace parameters || Print_quarks.trace then
          Print_quarks.print_inhibition_map parameters error handler compil
            Handler.print_rule_txt Handler.print_var_txt
            Handler.get_label_of_rule_txt Handler.get_label_of_var_txt
            Handler.print_labels "\n" inhibition_map
        else
          error
      in
      Remanent_state.set_errors error state, (nodes, wake_up_map, inhibition_map)

    let get_high_res_internal_influence_map =
      get_gen ~log_prefix:"Influence_map:"
        ~log_title:"Refining further the influence map"
        ~phase:(StoryProfiling.Internal_influence_map "high")
        (Remanent_state.get_internal_influence_map Public_data.High)
        compute_high_res_internal_influence_map

    let get_internal_influence_map ?(accuracy_level = Public_data.Low) state =
      match accuracy_level with
      | Public_data.Low -> get_raw_internal_influence_map state
      | Public_data.Medium -> get_intermediary_internal_influence_map state
      | Public_data.High | Public_data.Full ->
        get_high_res_internal_influence_map state

    let compute_map_gen
        (get :
          ?accuracy_level:Public_data.accuracy_level ->
          ( Reachability.static_information,
            Reachability.dynamic_information )
          Remanent_state.state ->
          ( Reachability.static_information,
            Reachability.dynamic_information )
          Remanent_state.state
          * 'a) store convert ?(accuracy_level = Public_data.Low)
        ?(do_we_show_title = fun _ -> true) ?log_title state =
      let show_title =
        match log_title with
        | None -> fun _ -> ()
        | Some log_title ->
          compute_show_title do_we_show_title (log_title accuracy_level)
      in
      let () = show_title state in
      let state, internal = get ~accuracy_level state in
      let state, rep = convert (fun _ -> ()) state internal in
      store accuracy_level rep state, rep

    let compute_influence_map ?(accuracy_level = Public_data.Low) _show_title =
      compute_map_gen get_internal_influence_map
        Remanent_state.set_influence_map
        (fun _ -> convert_influence_map)
        ~accuracy_level

    let get_influence_map ?(accuracy_level = Public_data.Low)
        ?(do_we_show_title = fun _ -> true)
        ?(log_title =
          fun x ->
            match x with
            | Public_data.Low -> Some "Compute the influence map"
            | Public_data.Medium | Public_data.High | Public_data.Full ->
              Some "Refine the influence map") =
      get_gen
        (Remanent_state.get_influence_map accuracy_level)
        (compute_influence_map ~accuracy_level ~do_we_show_title ~log_title)

    let nrules state =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let error = get_errors state in
      state, Handler.nrules parameters error handler

    let nvars state =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let error = get_errors state in
      state, Handler.nvars parameters error handler

    let convert_to_birectional_influence_map show_title state influence_map =
      let () = show_title state in
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let output =
        Bidirectional_influence_map.convert ~nrules ~nvars influence_map
      in
      state, output

    let compute_bidirectional_influence_map ?(accuracy_level = Public_data.Low)
        _show_title =
      compute_map_gen get_internal_influence_map
        Remanent_state.set_bidirectional_influence_map
        convert_to_birectional_influence_map ~accuracy_level

    let get_bidirectional_influence_map ?(accuracy_level = Public_data.Low)
        ?(do_we_show_title = fun _ -> true)
        ?(log_title =
          fun x ->
            match x with
            | Public_data.Low -> Some "Compute the bidirectional influence map"
            | Public_data.Medium | Public_data.High | Public_data.Full ->
              Some "Refine the bidirectional influence map") =
      get_gen
        (Remanent_state.get_bidirectional_influence_map accuracy_level)
        (compute_bidirectional_influence_map ~accuracy_level ~do_we_show_title
           ~log_title)

    let compute_influence_map_blackboard show_title state =
      let () = show_title state in
      let state, nrules = nrules state in
      let state, nvars = nvars state in
      let blackboard = Local_influence_map.init_blackboard nrules nvars in
      state, blackboard

    let get_influence_map_blackboard =
      get_gen ~log_title:"Preparing data-structures for local influence map"
        Remanent_state.get_local_influence_map_blackboard
        compute_influence_map_blackboard

    let get_local_internal_influence_map ?(accuracy_level = Public_data.Low)
        ?fwd ?bwd ~total origin state =
      let parameters = get_parameters state in
      let error = get_errors state in
      let state, bidirectional_influence_map =
        get_bidirectional_influence_map ~accuracy_level state
      in
      let state, blackboard = get_influence_map_blackboard state in
      let error, local_influence_map, _blackboard =
        Local_influence_map.explore_influence_map parameters error ?fwd ?bwd
          ~total blackboard origin bidirectional_influence_map
      in
      let state = set_errors error state in
      state, local_influence_map

    let get_local_influence_map ?(accuracy_level = Public_data.Low) ?fwd ?bwd
        ~total origin state =
      let state, internal_influence_map =
        get_local_internal_influence_map ~accuracy_level ?fwd ?bwd ~total origin
          state
      in
      convert_influence_map state internal_influence_map

    let get_all_nodes_of_influence_map ?(accuracy_level = Public_data.Low) state
        =
      let state, internal_influence_map =
        get_internal_influence_map ~accuracy_level state
      in
      extract_all_nodes_of_influence_map state internal_influence_map

    let query_inhibition_map ?accuracy_level state r1 r2 =
      let state, inf_map = get_influence_map ?accuracy_level state in
      let output = query_inhibition_map inf_map r1 r2 in
      state, output

    let get_most_accurate_influence_map state =
      let map = Remanent_state.get_influence_map_map state in
      find_most_precise map

    let output_internal_influence_map ?logger
        ?(accuracy_level = Public_data.Low) state =
      let parameters = get_parameters state in
      let state, influence_map =
        get_internal_influence_map ~accuracy_level state
      in
      let state, c_compil = get_c_compilation state in
      let state, handler = get_handler state in
      let error = get_errors state in
      let error =
        Print_quarks.dot_of_influence_map ?logger parameters error handler
          c_compil influence_map
      in
      set_errors error state

    let output_local_internal_influence_map ?logger
        ?(accuracy_level = Public_data.Low) ?fwd ?bwd ~total origin state =
      let parameters = get_parameters state in
      let state, influence_map =
        get_local_internal_influence_map ~accuracy_level ?fwd ?bwd ~total origin
          state
      in
      let state, c_compil = get_c_compilation state in
      let state, handler = get_handler state in
      let error = get_errors state in
      let error =
        Print_quarks.dot_of_influence_map ?logger parameters error handler
          c_compil influence_map
      in
      set_errors error state

    let output_best_internal_influence_map state =
      let map = Remanent_state.get_internal_influence_map_map state in
      match Public_data.AccuracyMap.max_key map with
      | None -> state
      | Some accuracy_level ->
        output_internal_influence_map ~accuracy_level state

    let dump_influence_map ?(accuracy_level = Public_data.Low) state =
      match Remanent_state.get_influence_map accuracy_level state with
      | None -> ()
      | Some influence_map ->
        print_influence_map (Remanent_state.get_parameters state) influence_map

    (******************************************************************)
    (*contact map*)
    (******************************************************************)

    let get_most_accurate_contact_map state =
      let map = Remanent_state.get_contact_map_map state in
      find_most_precise map

    let compute_raw_internal_contact_map show_title state =
      let state, _ = get_compilation state in
      let state, handler = get_handler state in
      let () = show_title state in
      let state, c_compil = get_c_compilation state in
      let parameters = Remanent_state.get_parameters state in
      let parameters =
        Remanent_parameters.update_prefix parameters "Compilation:"
      in
      let error = Remanent_state.get_errors state in
      let error =
        if Remanent_parameters.get_trace parameters || Print_cckappa.trace then
          Print_cckappa.print_compil parameters error handler c_compil
        else
          error
      in
      let error, contact_map =
        Preprocess.export_contact_map parameters error handler
      in
      let state = Remanent_state.set_errors error state in
      ( Remanent_state.set_internal_contact_map Public_data.Low contact_map state,
        contact_map )

    let dump_raw_internal_contact_map state handler =
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let error =
        match Remanent_parameters.get_cm_format parameters with
        | DOT -> Print_handler.dot_of_contact_map parameters error handler
        | GEPHI -> Print_handler.gexf_of_contact_map parameters error handler
        | _ ->
          let error, () = warn parameters error __POS__ Exit () in
          error
      in
      Remanent_state.set_errors error state

    let get_raw_internal_contact_map =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Generating the raw contact map"
        (*  ~dump:dump_raw_internal_contact_map *)
        (Remanent_state.get_internal_contact_map Public_data.Low)
        compute_raw_internal_contact_map

    let compute_intermediary_internal_contact_map _show_title state =
      let state, _ = get_reachability_analysis state in
      match
        Remanent_state.get_internal_contact_map Public_data.Medium state
      with
      | Some map -> state, map
      | None -> assert false

    let get_intermediary_internal_contact_map =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Generating the intermediary contact map"
        (*  ~dump:dump_raw_internal_contact_map *)
        (Remanent_state.get_internal_contact_map Public_data.Medium)
        compute_intermediary_internal_contact_map

    let get_internal_contact_map ?(accuracy_level = Public_data.Low) state =
      match accuracy_level with
      | Public_data.Low -> get_raw_internal_contact_map state
      | Public_data.Medium | Public_data.High | Public_data.Full ->
        get_intermediary_internal_contact_map state

    let convert_contact_map_map_to_list sol =
      Tools.array_rev_of_list
        (Mods.StringSetMap.Map.fold
           (fun a data l ->
             {
               User_graph.node_type = a;
               User_graph.node_id = None;
               User_graph.node_sites =
                 Tools.array_rev_of_list
                   (Mods.StringSetMap.Map.fold
                      (fun a (props, links) l ->
                        let links =
                          List.rev_map
                            (fun (i, j) -> (0, i), j)
                            (List.rev links)
                        in
                        {
                          User_graph.site_name = a;
                          User_graph.site_type =
                            User_graph.Port
                              {
                                User_graph.port_links = User_graph.LINKS links;
                                User_graph.port_states = Some props;
                              };
                        }
                        :: l)
                      data []);
             }
             :: l)
           sol [])

    let convert_contact_map show_title state contact_map =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let error = Remanent_state.get_errors state in
      let () = show_title state in
      let error, contact_map =
        AgentProj.monadic_proj_map_i
          (fun parameters error ag ->
            Handler.translate_agent ~message:"unknown agent type"
              ~ml_pos:(Some __POS__) parameters error handler ag)
          parameters error Mods.StringSetMap.Map.empty
          (fun parameters error _ ag sitemap ->
            SiteProj.monadic_proj_map_i
              (fun parameters errors site ->
                let error, site =
                  Handler.translate_site parameters errors handler ag
                    (site : Ckappa_sig.c_site_name)
                in
                error, Handler.print_site_contact_map site)
              parameters error ([], [])
              (fun parameters error (list_a, list_b) site (list_a', list_b') ->
                let error, list_a'' =
                  List.fold_left
                    (fun (error, list) state ->
                      let error, state =
                        Handler.translate_state parameters error handler ag site
                          state
                      in
                      match state with
                      | Ckappa_sig.Internal state -> error, state :: list
                      | Ckappa_sig.Counter _ | Ckappa_sig.Binding _ ->
                        warn parameters error __POS__ Exit list)
                    (error, list_a) (List.rev list_a')
                in
                let error, list_b'' =
                  List.fold_left
                    (fun (error, list) (agent, site) ->
                      ( error,
                        ( Ckappa_sig.int_of_agent_name agent,
                          Ckappa_sig.int_of_site_name site )
                        :: list ))
                    (error, list_b) (List.rev list_b')
                in
                error, (list_a'', list_b''))
              sitemap)
          contact_map
      in
      let contact_map = convert_contact_map_map_to_list contact_map in
      let error, contact_map = reindex parameters error handler contact_map in
      Remanent_state.set_errors error state, [| Array.of_list contact_map |]

    let compute_contact_map ?(accuracy_level = Public_data.Low) _show_title =
      compute_map_gen get_internal_contact_map Remanent_state.set_contact_map
        convert_contact_map ~accuracy_level

    let get_contact_map ?(accuracy_level = Public_data.Low) =
      get_gen
        (Remanent_state.get_contact_map accuracy_level)
        (compute_contact_map ~accuracy_level
           ~do_we_show_title:(fun _ -> true)
           ~log_title:(fun x ->
             match x with
             | Public_data.Low -> Some "Compute the contact map"
             | Public_data.Medium | Public_data.High | Public_data.Full ->
               Some "Refine the contact map"))

    let dump_contact_map accuracy state =
      match Remanent_state.get_contact_map accuracy state with
      | None -> ()
      | Some contact_map ->
        let logger =
          Remanent_parameters.get_logger (Remanent_state.get_parameters state)
        in
        Loggers.fprintf logger "Contact map:@ %a" User_graph.print_cc
          contact_map

    let compute_internal_scc_decomposition
        ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low)
        ?do_we_show_title:(_do_we_show_title = fun _ -> false)
        ?log_title:(_log_title = "") _show_title state =
      let parameters = Remanent_state.get_parameters state in
      let accuracy_level = accuracy_level_cm in
      let state, contact_map = get_internal_contact_map ~accuracy_level state in
      let errors = get_errors state in
      let errors, cm_graph =
        Contact_map_scc.convert_contact_map parameters errors contact_map
      in
      let state, (static, dynamic) = get_reachability_analysis state in
      let state, handler = get_handler state in
      let errors, dynamic, cm_graph =
        match accuracy_level_scc with
        | Public_data.Low -> errors, dynamic, cm_graph
        | Public_data.Medium | Public_data.High | Public_data.Full ->
          let maybe_reachable _parameters error static dynamic =
            Reachability.maybe_reachable static dynamic error
              Analyzer_headers.Embeddings
          in
          Contact_map_scc.filter_edges_in_converted_contact_map parameters
            errors handler static dynamic maybe_reachable cm_graph
      in
      let state =
        Remanent_state.set_reachability_result (static, dynamic) state
      in
      let errors, graph_scc =
        Contact_map_scc.compute_graph_scc parameters errors cm_graph
      in
      let state =
        Remanent_state.set_internal_scc_decomposition accuracy_level_cm
          accuracy_level_scc graph_scc state
      in
      let state = Remanent_state.set_errors errors state in
      state, graph_scc

    let get_internal_scc_decomposition ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low) =
      get_gen
        (Remanent_state.get_internal_scc_decomposition accuracy_level_cm
           accuracy_level_scc)
        (compute_internal_scc_decomposition ~accuracy_level_cm
           ~accuracy_level_scc
           ~do_we_show_title:(fun _ -> true)
           ~log_title:
             "Decompose the contact map in strongly connected components")

    let get_internal_scc_decomposition_map state =
      Remanent_state.get_internal_scc_decomposition_map state

    (*internal contact map*)

    (*((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name)*
      (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name))  list list*)

    (*scc = ((string * string) * (string * string)) list list*)

    let translate_scc_decomposition state
        (internal_scc : internal_scc_decomposition) =
      let error = Remanent_state.get_errors state in
      let parameters = get_parameters state in
      let state, handler = get_handler state in
      let error, scc =
        List.fold_left
          (fun (error, store_result) list ->
            let error, store_list =
              List.fold_left
                (fun (error, store_result) ((ag, st), (ag', st')) ->
                  let error, agent =
                    Handler.translate_agent ~message:"unknown agent type"
                      ~ml_pos:(Some __POS__) parameters error handler ag
                  in
                  let error, site =
                    Handler.translate_site parameters error handler ag st
                  in
                  let site = simplify_site site in
                  let error, agent' =
                    Handler.translate_agent ~message:"unknown agent type"
                      ~ml_pos:(Some __POS__) parameters error handler ag'
                  in
                  let error, site' =
                    Handler.translate_site parameters error handler ag' st'
                  in
                  let site' = simplify_site site' in
                  let pair = (agent, site), (agent', site') in
                  error, pair :: store_result)
                (error, []) list
            in
            error, store_list :: store_result)
          (error, [ [] ]) internal_scc
      in
      let state = set_errors error state in
      state, scc

    let compute_map2_gen get store convert
        ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low)
        ?(do_we_show_title = fun _ -> true) ?log_title state =
      let show_title =
        match log_title with
        | None -> fun _ -> ()
        | Some log_title ->
          compute_show_title do_we_show_title
            (log_title accuracy_level_cm accuracy_level_scc)
      in
      let () = show_title state in
      let state, internal =
        get ?accuracy_level_cm:(Some accuracy_level_cm)
          ?accuracy_level_scc:(Some accuracy_level_scc) state
      in
      let state, rep = convert state internal in
      store accuracy_level_cm accuracy_level_scc rep state, rep

    let compute_scc_map ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low) _show_title =
      compute_map2_gen get_internal_scc_decomposition
        Remanent_state.set_scc_decomposition translate_scc_decomposition
        ~accuracy_level_cm ~accuracy_level_scc

    let get_scc_decomposition ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low)
        ?(do_we_show_title = fun _ -> true)
        ?(log_title = fun _ _ -> Some "Detect potential polymers") =
      get_gen
        (Remanent_state.get_scc_decomposition accuracy_level_cm
           accuracy_level_scc)
        (compute_scc_map ~accuracy_level_cm ~accuracy_level_scc
           ~do_we_show_title ~log_title)

    let dump_internal_scc_decomposition ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low) state =
      let parameters = Remanent_state.get_parameters state in
      let state, handler = get_handler state in
      let logger = Remanent_parameters.get_logger parameters in
      let state, graph_scc =
        get_internal_scc_decomposition ~accuracy_level_cm ~accuracy_level_scc
          state
      in
      let error = get_errors state in
      let () = Loggers.fprintf logger "Potential polymerization:" in
      let () = Loggers.print_newline logger in
      let error =
        List.fold_left
          (fun error list ->
            let error =
              List.fold_left
                (fun error ((ag, st), (ag', st')) ->
                  let error, agent_name =
                    Handler.string_of_agent parameters error handler ag
                  in
                  let error, site_name =
                    Handler.string_of_site parameters error handler ag st
                  in
                  let error, agent_name' =
                    Handler.string_of_agent parameters error handler ag'
                  in
                  let error, site_name' =
                    Handler.string_of_site parameters error handler ag' st'
                  in
                  let () =
                    Loggers.fprintf logger "   (%s,%s)--(%s,%s); " agent_name
                      site_name agent_name' site_name'
                  in
                  let () = Loggers.print_newline logger in
                  error)
                error list
            in
            let () = Loggers.print_newline logger in
            error)
          error graph_scc
      in
      let state = set_errors error state in
      state

    let dump_scc_decomposition ?(accuracy_level_cm = Public_data.Low)
        ?(accuracy_level_scc = Public_data.Low) state =
      let parameters = Remanent_state.get_parameters state in
      let logger = Remanent_parameters.get_logger parameters in
      let state, graph_scc =
        get_scc_decomposition ~accuracy_level_cm ~accuracy_level_scc state
      in
      let error = get_errors state in
      let () = Loggers.fprintf logger "Potential polymerization:" in
      let () = Loggers.print_newline logger in
      let error =
        List.fold_left
          (fun error list ->
            let error =
              List.fold_left
                (fun error ((agent_name, site_name), (agent_name', site_name')) ->
                  let () =
                    Loggers.fprintf logger "(%s,%s)--(%s,%s); " agent_name
                      site_name agent_name' site_name'
                  in
                  error)
                error list
            in
            let () = Loggers.print_newline logger in
            error)
          error graph_scc
      in
      let state = set_errors error state in
      state

    let get_influence_map ?(accuracy_level = Public_data.Low)
        ?(do_we_show_title = fun _ -> true)
        ?(log_title =
          fun x ->
            match x with
            | Public_data.Low -> Some "Compute the influence map"
            | Public_data.Medium | Public_data.High | Public_data.Full ->
              Some "Refine the influence map") =
      get_gen
        (Remanent_state.get_influence_map accuracy_level)
        (compute_influence_map ~accuracy_level ~do_we_show_title ~log_title)

    let output_internal_contact_map ?logger ?(accuracy_level = Public_data.Low)
        state =
      let parameters = Remanent_state.get_parameters state in
      let state, contact_map = get_internal_contact_map ~accuracy_level state in
      let state, handler = get_handler state in
      let error = get_errors state in
      let scc_contact_map =
        Remanent_state.get_internal_scc_decomposition_map state
      in
      let error =
        match Remanent_parameters.get_cm_format parameters with
        | DOT ->
          Preprocess.dot_of_contact_map ?logger parameters error handler
            scc_contact_map contact_map
        | GEPHI ->
          Preprocess.gexf_of_contact_map ?logger parameters error handler
            scc_contact_map contact_map
        | _ ->
          let error, () = warn parameters error __POS__ Exit () in
          error
      in
      set_errors error state

    (*contact map interge *)

    let compute_contact_map_int show_title state =
      let state, _, _, contactmap = compute_env_init show_title state in
      state, contactmap

    let get_contact_map_int =
      get_gen ~phase:StoryProfiling.LKappa_signature
        Remanent_state.get_contact_map_int compute_contact_map_int

    (*Raw contact map*)

    let compute_raw_contact_map show_title state =
      let sol = ref Mods.StringSetMap.Map.empty in
      let state, handler = get_prehandler state in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let add_link (a, b) (c_id, d_id) sol =
        let sol_a =
          Mods.StringSetMap.Map.find_default Mods.StringSetMap.Map.empty a sol
        in
        let l, old = Mods.StringSetMap.Map.find_default ([], []) b sol_a in
        Mods.StringSetMap.Map.add a
          (Mods.StringSetMap.Map.add b
             ( l,
               ( Ckappa_sig.int_of_agent_name c_id,
                 Ckappa_sig.int_of_site_name d_id )
               :: old )
             sol_a)
          sol
      in
      let add_link (a, b) (a_id, b_id) (c, d) (c_id, d_id) sol =
        add_link (a, b) (c_id, d_id) (add_link (c, d) (a_id, b_id) sol)
      in
      (*----------------------------------------------------------------*)
      let add_internal_state (a, b) c sol =
        match c with
        | Ckappa_sig.Counter _ | Ckappa_sig.Binding _ -> sol
        | Ckappa_sig.Internal state ->
          let sol_a =
            Mods.StringSetMap.Map.find_default Mods.StringSetMap.Map.empty a sol
          in
          let old, l = Mods.StringSetMap.Map.find_default ([], []) b sol_a in
          Mods.StringSetMap.Map.add a
            (Mods.StringSetMap.Map.add b (state :: old, l) sol_a)
            sol
      in
      (*----------------------------------------------------------------*)
      let () = show_title state in
      let error =
        Ckappa_sig
        .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .iter parameters error
          (fun parameters error (i, j) s ->
            let error, ag =
              Handler.translate_agent ~message:"unknown agent type"
                ~ml_pos:(Some __POS__) parameters error handler i
            in
            let error, site =
              Handler.translate_site parameters error handler i j
            in
            let site = simplify_site site in
            let error =
              Ckappa_sig.Dictionary_of_States.iter parameters error
                (fun _parameters error _s state () () ->
                  let () = sol := add_internal_state (ag, site) state !sol in
                  error)
                s
            in
            error)
          handler.Cckappa_sig.states_dic
      in
      (*----------------------------------------------------------------*)
      let sol = !sol in
      let error, sol =
        Ckappa_sig
        .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
        .fold parameters error
          (fun _parameters error (i, (j, _k)) (i', j', _k') sol ->
            let error, ag_i =
              Handler.translate_agent ~message:"unknown agent type"
                ~ml_pos:(Some __POS__) parameters error handler i
            in
            let error, site_j =
              Handler.translate_site parameters error handler i j
            in
            let site_j = simplify_site site_j in
            let error, ag_i' =
              Handler.translate_agent ~message:"unknown agent type"
                ~ml_pos:(Some __POS__) parameters error handler i'
            in
            let error, site_j' =
              Handler.translate_site parameters error handler i' j'
            in
            let site_j' = simplify_site site_j' in
            let sol =
              add_link (ag_i, site_j) (i, j) (ag_i', site_j') (i', j') sol
            in
            error, sol)
          handler.Cckappa_sig.dual sol
      in
      let sol =
        Mods.StringSetMap.Map.map
          (Mods.StringSetMap.Map.map (fun (l, x) -> List.rev l, x))
          sol
      in
      let sol = convert_contact_map_map_to_list sol in
      let error, sol = reindex parameters error handler sol in
      let sol = [| Array.of_list sol |] in
      ( Remanent_state.set_errors error
          (Remanent_state.set_contact_map Public_data.Low sol state),
        sol )

    let get_raw_contact_map =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Compute the contact map"
        (Remanent_state.get_contact_map Public_data.Low)
        compute_raw_contact_map

    (******************************************************************)
    (*signature*)
    (******************************************************************)

    let compute_signature show_title state =
      let state, l = get_contact_map state in
      let () = show_title state in
      let state, l =
        Array.fold_left
          (fun (state, list) -> function
            | None -> state, list
            | Some site_node ->
              let a = site_node.User_graph.node_type in
              let interface = site_node.User_graph.node_sites in
              let state, acc =
                Array.fold_left
                  (fun (state, acc) site ->
                    let x = site.User_graph.site_name in
                    let states, rev_binding =
                      match site.User_graph.site_type with
                      | User_graph.Counter _ ->
                        failwith "KaSa does not deal with counters yet"
                      | User_graph.Port p ->
                        ( Option_util.unsome [] p.User_graph.port_states,
                          (match p.User_graph.port_links with
                          | User_graph.LINKS l ->
                            List.rev_map (fun ((_, i), j) -> i, j) l
                          | SOME | WHATEVER | TYPE _ -> assert false) )
                    in
                    let state, binding' =
                      List.fold_left
                        (fun (state, list) (x, y) ->
                          let state, sx =
                            translate_agent state
                              ~message:"unknown agent name id"
                              ~ml_pos:(Some __POS__)
                              (Ckappa_sig.agent_name_of_int x)
                          in
                          let state, sy =
                            translate_and_simplify_site
                              ~message:"unknown site name id"
                              ~ml_pos:(Some __POS__) state
                              (Ckappa_sig.agent_name_of_int x)
                              (Ckappa_sig.site_name_of_int y)
                          in
                          ( state,
                            (Loc.annot_with_dummy sx, Loc.annot_with_dummy sy)
                            :: list ))
                        (state, []) rev_binding
                    in
                    let states' =
                      NamedDecls.create
                        (Tools.array_map_of_list
                           (fun i -> Loc.annot_with_dummy i, ())
                           states)
                    in
                    ( state,
                      ( Loc.annot_with_dummy x,
                        {
                          Signature.internal_state = states';
                          links = Some binding';
                          counters_info = None;
                        } )
                      :: acc ))
                  (state, []) interface
              in
              ( state,
                (Loc.annot_with_dummy a, NamedDecls.create_from_list acc)
                :: list ))
          (state, []) l.(0)
      in

      let agent_sigs =
        LKappa_compiler.agent_sigs_of_agent_sigs_with_links_as_lists
          ~build_contact_map:true
          (NamedDecls.create_from_list l)
      in
      let signature = Signature.create ~counters_per_agent:[] agent_sigs in
      Remanent_state.set_signature signature state, signature

    let get_signature = get_gen Remanent_state.get_signature compute_signature

    (******************************************************************)
    (*Dump*)
    (******************************************************************)

    let dump_signature state =
      match Remanent_state.get_signature state with
      | None -> ()
      | Some _signature -> ()

    let dump_errors state =
      Exception.print
        (Remanent_state.get_parameters state)
        (Remanent_state.get_errors state)

    let dump_errors_light state =
      Exception.print_errors_light_for_kasim
        (Remanent_state.get_parameters state)
        (Remanent_state.get_errors state)

    (******************************************************************)
    (*Dead rules*)
    (******************************************************************)

    let compute_dead_rules _show_title state =
      let state, _ = get_reachability_analysis state in
      match Remanent_state.get_dead_rules state with
      | Some l -> state, l
      | None -> assert false

    let get_dead_rules =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Detecting which rules may be triggered during simulations"
        (*  ~dump:dump_raw_internal_contact_map *)
        Remanent_state.get_dead_rules compute_dead_rules

    let compute_separating_transitions _show_title state =
      let parameters = get_parameters state in
      let parameters' =
        Remanent_parameters.set_compute_separating_transitions parameters true
      in
      let state' = set_parameters parameters' state in
      let state', _ = compute_reachability_result _show_title state' in
      let state', _ = get_reachability_analysis state' in
      let state = set_parameters parameters state' in
      match Remanent_state.get_separating_transitions state with
      | Some l -> state, l
      | None -> assert false

    let get_separating_transitions =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Detecting separating transitions"
        Remanent_state.get_separating_transitions compute_separating_transitions

    (******************************************************************)
    (*Dead agents*)
    (******************************************************************)

    let compute_dead_agents _show_title state =
      let state, _ = get_reachability_analysis state in
      match Remanent_state.get_dead_agents state with
      | Some map -> state, map
      | None -> assert false

    let get_dead_agents =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Detecting which agents may occur during simulations"
        (*  ~dump:dump_raw_internal_contact_map *)
        Remanent_state.get_dead_agents compute_dead_agents

    (****************************************************************)
    (*constraints_list*)
    (****************************************************************)

    let compute_internal_constraints_list _show_title state =
      let state, _ = get_reachability_analysis state in
      match Remanent_state.get_internal_constraints_list state with
      | None ->
        let error = Remanent_state.get_errors state in
        let parameters = Remanent_state.get_parameters state in
        let error, output = warn parameters error __POS__ Exit [] in
        let state = Remanent_state.set_errors error state in
        state, output
      | Some output -> state, output

    let get_internal_constraints_list =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"Extract refinement lemmas"
        Remanent_state.get_internal_constraints_list
        compute_internal_constraints_list

    let compute_constraints_list _show_title state =
      let error = Remanent_state.get_errors state in
      let state, internal_constraints_list =
        get_internal_constraints_list state
      in
      let error, constraints_list =
        List.fold_left
          (fun (error, constraints_list) (domain_name, lemma_list) ->
            let error, current_list =
              List.fold_left
                (fun (error, current_list) lem ->
                  let hyp = Public_data.get_hyp lem in
                  let refine = Public_data.get_refinement lem in
                  let string_version =
                    Site_graphs.KaSa_site_graph.get_string_version hyp
                  in
                  let error, site_graph =
                    Ckappa_site_graph.site_graph_to_list error string_version
                  in
                  let error, refinement =
                    Ckappa_site_graph.site_graph_list_to_list error refine
                  in
                  let lemma =
                    { Public_data.hyp = site_graph; Public_data.refinement }
                  in
                  let current_list = lemma :: current_list in
                  error, current_list)
                (error, []) lemma_list
            in
            (*------------------------------------------------------*)
            let pair_list =
              (domain_name, List.rev current_list) :: constraints_list
            in
            error, pair_list)
          (error, []) internal_constraints_list
      in
      let state = Remanent_state.set_constraints_list constraints_list state in
      let state = Remanent_state.set_errors error state in
      state, constraints_list

    let get_constraints_list =
      get_gen ~do_we_show_title:title_only_in_kasa
        ~log_title:"translate refinement lemmas"
        Remanent_state.get_constraints_list compute_constraints_list

    let output_internal_constraints_list ?logger state =
      let state, constraints_list = get_internal_constraints_list state in
      let parameters = Remanent_state.get_parameters state in
      let error = Remanent_state.get_errors state in
      let state, kappa_handler = get_handler state in
      (*PRINT*)
      let error =
        Ckappa_site_graph.print_internal_pattern ?logger parameters error
          kappa_handler constraints_list
      in
      let state = Remanent_state.set_errors error state in
      state

    let get_constraints_list_to_json state =
      let state, constraints_list = get_constraints_list state in
      state, Remanent_state.lemmas_list_to_json constraints_list

    (*********************************************************)
    (*Symmetries*)
    (*********************************************************)

    let compute_symmetries ?(accuracy_level = Public_data.Low) _show_title state
        =
      let state, env = get_env state in
      let state, init = get_init state in
      let state, contact_map_int = get_contact_map_int state in
      match env, init, contact_map_int with
      | None, _, _ | _, None, _ | _, _, None -> state, None
      | Some env, Some init, Some contact_map_int ->
        let rules = Model.fold_rules (fun _ acc r -> r :: acc) [] env in
        let parameters = get_parameters state in
        let cache = LKappa_auto.init_cache () in
        let cc_cache = Pattern.PreEnv.of_env (Model.domain env) in
        let _cc_cache, chemical_species =
          Symmetry_interface.species_of_initial_state_env ~debug_mode:false env
            contact_map_int cc_cache init
        in
        let state, contact_map = get_contact_map ~accuracy_level state in
        let rate_convention =
          Remanent_parameters.get_rate_convention parameters
        in
        let _cache, symmetries =
          Symmetries.detect_symmetries parameters env cache rate_convention
            chemical_species rules contact_map
        in
        state, Some symmetries

    let get_symmetric_sites ?(accuracy_level = Public_data.Low) =
      get_gen
        (Remanent_state.get_symmetries accuracy_level)
        (compute_symmetries ~accuracy_level)

    let output_symmetries ?logger ?(accuracy_level = Public_data.Low) state =
      let parameters = Remanent_state.get_parameters state in
      let parameters =
        match logger with
        | None -> parameters
        | Some logger -> Remanent_parameters.set_logger parameters logger
      in
      let state, sym = get_symmetric_sites ~accuracy_level state in
      let state, env = get_env state in
      match sym, env with
      | None, _ | _, None -> state
      | Some sym, Some env ->
        let () = Symmetries.print_symmetries parameters env sym in
        state

    let get_data = Remanent_state.get_data
  end
