(**
   * list_tokens.ml
   * openkappa
   * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2011, the 17th of January
   * Last modification: Time-stamp: <Oct 02 2018>
   * *
   * Number agents, sites, states in ckappa represenations
   *
   * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

module Int_Set_and_Map = Mods.IntSetMap

let local_trace = false

let empty_site_list =
  { Ckappa_sig.used = []; Ckappa_sig.declared = []; Ckappa_sig.creation = [] }

let empty_agent_specification =
  {
    Ckappa_sig.binding_sites_usage = empty_site_list;
    Ckappa_sig.marked_sites_usage = empty_site_list;
  }

let empty_handler parameters error =
  let error, int_constraints =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0
  in
  let error, sites =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0 (*dimension*)
  in
  let error, agent_annotation =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.create parameters
      error 0 (*dimension*)
  in
  let error, states_dic =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .create parameters error (0, 0)
  in
  let error, dual =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .create parameters error
      (0, (0, 0))
  in
  ( error,
    {
      Cckappa_sig.nvars = 0;
      Cckappa_sig.nagents = Ckappa_sig.dummy_agent_name;
      Cckappa_sig.nrules = 0;
      Cckappa_sig.agents_dic = Ckappa_sig.Dictionary_of_agents.init ();
      Cckappa_sig.agents_annotation = agent_annotation;
      Cckappa_sig.interface_constraints = int_constraints;
      Cckappa_sig.sites;
      Cckappa_sig.states_dic;
      Cckappa_sig.dual;
    } )

let create_binding_state_dictionary parameters error =
  let dic = Ckappa_sig.Dictionary_of_States.init () in
  let error, output =
    Ckappa_sig.Dictionary_of_States.allocate parameters error
      Ckappa_sig.compare_unit_state_index (Ckappa_sig.Binding Ckappa_sig.C_Free)
      () Misc_sa.const_unit dic
  in
  match output with
  | None -> error, dic
  | Some (_, _, _, x) -> error, x

let create_internal_state_dictionary _parameters error =
  let dic = Ckappa_sig.Dictionary_of_States.init () in
  error, dic

let init_agent_declaration parameters error handler agent_id agent_string =
  let agent_annotation = agent_string, [] in
  let error, agents_annotation =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters error
      agent_id agent_annotation handler.Cckappa_sig.agents_annotation
  in
  error, { handler with Cckappa_sig.agents_annotation }

let add_agent_declaration parameters error handler agent_id pos_opt =
  match pos_opt with
  | None -> error, handler
  | Some pos ->
    let error, info_opt =
      Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters
        error agent_id handler.Cckappa_sig.agents_annotation
    in
    let error, (agent_name, list) =
      match info_opt with
      | None -> Exception.warn parameters error __POS__ Exit ("", [])
      | Some info -> error, info
    in
    let agent_annotation = agent_name, pos :: list in
    let error, agents_annotation =
      Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters
        error agent_id agent_annotation handler.Cckappa_sig.agents_annotation
    in
    error, { handler with Cckappa_sig.agents_annotation }

let declare_agent parameters error handler agent_string pos =
  let agents_dic = handler.Cckappa_sig.agents_dic in
  let error, (bool, output) =
    Ckappa_sig.Dictionary_of_agents.allocate_bool parameters error
      Ckappa_sig.compare_unit_agent_name agent_string () Misc_sa.const_unit
      agents_dic
  in
  let error, (handler, agent_name) =
    match output with
    | None ->
      Exception.warn parameters error __POS__ Exit
        (handler, Ckappa_sig.dummy_agent_name)
    | Some (k, _, _, dic) ->
      if bool then (
        let error, int_constraints =
          Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters
            error k empty_agent_specification
            handler.Cckappa_sig.interface_constraints
        in
        let error, sites =
          Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters
            error k
            (Ckappa_sig.Dictionary_of_sites.init ())
            handler.Cckappa_sig.sites
        in
        let handler =
          let k' = Ckappa_sig.next_agent_name k in
          if Ckappa_sig.compare_agent_name k' handler.Cckappa_sig.nagents > 0
          then
            { handler with Cckappa_sig.nagents = k' }
          else
            handler
        in
        let error, handler =
          init_agent_declaration parameters error
            {
              handler with
              Cckappa_sig.agents_dic = dic;
              Cckappa_sig.interface_constraints = int_constraints;
              Cckappa_sig.sites;
            }
            k agent_string
        in
        error, (handler, k)
      ) else
        error, (handler, k)
  in
  let error, handler =
    add_agent_declaration parameters error handler agent_name pos
  in
  error, (handler, agent_name)

let declare_site create parameters make_site make_state (error, handler)
    agent_name site_name list =
  let site = make_site site_name in
  let states_dic = handler.Cckappa_sig.states_dic in
  let error, sites =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters error
      agent_name handler.Cckappa_sig.sites
  in
  match sites with
  | None ->
    Exception.warn parameters error __POS__ Exit
      (handler, [], Ckappa_sig.dummy_site_name)
  | Some sites ->
    let error, (bool, output) =
      Ckappa_sig.Dictionary_of_sites.allocate_bool parameters error
        Ckappa_sig.compare_unit_site_name site () Misc_sa.const_unit sites
    in
    (match output with
    | None ->
      Exception.warn parameters error __POS__ Exit
        (handler, [], Ckappa_sig.dummy_site_name)
    | Some (k, _, _, sites) ->
      let error, (states_dic, dic_states, handler) =
        if bool then (
          let error, dic_states = create parameters error in
          let error, states_dic =
            Ckappa_sig
            .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
            .set parameters error (agent_name, k) dic_states states_dic
          in
          let error, new_sites =
            Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set
              parameters error agent_name sites handler.Cckappa_sig.sites
          in
          ( error,
            ( states_dic,
              dic_states,
              { handler with Cckappa_sig.sites = new_sites } ) )
        ) else (
          match
            Ckappa_sig
            .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
            .get parameters error (agent_name, k) states_dic
          with
          | error, None ->
            let error, dic = create parameters error in
            Exception.warn parameters error __POS__ Exit
              (states_dic, dic, handler)
          | error, Some _u ->
            let error, dic_states =
              match
                Ckappa_sig
                .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
                .get parameters error (agent_name, k) states_dic
              with
              | error, None ->
                let error, dic_states = create parameters error in
                Exception.warn parameters error __POS__ Exit dic_states
              | error, Some dic_states -> error, dic_states
            in
            error, (states_dic, dic_states, handler)
        )
      in
      let error, (new_dic_states, l, bool) =
        List.fold_left
          (fun (error, (dic_states, l, bool)) internal ->
            let state = make_state internal in
            let error, (bool2, output) =
              Ckappa_sig.Dictionary_of_States.allocate_bool parameters error
                Ckappa_sig.compare_unit_state_index state () Misc_sa.const_unit
                dic_states
            in
            match output with
            | None ->
              Exception.warn parameters error __POS__ Exit (dic_states, l, bool2)
            | Some (state_id, _, _, dic) ->
              let l = (agent_name, k, state_id) :: l in
              if bool2 then
                error, (dic, l, bool2)
              else
                error, (dic_states, l, bool))
          (error, (dic_states, [], bool))
          list
      in
      if bool then (
        let error, states_dic =
          Ckappa_sig
          .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
          .set parameters error (agent_name, k) new_dic_states states_dic
        in
        let error, handler = error, { handler with Cckappa_sig.states_dic } in
        error, (handler, l, k)
      ) else
        error, (handler, l, k))

let declare_site_with_internal_states parameters =
  declare_site create_internal_state_dictionary parameters
    (fun x -> Ckappa_sig.Internal x)
    (fun x -> Ckappa_sig.Internal x)

let declare_site_with_binding_states parameters =
  declare_site create_binding_state_dictionary parameters
    (fun x -> Ckappa_sig.Binding x)
    (fun x -> Ckappa_sig.Binding x)

let declare_site_with_counter parameters (error, handler) ag site =
  declare_site create_internal_state_dictionary parameters
    (fun x -> Ckappa_sig.Counter x)
    (fun x -> Ckappa_sig.Counter x)
    (error, handler) ag site []

let declare_dual parameter error handler ag site state ag' site' state' =
  let dual = handler.Cckappa_sig.dual in
  let error, dual =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .set parameter error
      (ag, (site, state))
      (ag', site', state') dual
  in
  let error, dual =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .set parameter error
      (ag', (site', state'))
      (ag, site, state) dual
  in
  error, { handler with Cckappa_sig.dual }

let scan_agent parameters (error, handler) agent =
  let error, (handler, ag_id) =
    declare_agent parameters error handler agent.Ckappa_sig.agent_name
      (Some agent.Ckappa_sig.agent_name_pos)
  in
  let rec aux error interface handler =
    match interface with
    | Ckappa_sig.EMPTY_INTF -> error, handler
    | Ckappa_sig.COUNTER_SEP (counter, interface) ->
      let error, (handler, _, _c) =
        declare_site_with_counter parameters (error, handler) ag_id
          counter.Ckappa_sig.counter_name
      in
      aux error interface handler
    | Ckappa_sig.PORT_SEP (port, interface) ->
      let site_name = port.Ckappa_sig.port_name in
      let error, handler =
        match port.Ckappa_sig.port_int with
        | [] -> error, handler
        | list ->
          let list =
            List.fold_left
              (fun list x ->
                match x with
                | None -> list
                | Some x -> x :: list)
              [] (List.rev list)
          in
          let error, (handler, _, _c) =
            declare_site_with_internal_states parameters (error, handler) ag_id
              site_name list
          in
          error, handler
      in
      let error, handler =
        match port.Ckappa_sig.port_link with
        | Ckappa_sig.LNK_MISSING | Ckappa_sig.FREE | Ckappa_sig.LNK_ANY _ ->
          error, handler
        | Ckappa_sig.LNK_VALUE (_, agent', site', _, _)
        | Ckappa_sig.LNK_TYPE ((agent', _), (site', _)) ->
          let error, (handler, ag_id') =
            declare_agent parameters error handler agent' None
          in
          let error, (handler, _, _site_id) =
            declare_site_with_binding_states parameters (error, handler) ag_id
              site_name []
          in
          let error, (handler, _, site_id') =
            declare_site_with_binding_states parameters (error, handler) ag_id'
              site' []
          in
          let error, (handler, l1, site_id) =
            declare_site_with_binding_states parameters (error, handler) ag_id
              site_name
              [ Ckappa_sig.C_Lnk_type (ag_id', site_id') ]
          in
          let error, (handler, l2, _site_id') =
            declare_site_with_binding_states parameters (error, handler) ag_id'
              site'
              [ Ckappa_sig.C_Lnk_type (ag_id, site_id) ]
          in
          let error, handler =
            match l1, l2 with
            | ( [ (agent_id1, site_id1, state_id1) ],
                [ (agent_id2, site_id2, state_id2) ] ) ->
              declare_dual parameters error handler agent_id1 site_id1 state_id1
                agent_id2 site_id2 state_id2
            | _ -> Exception.warn parameters error __POS__ Exit handler
          in
          error, handler
        | Ckappa_sig.LNK_SOME _ ->
          let error, (handler, _, _site_id) =
            declare_site_with_binding_states parameters (error, handler) ag_id
              site_name []
          in
          error, handler
      in
      aux error interface handler
  in
  aux error agent.Ckappa_sig.ag_intf handler

let rec scan_mixture parameters remanent mixture =
  match mixture with
  | Ckappa_sig.EMPTY_MIX -> remanent
  | Ckappa_sig.SKIP mixture -> scan_mixture parameters remanent mixture
  | Ckappa_sig.COMMA (agent, mixture)
  | Ckappa_sig.DOT (_, agent, mixture)
  | Ckappa_sig.PLUS (_, agent, mixture) ->
    let remanent = scan_agent parameters remanent agent in
    scan_mixture parameters remanent mixture

let scan_token parameters remanent _alg =
  (*TO DO*)
  match Remanent_parameters.get_called_from parameters with
  | Remanent_parameters_sig.KaSa ->
    let error, remanent = remanent in
    Exception.warn parameters error __POS__
      ~message:"Tokens are not implemented in KaSa yet" Exit remanent
  | Remanent_parameters_sig.KaSim | Remanent_parameters_sig.Internalised
  | Remanent_parameters_sig.Server ->
    remanent

let scan_alg _parameters remanent _alg =
  (*TO DO*)
  remanent

let scan_initial_states parameters =
  List.fold_left (fun remanent ((alg, _pos), init_t) ->
      let remanent = scan_alg parameters remanent alg in
      match init_t with
      | Ast.INIT_MIX (mixture, _pos') ->
        scan_mixture parameters remanent mixture
      | Ast.INIT_TOK tk_l ->
        List.fold_left (scan_token parameters) remanent tk_l)

let scan_declarations parameters =
  List.fold_left (fun remanent a -> scan_agent parameters remanent a)

let scan_observables _scan_mixt _parameters remanent _variable =
  (*TODO*)
  remanent

let scan_perts scan_mixt parameters =
  List.fold_left (fun remanent ((_, _, m, _), _) ->
      List.fold_left
        (fun remanent m ->
          match m with
          | Ast.APPLY (_, (r, _)) ->
            scan_mixture parameters
              (scan_mixt parameters remanent r.Ckappa_sig.lhs)
              r.Ckappa_sig.rhs
          | Ast.CFLOWMIX (_, (m, _)) | Ast.SPECIES_OF (_, _, (m, _)) ->
            scan_mixt parameters remanent m
          | Ast.UPDATE _ | Ast.STOP _ | Ast.SNAPSHOT _ | Ast.PLOTENTRY
          | Ast.PRINT _ | Ast.CFLOWLABEL _ | Ast.DINOFF _ | Ast.DIN _ ->
            remanent)
        remanent m)

let scan_rules scan_mixt parameters a b =
  let _ =
    if Remanent_parameters.get_trace parameters then (
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "Scan rules!"
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      ()
    )
  in
  List.fold_left
    (fun remanent (_, (rule, _)) ->
      scan_mixture parameters
        (scan_mixt parameters remanent rule.Ckappa_sig.lhs)
        rule.Ckappa_sig.rhs)
    a b

let reverse_agents_annotation parameters (error, remanent) =
  let agents_annotation = remanent.Cckappa_sig.agents_annotation in
  let error, agents_annotation =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.fold parameters error
      (fun parameters error i (a, l) agents_annotation ->
        Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.set parameters
          error i
          (a, List.rev l)
          agents_annotation)
      agents_annotation agents_annotation
  in

  error, { remanent with Cckappa_sig.agents_annotation }

let scan_compil parameters error compil =
  let parameters =
    Remanent_parameters.set_trace parameters
      (local_trace || Remanent_parameters.get_trace parameters)
  in
  let also_explore_tested_agents =
    Remanent_parameters.lexical_analysis_of_tested_only_patterns parameters
  in
  let scan_tested_mixture =
    if also_explore_tested_agents then
      scan_mixture
    else
      fun _parameters remanent _mixture ->
    remanent
  in
  let remanent = empty_handler parameters error in
  let remanent = scan_declarations parameters remanent compil.Ast.signatures in
  let remanent = scan_initial_states parameters remanent compil.Ast.init in
  let remanent =
    scan_observables scan_tested_mixture parameters remanent
      compil.Ast.observables
  in
  let remanent =
    scan_perts scan_tested_mixture parameters remanent compil.Ast.perturbations
  in
  let remanent =
    scan_rules scan_tested_mixture parameters remanent compil.Ast.rules
  in
  let remanent = reverse_agents_annotation parameters remanent in
  remanent
