(**
   * print_handler.ml
   * openkappa
   * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2011, the 16th of March
   * Last modification: Time-stamp: <Mar 19 2020>
   * *
   * Primitives to use a kappa handler
   *
   * Copyright 2010,2011,2012,2013,2014,2015 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let local_trace = false
let nrules _parameter _error handler = handler.Cckappa_sig.nrules
let nvars _parameter _error handler = handler.Cckappa_sig.nvars
let nagents _parameter _error handler = handler.Cckappa_sig.nagents

let check_pos parameter ka_pos ml_pos message error error' =
  match ml_pos with
  | None -> error'
  | Some ml_pos ->
    Exception.check_point Exception.warn parameter ~message ?pos:ka_pos error
      error' ml_pos Exit

let translate_agent ?(ml_pos = None) ?ka_pos ?(message = "") parameter error
    handler ag =
  let error', (a, _, _) =
    Misc_sa.unsome
      (Ckappa_sig.Dictionary_of_agents.translate parameter error ag
         handler.Cckappa_sig.agents_dic) (fun error ->
        Exception.warn parameter error __POS__ Exit ("", (), ()))
  in
  check_pos parameter ka_pos ml_pos message error error', a

let translate_site ?(ml_pos = None) ?(ka_pos = None) ?(message = "") parameter
    error handler agent_name site =
  let error', dic =
    Misc_sa.unsome
      (Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameter
         error agent_name handler.Cckappa_sig.sites) (fun error ->
        Exception.warn parameter error __POS__ Exit
          (Ckappa_sig.Dictionary_of_sites.init ()))
  in
  let error', (a, _, _) =
    Misc_sa.unsome
      (Ckappa_sig.Dictionary_of_sites.translate parameter error' site dic)
      (fun error ->
        Exception.warn parameter error __POS__ Exit
          (Ckappa_sig.Internal "", (), ()))
  in
  check_pos parameter ka_pos ml_pos message error error', a

let translate_state ?(ml_pos = None) ?(ka_pos = None) ?(message = "") parameter
    error handler agent site state =
  let error', dic =
    Misc_sa.unsome
      (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
       .get parameter error (agent, site) handler.Cckappa_sig.states_dic)
      (fun error ->
        Exception.warn parameter error __POS__ Exit
          (Ckappa_sig.Dictionary_of_States.init ()))
  in
  let error', (a, _, _) =
    Misc_sa.unsome
      (Ckappa_sig.Dictionary_of_States.translate parameter error' state dic)
      (fun error ->
        Exception.warn parameter error __POS__ Exit
          (Ckappa_sig.Internal "", (), ()))
  in
  check_pos parameter ka_pos ml_pos message error error', a

let translate_binding_type parameter error handler agent site =
  let error, agent_name = translate_agent parameter error handler agent in
  let error, site_name =
    match translate_site parameter error handler agent site with
    | error, Ckappa_sig.Binding s -> error, s
    | error, (Ckappa_sig.Internal s | Ckappa_sig.Counter s) ->
      Exception.warn parameter error __POS__ Exit s
  in
  let binding_type_symbol = Remanent_parameters.get_at_symbol parameter in
  ( error,
    Public_data.string_of_binding_type ~binding_type_symbol ~agent_name
      ~site_name () )

let dual ?(ml_pos = None) ?(ka_pos = None) ?(message = "") parameter error
    handler agent site state =
  let error', a =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .unsafe_get parameter error
      (agent, (site, state))
      handler.Cckappa_sig.dual
  in
  check_pos parameter ka_pos ml_pos message error error', a

let is_binding_site ?(ml_pos = None) ?(ka_pos = None) ?(message = "") parameter
    error handler agent site =
  let error, site =
    translate_site ~ml_pos ~ka_pos ~message parameter error handler agent site
  in
  match site with
  | Ckappa_sig.Internal _ | Ckappa_sig.Counter _ -> error, false
  | Ckappa_sig.Binding _ -> error, true

let is_internal_site ?(ml_pos = None) ?(ka_pos = None) ?(message = "") parameter
    error handler agent site =
  let error, site =
    translate_site ~ml_pos ~ka_pos ~message parameter error handler agent site
  in
  match site with
  | Ckappa_sig.Internal _ -> error, true
  | Ckappa_sig.Counter _ | Ckappa_sig.Binding _ -> error, false

let is_counter ?(ml_pos = None) ?(ka_pos = None) ?(message = "") parameter error
    handler agent site =
  let error, site =
    translate_site ~ml_pos ~ka_pos ~message parameter error handler agent site
  in
  match site with
  | Ckappa_sig.Counter _ -> error, true
  | Ckappa_sig.Internal _ | Ckappa_sig.Binding _ -> error, false

let last_site_of_agent ?(ml_pos = None) ?(ka_pos = None) ?(message = "")
    parameters error handler agent_name =
  let error', dic =
    Misc_sa.unsome
      (Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters
         error agent_name handler.Cckappa_sig.sites) (fun error ->
        Exception.warn parameters error __POS__ Exit
          (Ckappa_sig.Dictionary_of_sites.init ()))
  in
  let error', last_entry =
    Ckappa_sig.Dictionary_of_sites.last_entry parameters error' dic
  in
  check_pos parameters ka_pos ml_pos message error error', last_entry

let last_state_of_site ?(ml_pos = None) ?(ka_pos = None) ?(message = "")
    parameters error handler agent_name site_name =
  let error', dic =
    Misc_sa.unsome
      (Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
       .get parameters error (agent_name, site_name)
         handler.Cckappa_sig.states_dic)
      (fun error ->
        Exception.warn parameters error __POS__ Exit
          (Ckappa_sig.Dictionary_of_States.init ()))
  in
  let error', last_entry =
    Ckappa_sig.Dictionary_of_States.last_entry parameters error' dic
  in
  check_pos parameters ka_pos ml_pos message error error', last_entry

let complementary_interface ?(ml_pos = None) ?(ka_pos = None) ?(message = "")
    parameters error handler agent_name interface =
  let error, last_entry =
    last_site_of_agent ~ml_pos ~ka_pos ~message parameters error handler
      agent_name
  in
  let l =
    let rec aux k output =
      if Ckappa_sig.compare_site_name k Ckappa_sig.dummy_site_name < 0 then
        output
      else
        aux (Ckappa_sig.pred_site_name k) (k :: output)
    in
    aux last_entry []
  in
  error, Misc_sa.list_minus l interface

let is_reverse parameters error compiled rule_id =
  let rules = compiled.Cckappa_sig.rules in
  let error, rule =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
      rule_id rules
  in
  match rule with
  | None -> Exception.warn parameters error __POS__ Exit false
  | Some rule ->
    error, rule.Cckappa_sig.e_rule_initial_direction = Ckappa_sig.Reverse

let has_no_label parameters error compiled rule_id =
  let rules = compiled.Cckappa_sig.rules in
  let error, rule =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
      rule_id rules
  in
  match rule with
  | None -> Exception.warn parameters error __POS__ Exit true
  | Some rule ->
    ( error,
      (match rule.Cckappa_sig.e_rule_label with
      | None -> true
      | Some _ -> false) )

let info_of_rule parameters ?(with_rates = false) ?(original = false) error
    compiled (rule_id : Ckappa_sig.c_rule_id) =
  let rules = compiled.Cckappa_sig.rules in
  let error, rule =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
      rule_id rules
  in
  match rule with
  | None ->
    Exception.warn parameters error __POS__ Exit
      ( "",
        Locality.dummy,
        Public_data.Dummy_rule_direction,
        "",
        Ckappa_sig.dummy_rule_id )
  | Some rule ->
    let label_opt = rule.Cckappa_sig.e_rule_label in
    let error, (label, _) =
      Misc_sa.unsome (error, label_opt) (fun error ->
          error, Locality.dummy_annot "")
    in
    let label =
      if label = "" then
        ""
      else (
        match rule.Cckappa_sig.e_rule_initial_direction with
        | Ckappa_sig.Direct -> label
        | Ckappa_sig.Reverse -> Ast.flip_label label
      )
    in
    let position = rule.Cckappa_sig.e_rule_rule.Ckappa_sig.position in
    let direction =
      match rule.Cckappa_sig.e_rule_initial_direction with
      | Ckappa_sig.Direct -> Public_data.Direct_rule
      | Ckappa_sig.Reverse -> Public_data.Reverse_rule
    in
    let ast =
      match original, with_rates with
      | true, true -> rule.Cckappa_sig.e_rule_rule.Ckappa_sig.original_ast
      | true, false ->
        rule.Cckappa_sig.e_rule_rule.Ckappa_sig.original_ast_no_rate
      | false, true -> rule.Cckappa_sig.e_rule_rule.Ckappa_sig.ast
      | false, false -> rule.Cckappa_sig.e_rule_rule.Ckappa_sig.ast_no_rate
    in
    error, (label, position, direction, ast, rule_id)

let hide rule = { rule with Public_data.rule_hidden = true }

let info_of_agent parameters error handler _compiled agent =
  let info_of_agents = handler.Cckappa_sig.agents_annotation in
  let error, info_opt =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameters error
      agent info_of_agents
  in
  let error, (agent_name, positions) =
    match info_opt with
    | None -> Exception.warn parameters error __POS__ Exit ("", [])
    | Some (agent_name, positions) -> error, (agent_name, positions)
  in
  error, (agent_name, positions, agent)

let info_of_var parameters error handler compiled
    (rule_id : Ckappa_sig.c_rule_id) =
  let vars = compiled.Cckappa_sig.variables in
  let nrules = nrules parameters error handler in
  let var_id = Ckappa_sig.sub_rule_id rule_id nrules in
  let error, var =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error var_id
      vars
  in
  match var with
  | None ->
    Exception.warn parameters error __POS__ Exit
      ( "VAR " ^ Ckappa_sig.string_of_rule_id var_id,
        Locality.dummy,
        Public_data.Variable,
        "",
        var_id )
  | Some var ->
    ( error,
      ( fst var.Cckappa_sig.e_id_dot,
        snd var.Cckappa_sig.e_id,
        Public_data.Variable,
        ""
        (* TO DO: string for the ast representation (from var.Cckappa_sig.c_variable?) *),
        var_id ) )

let string_of_info ?(with_rule = true) ?(with_rule_name = true)
    ?(with_rule_id = true) ?(with_loc = true) ?(with_ast = true)
    ?(kind = "rule ") (label, pos, _direction, ast, id) =
  let label =
    if with_rule_name then
      label
    else
      ""
  in
  let pos =
    if (not with_loc) || pos = Locality.dummy then
      ""
    else
      Locality.to_string pos
  in
  let ast =
    if not with_ast then
      ""
    else
      ast
  in
  let id =
    if not with_rule_id then
      ""
    else
      Ckappa_sig.string_of_rule_id id
  in
  let prefix =
    if not with_rule then
      ""
    else
      kind
  in
  let s =
    match label, pos, ast, id with
    | "", "", "", s | "", "", s, _ | "", s, "", _ | s, "", _, _ -> prefix ^ s
    | "", s2, s1, _ | s1, s2, _, _ -> prefix ^ s1 ^ " (" ^ s2 ^ ")"
  in
  s

let pos_of_info (_, info, _, _, _) = info

let string_of_rule ?(with_rule = true) ?(with_rule_name = true)
    ?(with_rule_id = true) ?(with_loc = true) ?(with_ast = true) parameters
    error compiled rule_id =
  let kind =
    if with_rule then
      "rule "
    else
      ""
  in
  let error, info = info_of_rule parameters error compiled rule_id in
  ( error,
    string_of_info ~with_rule_name ~with_rule_id ~with_loc ~with_ast ~kind info
  )

let pos_of_rule parameters error _ compiled rule_id =
  let error, info = info_of_rule parameters error compiled rule_id in
  error, pos_of_info info

let string_of_var ?(with_rule = true) ?(with_rule_name = true)
    ?(with_rule_id = true) ?(with_loc = true) ?(with_ast = true) parameters
    error handler compiled (rule_id : Ckappa_sig.c_rule_id) =
  let kind =
    if with_rule then
      "var "
    else
      ""
  in
  let error, info =
    info_of_var parameters error handler compiled
      (rule_id : Ckappa_sig.c_rule_id)
  in
  ( error,
    string_of_info ~with_rule_name ~with_rule_id ~with_loc ~with_ast ~kind info
  )

let pos_of_var parameters error handler compiled rule_id =
  let error, info = info_of_var parameters error handler compiled rule_id in
  error, pos_of_info info

let convert_id rule var parameters error handler compiled id =
  let int = Ckappa_sig.int_of_rule_id id in
  let nrules = nrules parameters error handler in
  if int < nrules then (
    let error, (a, b, c, d, e) = info_of_rule parameters error compiled id in
    error, Public_data.Rule (rule e b a d c)
  ) else (
    let error, (a, b, c, d, e) =
      info_of_var parameters error handler compiled id
    in
    error, Public_data.Var (var e b a d c)
  )

let convert_id_short =
  convert_id
    (fun a _ _ _ _ -> Ckappa_sig.int_of_rule_id a)
    (fun a _ _ _ _ -> Ckappa_sig.int_of_rule_id a)

let convert_id_refined =
  convert_id
    (fun a b d e c ->
      {
        Public_data.rule_id = Ckappa_sig.int_of_rule_id a;
        Public_data.rule_position = b;
        Public_data.rule_direction = c;
        Public_data.rule_label = d;
        Public_data.rule_ast = e;
        Public_data.rule_hidden = false;
      })
    (fun a b d e _ ->
      {
        Public_data.var_id = Ckappa_sig.int_of_rule_id a;
        Public_data.var_position = b;
        Public_data.var_label = d;
        Public_data.var_ast = e;
      })

let string_of_rule_or_var ?(with_rule = true) ?(with_rule_name = true)
    ?(with_rule_id = true) ?(with_loc = true) ?(with_ast = true) parameters
    error handler compiled (rule_id : Ckappa_sig.c_rule_id) =
  let nrules = nrules parameters error handler in
  if Ckappa_sig.compare_rule_id rule_id (Ckappa_sig.rule_id_of_int nrules) < 0
  then
    string_of_rule ~with_rule ~with_rule_name ~with_rule_id ~with_loc ~with_ast
      parameters error compiled rule_id
  else
    string_of_var ~with_rule ~with_rule_name ~with_rule_id ~with_loc ~with_ast
      parameters error handler compiled rule_id

(*mapping agent of type int to string*)
let string_of_agent parameter error handler_kappa
    (agent_type : Ckappa_sig.c_agent_name) =
  let agents_dic = handler_kappa.Cckappa_sig.agents_dic in
  (*get sites dictionary*)
  let error, output =
    Ckappa_sig.Dictionary_of_agents.translate parameter error agent_type
      agents_dic
  in
  match output with
  | None -> Exception.warn parameter error __POS__ Exit ""
  | Some (agent_name, _, _) -> error, agent_name

let print_site parameter ?state ?(add_parentheses = false) site =
  let state =
    match state with
    | None ->
      if add_parentheses then
        Some ""
      else
        state
    | Some _ -> state
  in
  match site, state with
  | Ckappa_sig.Counter a, Some state ->
    a
    ^ Remanent_parameters.get_open_counterval parameter
    ^ Remanent_parameters.get_counterval_symbol parameter
    ^ state
    ^ Remanent_parameters.get_close_counterval parameter
  | Ckappa_sig.Internal a, Some state ->
    a
    ^ Remanent_parameters.get_open_internal_state parameter
    ^ Remanent_parameters.get_internal_state_symbol parameter
    ^ state
    ^ Remanent_parameters.get_close_internal_state parameter
  | Ckappa_sig.Binding a, Some state ->
    a
    ^ Remanent_parameters.get_open_binding_state parameter
    ^ Remanent_parameters.get_bound_symbol parameter
    ^ state
    ^ Remanent_parameters.get_close_binding_state parameter
  | (Ckappa_sig.Binding a | Ckappa_sig.Internal a | Ckappa_sig.Counter a), None
    ->
    a

(*print function for contact map*)

let print_site_contact_map site =
  match site with
  | Ckappa_sig.Internal a -> a
  | Ckappa_sig.Binding a -> a
  | Ckappa_sig.Counter a -> a

(*mapping state of type int to string*)

let print_state parameter error handler state =
  match state with
  | Ckappa_sig.Internal a -> error, a
  | Ckappa_sig.Counter a -> error, string_of_int a
  | Ckappa_sig.Binding Ckappa_sig.C_Free ->
    error, Remanent_parameters.get_free_symbol parameter
  | Ckappa_sig.Binding (Ckappa_sig.C_Lnk_type (a, b)) ->
    let error, s = translate_binding_type parameter error handler a b in
    error, s

let print_state_fully_deciphered parameter error handler_kappa state =
  match state with
  | Ckappa_sig.Internal a ->
    ( error,
      Remanent_parameters.get_open_internal_state parameter
      ^ Remanent_parameters.get_internal_state_symbol parameter
      ^ a
      ^ Remanent_parameters.get_close_internal_state parameter )
  | Ckappa_sig.Counter a ->
    ( error,
      Remanent_parameters.get_open_counterval parameter
      ^ Remanent_parameters.get_counterval_symbol parameter
      ^ string_of_int a
      ^ Remanent_parameters.get_close_counterval parameter )
  | Ckappa_sig.Binding Ckappa_sig.C_Free ->
    ( error,
      Remanent_parameters.get_open_binding_state parameter
      ^ Remanent_parameters.get_free_symbol parameter
      ^ Remanent_parameters.get_close_binding_state parameter )
  | Ckappa_sig.Binding (Ckappa_sig.C_Lnk_type (agent_name, b)) ->
    let error, s =
      translate_binding_type parameter error handler_kappa agent_name b
    in
    ( error,
      Remanent_parameters.get_open_binding_state parameter
      ^ s
      ^ Remanent_parameters.get_close_binding_state parameter )

let string_of_state_gen print_state parameter error handler_kappa agent_name
    site_name state =
  let error, b_counter =
    is_counter parameter error handler_kappa agent_name site_name
  in
  if b_counter then
    print_state parameter error handler_kappa
      (Ckappa_sig.Counter (Ckappa_sig.int_of_state_index state))
  else (
    let error, state_dic =
      match
        Ckappa_sig
        .Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
        .get parameter error (agent_name, site_name)
          handler_kappa.Cckappa_sig.states_dic
      with
      | error, None ->
        Exception.warn parameter error __POS__ Exit
          (Ckappa_sig.Dictionary_of_States.init ())
      | error, Some i -> error, i
    in
    let error, value =
      match
        Ckappa_sig.Dictionary_of_States.translate parameter error state
          state_dic
      with
      | error, None ->
        Exception.warn parameter error __POS__ Exit (Ckappa_sig.Internal "")
      | error, Some (value, _, _) -> error, value
    in
    print_state parameter error handler_kappa value
  )

let string_of_state = string_of_state_gen print_state

let string_of_state_fully_deciphered =
  string_of_state_gen print_state_fully_deciphered

let string_of_site_aux ?(ml_pos = None) ?(ka_pos = None) ?(message = "")
    parameter error handler_kappa ?state agent_name
    (site_int : Ckappa_sig.c_site_name) =
  let error', site_type =
    translate_site parameter error handler_kappa agent_name site_int
  in
  let error', state =
    match state with
    | None -> error', None
    | Some a ->
      let error', s =
        string_of_state parameter error' handler_kappa agent_name site_int a
      in
      error', Some s
  in
  check_pos parameter ka_pos ml_pos message error error', site_type, state

let string_of_site parameter error handler_kappa ?state
    ?(add_parentheses = false) agent_type site_int =
  let error, site_type, state =
    string_of_site_aux parameter error handler_kappa ?state agent_type site_int
  in
  error, print_site parameter ?state ~add_parentheses site_type

(*this function used in views_domain*)
let string_of_site_update_views parameter error handler_kappa agent_type
    site_int =
  let error, site_type, _ =
    string_of_site_aux parameter error handler_kappa agent_type site_int
  in
  let add_parentheses = true in
  error, print_site parameter ~add_parentheses site_type

let string_of_site_in_natural_language parameter error handler_kapp agent_type
    (site_int : Ckappa_sig.c_site_name) =
  let error, site_type, _ =
    string_of_site_aux parameter error handler_kapp agent_type site_int
  in
  match site_type with
  | Ckappa_sig.Internal x -> error, "the internal state of site " ^ x
  | Ckappa_sig.Binding x -> error, "the binding state of site " ^ x
  | Ckappa_sig.Counter x -> error, "the value of counter " ^ x

let string_of_site_in_file_name parameter error handler_kapp agent_type
    (site_int : Ckappa_sig.c_site_name) =
  let error, site_type, _ =
    string_of_site_aux parameter error handler_kapp agent_type site_int
  in
  match site_type with
  | Ckappa_sig.Internal x -> error, x ^ "_"
  | Ckappa_sig.Binding x -> error, x ^ "^"
  | Ckappa_sig.Counter x -> error, x ^ "="

let string_of_site_contact_map ?(ml_pos = None) ?(ka_pos = None) ?(message = "")
    parameter error handler_kappa agent_name site_int =
  let error, site_type, _ =
    string_of_site_aux ~ml_pos ~ka_pos ~message parameter error handler_kappa
      agent_name site_int
  in
  error, print_site_contact_map site_type

let print_labels parameters error handler couple =
  let _ = Quark_type.Labels.dump_couple parameters error handler couple in
  error

let get_label_of_rule_txt _parameters error rule =
  error, rule.Cckappa_sig.e_rule_label

let get_label_of_rule_dot _parameters error rule =
  error, rule.Cckappa_sig.e_rule_label_dot

let get_label_of_var_txt _parameters error rule =
  error, fst rule.Cckappa_sig.e_id

let get_label_of_var_dot _parameters error rule =
  error, fst rule.Cckappa_sig.e_id_dot

let print_rule_txt parameters error rule_id m1 _m2 rule =
  let m = "'" ^ m1 ^ "' " in
  let error, _ =
    ( error,
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (if m = "" then
           "rule(" ^ Ckappa_sig.string_of_rule_id rule_id ^ "): "
         else
           ("rule(" ^ Ckappa_sig.string_of_rule_id rule_id) ^ "):" ^ m) )
  in
  let error = Print_ckappa.print_rule parameters error rule in
  error

let print_var_txt parameters error var_id m1 _m2 var =
  let m = "'" ^ m1 ^ "' " in
  let error, _ =
    ( error,
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (if m = "" then
           "var(" ^ Ckappa_sig.string_of_rule_id var_id ^ ")"
         else
           ("var(" ^ Ckappa_sig.string_of_rule_id var_id) ^ "):" ^ m) )
  in
  let error = Print_ckappa.print_alg parameters error var in
  error

let print_rule_dot parameters error _rule_id m1 m2 rule =
  let error =
    if m1 <> "" && not (Remanent_parameters.get_prompt_full_rule_def parameters)
    then (
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" m1
      in
      error
    ) else (
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s:" m2
      in
      let error = Print_ckappa.print_rule parameters error rule in
      error
    )
  in
  error

let print_var_dot parameters (error : Exception.method_handler) _var_id m1 m2
    var =
  let error =
    if m1 <> "" && not (Remanent_parameters.get_prompt_full_var_def parameters)
    then (
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" m1
      in
      error
    ) else (
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s:" m2
      in
      let error = Print_ckappa.print_alg parameters error var in
      error
    )
  in
  error

let print_rule_or_var parameters error handler compiled print_rule print_var
    get_label_of_rule get_label_of_var rule_id =
  let rules = compiled.Cckappa_sig.rules in
  let vars = compiled.Cckappa_sig.variables in
  let nrules = nrules parameters error handler in
  if Ckappa_sig.compare_rule_id rule_id (Ckappa_sig.rule_id_of_int nrules) < 0
  then (
    let error, rule =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
        rule_id rules
    in
    match rule with
    | None ->
      let a, b = Exception.warn parameters error __POS__ Exit () in
      a, false, b
    | Some rule ->
      let error, label = get_label_of_rule parameters error rule in
      let error, (m1, _) =
        Misc_sa.unsome (error, label) (fun error ->
            error, Locality.dummy_annot "")
      in
      let m1 =
        if m1 = "" then
          m1
        else (
          match rule.Cckappa_sig.e_rule_initial_direction with
          | Ckappa_sig.Direct -> m1
          | Ckappa_sig.Reverse -> Ast.flip_label m1
        )
      in
      let error =
        print_rule parameters error rule_id m1
          (Ckappa_sig.string_of_rule_id rule_id)
          rule.Cckappa_sig.e_rule_rule
      in
      error, true, ()
  ) else (
    let var_id = Ckappa_sig.sub_rule_id rule_id nrules in
    let error, var =
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error
        var_id vars
    in
    match var with
    | None ->
      let a, b = Exception.warn parameters error __POS__ Exit () in
      a, false, b
    | Some var ->
      let b = var.Cckappa_sig.c_variable in
      let error, m1 = get_label_of_var parameters error var in
      let m2 = Ckappa_sig.string_of_rule_id var_id in
      let error = print_var parameters error var_id m1 m2 b in
      error, true, ()
  )

let has_a_binding_state parameter error kappa_handler agent_type site =
  let error, site =
    translate_site parameter error kappa_handler agent_type site
  in
  match site with
  | Ckappa_sig.Internal s | Ckappa_sig.Counter s ->
    let new_site = Ckappa_sig.Binding s in
    let error, dic_opt =
      Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.get parameter error
        agent_type kappa_handler.Cckappa_sig.sites
    in
    (match dic_opt with
    | None -> Exception.warn parameter error __POS__ Exit false
    | Some dic ->
      Ckappa_sig.Dictionary_of_sites.member parameter error new_site dic)
  | Ckappa_sig.Binding _ -> Exception.warn parameter error __POS__ Exit false

let id_of_binding_type parameter error handler_kappa agent_type site agent_type'
    site' =
  let state = Ckappa_sig.C_Lnk_type (agent_type', site') in
  let error, state_dic =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .get parameter error (agent_type, site) handler_kappa.Cckappa_sig.states_dic
  in
  match state_dic with
  | None ->
    Exception.warn parameter error __POS__ Exit Ckappa_sig.dummy_state_index
  | Some state_dic ->
    let error, bool =
      Ckappa_sig.Dictionary_of_States.member parameter error
        (Ckappa_sig.Binding state) state_dic
    in
    if not bool then
      Exception.warn parameter error __POS__
        ~message:
          ("agent "
          ^ string_of_int (Ckappa_sig.int_of_agent_name agent_type)
          ^ " site"
          ^ string_of_int (Ckappa_sig.int_of_site_name site)
          ^ "agent "
          ^ string_of_int (Ckappa_sig.int_of_agent_name agent_type')
          ^ " site"
          ^ string_of_int (Ckappa_sig.int_of_site_name site'))
        Exit Ckappa_sig.dummy_state_index
    else (
      match
        Ckappa_sig.Dictionary_of_States.allocate_bool parameter error
          Ckappa_sig.compare_unit_state_index (Ckappa_sig.Binding state) ()
          Misc_sa.const_unit state_dic
      with
      | error, (_bool, None) ->
        Exception.warn parameter error __POS__ Exit Ckappa_sig.dummy_state_index
      | error, (_bool, Some (a, _, _, _)) -> error, a
    )

let state_list parameter handler error agent_type site_type =
  let error, n =
    last_state_of_site parameter error handler agent_type site_type
  in
  let rec aux k l =
    if Ckappa_sig.compare_state_index Ckappa_sig.dummy_state_index k > 0 then
      l
    else
      aux (Ckappa_sig.pred_state_index k) (k :: l)
  in
  error, aux n []
