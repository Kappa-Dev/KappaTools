(**
   * translate_sig.ml
   * openkappa
   * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2010, the 12th of August
   * Last modification: Time-stamp: <Oct 02 2018>
   * *
   * Pretty printing of token library
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let trace = false
let local_trace = false

let string_of_port parameters port =
  "[state_min:"
  ^ Ckappa_sig.string_of_state_index_option_min parameters
      port.Cckappa_sig.site_state.Cckappa_sig.min
  ^ ";state_max:"
  ^ Ckappa_sig.string_of_state_index_option_max parameters
      port.Cckappa_sig.site_state.Cckappa_sig.max
  ^ "]"

let print_kasim_site x =
  match x with
  | () -> ""

let print_agent parameters error _handler agent =
  match agent with
  | Cckappa_sig.Unknown_agent (s, _id) ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%sunknown_agent:%s"
        (Remanent_parameters.get_prefix parameters)
        s
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error
  | Cckappa_sig.Dead_agent (agent, s, l, l') ->
    let parameters =
      Remanent_parameters.update_prefix parameters
        ("agent_type_"
        ^ Ckappa_sig.string_of_agent_name agent.Cckappa_sig.agent_name
        ^ ":")
    in
    let error =
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun a b error ->
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%ssite_type_%s->state:%s"
              (Remanent_parameters.get_prefix parameters)
              (Ckappa_sig.string_of_site_name a)
              (string_of_port parameters b)
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error)
        agent.Cckappa_sig.agent_interface error
    in
    let error =
      Cckappa_sig.KaSim_Site_map_and_set.Set.fold
        (fun x error ->
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%sUndefined site:%s"
              (Remanent_parameters.get_prefix parameters)
              (Print_handler.string_of_site parameters x)
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error)
        s error
    in
    let error =
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun s _ error ->
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%sdead site type %s"
              (Remanent_parameters.get_prefix parameters)
              (Ckappa_sig.string_of_site_name s)
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error)
        l error
    in
    let error =
      Ckappa_sig.Site_map_and_set.Map.fold
        (fun s _ error ->
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%sdead site type %s"
              (Remanent_parameters.get_prefix parameters)
              (Ckappa_sig.string_of_site_name s)
          in
          let () =
            Loggers.print_newline (Remanent_parameters.get_logger parameters)
          in
          error)
        l' error
    in
    error
  | Cckappa_sig.Agent agent ->
    let parameters =
      Remanent_parameters.update_prefix parameters
        ("agent_type_"
        ^ Ckappa_sig.string_of_agent_name agent.Cckappa_sig.agent_name
        ^ ":")
    in
    Ckappa_sig.Site_map_and_set.Map.fold
      (fun a b error ->
        let _ =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%ssite_type_%s->state:%s"
            (Remanent_parameters.get_prefix parameters)
            (Ckappa_sig.string_of_site_name a)
            (string_of_port parameters b)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        error)
      agent.Cckappa_sig.agent_interface error
  | Cckappa_sig.Ghost ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%sGhost"
        (Remanent_parameters.get_prefix parameters)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error

let print_diffagent parameters error _handler agent =
  let parameters =
    Remanent_parameters.update_prefix parameters
      ("agent_type_"
      ^ Ckappa_sig.string_of_agent_name agent.Cckappa_sig.agent_name
      ^ ":")
  in
  Ckappa_sig.Site_map_and_set.Map.fold
    (fun a b error ->
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%ssite_type_%s->state:%s"
          (Remanent_parameters.get_prefix parameters)
          (Ckappa_sig.string_of_site_name a)
          (string_of_port parameters b)
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      error)
    agent.Cckappa_sig.agent_interface error

let print_mixture parameters error handler mixture =
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s"
      (Remanent_parameters.get_prefix parameters)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.print
      (Remanent_parameters.update_prefix parameters "agent_id_")
      error
      (fun parameters error a ->
        let _ = print_agent parameters error handler a in
        error)
      mixture.Cckappa_sig.views
  in
  let error =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.print
      (Remanent_parameters.update_prefix parameters "bonds:agent_id_")
      error
      (fun parameters error a ->
        let error =
          Ckappa_sig.Site_map_and_set.Map.fold
            (fun k a error ->
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  "%ssite_type_%s->agent_id_%s.site_type_%s"
                  (Remanent_parameters.get_prefix parameters)
                  (Ckappa_sig.string_of_site_name k)
                  (Ckappa_sig.string_of_agent_id a.Cckappa_sig.agent_index)
                  (Ckappa_sig.string_of_site_name a.Cckappa_sig.site)
              in
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              error)
            a error
        in
        error)
      mixture.Cckappa_sig.bonds
  in
  let error =
    List.fold_left
      (fun error (i, j) ->
        let _ =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%i+%i"
            (Ckappa_sig.int_of_agent_id i)
            (Ckappa_sig.int_of_agent_id j)
        in
        error)
      error mixture.Cckappa_sig.plus
  in
  let error =
    List.fold_left
      (fun error (i, j) ->
        let _ =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%i.%i"
            (Ckappa_sig.int_of_agent_id i)
            (Ckappa_sig.int_of_agent_id j)
        in
        error)
      error mixture.Cckappa_sig.dot
  in
  error

let print_diffview parameters error handler diff =
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s"
      (Remanent_parameters.get_prefix parameters)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.print
      (Remanent_parameters.update_prefix parameters "agent_id_")
      error
      (fun parameters error a ->
        let _ = print_diffagent parameters error handler a in
        error)
      diff
  in
  error

let rec print_short_alg parameters error handler alg =
  match alg with
  | Alg_expr.BIN_ALG_OP (Operator.MULT, a1, a2), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "*" in
    let error = print_short_alg parameters error handler a2 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.BIN_ALG_OP (Operator.SUM, a1, a2), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_sep_plus_symbol parameters)
    in
    let error = print_short_alg parameters error handler a2 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.BIN_ALG_OP (Operator.DIV, a1, a2), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "/" in
    let error = print_short_alg parameters error handler a2 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.BIN_ALG_OP (Operator.MINUS, a1, a2), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "-" in
    let error = print_short_alg parameters error handler a2 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.BIN_ALG_OP (Operator.POW, a1, a2), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "**" in
    let error = print_short_alg parameters error handler a2 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.BIN_ALG_OP (Operator.MODULO, a1, a2), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "mod" in
    let error = print_short_alg parameters error handler a2 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.UN_ALG_OP (Operator.LOG, a1), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%slog%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.UN_ALG_OP (Operator.SQRT, a1), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%ssqrt%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.UN_ALG_OP (Operator.EXP, a1), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%sexp%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.UN_ALG_OP (Operator.SINUS, a1), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%ssin%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.UN_ALG_OP (Operator.COSINUS, a1), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%scos%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
    (* | Ast.UN_ALG_OP(Operator.ABS,a1),_ ->
       let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "(abs(" in
       let error = print_short_alg parameters error handler a1 in
       let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "))" in
       error *)
  | Alg_expr.UN_ALG_OP (Operator.TAN, a1), _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%stan%s"
        (Remanent_parameters.get_agent_open_symbol parameters)
        (Remanent_parameters.get_agent_open_symbol parameters)
    in
    let error = print_short_alg parameters error handler a1 in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s%s"
        (Remanent_parameters.get_agent_close_symbol parameters)
        (Remanent_parameters.get_agent_close_symbol parameters)
    in
    error
  | Alg_expr.STATE_ALG_OP Operator.TIME_VAR, _ ->
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "#TIME#"
    in
    error
  | Alg_expr.STATE_ALG_OP Operator.EVENT_VAR, _ ->
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "#EVENT#"
    in
    error
  | Alg_expr.ALG_VAR s, _ ->
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "(OBS(%s))" s
    in
    error
  | Alg_expr.CONST (Nbr.F f), _ ->
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "%f " f
    in
    error
  (*MOD: add print integer at compilation variables*)
  | Alg_expr.CONST (Nbr.I i), _ ->
    let _ =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "%d " i
    in
    error
  | Alg_expr.UN_ALG_OP (Operator.UMINUS, _), _
  | Alg_expr.UN_ALG_OP (Operator.INT, _), _
  | Alg_expr.BIN_ALG_OP (Operator.MAX, _, _), _
  | Alg_expr.BIN_ALG_OP (Operator.MIN, _, _), _
  | ( Alg_expr.STATE_ALG_OP
        ( Operator.NULL_EVENT_VAR | Operator.TMAX_VAR | Operator.EMAX_VAR
        | Operator.CPUTIME ),
      _ )
  | Alg_expr.CONST (Nbr.I64 _), _
  | Alg_expr.DIFF_TOKEN (_, _), _
  | Alg_expr.DIFF_KAPPA_INSTANCE (_, _), _
  | Alg_expr.IF _, _
  | Alg_expr.TOKEN_ID _, _
  | Alg_expr.KAPPA_INSTANCE _, _ ->
    (*to do*) error
(* | Ast.INFINITY _ ->
   let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "+oo" in
   error *)
(* | _ -> (*to do*)
   error *)

let print_var parameters error handler var =
  let s = fst var.Cckappa_sig.e_id in
  let _ =
    if s <> "" then
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s: " s
  in
  print_short_alg parameters error handler
    (Locality.dummy_annot var.Cckappa_sig.c_variable)

let print_variables parameters error handler var =
  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.print parameters error
    (fun parameters error var ->
      let _ =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s"
          (Remanent_parameters.get_prefix parameters)
      in
      let error = print_var parameters error handler var in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      error)
    var

let print_signatures _parameters error _handler _signature = error

let print_default_counters parameters error _handler map =
  if Ckappa_sig.AgentSite_map_and_set.Map.is_empty map then
    error
  else (
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s:"
        (Remanent_parameters.get_prefix parameters)
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    let () =
      Ckappa_sig.AgentSite_map_and_set.Map.iter
        (fun (a, s) state_opt ->
          let () =
            Loggers.fprintf
              (Remanent_parameters.get_logger parameters)
              "%s: %s.%s %s"
              (Remanent_parameters.get_prefix parameters)
              (Ckappa_sig.string_of_agent_name a)
              (Ckappa_sig.string_of_site_name s)
              (match state_opt with
              | None -> ""
              | Some a -> "->" ^ Ckappa_sig.string_of_state_index a)
          in
          Loggers.print_newline (Remanent_parameters.get_logger parameters))
        map
    in
    error
  )

let print_bond parameters relation (add1, add2) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s)@@site_type_%s%s(agent_id_%s,agent_type_%s)@@site_type_%s"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id add1.Cckappa_sig.agent_index)
      (Ckappa_sig.string_of_agent_name add1.Cckappa_sig.agent_type)
      (Ckappa_sig.string_of_site_name add1.Cckappa_sig.site)
      relation
      (Ckappa_sig.string_of_agent_id add2.Cckappa_sig.agent_index)
      (Ckappa_sig.string_of_agent_name add2.Cckappa_sig.agent_type)
      (Ckappa_sig.string_of_site_name add2.Cckappa_sig.site)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  ()

let print_half_bond parameters _relation (add1, _) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s)@@site_type_%s"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id add1.Cckappa_sig.agent_index)
      (Ckappa_sig.string_of_agent_name add1.Cckappa_sig.agent_type)
      (Ckappa_sig.string_of_site_name add1.Cckappa_sig.site)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  ()

let print_remove parameters (index, agent, list) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s)"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id index)
      (Ckappa_sig.string_of_agent_name agent.Cckappa_sig.agent_name)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let parameters_doc =
    Remanent_parameters.update_prefix parameters "documented_site:"
  in
  let () =
    Ckappa_sig.Site_map_and_set.Map.iter
      (fun site _ ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters_doc)
            "%s(agent_id_%s,agent_type_%s)@@site_type_%s"
            (Remanent_parameters.get_prefix parameters_doc)
            (Ckappa_sig.string_of_agent_id index)
            (Ckappa_sig.string_of_agent_name agent.Cckappa_sig.agent_name)
            (Ckappa_sig.string_of_site_name site)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        ())
      agent.Cckappa_sig.agent_interface
  in
  let parameters =
    Remanent_parameters.update_prefix parameters "undocumented_site:"
  in
  let () =
    List.iter
      (fun site ->
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameters)
            "%s(agent_id_%s,agent_type_%s)@@site_type_%s"
            (Remanent_parameters.get_prefix parameters)
            (Ckappa_sig.string_of_agent_id index)
            (Ckappa_sig.string_of_agent_name agent.Cckappa_sig.agent_name)
            (Ckappa_sig.string_of_site_name site)
        in
        let () =
          Loggers.print_newline (Remanent_parameters.get_logger parameters)
        in
        ())
      list
  in
  ()

let print_created_agent parameters (index, agent_name) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s)"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id index)
      (Ckappa_sig.string_of_agent_name agent_name)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  ()

let print_translate_counters parameters (site_address, action) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s,site_%s):(%s%s%s%s%s/%s%s%s%s%s,%s)"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id site_address.Cckappa_sig.agent_index)
      (Ckappa_sig.string_of_agent_name site_address.Cckappa_sig.agent_type)
      (Ckappa_sig.string_of_site_name site_address.Cckappa_sig.site)
      (match action.Cckappa_sig.precondition.Cckappa_sig.min with
      | None ->
        Remanent_parameters.get_open_int_interval_exclusive_symbol parameters
      | Some _ ->
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters)
      (match action.Cckappa_sig.precondition.Cckappa_sig.min with
      | None -> Remanent_parameters.get_minus_infinity_symbol parameters
      | Some i -> string_of_int (Ckappa_sig.int_of_state_index i))
      (Remanent_parameters.get_int_interval_separator_symbol parameters)
      (match action.Cckappa_sig.precondition.Cckappa_sig.max with
      | None -> Remanent_parameters.get_plus_infinity_symbol parameters
      | Some i -> string_of_int (Ckappa_sig.int_of_state_index i))
      (match action.Cckappa_sig.precondition.Cckappa_sig.max with
      | None ->
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters
      | Some _ ->
        Remanent_parameters.get_open_int_interval_exclusive_symbol parameters)
      (match action.Cckappa_sig.postcondition.Cckappa_sig.min with
      | None ->
        Remanent_parameters.get_open_int_interval_exclusive_symbol parameters
      | Some _ ->
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters)
      (match action.Cckappa_sig.postcondition.Cckappa_sig.min with
      | None -> Remanent_parameters.get_minus_infinity_symbol parameters
      | Some i -> string_of_int (Ckappa_sig.int_of_state_index i))
      (Remanent_parameters.get_int_interval_separator_symbol parameters)
      (match action.Cckappa_sig.postcondition.Cckappa_sig.max with
      | None -> Remanent_parameters.get_plus_infinity_symbol parameters
      | Some i -> string_of_int (Ckappa_sig.int_of_state_index i))
      (match action.Cckappa_sig.postcondition.Cckappa_sig.max with
      | None ->
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters
      | Some _ ->
        Remanent_parameters.get_open_int_interval_exclusive_symbol parameters)
      (if action.Cckappa_sig.increment = 0 then
         "-"
       else if action.Cckappa_sig.increment > 0 then
         Remanent_parameters.get_counterdeltaplus_symbol parameters
         ^ string_of_int action.Cckappa_sig.increment
       else
         Remanent_parameters.get_counterdeltaminus_symbol parameters
         ^ string_of_int (-action.Cckappa_sig.increment))
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  ()

let print_removed_counters parameters (site_address, condition) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s,site_%s):(%s%s%s%s%s)--"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id site_address.Cckappa_sig.agent_index)
      (Ckappa_sig.string_of_agent_name site_address.Cckappa_sig.agent_type)
      (Ckappa_sig.string_of_site_name site_address.Cckappa_sig.site)
      (match condition.Cckappa_sig.min with
      | None ->
        Remanent_parameters.get_open_int_interval_exclusive_symbol parameters
      | Some _ ->
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters)
      (match condition.Cckappa_sig.min with
      | None -> Remanent_parameters.get_minus_infinity_symbol parameters
      | Some i -> string_of_int (Ckappa_sig.int_of_state_index i))
      (Remanent_parameters.get_int_interval_separator_symbol parameters)
      (match condition.Cckappa_sig.max with
      | None -> Remanent_parameters.get_minus_infinity_symbol parameters
      | Some i -> string_of_int (Ckappa_sig.int_of_state_index i))
      (match condition.Cckappa_sig.max with
      | None ->
        Remanent_parameters.get_open_int_interval_inclusive_symbol parameters
      | Some _ ->
        Remanent_parameters.get_open_int_interval_exclusive_symbol parameters)
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  ()

let print_new_counters parameters (site_address, state) =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s(agent_id_%s,agent_type_%s,site_%s):(%s)++"
      (Remanent_parameters.get_prefix parameters)
      (Ckappa_sig.string_of_agent_id site_address.Cckappa_sig.agent_index)
      (Ckappa_sig.string_of_agent_name site_address.Cckappa_sig.agent_type)
      (Ckappa_sig.string_of_site_name site_address.Cckappa_sig.site)
      (string_of_int (Ckappa_sig.int_of_state_index state))
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  ()

let print_actions parameters error _handler actions =
  let parameters_unbinding =
    Remanent_parameters.update_prefix parameters "unbinding:"
  in
  let _ =
    List.iter
      (print_bond parameters_unbinding "....")
      (List.rev actions.Cckappa_sig.release)
  in
  let parameters_half_unbinding =
    Remanent_parameters.update_prefix parameters "1/2unbinding:"
  in
  let _ =
    List.iter
      (print_half_bond parameters_half_unbinding "....")
      (List.rev actions.Cckappa_sig.half_break)
  in
  let parameters_removal =
    Remanent_parameters.update_prefix parameters "deletion:"
  in
  let _ =
    List.iter
      (print_remove parameters_removal)
      (List.rev actions.Cckappa_sig.remove)
  in
  let parameters_creation =
    Remanent_parameters.update_prefix parameters "creation:"
  in
  let _ =
    List.iter
      (print_created_agent parameters_creation)
      (List.rev actions.Cckappa_sig.creation)
  in
  let parameters_binding =
    Remanent_parameters.update_prefix parameters "binding:"
  in
  let _ =
    List.iter
      (print_bond parameters_binding "----")
      (List.rev actions.Cckappa_sig.bind)
  in
  let parameters_translate_counters =
    Remanent_parameters.update_prefix parameters "counters:"
  in
  let _ =
    List.iter
      (print_translate_counters parameters_translate_counters)
      (List.rev actions.Cckappa_sig.translate_counters)
  in
  let _ =
    List.iter
      (print_removed_counters parameters_translate_counters)
      (List.rev actions.Cckappa_sig.removed_counters)
  in
  let _ =
    List.iter
      (print_new_counters parameters_translate_counters)
      (List.rev actions.Cckappa_sig.new_counters)
  in
  error

let print_rule parameters error handler rule =
  let parameters_lhs = Remanent_parameters.update_prefix parameters "lhs:" in
  let error =
    print_mixture parameters_lhs error handler
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs
  in
  let parameters_rhs = Remanent_parameters.update_prefix parameters "rhs:" in
  let error =
    print_mixture parameters_rhs error handler
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_rhs
  in
  let parameters_lhsdiff =
    Remanent_parameters.update_prefix parameters "direct:"
  in
  let error =
    print_diffview parameters_lhsdiff error handler
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_direct
  in
  let parameters_rhsdiff =
    Remanent_parameters.update_prefix parameters "reverse:"
  in
  let error =
    print_diffview parameters_rhsdiff error handler
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.diff_reverse
  in
  let parameters_actions =
    Remanent_parameters.update_prefix parameters "actions:"
  in
  let error =
    print_actions parameters_actions error handler
      rule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.actions
  in
  error

let print_rules parameters error handler rules =
  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.print parameters error
    (fun parameters error rule -> print_rule parameters error handler rule)
    rules

let print_observables _parameters error _handler _obs = error

let print_init parameters error handler init =
  let parameters_rhs =
    Remanent_parameters.update_prefix parameters "mixture:"
  in
  let error =
    print_mixture parameters_rhs error handler init.Cckappa_sig.e_init_c_mixture
  in
  error

let print_inits parameters error handler init =
  Int_storage.Nearly_inf_Imperatif.print parameters error
    (fun parameters error init -> print_init parameters error handler init)
    init

let print_perturbations _parameters error _handler _perturbations = error

let print_agent_annotation parameters error handler =
  Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.iter parameters error
    (fun parameters error agent_name (agent_string, locations) ->
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s%i:%s:"
          (Remanent_parameters.get_prefix parameters)
          (Ckappa_sig.int_of_agent_name agent_name)
          agent_string
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      let () =
        List.iter
          (fun position ->
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                "%s       %s"
                (Remanent_parameters.get_prefix parameters)
                (Locality.to_string position)
            in
            Loggers.print_newline (Remanent_parameters.get_logger parameters))
          locations
      in
      error)
    handler.Cckappa_sig.agents_annotation

let print_compil parameters error handler compil =
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s"
      (Remanent_parameters.get_prefix parameters)
  in
  let parameters' = Remanent_parameters.update_prefix parameters "variables:" in
  let error =
    print_variables parameters' error handler compil.Cckappa_sig.variables
  in
  let parameters' = Remanent_parameters.update_prefix parameters "signature:" in
  let error =
    print_signatures parameters' error handler compil.Cckappa_sig.signatures
  in
  let parameters' =
    Remanent_parameters.update_prefix parameters "agent definition:"
  in
  let error = print_agent_annotation parameters' error handler in
  let parameters' =
    Remanent_parameters.update_prefix parameters "default_counters:"
  in
  let error =
    print_default_counters parameters' error handler
      compil.Cckappa_sig.counter_default
  in
  let parameters' = Remanent_parameters.update_prefix parameters "rules:" in
  let error = print_rules parameters' error handler compil.Cckappa_sig.rules in
  let parameters' =
    Remanent_parameters.update_prefix parameters "observables:"
  in
  let error =
    print_observables parameters' error handler compil.Cckappa_sig.observables
  in
  let parameters' =
    Remanent_parameters.update_prefix parameters "initial_states:"
  in
  let error = print_inits parameters' error handler compil.Cckappa_sig.init in
  let parameters' =
    Remanent_parameters.update_prefix parameters "perturbations:"
  in
  let error =
    print_perturbations parameters' error handler
      compil.Cckappa_sig.perturbations
  in
  error
