(**
 * print_ckappa.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: March, the 23rd of 2011
 * Last modification: Time-stamp: <Aug 31 2018>
 * *
 * Signature for prepreprocessing language ckappa
 *
 * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let local_trace = false

let print_agent_name parameter error agent_name =
  let () =
    Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" agent_name
  in
  error

let print_site_name parameter error site_name =
  let () =
    Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" site_name
  in
  error

let print_internal_state parameter error internal_state =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%s" internal_state
  in
  error

let print_binding_state parameter error binding_state =
  match binding_state with
  | Ckappa_sig.Free -> error
  | Ckappa_sig.Lnk_type (agent_name, site_name) ->
    let binding_type_symbol =
      Remanent_parameters.get_btype_sep_symbol parameter
    in
    let () =
      Loggers.print_binding_type
        (Remanent_parameters.get_logger parameter)
        ~binding_type_symbol ~agent_name ~site_name ()
    in
    error

let print_link_state parameter error link =
  match link with
  | Ckappa_sig.LNK_VALUE (agent_index, agent_name, site_name, link_index, _) ->
    (match Remanent_parameters.get_link_mode parameter with
    | Remanent_parameters_sig.Bound_indices ->
      let _ =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameter)
          "%s%s%s%s"
          (Remanent_parameters.get_open_binding_state parameter)
          (Remanent_parameters.get_bound_symbol parameter)
          (Ckappa_sig.string_of_c_link_value link_index)
          (Remanent_parameters.get_close_binding_state parameter)
      in
      error
    | Remanent_parameters_sig.Site_address ->
      let () =
        (*CHECK*)
        Loggers.fprintf
          (Remanent_parameters.get_logger parameter)
          "%s%s(%s,%s)%s%s"
          (Remanent_parameters.get_open_binding_state parameter)
          (Remanent_parameters.get_bound_symbol parameter)
          agent_name
          (Ckappa_sig.string_of_agent_id agent_index)
          (Remanent_parameters.get_at_symbol parameter)
          site_name
      in
      error
    | Remanent_parameters_sig.Bound_type ->
      let binding_type_symbol =
        Remanent_parameters.get_btype_sep_symbol parameter
      in
      let s =
        Public_data.string_of_binding_type ~binding_type_symbol ~agent_name
          ~site_name ()
      in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameter)
          "%s%s%s%s"
          (Remanent_parameters.get_open_binding_state parameter)
          (Remanent_parameters.get_bound_symbol parameter)
          s
          (Remanent_parameters.get_close_binding_state parameter)
      in
      error)
  | Ckappa_sig.FREE ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%s"
        (Remanent_parameters.get_open_binding_state parameter)
        (Remanent_parameters.get_free_symbol parameter)
        (Remanent_parameters.get_close_binding_state parameter)
    in
    error
  | Ckappa_sig.LNK_ANY _ ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%s"
        (Remanent_parameters.get_open_binding_state parameter)
        (Remanent_parameters.get_link_to_any parameter)
        (Remanent_parameters.get_close_binding_state parameter)
    in
    error
  | Ckappa_sig.LNK_MISSING -> error
  | Ckappa_sig.LNK_SOME _ ->
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%s"
        (Remanent_parameters.get_open_binding_state parameter)
        (Remanent_parameters.get_link_to_some parameter)
        (Remanent_parameters.get_close_binding_state parameter)
    in
    error
  | Ckappa_sig.LNK_TYPE ((agent_name, _), (site_name, _)) ->
    let binding_type_symbol =
      Remanent_parameters.get_btype_sep_symbol parameter
    in
    let s =
      Public_data.string_of_binding_type ~binding_type_symbol ~agent_name
        ~site_name ()
    in
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%s%s"
        (Remanent_parameters.get_open_binding_state parameter)
        (Remanent_parameters.get_bound_symbol parameter)
        s
        (Remanent_parameters.get_close_binding_state parameter)
    in
    error

let print_port parameter error port =
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%s" port.Ckappa_sig.port_name
  in
  let _ =
    List.iter
      (fun x ->
        Loggers.fprintf
          (Remanent_parameters.get_logger parameter)
          "%s%s%s%s"
          (Remanent_parameters.get_open_internal_state parameter)
          (Remanent_parameters.get_internal_state_symbol parameter)
          (match x with
          | Some x -> x
          | None -> "#")
          (Remanent_parameters.get_close_internal_state parameter))
      port.Ckappa_sig.port_int
  in
  let error = print_link_state parameter error port.Ckappa_sig.port_link in
  error

let print_counter parameter error counter =
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%s" counter.Ckappa_sig.counter_name
  in
  let _ =
    match counter.Ckappa_sig.counter_test with
    | Some (Ckappa_sig.CEQ n) ->
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%i%s"
        (Remanent_parameters.get_open_counterceq parameter)
        (Remanent_parameters.get_counterceq_symbol parameter)
        n
        (Remanent_parameters.get_close_counterceq parameter)
    | Some (Ckappa_sig.CGTE n) ->
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%i%s"
        (Remanent_parameters.get_open_countercgte parameter)
        (Remanent_parameters.get_countercgte_symbol parameter)
        n
        (Remanent_parameters.get_close_countercgte parameter)
    | Some (Ckappa_sig.CVAR s) ->
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%s%s"
        (Remanent_parameters.get_open_countercvar parameter)
        (Remanent_parameters.get_countercvar_symbol parameter)
        s
        (Remanent_parameters.get_close_countercvar parameter)
    | Some Ckappa_sig.UNKNOWN | None -> ()
  in
  let () =
    match counter.Ckappa_sig.counter_delta with
    | Some 0 | None -> ()
    | Some n when n > 0 ->
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%i%s"
        (Remanent_parameters.get_open_counterdelta parameter)
        (Remanent_parameters.get_counterdeltaplus_symbol parameter)
        n
        (Remanent_parameters.get_close_counterdelta parameter)
    | Some n (*when n<0*) ->
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s%i%s"
        (Remanent_parameters.get_open_counterdelta parameter)
        (Remanent_parameters.get_counterdeltaminus_symbol parameter)
        (-n)
        (Remanent_parameters.get_close_countercvar parameter)
  in
  error

let print_interface parameter error interface =
  let rec aux error bool interface =
    match interface with
    | Ckappa_sig.EMPTY_INTF -> error
    | Ckappa_sig.COUNTER_SEP (counter, interface) ->
      let _ =
        Misc_sa.print_comma parameter bool
          (Remanent_parameters.get_site_sep_comma_symbol parameter)
      in
      let error = print_counter parameter error counter in
      aux error true interface
    | Ckappa_sig.PORT_SEP (port, interface) ->
      let _ =
        Misc_sa.print_comma parameter bool
          (Remanent_parameters.get_site_sep_comma_symbol parameter)
      in
      let error = print_port parameter error port in
      aux error true interface
  in
  aux error false interface

let print_agent parameter error agent =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%s%s" agent.Ckappa_sig.agent_name
      (Remanent_parameters.get_agent_open_symbol parameter)
  in
  let error = print_interface parameter error agent.Ckappa_sig.ag_intf in
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%s"
      (Remanent_parameters.get_agent_close_symbol parameter)
  in
  error

let print_mixture parameter error mixture =
  let rec aux error bool mixture =
    match mixture with
    | Ckappa_sig.EMPTY_MIX -> error
    | Ckappa_sig.SKIP mixture ->
      if Remanent_parameters.get_do_we_show_ghost parameter then (
        let () =
          Misc_sa.print_comma parameter bool
            (Remanent_parameters.get_agent_sep_comma_symbol parameter)
        in
        let () =
          Loggers.fprintf
            (Remanent_parameters.get_logger parameter)
            "%s"
            (Remanent_parameters.get_ghost_agent_symbol parameter)
        in
        aux error true mixture
      ) else
        aux error bool mixture
    | Ckappa_sig.COMMA (agent, mixture) ->
      let () =
        Misc_sa.print_comma parameter bool
          (Remanent_parameters.get_agent_sep_comma_symbol parameter)
      in
      let error = print_agent parameter error agent in
      aux error true mixture
    | Ckappa_sig.DOT (i, agent, mixture) ->
      let () =
        if bool then
          Loggers.fprintf
            (Remanent_parameters.get_logger parameter)
            "%s%s"
            (Remanent_parameters.get_agent_sep_dot_symbol parameter)
            (Ckappa_sig.string_of_agent_id i)
      in
      let error = print_agent parameter error agent in
      aux error true mixture
    | Ckappa_sig.PLUS (i, agent, mixture) ->
      let () =
        if bool then
          Loggers.fprintf
            (Remanent_parameters.get_logger parameter)
            "%s%s"
            (Remanent_parameters.get_agent_sep_plus_symbol parameter)
            (Ckappa_sig.string_of_agent_id i)
      in
      let error = print_agent parameter error agent in
      aux error true mixture
  in
  aux error false mixture

let get_agent_open_symbol parameter =
  Loggers.fprintf
    (Remanent_parameters.get_logger parameter)
    "%s"
    (Remanent_parameters.get_agent_open_symbol parameter)

let get_agent_close_symbol parameter =
  Loggers.fprintf
    (Remanent_parameters.get_logger parameter)
    "%s"
    (Remanent_parameters.get_agent_close_symbol parameter)

let rec print_alg parameter (error : Exception.method_handler) alg =
  match alg with
  | Alg_expr.BIN_ALG_OP (op, (alg1, _), (alg2, _)) ->
    let () = get_agent_open_symbol parameter in
    let error = print_alg parameter error alg1 in
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s"
        (Operator.bin_alg_op_to_string op)
    in
    let error = print_alg parameter error alg2 in
    let () = get_agent_close_symbol parameter in
    error
  | Alg_expr.UN_ALG_OP (op, (alg, _)) ->
    let () = get_agent_open_symbol parameter in
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%a" Operator.print_un_alg_op op
    in
    let error = print_alg parameter error alg in
    let () = get_agent_close_symbol parameter in
    error
  | Alg_expr.STATE_ALG_OP state_alg_op ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%a" Operator.print_state_alg_op state_alg_op
    in
    error
  | Alg_expr.ALG_VAR string ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" string
    in
    error
  | Alg_expr.TOKEN_ID token ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" token
    in
    error
  | Alg_expr.KAPPA_INSTANCE mixture -> print_mixture parameter error mixture
  | Alg_expr.CONST t ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s" (Nbr.to_string t)
    in
    error
  | Alg_expr.DIFF_TOKEN ((expr, _), token) ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "d" in
    let error = print_alg parameter error expr in
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "/d%s" token
    in
    error
  | Alg_expr.DIFF_KAPPA_INSTANCE ((expr, _), pattern) ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "d" in
    let error = print_alg parameter error expr in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "/d" in
    print_mixture parameter error pattern
  | Alg_expr.IF (cond, (yes, _), (no, _)) ->
    let () = get_agent_open_symbol parameter in
    let error = print_bool parameter error cond in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "[?]" in
    let error = print_alg parameter error yes in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "[:]" in
    let error = print_alg parameter error no in
    let () = get_agent_close_symbol parameter in
    error

and print_bool parameter (error : Exception.method_handler) = function
  | Alg_expr.TRUE, _ ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "[true]"
    in
    error
  | Alg_expr.FALSE, _ ->
    let () =
      Loggers.fprintf (Remanent_parameters.get_logger parameter) "[false]"
    in
    error
  | Alg_expr.COMPARE_OP (op, (alg1, _), (alg2, _)), _ ->
    let () = get_agent_open_symbol parameter in
    let error = print_alg parameter error alg1 in
    let () =
      match
        Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
      with
      | None -> ()
      | Some formatter -> Operator.print_compare_op formatter op
    in
    let error = print_alg parameter error alg2 in
    let () = get_agent_close_symbol parameter in
    error
  | Alg_expr.BIN_BOOL_OP (op, b1, b2), _ ->
    let () = get_agent_open_symbol parameter in
    let error = print_bool parameter error b1 in
    let () =
      match
        Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
      with
      | None -> ()
      | Some formatter -> Operator.print_bin_bool_op formatter op
    in
    let error = print_bool parameter error b2 in
    let () = get_agent_close_symbol parameter in
    error
  | Alg_expr.UN_BOOL_OP (op, b1), _ ->
    let () =
      match
        Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
      with
      | None -> ()
      | Some formatter -> Operator.print_un_bool_op formatter op
    in
    let () = get_agent_open_symbol parameter in
    let error = print_bool parameter error b1 in
    let () = get_agent_close_symbol parameter in
    error

let print_rule parameter error rule =
  let error = print_mixture parameter error rule.Ckappa_sig.lhs in
  let arrow = Remanent_parameters.get_uni_arrow_symbol parameter in
  let _ =
    Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" arrow
  in
  let error = print_mixture parameter error rule.Ckappa_sig.rhs in
  error
