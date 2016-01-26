 (**
  * print_ckappa.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: March, the 23rd of 2011
  * Last modification: December, the 9th of 2014
  * *
  * Signature for prepreprocessing language ckappa
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "ckappa_print") message exn (fun () -> default)

let local_trace = false

let print_agent_name parameter error agent_name =
  let  () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" agent_name in
    error

let print_site_name parameter error site_name =
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" site_name in
    error

let print_internal_state parameter error internal_state =
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" internal_state in
    error

let print_binding_state parameter error binding_state =
  match binding_state
  with
    | Ckappa_sig.Free -> error
    | Ckappa_sig.Lnk_type (agent_name,site_name) ->
        let error = print_agent_name parameter error agent_name in
        let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "@@" in
        let error = print_site_name parameter error site_name in
          error


let print_link_state parameter error link =
  match link
  with
    | Ckappa_sig.LNK_VALUE (agent_index,agent_name,site_name,link_index,_) ->
        begin
          match Remanent_parameters.get_link_mode parameter
          with
            | Remanent_parameters_sig.Bound_indices ->
                let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%i" (Remanent_parameters.get_bound_symbol parameter) link_index in
                  error
            | Remanent_parameters_sig.Site_address ->
                let () = Loggers.fprintf
                          (Remanent_parameters.get_logger parameter)
                          "%s(%s,%i)%s%s"
                          (Remanent_parameters.get_bound_symbol parameter)
                          agent_name
                          agent_index
                          (Remanent_parameters.get_at_symbol parameter)
                          site_name
                in
                  error
            | Remanent_parameters_sig.Bound_type  ->
                let () = Loggers.fprintf
                  (Remanent_parameters.get_logger parameter)
                  "%s%s%s%s"
		  (Remanent_parameters.get_bound_symbol parameter)
                          agent_name
			   (Remanent_parameters.get_at_symbol parameter)
                          site_name
                in
                  error
        end
    | Ckappa_sig.FREE -> error
    | Ckappa_sig.LNK_ANY _  ->
       let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s"  (Remanent_parameters.get_link_to_any_symbol parameter) in
       error
    | Ckappa_sig.LNK_SOME _ ->
       let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" (Remanent_parameters.get_link_to_some_symbol parameter) in
       error

    (*MOD:change ex: A(x!B@x) to A(x!x.B) as the input in kappa file*)
    | Ckappa_sig.LNK_TYPE ((agent_type,_),(site_type,_)) ->
       let _ = Loggers.fprintf
                 (Remanent_parameters.get_logger parameter)
                 "%s%s%s%s"
		  (Remanent_parameters.get_bound_symbol parameter)
                 site_type
		 (Remanent_parameters.get_btype_sep_symbol parameter)
                 agent_type
       in
       error

let print_internal_state parameter error internal =
  List.iter
    (Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%s" (Remanent_parameters.get_internal_symbol parameter))
    internal

let print_port parameter error port =
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" port.Ckappa_sig.port_nme in
  let _ = print_internal_state parameter error port.Ckappa_sig.port_int in
  let error = print_link_state parameter error port.Ckappa_sig.port_lnk in
    error

let print_interface  parameter error interface =
  let rec aux error bool interface =
    match interface
    with
    | Ckappa_sig.EMPTY_INTF -> error
    | Ckappa_sig.PORT_SEP (port,interface) ->
        let _ = Misc_sa.print_comma parameter bool (Remanent_parameters.get_site_sep_comma_symbol parameter) in
        let error = print_port parameter error port in
         aux error true interface
  in aux error false interface

let print_agent parameter error agent =
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%s" agent.Ckappa_sig.ag_nme (Remanent_parameters.get_agent_open_symbol parameter) in
  let error = print_interface parameter error agent.Ckappa_sig.ag_intf in
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" (Remanent_parameters.get_agent_close_symbol parameter) in
  error

let print_mixture  parameter error mixture =
  let rec aux error bool mixture =
    match mixture
    with
    | Ckappa_sig.EMPTY_MIX -> error
    | Ckappa_sig.SKIP mixture ->
      if Remanent_parameters.get_do_we_show_ghost parameter
      then
	let () = Misc_sa.print_comma parameter bool (Remanent_parameters.get_agent_sep_comma_symbol parameter) in
       let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" (Remanent_parameters.get_ghost_agent_symbol parameter) in
       aux error true mixture
      else
	aux error bool mixture
    | Ckappa_sig.COMMA (agent,mixture) ->
       	let () = Misc_sa.print_comma parameter bool (Remanent_parameters.get_agent_sep_comma_symbol parameter) in
        let error = print_agent parameter error agent in
           aux error true mixture
    | Ckappa_sig.DOT (i,agent,mixture) ->
         let () = if bool then Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%i" (Remanent_parameters.get_agent_sep_dot_symbol parameter) i in
         let error = print_agent parameter error agent in
           aux error true mixture
    | Ckappa_sig.PLUS (i,agent,mixture) ->
         let () = if bool then Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%i" (Remanent_parameters.get_agent_sep_plus_symbol parameter) i in
         let error = print_agent parameter error agent in
           aux error true mixture
  in aux error false mixture

let rec print_alg parameter (error:Exception.method_handler) alg =
  match alg
  with
  | Ast.BIN_ALG_OP (op,(alg1,_),(alg2,_)) ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "(" in
    let error = print_alg parameter error alg1 in
    let () =
      match
	Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
      with
      | None -> ()
      | Some formatter -> Operator.print_bin_alg_op formatter op in
    let error = print_alg parameter error alg2 in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ")" in
    error
  | Ast.UN_ALG_OP (op,(alg,_)) ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "(" in
    let () =
      match
	Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
      with
      | None -> ()
      | Some formatter -> Operator.print_un_alg_op formatter op
    in
    let error = print_alg parameter error alg in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  ")" in
    error
  | Ast.STATE_ALG_OP state_alg_op ->
    let () =
      match
	Loggers.formatter_of_logger (Remanent_parameters.get_logger parameter)
      with
      | None -> ()
      | Some formatter -> Operator.print_state_alg_op formatter state_alg_op
    in
    error
  | Ast.OBS_VAR string ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "%s" string
    in error
  | Ast.TOKEN_ID token ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "%s" token
    in error
  | Ast.KAPPA_INSTANCE mixture ->
    print_mixture parameter error mixture
  | Ast.CONST t ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "%s" (Nbr.to_string t)
    in error
  | Ast.TMAX ->
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter)  "TMAX"
    in error
  | Ast.EMAX ->
    let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "EMAX"
    in error
  | Ast.PLOTNUM ->
     let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "[p]" in
     error

let print_rule parameter error rule =
  let error = print_mixture parameter error rule.Ckappa_sig.lhs in
  let arrow =
    match rule.Ckappa_sig.arrow
    with
      | Ast.RAR -> (Remanent_parameters.get_uni_arrow_symbol parameter)
      | Ast.LRAR -> (Remanent_parameters.get_bi_arrow_symbol parameter)
  in
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" arrow in
  let error = print_mixture parameter error rule.Ckappa_sig.rhs in
    error

