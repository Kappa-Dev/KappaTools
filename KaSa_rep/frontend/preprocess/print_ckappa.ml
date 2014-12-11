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
  let  _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" agent_name in 
    error
  
let print_site_name parameter error site_name = 
  let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" site_name in
    error
  
let print_internal_state parameter error internal_state = 
  let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" internal_state in 
    error
  
let print_binding_state parameter error binding_state = 
  match binding_state 
  with 
    | Ckappa_sig.Free -> error 
    | Ckappa_sig.Lnk_type (agent_name,site_name) -> 
        let error = print_agent_name parameter error agent_name in 
        let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "@" in  
        let error = print_site_name parameter error site_name in 
          error

      
let print_link_state parameter error link = 
  match link 
  with 
    | Ckappa_sig.LNK_VALUE (agent_index,agent_name,site_name,link_index,_) ->
        begin
          match parameter.Remanent_parameters_sig.link_mode 
          with 
            | Remanent_parameters_sig.Bound_indices ->
                let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s%i" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.bound link_index in 
                  error
            | Remanent_parameters_sig.Site_address -> 
                let _ = Printf.fprintf 
                          parameter.Remanent_parameters_sig.log 
                          "%s(%s,%i)%s%s" 
                          parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.bound 
                          agent_name 
                          agent_index 
                          parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.at
                          site_name 
                in 
                  error
            | Remanent_parameters_sig.Bound_type  ->
                let _ = Printf.fprintf 
                          parameter.Remanent_parameters_sig.log 
                          "%s%s%s%s" 
                          parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.bound 
                          agent_name   
                          parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.at
                          site_name
                in 
                  error 
        end 
    | Ckappa_sig.FREE -> error
    | Ckappa_sig.LNK_ANY _  -> 
                let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.link_to_any in 
                  error 
    | Ckappa_sig.LNK_SOME _ -> 
                 let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.link_to_some in 
                  error 
    | Ckappa_sig.LNK_TYPE ((agent_type,_),(site_type,_)) -> 
                let _ = Printf.fprintf 
                          parameter.Remanent_parameters_sig.log 
                          "%s%s%s%s" 
                          parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.bound 
                          agent_type   
                          parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.at
                          site_type
                in 
                  error 
   
let print_internal_state parameter error internal = 
  List.iter 
    (fun x -> Printf.fprintf parameter.Remanent_parameters_sig.log "%s%s" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.internal x)
    internal 

let print_port parameter error port = 
  let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" port.Ckappa_sig.port_nme in
  let _ = print_internal_state parameter error port.Ckappa_sig.port_int in 
  let error = print_link_state parameter error port.Ckappa_sig.port_lnk in 
    error 
  
let print_interface  parameter error interface =
  let rec aux error bool interface = 
    match interface 
    with 
    | Ckappa_sig.EMPTY_INTF -> error 
    | Ckappa_sig.PORT_SEP (port,interface) -> 
        let _ = Misc_sa.print_comma parameter bool parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.site_sep_comma in
        let error = print_port parameter error port in 
         aux error true interface 
  in aux error false interface 
          
let print_agent parameter error agent = 
  let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s%s" agent.Ckappa_sig.ag_nme parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.agent_open in 
  let error = print_interface parameter error agent.Ckappa_sig.ag_intf in 
  let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.agent_close in 
                 error 
    
let print_mixture  parameter error mixture =
  let rec aux error bool mixture =  
    match mixture 
    with 
    | Ckappa_sig.EMPTY_MIX -> error 
    | Ckappa_sig.SKIP _ -> 
         let _ = Misc_sa.print_comma parameter bool parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.site_sep_comma in
         let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "%s" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.ghost_agent in 
                  error 
    | Ckappa_sig.COMMA (agent,mixture) -> 
         let _ = if bool then Printf.fprintf parameter.Remanent_parameters_sig.log "%s" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.agent_sep_comma in
         let error = print_agent parameter error agent in 
           aux error true mixture 
    | Ckappa_sig.DOT (i,agent,mixture) -> 
         let _ = if bool then Printf.fprintf parameter.Remanent_parameters_sig.log "%s%i" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.agent_sep_dot i in
         let error = print_agent parameter error agent in 
           aux error true mixture 
    | Ckappa_sig.PLUS (i,agent,mixture) -> 
         let _ = if bool then Printf.fprintf parameter.Remanent_parameters_sig.log "%s%i" parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.agent_sep_plus i in
         let error = print_agent parameter error agent in 
           aux error true mixture 
  in aux error false mixture 

let rec print_alg parameter error alg =
  match alg 
  with 
  | Ast.BIN_ALG_OP (op,(alg1,_),(alg2,_)) -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log  "(" in 
    let _ = print_alg parameter error alg1 in 
    let _ = Term.print_bin_alg_op parameter.Remanent_parameters_sig.formatter op in 
    let _ = print_alg parameter error alg2 in 
    let _ =  Printf.fprintf parameter.Remanent_parameters_sig.log ")" in 
    error
  | Ast.UN_ALG_OP (op,(alg,_)) -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log  "(" in 
    let _ = Term.print_un_alg_op parameter.Remanent_parameters_sig.formatter op in 
    let _ = print_alg parameter error alg in 
    let _ =  Printf.fprintf parameter.Remanent_parameters_sig.log  ")" in 
    error
  | Ast.STATE_ALG_OP state_alg_op -> 
    let _ = Term.print_state_alg_op parameter.Remanent_parameters_sig.formatter state_alg_op in 
    error 
  | Ast.OBS_VAR string -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log  "%s" string 
    in error
  | Ast.TOKEN_ID token -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log  "%s" token 
    in error
  | Ast.KAPPA_INSTANCE mixture -> 
    print_mixture parameter error mixture 
  | Ast.CONST t -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log  "%s" (Nbr.to_string t) 
    in error
  | Ast.TMAX -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log  "TMAX" 
    in error  
  | Ast.EMAX -> 
    let _ = Printf.fprintf parameter.Remanent_parameters_sig.log "EMAX" 
    in error 
  | Ast.PLOTNUM ->
     let () = Printf.fprintf parameter.Remanent_parameters_sig.log "[p]" in
     error

let print_rule parameter error rule = 
  let error = print_mixture parameter error rule.Ckappa_sig.lhs in 
  let arrow = 
    match rule.Ckappa_sig.arrow 
    with 
      | Ast.RAR -> parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.uni_arrow 
      | Ast.LRAR -> parameter.Remanent_parameters_sig.symbols.Remanent_parameters_sig.bi_arrow 
  in 
  let _ = Printf.fprintf parameter.Remanent_parameters_sig.log " %s " arrow in  
  let error = print_mixture parameter error rule.Ckappa_sig.rhs in 
    error 

