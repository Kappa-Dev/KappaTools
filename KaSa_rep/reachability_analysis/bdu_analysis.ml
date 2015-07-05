(**
  * bdu_analysi.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 26th of June
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Covering_classes
open Covering_classes_type
open Printf
open Cckappa_sig
open Int_storage
open Mvbdu_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*TYPES*)

type pair_list = (int * (int * int)) list
  
type bdu_analysic =
    {
      store_pair_creation : ((int * int) list *
			    ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			      Boolean_mvbdu.list_dic, bool, int)
				Memo_sig.handler * bool Mvbdu_sig.mvbdu)) AgentMap.t;
      store_half_break_bdu : ((int * int) list *
			   ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			     Boolean_mvbdu.list_dic, bool, int)
			     Memo_sig.handler * bool Mvbdu_sig.mvbdu)) AgentMap.t;
      store_test_modif : ((int * int) list *
			   ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			     Boolean_mvbdu.list_dic, bool, int)
			     Memo_sig.handler * bool Mvbdu_sig.mvbdu)) AgentMap.t;
     (* store_test     : pair_list AgentMap.t;
      store_bdu_test : (int *
		       ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			 Boolean_mvbdu.list_dic, bool, int)
			 Memo_sig.handler * bool Mvbdu_sig.mvbdu)) list AgentMap.t;
      store_modified : pair_list AgentMap.t;
      store_bdu_modified : (int *
			   ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			     Boolean_mvbdu.list_dic, bool, int)
			       Memo_sig.handler * bool Mvbdu_sig.mvbdu)) list AgentMap.t;
      store_test_modif : ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			   Boolean_mvbdu.list_dic, bool, int)
			     Memo_sig.handler * bool Mvbdu_sig.mvbdu) AgentMap.t;*)
      store_iterate_bdu : ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
			    Boolean_mvbdu.list_dic, bool, int)
			      Memo_sig.handler * bool Mvbdu_sig.mvbdu) AgentMap.t;
      store_iterate_half_created_cv : ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
					Boolean_mvbdu.list_dic, bool, int)
					  Memo_sig.handler * bool Mvbdu_sig.mvbdu) AgentMap.t;
    }

(************************************************************************************)    
(*compute BDU for each rule in a covering class*)

let rec print_a_list (l: int List_sig.list) =
  fprintf stdout "list_id:%i:" l.List_sig.id;
  let v = l.List_sig.value in
  match v with
    | List_sig.Empty -> print_string "\n"
    | List_sig.Cons precell ->
      Printf.fprintf stdout "value:[";
      print_precell precell
      
and print_precell p =
  fprintf stdout "site_type:%i:site_state:%i]\n" 
    p.List_sig.variable  p.List_sig.association;
  print_a_list p.List_sig.tail

(*---------------------------------------------------------------------------*)
(*Build bdu from a pair of list (site, state)*)

let build_bdu parameter error pair_list =
  (*build bdu for this list*)
  let remanent_bdu = Sanity_test.remanent parameter in
  let error        = remanent_bdu.Sanity_test_sig.error in
  let allocate     = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  (*'b: memo_tables; 'a: mvbdu_dic; 'c: list_dic*)
  let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
    remanent_bdu.Sanity_test_sig.mvbdu_handler
  in
  let a_val = Mvbdu_sig.Leaf true in
  let b_val = Mvbdu_sig.Leaf false in
  (*build bdu from a_val: 
    a',a'_id: output of build_already_compressed_cell;
    a'', a''_id: output of compress_node
  *)
  let error, handler, a', a'_id, a'', a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val
  in
  (*define function f*)
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> a')
        in error, handler, a
  in
  (*build bdu_b from b_val*)
  let error, handler, b', b'_id, b'', b''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      b_val
      b_val
  in
  (*---------------------------------------------------------------------------*)    
  (*build bdu_list from a list of pair [site, state] computed above in cv*)
  let error, (handler, list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      pair_list
  in
  (*compute redefine in a list_a, a': mvbdu_input*)
  let error, handler, mvbdu_redefine =
    f (Boolean_mvbdu.redefine parameter error parameter handler a') list_a
  in
  (*---------------------------------------------------------------------------*)
  (*return redefine*)
  error, (handler, mvbdu_redefine)

(************************************************************************************)    
(*Build init*)

let bdu_init parameter =
  let remanent_bdu = Sanity_test.remanent parameter in
  let error        = remanent_bdu.Sanity_test_sig.error in
  let allocate     = remanent_bdu.Sanity_test_sig.allocate_mvbdu in
  let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
    remanent_bdu.Sanity_test_sig.mvbdu_handler
  in
  let a_val = Mvbdu_sig.Leaf false in
  let error, handler, a', a'_id, a'', a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val
  in
  error, (handler, a')

(************************************************************************************)
(*TEST rules*)
(*
let collect_test parameter error rule_id views store_bdu =
  AgentMap.fold
    parameter
    error
    (fun parameter error agent_id agent store_bdu ->
      match agent with
        | Ghost -> error, store_bdu
        | Agent agent ->
          let agent_type = agent.agent_name in
          let pair_list = 
            Site_map_and_set.fold_map
              (fun site port current_list ->
                let state = int_of_port port in
                (rule_id, (site, state)) :: current_list
              ) agent.agent_interface []
          in
	  (*------------------------------------------------------------------------*)
	  (*get old*)
          let error, old_pair =
            match AgentMap.unsafe_get parameter error agent_type store_bdu with
              | error, None -> error, []
              | error, Some r -> error, r
          in
	  (*------------------------------------------------------------------------*)
          let new_list = List.concat [pair_list; old_pair] in
	  (*------------------------------------------------------------------------*)
          AgentMap.set parameter error agent_type (List.rev new_list) store_bdu
    ) views store_bdu
  *)
(*------------------------------------------------------------------------*)
(*
let collect_bdu_test parameter error result_test store_bdu_test =
  AgentMap.fold
    parameter
    error
    (fun parameter error agent_type result_test_list store_bdu_test ->
      let bdu_test =
	let rec aux acc =
	  match acc with
	    | [] -> []
	    | (r, p) :: tl ->
	      match tl with
		| [] -> [] (*FIXME*)
		| (r', p') :: tl' ->
		  if r = r'
		  then
		    let error, (handler, bdup) = build_bdu parameter error
		      (List.concat [[p];[p']]) in
		    (r, (handler, bdup)) :: aux tl
		  else
		    let error, bdup = build_bdu parameter error [p] in
		    (r, bdup) :: aux tl
	in aux result_test_list
      in
      AgentMap.set parameter error agent_type bdu_test store_bdu_test
    ) result_test store_bdu_test
  *)
(************************************************************************************)    
(*compute bdu for initial state or creation actions*)

let collect_pair_creation parameter error viewsrhs creation store_creation =
  let error, (handler, bdu_init) = bdu_init parameter in
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> bdu_init)
        in error, handler, a
  in
  List.fold_left (fun (error, store_creation) (agent_id, agent_type) ->
    let error, agent = AgentMap.get parameter error agent_id viewsrhs in
    match agent with
      | None -> warn parameter error (Some "line 772") Exit store_creation
      | Some Ghost -> error, store_creation
      | Some Agent agent ->
        (*get the site and state on the rhs*)
        let (pair_list, (handler, bdu)) =
          Site_map_and_set.fold_map
            (fun site port (current_list, _)  ->
              let state = int_of_port port in
              let l = (site, state) :: current_list in
	      let error, (handler, bdu) =
		build_bdu parameter error l
	      in
	      l, (handler, bdu)
            ) agent.agent_interface ([], (handler, bdu_init))
        in
	(*---------------------------------------------------------------------------*)
        let error, (old_list, (handler, old_bdu)) =
          match AgentMap.unsafe_get parameter error agent_type store_creation with
            | error, None -> error, ([], (handler, bdu_init))
            | error, Some (l, (handler, b)) -> error, (l, (handler, b))
        in
	let new_list = List.concat [pair_list; old_list] in
	let error, handler, new_bdu =
	  f (Boolean_mvbdu.boolean_mvbdu_or
	       parameter handler error parameter old_bdu) bdu
	in
	(*--------------------------------------------------------------------------*)
        let error, store_creation =
          AgentMap.set
            parameter
            error
            agent_type
            (List.rev new_list, (handler, new_bdu))
            store_creation
        in
        error, store_creation
  ) (error, store_creation) creation

(************************************************************************************)
(*Side effects: unknow binding - know binding - deletion*)

(*unknow binding*)

let collect_half_break_bdu parameter error kappa_handler store_half_break half_break =
  let error, (handler, bdu_init) = bdu_init parameter in
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> bdu_init)
        in error, handler, a
  in
  List.fold_left (fun (error, store_half_break) (site_add, state) ->
    let site = site_add.site in
    let agent_type = site_add.agent_type in
    (*get state*)
    let error, (min, max) =
      match state with
	| None ->
	  begin
	    let error, value_state =
	      Misc_sa.unsome
		(Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
		   parameter
		   error
		   (agent_type, site)
		   kappa_handler.states_dic)
		(fun error -> warn parameter error (Some "line 296")
		  Exit (Dictionary_of_States.init ()))
	    in
	    let error, last_entry =
	      Dictionary_of_States.last_entry parameter error value_state
	    in
	    error, (1, last_entry)
	  end
	| Some interval -> error, (interval.min, interval.max)
    in
    let pair_list = [site, min] in
     (*build bdu for half_break*)
    let error, (handler, bdu_half_break) =
      build_bdu parameter error pair_list
    in
    (*get old_one*)
    let error, (old_list, (handler, old_bdu)) =
      match AgentMap.unsafe_get parameter error agent_type store_half_break with
	| error, None -> error, ([], (handler, bdu_init))
	| error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
    in
    let new_list = List.concat [pair_list; old_list] in
    (*get new_bdu*)
    let error, handler, new_bdu =
      f (Boolean_mvbdu.boolean_mvbdu_or
	   parameter handler error parameter old_bdu) bdu_half_break
    in
    (*store*)
    AgentMap.set
      parameter
      error
      agent_type
      (List.rev new_list, (handler, new_bdu))
      store_half_break
  ) (error, store_half_break) half_break

(*remove actions (deletion)*)
    
(************************************************************************************) 
(*Modified rules*)
(*
let collect_modified parameter error rule_id diff_reverse store_modified =
  AgentMap.fold
    parameter
    error
    (fun parameter error agent_id site_modif store_modified ->
      if Site_map_and_set.is_empty_map
        site_modif.agent_interface
      then error, store_modified
      else
        let agent_type = site_modif.agent_name in
        let pair_list =
          Site_map_and_set.fold_map 
	    (fun site port current_list ->
	      let state = int_of_port port in
	      (rule_id, (site, state)) :: current_list
	    ) site_modif.agent_interface []
        in
	(*------------------------------------------------------------------------*)
        let error, old_list =
          match AgentMap.unsafe_get parameter error agent_type store_modified with
	    | error, None -> error, []
	    | error, Some s -> error, s
        in
        let new_list = List.concat [pair_list; old_list] in
	(*------------------------------------------------------------------------*)
        let error, store_modified =
          AgentMap.set
	    parameter
	    error
	    agent_type
	    (List.rev new_list)
	    store_modified
        in
        error, store_modified
    ) diff_reverse store_modified
  *)
(*------------------------------------------------------------------------------*)
(*
let collect_bdu_modified parameter error result_modified store_bdu_modified =
  AgentMap.fold
    parameter
    error
    (fun parameter error agent_type result_modified_list store_bdu_modified ->
      let bdu_modified =
	let rec aux acc =
	  match acc with
	    | [] -> []
	    | (r, p) :: tl ->
	      match tl with
		| [] -> []
		| (r', p') :: tl' ->
		  if r = r'
		  then
		    let error, (handler, bdup) = build_bdu parameter error
		      (List.concat [[p];[p']]) in
		    (r, (handler, bdup)) :: aux tl
		  else
		    let error, bdup = build_bdu parameter error [p] in
		    (r, bdup) :: aux tl
	in aux result_modified_list
      in
      AgentMap.set parameter error agent_type bdu_modified store_bdu_modified
    ) result_modified store_bdu_modified
*)
    (*
let bdu_test_modif parameter error store_bdu_modif store_bdu_test store_result = (*TEST*)
  let error, (handler, bdu_init) = bdu_init parameter in
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> bdu_init)
        in error, handler, a
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type bdu_modif_list bdu_test_list store_result ->
      let res =
	List.fold_left (fun (handler, bdu_a) (r, (handler, bdu_modif)) ->
	  List.fold_left (fun (handler, bdu_b) (r', (handler, bdu_test)) ->
	  (*intersection bdu_modif and bdu_test*)
	    let error, handler, inter_bdu =
	      f (Boolean_mvbdu.boolean_mvbdu_and
		   parameter handler error parameter bdu_modif) bdu_test
	    in
	    let error, handler, or_bdu =
	      f (Boolean_mvbdu.boolean_mvbdu_or
		   parameter handler error parameter inter_bdu) bdu_b
	    in
	    (*let _ =
	      fprintf stdout "r:%i:bdu_modif\n" r;
	      handler.Memo_sig.print_mvbdu stdout "" bdu_modif;
	      fprintf stdout "r:%i:bdu_test\n" r;
	      handler.Memo_sig.print_mvbdu stdout "" bdu_test;
	      fprintf stdout "r:%i:bdu_a\n" r;
	      handler.Memo_sig.print_mvbdu stdout "" bdu_a;
	      fprintf stdout "r':%i:bdu_b\n" r';
	      handler.Memo_sig.print_mvbdu stdout "" bdu_b;
	      fprintf stdout "bdu_inter\n";
	      handler.Memo_sig.print_mvbdu stdout "" inter_bdu;
	      fprintf stdout "bdu_or\n";
	      handler.Memo_sig.print_mvbdu stdout "" or_bdu
	    in*)
	    (handler, or_bdu)
	  ) (handler, bdu_a) bdu_test_list
	) (handler, bdu_init) bdu_modif_list
      in
      AgentMap.set
	parameter
	error
	agent_type
	res
	store_result
    )
    store_bdu_modif store_bdu_test store_result
    *)
    
(************************************************************************************)
(*Covering class*)

let collect_test_modif parameter error viewslhs diff_reverse store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> bdu_init)
        in error, handler, a
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_id agent site_modif store_result ->
    (*If there is no modified site then return the enabled rules*)
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then error, store_result
      else
        match agent with
          | Ghost -> error, store_result
          | Agent agent ->
            let agent_type = agent.agent_name in
	    let (pair_list, (handler, bdu)) =
	      Site_map_and_set.fold_map
		(fun site port (current_list, _) ->
		  let state = int_of_port port in
		  let l = (site, state) :: current_list in
		  (*Build bdu*)
		  let error, (handler, bdu) =
		    build_bdu parameter error l
		  in
		  l, (handler, bdu)
		) agent.agent_interface ([], (handler, bdu_init)) (*option type?*)
	    in
	    (*Get the old one*)
	    let error, (old_list, (handler, old_bdu)) =
	      match AgentMap.unsafe_get parameter error agent_type store_result with
		| error, None -> error, ([], (handler, bdu_init))
		| error, Some (l, (handler, bdu)) -> error, (l, (handler, bdu))
	    in
	    (*new*)
	    let new_list = List.concat [pair_list; old_list] in
	    let error, handler, new_bdu = (*FIXME: don't use f?*)
	      f (Boolean_mvbdu.boolean_mvbdu_or
		   parameter handler error parameter old_bdu) bdu
	    in
	    AgentMap.set
	      parameter
	      error
	      agent_type
	      (List.rev new_list, (handler, new_bdu))
	      store_result
    ) viewslhs diff_reverse store_result

(************************************************************************************)
(*Iteration: creation union covering class*)

let iteration_creation_cv parameter error store_creation store_cv store_result = (*FIXME: in case there is no creation or bdu_test_modif*)
  let error, (handler, bdu_init) = bdu_init parameter in
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> bdu_init)
        in error, handler, a
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type (_, (handler, bdu_created)) (_, (handler, bdu_cv))
      store_result ->
	(*TODO: check the condition of bdu_created is empty, bdu_cv is empty: 
	  change type option?*)
	(*union *)
	let error, handler, bdu_iterate =
	  f (Boolean_mvbdu.boolean_mvbdu_or
	       parameter handler error parameter bdu_created) bdu_cv
	in
	(*store*)
	AgentMap.set
	  parameter
	  error
	  agent_type
	  (handler, bdu_iterate)
	  store_result
    ) store_creation store_cv store_result

(*iterate combine between iteration_creation_cv with half_break (*TODO: remove*)*)
let iterate_half_break_created_cv parameter error store_iterate_created_cv store_half_break
    store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  let f x y =
    match x y with
      | error, (handler, Some a) -> error, handler, a
      | error, (handler, None) ->
        let error, a =
          Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> bdu_init)
        in error, handler, a
  in
  AgentMap.fold2_common parameter error
    (fun parameter error agent_type (handler, bdu_iterate) (_, (handler, bdu_half_break))
      store_result ->
	(*union *)
	let error, handler, bdu_iterate =
	  f (Boolean_mvbdu.boolean_mvbdu_or
	       parameter handler error parameter bdu_iterate) bdu_half_break
	in
	(*store*)
	AgentMap.set
	  parameter
	  error
	  agent_type
	  (handler, bdu_iterate)
	  store_result
    )
    store_iterate_created_cv store_half_break store_result
  
      
(************************************************************************************)
(*RULE*)

let scan_rule parameter error handler rule_id rule rules store_result =
  (*------------------------------------------------------------------------------*)
  (*compute creation, and bdu iterate of creation rules*)
  let error, store_pair_creation =
    collect_pair_creation
      parameter
      error
      rule.rule_rhs.views
      rule.actions.creation
      store_result.store_pair_creation
  in
  (*------------------------------------------------------------------------------*)
  (*side effect - unknow binding*)
  let error, store_half_break_bdu =
    collect_half_break_bdu
      parameter
      error
      handler
      store_result.store_half_break_bdu
      rule.actions.half_break
  in
  (*------------------------------------------------------------------------------*)
  (*side effect - deletion*)
  (*let error, store_remove_bdu =
    collect_remove_bdu
      parameter
      error
      store_result.store_remove_bdu
      rule.actions.remove
  in*)
  (*------------------------------------------------------------------------------*)
  (*compute bdu test*)
  let error, store_test_modif =
    collect_test_modif
      parameter
      error
      rule.rule_lhs.views
      rule.diff_reverse
      store_result.store_test_modif
  in
  (*------------------------------------------------------------------------------*)
  (*compute bdu test*)
  (*let error, store_test =
    collect_test
      parameter
      error
      rule_id
      rule.rule_lhs.views
      store_result.store_test
  in*)
  (*------------------------------------------------------------------------------*)
  (*compute bdu test*)
  (*let error, store_bdu_test =
    collect_bdu_test
      parameter
      error
      store_test
      store_result.store_bdu_test
  in*)
  (*------------------------------------------------------------------------------*)
  (*compute modified*)
  (*let error, store_modified =
    collect_modified
      parameter
      error
      rule_id
      rule.diff_reverse
      store_result.store_modified
  in*)
  (*------------------------------------------------------------------------------*)
  (*compute bdu modified*)
  (*let error, store_bdu_modified =
    collect_bdu_modified
      parameter
      error
      store_modified
      store_result.store_bdu_modified
  in
  let error, store_test_modif =
    bdu_test_modif
      parameter
      error
      store_bdu_modified
      store_bdu_test
      store_result.store_test_modif
  in*)
  (*------------------------------------------------------------------------------*)
  (*iterate creation and covering class*)
  let error, store_iterate_bdu =
    iteration_creation_cv
      parameter
      error
      store_pair_creation
      store_test_modif
      store_result.store_iterate_bdu
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  let error, store_iterate_half_created_cv =
    iterate_half_break_created_cv
      parameter
      error
      store_iterate_bdu
      store_half_break_bdu
      store_result.store_iterate_half_created_cv
  in
  (*------------------------------------------------------------------------------*)
  (*store*)
  error,
  {
    store_pair_creation = store_pair_creation;
    store_half_break_bdu = store_half_break_bdu;
    (*store_remove_bdu = store_remove_bdu;*) (*TODO*)
    (*store_lhs_modified  = store_lhs_modified;
    store_test          = store_test;
    store_bdu_test      = store_bdu_test;
    store_modified      = store_modified;
    store_bdu_modified  = store_bdu_modified;*)
    store_test_modif = store_test_modif;
    store_iterate_bdu = store_iterate_bdu;
    store_iterate_half_created_cv = store_iterate_half_created_cv
  }   
  
(************************************************************************************)
(*RULES*)

let scan_rule_set parameter error handler rules =
  let error, init_pair_creation = AgentMap.create parameter error 0 in
  let error, init_half_break_bdu  = AgentMap.create parameter error 0 in
  (*let error, init_lhs_modified  = AgentMap.create parameter error 0 in
  let error, init_test          = AgentMap.create parameter error 0 in
  let error, init_bdu_test      = AgentMap.create parameter error 0 in
  let error, init_modified      = AgentMap.create parameter error 0 in*)
  let error, init_test_modif      = AgentMap.create parameter error 0 in
  let error, init_iterate_bdu  = AgentMap.create parameter error 0 in
  let error, init_iterate_half_created_cv  = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_pair_creation = init_pair_creation;
      store_half_break_bdu = init_half_break_bdu;
      (*store_lhs_modified = init_lhs_modified;
      store_test         = init_test;
      store_bdu_test     = init_bdu_test;
      store_modified     = init_test;*)
      store_test_modif = init_test_modif;
      store_iterate_bdu = init_iterate_bdu;
      store_iterate_half_created_cv = init_iterate_half_created_cv;
    }
  in
  (*------------------------------------------------------------------------------*)
  (*map each agent to a covering classes*)
  let error, store_results =
    Nearly_inf_Imperatif.fold
      parameter error
      (fun parameter error rule_id rule store_result ->
        (*let _ = Printf.fprintf stdout "rule_id:%i:\n" rule_id in*)
        let error, result =
          scan_rule
            parameter
            error
            handler
            rule_id
            rule.e_rule_c_rule
            rules
            store_result
        in
        error, result
      ) rules init_bdu
  in
  error, store_results
    
(************************************************************************************)
(*PRINT*)
      
(*let print_result_common parameter error result_common =
  AgentMap.print error
    (fun error parameter list ->
      let _ =
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (r, (s, st)) :: tl ->
	      fprintf stdout "rule_id:%i:site_type:%i:state:%i\n" r s st;
	      aux tl
	in aux list
      in
      error
    ) parameter result_common
    
let print_result_bdu_common parameter error bdu_common =
  AgentMap.print error
    (fun error parameter list ->
      let _ =
        let rec aux acc =
          match acc with
            | [] -> ()
            | (r, (handler, bdu)) :: tl ->
              let _ = fprintf stdout "rule_id:%i\n" r in
              let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
              aux tl
        in
        aux list
      in
      error
    ) parameter bdu_common*)
    
let print_pair_creation parameter error result =
  AgentMap.print error
    (fun error parameter (l, (handler, bdu)) ->
      let _ =
	let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	fprintf stdout "CREATION rules\n";
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (s, st) :: tl ->
	      fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	in
	aux l
      in
      error
    ) parameter result
    
let print_half_break parameter error result =
  AgentMap.print error
    (fun error parameter (l, (handler, bdu)) ->
      let _ =
	let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	fprintf stdout "HALF_BREAK rules\n";
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (s, st) :: tl ->
	      fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	in
	aux l
      in
      error
    ) parameter result
    
let print_test_modif parameter error result =
  AgentMap.print error
    (fun error parameter (l, (handler, bdu)) ->
      let _ =
	let _ = handler.Memo_sig.print_mvbdu stdout "" bdu in
	fprintf stdout "LHS-MODIFIED rules\n";
	let rec aux acc =
	  match acc with
	    | [] -> ()
	    | (s, st) :: tl ->
	      fprintf stdout "site_type:%i:state:%i\n" s st; aux tl
	in
	aux l
      in
      error
    ) parameter result

let print_iterate_bdu parameter error result =
  AgentMap.print error
    (fun error parameter (handler, bdu) ->
      let _ =
	fprintf stdout "ITERATE CREATION-COVERING CLASS\n";
	handler.Memo_sig.print_mvbdu stdout "" bdu
      in
      error
    ) parameter result

let print_iterate_half_created_cv parameter error result =
  AgentMap.print error
    (fun error parameter (handler, bdu) ->
      let _ =
	fprintf stdout "ITERATE HALF_BREAK-CREATION-COVERING CLASS\n";
	handler.Memo_sig.print_mvbdu stdout "" bdu
      in
      error
    ) parameter result
    
(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let () =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU INIT\n";
    let error, (handler, init_bdu) = bdu_init parameter in
    handler.Memo_sig.print_mvbdu stdout "" init_bdu
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU CREATION - CREATION rules\n";
    print_pair_creation parameter error result.store_pair_creation
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU HALF_BREAK - HALF_BREAK rules\n";
    print_half_break parameter error result.store_half_break_bdu
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU TEST_MODIF rules\n";
    print_test_modif parameter error result.store_test_modif
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATE CREATION - CV rules\n";
    print_iterate_bdu parameter error result.store_iterate_bdu
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "ITERATE HALF_BREAK - CREATION - CV rules\n";
    print_iterate_half_created_cv parameter error result.store_iterate_half_created_cv
  in
  (*let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU LHS - MODIFIED rules\n";
    print_result_bdu_lhs_modified parameter error result.store_lhs_modified
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "TEST rules\n";
    print_result_common parameter error result.store_test
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU of TEST rules\n";
    print_result_bdu_common parameter error result.store_bdu_test 
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "MODIFIED rules\n";
    print_result_common parameter error result.store_modified
  in
  let error =
    fprintf stdout "--------------------------------------------\n";
    fprintf stdout "BDU of MODIFIED rules\n";
    print_result_bdu_common parameter error result.store_bdu_modified
  in*)
  error

(************************************************************************************)
(*MAIN*)

let bdu_main parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.rules in
  let _ = print_result parameter error result in
  error, result
