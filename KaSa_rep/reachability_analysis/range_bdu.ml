 (**
  * range_bdu.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 24th of Aug
  * Last modification: 
  * 
  * 
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Mvbdu_sig
open Mvbdu_sanity
open Bdu_analysis
open Int_storage
open Cckappa_sig
open Memo_sig
open Boolean_mvbdu
open Printf

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "RANGE BDU") message exn
                 (fun () -> default)

let trace = false

(************************************************************************************)
(*TYPE*)

module AgentMap = Quick_Nearly_inf_Imperatif

type pair_site = (int * int) list

type bdu = bool Mvbdu_sig.mvbdu

type handler = (Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
	        Boolean_mvbdu.list_dic, bool, int) Memo_sig.handler

type handler_bdu = handler * bdu

type site_bdu  = pair_site * handler_bdu

type bdu_range =
    {
      store_test : site_bdu AgentMap.t
    }

(************************************************************************************)
(*is an independent bdu?*)
(*
let is_independent mvbdu =
  match mvbdu.value with
    | Leaf a -> false
    | Node x ->
      let a = x.branch_false.value in
      let b = x.branch_true.value in
      let _ = 
        fprintf stdout "value_branch_false\n";
        print_cell stdout "" a in
      let _ = 
        fprintf stdout "value_branch_true\n";
        print_cell stdout "" b in
      true*)

let get_common x =
  let var = x.variable in
  let up_bound = x.upper_bound in
  let b_false = x.branch_false in
  let b_true = x.branch_true in
  var, up_bound, b_false, b_true

(*it is not indepenent when it has two different outgoing*)
let rec is_not_independent_aux error list =
  match list with
    | [] -> error, true
    | head :: tail ->
      begin
        (*taking the value in the head*)
        match head.value with
          | Leaf _ ->
            (*if it is a leaf then continue to the tail*)
            is_not_independent_aux error tail
          | Node x ->
            let _ =
              fprintf stdout "x_branch_true:\n";
              print_mvbdu stdout "" x.branch_true;
              fprintf stdout "x_branch_false:\n";
              print_mvbdu stdout "" x.branch_false
            in
            (*getting branch_false == branch_true, the sibbling should be different*)
            if x.branch_true == x.branch_false
            then
              error, false
            else
              (*continue to get the value of branch_false*)
              match x.branch_false.value with
                | Leaf _ ->
                  (*if it is a not then continue with the tail*)
                  is_not_independent_aux error tail
                | Node y ->
                  let _ =
                    fprintf stdout "y_branch_true:\n";
                    print_mvbdu stdout "" y.branch_true;
                    fprintf stdout "y_branch_false:\n";
                    print_mvbdu stdout "" y.branch_false
                  in
                  (*TODO*)
                  error, false
      end

    (*| (Leaf a, Leaf b) :: tail ->
      let cmp = compare a b in
      if cmp = 0
      then
        is_not_independent_aux tail
      else
        (*two branches have different value*)
        true
    | (Leaf _, _) :: _  | (_, Leaf _) :: _ -> 
      (*if one of the outgoing is a leaf then it is not indepenent*)
      true
    | (Node x, Node y) :: tail ->      
      let var_x, up_bound_x, b_false_x, b_true_x = get_common x in
      let var_y, up_bound_y, b_false_y, b_true_y = get_common y in
      (*first compare variable of x and y*)
      let cmp_var = compare var_x var_y in
      if cmp_var = 0
      then
      (*then continue compare upper_bound of x and y*)
        begin
          let cmp_up = compare up_bound_x up_bound_y in
          (*if their upper_bound is equal then continue value of its branches*)
          if cmp_up = 0
          then
            is_not_independent_aux
              ((b_true_x.value, b_false_y.value) ::
                  (b_false_x.value, b_false_y.value) ::
                  working_list)
          else
            false
        end
      else
        false*)

let is_not_independent error mvbdu = is_not_independent_aux error [mvbdu]

let collect_bdu_test parameter error viewslhs store_result =
  let error, (handler, bdu_init) = bdu_init parameter in
  AgentMap.fold parameter error
    (fun parameter error agent_id agent store_result ->
      match agent with
        | Ghost -> error, store_result
        | Agent agent ->
          let agent_type = agent.agent_name in
          let l, (handler, bdu_test) = 
            common_site_bdu parameter error agent handler bdu_init
          in
          let _ =
            is_not_independent error bdu_test
          in
          store_common
            parameter error
            agent_type
            handler
            bdu_init
            l
            bdu_test
            store_result
    ) viewslhs store_result

let scan_rule parameter error handler rule store_result =
  let error, store_test =
    collect_bdu_test
      parameter
      error
      rule.rule_lhs.views
      store_result.store_test
  in
  error,
  {
    store_test = store_test
  }

let scan_rule_set parameter error handler rules =
  let error, init_test = AgentMap.create parameter error 0 in
  let init_bdu =
    {
      store_test = init_test
    }
  in
  let error, store_result =
    Nearly_inf_Imperatif.fold parameter error
      (fun parameter error rule_id rule store_result ->
        let _ = fprintf stdout "rule_id_%i:\n" rule_id in
        let error, result =
          scan_rule
            parameter
            error
            handler
            rule.e_rule_c_rule
            store_result
        in
        error, result
      ) rules init_bdu      
  in
  error, store_result

let print_result parameter error result =
  AgentMap.print error
    (fun error parameter (_, (handler, bdu_test)) ->
      let _ =
        handler.print_mvbdu stdout "" bdu_test
      in
      error
    ) parameter result

let main parameter error handler cc_compil =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type:" in
  let error, result = scan_rule_set parameter error handler cc_compil.rules in
  let _ = print_result parameter error result.store_test in
  error, result
