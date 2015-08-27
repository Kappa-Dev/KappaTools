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
open Memo_sig
open Boolean_mvbdu
open Printf
open Sanity_test_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "RANGE BDU") message exn
    (fun () -> default)
    
let trace = false

(************************************************************************************) 
(*Range bdu: finding which variable in BDU is not independent.
  - It is not indepenent when it has two different outgoing.
*)

let rec dependent_aux error working_list list_result =
  match working_list with
    | [] -> error, list_result
    | head :: tail ->
      begin
        (*taking the value in the head: first level t*)
        match head.value with
          | Leaf _ -> dependent_aux error tail list_result
          | Node x ->
            (*checking the two branches: false and true.
              If bfalse != btrue then they are having a different sibbling.
            *)
            if x.branch_true == x.branch_false
            then
              error, list_result
            else
              (*the sibbling is different. Then continue to check in the branch false of this node*)
              match x.branch_false.value with         
                | Leaf _ -> dependent_aux error tail list_result
                | Node y ->
                  error, (
                    (x.branch_false.value,
                     x.branch_true.value,
                     y.branch_false.value,
                     y.branch_true.value
                    ) 
                    :: list_result)
                  (*continue to check the two branches*)
                  (*if y.branch_true == y.branch_false
                  then
                    error, list_result
                  else
                    match y.branch_false.value with
                      | Leaf _ -> is_not_independent_aux error tail list_result
                      | Node z ->
                        if y.branch_true == z.branch_true
                        then
                          error, (y.branch_true.value :: list_result)
                        else
                          is_not_independent_aux error (x.branch_false :: x.branch_true :: tail) list_result*)
      end

let dependent error mvbdu = dependent_aux error [mvbdu][]

let rec print_dependent l =
  match l with
    | [] -> []
    | (x_false, x_true, y_false, y_true) :: tl ->
      let _ = 
        fprintf stdout "x_false:\n";
        print_cell stdout "" x_false;
        fprintf stdout "x_true:\n";
        print_cell stdout "" x_true;
        fprintf stdout "y_false:\n";
        print_cell stdout "" y_false;
        fprintf stdout "y_true:\n";
        print_cell stdout "" y_true
      in
      print_dependent tl

(************************************************************************************) 
(*TEST, build bdu use code from mvbdu_test*)

let bdu_test remanent parameter =
  let error = remanent.error in
  let allocate = remanent.allocate_mvbdu in
  let (handler: ('b, 'a, 'c, bool, int) Memo_sig.handler) =
    remanent.mvbdu_handler in
  let a_val = Leaf true in
  let b_val = Leaf false in
  let error,(handler:('b,'a,'c,bool,int) Memo_sig.handler),a',(a'_id:int),a'',a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val 
  in
  (*a': is mvbdu*)
  let error,handler,b',b'_id,b'',b''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      b_val
      b_val
  in 
  (*----------------------------------------------------------------------------------*)
  (*Build the first node*)
  let c = 
    Node 
      {
        variable = 0;
        branch_true = a';
        branch_false = b';
        upper_bound = 1
      }
  in
  let c_val =
    Node
      {
        variable = 0;
        branch_true = a'_id;
        branch_false = b'_id;
        upper_bound = 1
      }
  in
  let error,handler,c',c'_id,c'',c''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      c_val
      c
  in
 (*----------------------------------------------------------------------------------*)
  (*Build second node*)
  let g = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 0;
        Mvbdu_sig.branch_true = a';
        Mvbdu_sig.branch_false = b';
        Mvbdu_sig.upper_bound = 1
      }
  in 
  let g_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 0;
        Mvbdu_sig.branch_true = a'_id;
        Mvbdu_sig.branch_false = b'_id;
        Mvbdu_sig.upper_bound = 1
      }
  in 
  let error,handler,g',g'_id,g'',g''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      g_val
      g
  in
  (*----------------------------------------------------------------------------------*)
  (*Build list (var, up)*)
  let list = [(1,1); (0,0)] in
  let error,(handler,list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      list
  in
  let list' = [(1, 1); (0,1)] in
  let error,(handler,list_b) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      list'
  in
  let f x y = 
    match x y with 
      | error,(handler,Some a) -> error,handler,a 
      | error,(handler,None) -> 
        let error, a =
          Exception.warn parameter error (Some "") (Some "")  Exit (fun _ -> a') in 
        error, handler, a
  in
  (*----------------------------------------------------------------------------------*)
  (*redefine*)
  let error,handler,mvbdu1 =
    f (Boolean_mvbdu.redefine parameter error parameter handler c') list_a in
  let error,handler,mvbdu2 =
    f (Boolean_mvbdu.redefine parameter error parameter handler g') list_b in
  let error,handler,mvbdu =
    f (Boolean_mvbdu.boolean_mvbdu_or
         parameter handler error parameter mvbdu1) mvbdu2
  in
  let _ =
    handler.print_mvbdu stdout "" mvbdu
  in
  (*----------------------------------------------------------------------------------*)
  let error, dep =
    dependent error mvbdu
  in
  let _ =
    fprintf stdout "-----------------------------------------\n";
    print_dependent dep
  in
  (*----------------------------------------------------------------------------------*)
  (*remanent*)
  {
    remanent with 
      Sanity_test_sig.error = error ; 
      Sanity_test_sig.mvbdu_handler = handler
  },
  ("Mvbdu.001",fun remanent ->
    let b = Mvbdu_core.mvbdu_equal c'' c'' in
    remanent, b, None) :: (List.map (fun (a, b, c) -> a,
      fun remanent -> Mvbdu_sanity.test remanent c b) [])
    
(*****************************************************************************************)

let main () =
  let error = Exception.empty_error_handler in
  let error,parameters,_  = Get_option.get_option error in 
  let remanent,bdu_test_list = bdu_test (Sanity_test.remanent parameters) parameters in 
  let _ =
    List.fold_left 
      (fun remanent (s,p) -> Sanity_test.test remanent p s)
      remanent 
      bdu_test_list
  in 
  ()

let _ = main ()
