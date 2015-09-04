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
open Quark_type

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "RANGE BDU") message exn
    (fun () -> default)
    
let trace = false

(************************************************************************************) 
(*TYPE*)

(*module Int2Set_and_map =
  Set_and_map.Make
    (struct 
        type t = int * int (*var, bound*)
        let compare = compare
     end)*)

(************************************************************************************) 
(*Range bdu: finding which variable in BDU is not independent.
  - It is not indepenent when it has two different outgoing.
*)
  

(*let branch_combine l = (*TODO*)
  let rec aux acc result =
    match acc with
      | [] -> result
      | (var, new_bound) :: []  -> result
      | (var, new_bound) :: ((var', new_bound') :: tl) ->
        if var != var' 
        then
          result
        else
          aux tl ((var', new_bound') :: result)
  in aux l []*)

let rec print l =
  match l with
    | [] -> []
    | s :: tail ->
      Int2Set_and_map.iter_set (fun (var, bound) ->
        let () =
          fprintf stdout "Range of var x%i is: %i\n" var bound;
        in
        ()
      ) s; print tail
        
let rec aux parameter error working_list =
  match working_list with
    | [] -> (*return a list of set*) []      
    | (cell, bool, var, bound) :: tail ->
      match cell.value with
        | Leaf _ -> aux parameter error tail
        | Node x ->
          let new_var   = x.variable    in
          let new_bound = x.upper_bound in
          let new_branch_false = x.branch_false, false, new_var, new_bound in
          let new_branch_true = x.branch_true, true, new_var, new_bound in
          let new_tail = aux parameter error (new_branch_false :: new_branch_true :: tail) in
          (*we want the structure x < y *)
          match compare var new_var with
            | a when a < 0 -> new_tail
            | a when a > 0 -> []
            | _ ->
              if bool
              then
                []
              else
                let new_set = Int2Set_and_map.empty_set in
                let pair = new_var, new_bound in
                let error, add_pair = Int2Set_and_map.add_set parameter error pair new_set in
                
                let l = add_pair :: new_tail in
                l
                    
let dependent parameter error mvbdu =
  match mvbdu.value with
    | Leaf _ -> error, []
    | Node x ->
      error, aux parameter error [x.branch_false, false, x.variable, x.upper_bound;
                 x.branch_true, true, x.variable, x.upper_bound]

(************************************************************************************) 
(*TEST, build bdu used code from mvbdu_test*)

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
        variable = 1;
        branch_true = a';
        branch_false = b';
        upper_bound = 1
      }
  in
  let c_val =
    Node
      {
        variable = 1;
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
  (*Build list (var, up)*)
  let list = [2,1; 1,0] in
  let list' = [1,1] in
  (*let list = [3,0; 2,1; 1,0] in
  let list' = [3,1; 2,1; 1,1] in*)
  let error,(handler,list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      list
  in
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
    f (Boolean_mvbdu.redefine parameter error parameter handler c') list_b in
  let error, handler, mvbdu =
    f (boolean_mvbdu_or parameter handler error parameter mvbdu1) mvbdu2
  in
  let error =    
    Boolean_mvbdu.print_boolean_mvbdu error (Remanent_parameters.update_prefix parameter " ")
      mvbdu;
    fprintf stdout "---------------------------------\n";
    Boolean_mvbdu.print_boolean_mvbdu error (Remanent_parameters.update_prefix parameter " ")
      mvbdu1;
    fprintf stdout "---------------------------------\n";
    Boolean_mvbdu.print_boolean_mvbdu error (Remanent_parameters.update_prefix parameter " ")
      mvbdu2
  in
  let error, l = dependent parameter error mvbdu in
  let _ = 
    fprintf stdout "---------------------------------\n";
    print l
  in
  (*----------------------------------------------------------------------------------*)
  (*remanent*)
  {
    remanent with 
      Sanity_test_sig.error = error ; 
      Sanity_test_sig.mvbdu_handler = handler
  },
  ("Mvbdu.001",fun remanent ->
    let b = Mvbdu_core.mvbdu_equal c' c'' in
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
