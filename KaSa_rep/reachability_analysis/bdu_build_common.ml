(**
  * bdu_build.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of September
  * Last modification: Time-stamp: <2015-10-19 17:38:11 feret>
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Boolean_mvbdu
open Memo_sig
open Mvbdu_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Build BDU") message exn (fun () -> default)  

let trace = false

(*---------------------------------------------------------------------------------*)
(*common function for building bdu from a list of pair (site, state)*)

let f parameter x y =
    match x y  with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a =
        Exception.warn parameter error (Some "") (Some "") Exit (fun _ -> raise Exit)
      in error, handler, a
  
let build_bdu parameter handler error pair_list =
  (*  let remanent_bdu = Boolean_mvbdu.init_remanent parameter error in *)
  (*  let error        = remanent_bdu.Sanity_test_sig.error in*)
  (*  let allocate     = Boolean_mvbdu.allocate in*)
  (*'b: memo_tables; 'a: mvbdu_dic; 'c: list_dic*)
  (*  let handler,a_val = Boolean_mvbdu.boolean_mvbdu_constant_true parameter handler in*)
  (*build bdu from a_val: 
    a',a'_id: output of build_already_compressed_cell;
    a'', a''_id: output of compress_node
  *)
 (* let error, handler, a', a'_id, a'', a''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val
  in
  (*build bdu_b from b_val*)
  let error, handler, b', b'_id, b'', b''_id =
    Mvbdu_test.build_without_and_with_compressing
      allocate
      error
      handler
      b_val
      b_val
  in*)
  (*---------------------------------------------------------------------------*)    
  (*build bdu_list from a list of pair [site, state] computed above in cv*)
  let error, handler,mvbdu_true =
    f parameter 
      (Boolean_mvbdu.boolean_mvbdu_true parameter handler error)
      parameter
  in 
  
  let handler, (error, list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.list_allocate parameter)
      error
      parameter
      handler
      pair_list
  in
  (*compute redefine in a list_a, a': mvbdu_input*)
  let error, handler, mvbdu =
    f
      parameter 
      (Boolean_mvbdu.redefine
	 parameter
	 handler
	 parameter
	 error
	 mvbdu_true)
	 list_a
     
		
  in
  (*---------------------------------------------------------------------------*)
  (*return redefine*)
  error, (handler, mvbdu)


