(**
   * mvbdu_test.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * 
   * Creation: 08/03/2010
   * Last modification: 19/12/2010
   * * 
   * This library provides test benchmarks for the library of sets of finite maps from integers to integers
   *  
   * Copyright 2010 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)

let build_without_and_with_compressing (allocate:('a,'b,'c,'d) Sanity_test_sig.f)
    error handler bdu_skel bdu_val  = 
  let error, output =
    Mvbdu_core.build_already_compressed_cell
      allocate
      error
      handler
      bdu_skel
      bdu_val
  in     
  let error, handler, a', a'_id = 
    match output with 
      | None ->
        error,
        handler,
        {Mvbdu_sig.id = (-1); Mvbdu_sig.value = bdu_val},
        -1
      | Some (a_id,_,a',handler) ->
        error,
        handler,
        a',
        a_id
  in 
  let error,output =
    Mvbdu_core.compress_node
      allocate
      error
      handler
      bdu_val
  in     
  match output with 
    | None ->
      error, 
      handler,
      a',
      a'_id,
      {Mvbdu_sig.id = (-1); Mvbdu_sig.value = bdu_val},
      (-1)
    | Some (a''_id,_,a'',handler) ->
      error,
      handler,
      a',
      a'_id,
      a'',
      a''_id     
        
let bdu_test remanent parameters =
  let error = remanent.Sanity_test_sig.error in 
  let allocate = remanent.Sanity_test_sig.allocate_mvbdu in 
  let (handler:('b,'a,'c,'d,bool,int) Memo_sig.handler) =
    remanent.Sanity_test_sig.mvbdu_handler
  in 
  let a_val = Mvbdu_sig.Leaf true in
  let b_val = Mvbdu_sig.Leaf false in 
  let error,(handler:('b,'a,'c,'d,bool,int) Memo_sig.handler),a',(a'_id:int),a'',a''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      a_val
      a_val 
  in   
  let error,handler,b',b'_id,b'',b''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      b_val
      b_val
  in 
  let c = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1 ;
        Mvbdu_sig.branch_true = a';
        Mvbdu_sig.branch_false = b';
        Mvbdu_sig.upper_bound = 2
      }
  in   
  let c_val =
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = a'_id;
        Mvbdu_sig.branch_false = b'_id;
        Mvbdu_sig.upper_bound = 2
      }
  in  
  let error,handler,c',c'_id,c'',c''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      c_val
      c
  in     
  let d = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1 ;
        Mvbdu_sig.branch_true = a';
        Mvbdu_sig.branch_false = b';
        Mvbdu_sig.upper_bound = 2
      }
  in 
  let d_val =
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = a'_id;
        Mvbdu_sig.branch_false = b'_id;
        Mvbdu_sig.upper_bound = 2
      }
  in 
  let error,handler,d',d'_id,d'',d''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      d_val
      d
  in   
  let e = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = a';
        Mvbdu_sig.branch_false = a';
        Mvbdu_sig.upper_bound = 2
      }
  in 
  let e_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = a'_id;
        Mvbdu_sig.branch_false = a'_id;
        Mvbdu_sig.upper_bound = 2
      }
  in 
  let error,handler,e',e'_id,e'',e''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      e_val
      e
  in   
  let f = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = a';
        Mvbdu_sig.branch_false = c';
        Mvbdu_sig.upper_bound = 1
      }
      
  in 
  let f_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = a'_id;
        Mvbdu_sig.branch_false = c'_id;
        Mvbdu_sig.upper_bound = 1
      }
  in 
  let error,handler,f',f'_id,f'',f''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      f_val 
      f
  in     
  let g = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 2;
        Mvbdu_sig.branch_true = b';
        Mvbdu_sig.branch_false = c';
        Mvbdu_sig.upper_bound = 1
      }
  in 
  let g_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 2;
        Mvbdu_sig.branch_true = b'_id;
        Mvbdu_sig.branch_false = c'_id;
        Mvbdu_sig.upper_bound = 1
      }
  in 
  let error,handler,g',g'_id,g'',g''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      g_val
      g
  in     
  let h = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = b';
        Mvbdu_sig.branch_false = c';
        Mvbdu_sig.upper_bound = 3
      }
      
  in
  let h_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = b'_id;
        Mvbdu_sig.branch_false = c'_id;
        Mvbdu_sig.upper_bound = 3
      }
  in 
  let error,handler,h',h'_id,h'',h''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      h_val
      h
  in     
  let i = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = b';
        Mvbdu_sig.branch_false = c';
        Mvbdu_sig.upper_bound = 0
      }
  in
  let i_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = b'_id;
        Mvbdu_sig.branch_false = c'_id;
        Mvbdu_sig.upper_bound = 0
      } 
  in 
  let error,handler,i',i'_id,i'',i''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      i_val
      i
  in     
  let j = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = b';
        Mvbdu_sig.branch_false = c';
        Mvbdu_sig.upper_bound = 2
      }
  in 
  let j_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = b'_id;
        Mvbdu_sig.branch_false = c'_id;
        Mvbdu_sig.upper_bound = 2
      }  
  in 
  let error,handler,j',j'_id,j'',j''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      j_val
      j
  in     
  let k = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = c';
        Mvbdu_sig.branch_false = b';
        Mvbdu_sig.upper_bound = 0
      }
  in 
  let k_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.branch_true = c'_id;
        Mvbdu_sig.branch_false = b'_id;
        Mvbdu_sig.upper_bound = 0
      }
  in 
  let error,handler,k',k'_id,k'',k''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      k_val
      k
  in     
  let copy bdu = 
    { bdu with Mvbdu_sig.value =
        match bdu.Mvbdu_sig.value with
          | Mvbdu_sig.Node x -> Mvbdu_sig.Node x
          | Mvbdu_sig.Leaf a -> Mvbdu_sig.Leaf a
    }
  in 
  let copy_c = copy c' in 
  let l = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.upper_bound = 1;
        Mvbdu_sig.branch_true = b';
        Mvbdu_sig.branch_false = copy_c
      }
  in 
  let l_val = 
    Mvbdu_sig.Node 
      {
        Mvbdu_sig.variable = 1;
        Mvbdu_sig.upper_bound = 1;
        Mvbdu_sig.branch_true = b'_id;
        Mvbdu_sig.branch_false = copy_c.Mvbdu_sig.id
      } 
  in 
  let f x y = 
    match x y with 
      | error,(handler,Some a) -> error,handler,a 
      | error,(handler,None) -> 
        let error, a =
          Exception.warn parameters error (Some "") (Some "")  Exit (fun _ -> a') in 
        error, handler, a
  in 
  let error, handler, l', l'_id, l'', l''_id =
    build_without_and_with_compressing
      allocate
      error
      handler
      l_val 
      l
  in     
  let error, handler, bmvbdu_true0 =
    f (Boolean_mvbdu.boolean_mvbdu_true parameters handler error) parameters in
  let error, handler, bmvbdu_false0 =
    f (Boolean_mvbdu.boolean_mvbdu_false parameters handler error) parameters in
  let error, handler, bmvbdu_true1 =
    f (Boolean_mvbdu.boolean_mvbdu_constant_true
         parameters handler error parameters)
      bmvbdu_true0 
  in 
  let error, handler, bmvbdu_true2 =
    f (Boolean_mvbdu.boolean_mvbdu_constant_true 
         parameters handler error parameters)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_false1 =
    f (Boolean_mvbdu.boolean_mvbdu_constant_false
         parameters handler error parameters)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false2 =
    f (Boolean_mvbdu.boolean_mvbdu_constant_false
         parameters handler error parameters)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_false3 =
    f (Boolean_mvbdu.boolean_mvbdu_or 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_true3 =
    f (Boolean_mvbdu.boolean_mvbdu_or
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0
  in
  let error,handler,bmvbdu_true4 =
    f (Boolean_mvbdu.boolean_mvbdu_or 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_true5 =
    f (Boolean_mvbdu.boolean_mvbdu_or 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_false4 =
    f (Boolean_mvbdu.boolean_mvbdu_and
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_false5 =
    f (Boolean_mvbdu.boolean_mvbdu_and
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_true6 =
    f (Boolean_mvbdu.boolean_mvbdu_and 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false6 =
    f (Boolean_mvbdu.boolean_mvbdu_and 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_false7 =
    f (Boolean_mvbdu.boolean_mvbdu_xor 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_true7 =
    f (Boolean_mvbdu.boolean_mvbdu_xor 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false8 =
    f (Boolean_mvbdu.boolean_mvbdu_xor
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_true8 =
    f (Boolean_mvbdu.boolean_mvbdu_xor
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_true9 =
    f (Boolean_mvbdu.boolean_mvbdu_nand 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_true10 =
    f (Boolean_mvbdu.boolean_mvbdu_nand 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false9 =
    f (Boolean_mvbdu.boolean_mvbdu_nand 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,(handler:Boolean_mvbdu.handler),bmvbdu_true11 =
    f (Boolean_mvbdu.boolean_mvbdu_nand 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_true12 =
    f (Boolean_mvbdu.boolean_mvbdu_nsnd 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_false10 =
    f (Boolean_mvbdu.boolean_mvbdu_nsnd 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false11 =
    f (Boolean_mvbdu.boolean_mvbdu_nsnd
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0
  in
  let error,(handler:Boolean_mvbdu.handler),bmvbdu_true13 =
    f (Boolean_mvbdu.boolean_mvbdu_nsnd 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_true14 =
    f (Boolean_mvbdu.boolean_mvbdu_nfst 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_true15 =
    f (Boolean_mvbdu.boolean_mvbdu_nfst 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false12 =
    f (Boolean_mvbdu.boolean_mvbdu_nfst 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,(handler:Boolean_mvbdu.handler),bmvbdu_false13 =
    f (Boolean_mvbdu.boolean_mvbdu_nfst 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_false14 =
    f (Boolean_mvbdu.boolean_mvbdu_snd 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0
  in 
  let error,handler,bmvbdu_true16 =
    f (Boolean_mvbdu.boolean_mvbdu_snd 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_true17 =
    f (Boolean_mvbdu.boolean_mvbdu_snd 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0
  in
  let error,(handler:Boolean_mvbdu.handler),bmvbdu_false15 =
    f (Boolean_mvbdu.boolean_mvbdu_snd 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_false16 =
    f (Boolean_mvbdu.boolean_mvbdu_fst 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0 
  in 
  let error,handler,bmvbdu_false17 =
    f (Boolean_mvbdu.boolean_mvbdu_fst 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_true18 =
    f (Boolean_mvbdu.boolean_mvbdu_fst 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,(handler:Boolean_mvbdu.handler),bmvbdu_true19 =
    f (Boolean_mvbdu.boolean_mvbdu_fst 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0 
  in
  let error,handler,bmvbdu_true20 =
    f (Boolean_mvbdu.boolean_mvbdu_nor 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_false0
  in 
  let error,handler,bmvbdu_false18 =
    f (Boolean_mvbdu.boolean_mvbdu_nor 
         parameters handler error parameters bmvbdu_false0)
      bmvbdu_true0 
  in
  let error,handler,bmvbdu_false19 =
    f (Boolean_mvbdu.boolean_mvbdu_nor 
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_true0 
  in
  let error,(handler:Boolean_mvbdu.handler),bmvbdu_false20 =
    f (Boolean_mvbdu.boolean_mvbdu_nor
         parameters handler error parameters bmvbdu_true0)
      bmvbdu_false0
  in
  let error,handler,bmvbdu_false21 =
    f (Boolean_mvbdu.boolean_constant_bi_false
         parameters handler error parameters bmvbdu_false0) bmvbdu_false0 in 
  let error,handler,bmvbdu_false22 =
    f (Boolean_mvbdu.boolean_constant_bi_false
         parameters handler error parameters bmvbdu_false0) bmvbdu_true0 in
  let error,handler,bmvbdu_false23 =
    f (Boolean_mvbdu.boolean_constant_bi_false
         parameters handler error parameters bmvbdu_true0) bmvbdu_true0 in
  let error,handler,bmvbdu_false24 =
    f (Boolean_mvbdu.boolean_constant_bi_false
         parameters handler error parameters bmvbdu_true0) bmvbdu_false0 in
  let error,handler,bmvbdu_true21 =
    f (Boolean_mvbdu.boolean_constant_bi_true
         parameters handler error parameters bmvbdu_false0) bmvbdu_false0 in 
  let error,handler,bmvbdu_true22 =
    f (Boolean_mvbdu.boolean_constant_bi_true
         parameters handler error parameters bmvbdu_false0) bmvbdu_true0 in
  let error,handler,bmvbdu_true23 =
    f (Boolean_mvbdu.boolean_constant_bi_true
         parameters handler error parameters bmvbdu_true0) bmvbdu_true0 in
  let error,handler,bmvbdu_true24 =
    f (Boolean_mvbdu.boolean_constant_bi_true
         parameters handler error parameters bmvbdu_true0) bmvbdu_false0 in
  let error,handler,bmvbdu_true25 =
    f (Boolean_mvbdu.boolean_mvbdu_imply 
         parameters handler error parameters bmvbdu_false0) bmvbdu_false0 in 
  let error,handler,bmvbdu_true26 =
    f (Boolean_mvbdu.boolean_mvbdu_imply
         parameters handler error parameters bmvbdu_false0) bmvbdu_true0 in
  let error,handler,bmvbdu_true27 =
    f (Boolean_mvbdu.boolean_mvbdu_imply
         parameters handler error parameters bmvbdu_true0) bmvbdu_true0 in
  let error,handler,bmvbdu_false25 =
    f (Boolean_mvbdu.boolean_mvbdu_imply
         parameters handler error parameters bmvbdu_true0) bmvbdu_false0 in
  let error,handler,bmvbdu_true28 =
    f (Boolean_mvbdu.boolean_mvbdu_is_implied
         parameters handler error parameters bmvbdu_false0) bmvbdu_false0 in 
  let error,handler,bmvbdu_false26 =
    f (Boolean_mvbdu.boolean_mvbdu_is_implied
         parameters handler error parameters bmvbdu_false0) bmvbdu_true0 in
  let error,handler,bmvbdu_true29 =
    f (Boolean_mvbdu.boolean_mvbdu_is_implied
         parameters handler error parameters bmvbdu_true0) bmvbdu_true0 in
  let error,handler,bmvbdu_true30 =
    f (Boolean_mvbdu.boolean_mvbdu_is_implied
         parameters handler error parameters bmvbdu_true0) bmvbdu_false0 in
  let error,handler,bmvbdu_false27 =
    f (Boolean_mvbdu.boolean_mvbdu_nimply
         parameters handler error parameters bmvbdu_false0) bmvbdu_false0 in 
  let error,handler,bmvbdu_false28 =
    f (Boolean_mvbdu.boolean_mvbdu_nimply
         parameters handler error parameters bmvbdu_false0) bmvbdu_true0 in
  let error,handler,bmvbdu_false29 =
    f (Boolean_mvbdu.boolean_mvbdu_nimply
         parameters handler error parameters bmvbdu_true0) bmvbdu_true0 in
  let error,handler,bmvbdu_true31 =
    f (Boolean_mvbdu.boolean_mvbdu_nimply
         parameters handler error parameters bmvbdu_true0) bmvbdu_false0 in
  let error,handler,bmvbdu_false30 =
    f (Boolean_mvbdu.boolean_mvbdu_nis_implied
         parameters handler error parameters bmvbdu_false0) bmvbdu_false0 in 
  let error,handler,bmvbdu_true32 =
    f (Boolean_mvbdu.boolean_mvbdu_nis_implied
         parameters handler error parameters bmvbdu_false0) bmvbdu_true0 in
  let error,handler,bmvbdu_false31 =
    f (Boolean_mvbdu.boolean_mvbdu_nis_implied
         parameters handler error parameters bmvbdu_true0) bmvbdu_true0 in
  let error,handler,bmvbdu_false32 =
    f (Boolean_mvbdu.boolean_mvbdu_nis_implied
         parameters handler error parameters bmvbdu_true0) bmvbdu_false0 in
  let error,handler,bmvbdu_true33 =
    f (Boolean_mvbdu.clean_head parameters error handler) e' in
  let list = [4,1; 2,2; 1,3] in 
  let list' = [2,2; 4,1; 1,3] in 
  let error,(handler,list_a) =
    List_algebra.build_list
      (Boolean_mvbdu.association_list_allocate parameters)
      error
      parameters
      handler
      list
  in 
  let error,(handler,list_b) =
    List_algebra.build_sorted_list
      (Boolean_mvbdu.association_list_allocate parameters)
      parameters
      error
      handler
      list
  in
  let error,(handler,list_c) =
    List_algebra.build_reversed_sorted_list
      (Boolean_mvbdu.association_list_allocate parameters)
      parameters
      error
      handler
      list
  in 
  let error,(handler,list_a') =
    List_algebra.build_list
      (Boolean_mvbdu.association_list_allocate parameters)
      error
      parameters
      handler
      list'
  in 
  let error,(handler,list_b') =
    List_algebra.build_sorted_list
      (Boolean_mvbdu.association_list_allocate parameters)
      parameters
      error
      handler
      list'
  in
  let error,(handler,list_c') =
    List_algebra.build_reversed_sorted_list
      (Boolean_mvbdu.association_list_allocate parameters)
      parameters 
      error
      handler
      list'
  in 
  let error,handler,mvbdu =
    f (Boolean_mvbdu.redefine parameters error parameters handler l') list_a in 
  let error,handler,l''' =
    f (Boolean_mvbdu.clean_head parameters error handler) l'' in 
  let error =
    Boolean_mvbdu.print_boolean_mvbdu
      error
      (Remanent_parameters.update_prefix parameters "l': ")
      l''
  in  
  let error =
    Boolean_mvbdu.print_boolean_mvbdu
      error
      (Remanent_parameters.update_prefix parameters "l'': ")
      l''' 
  in 
  let error =
    Boolean_mvbdu.print_boolean_mvbdu 
      error 
      (Remanent_parameters.update_prefix parameters "mvbdu:")
      mvbdu
  in 
  let error =
    Boolean_mvbdu.print_memo
      error
      handler
      (Remanent_parameters.update_prefix parameters "Memoization tables:")
  in 
  let handler_0 = handler in 

  (* WRAPPED MVBDU ORIGINAL *)
  let b0 = Mvbdu_wrapper.Mvbdu.is_init () in 
  let error', handler = Mvbdu_wrapper.Mvbdu.init parameters error in 
  let b1 = Mvbdu_wrapper.Mvbdu.is_init () in 
  let error'',handler = Mvbdu_wrapper.Mvbdu.init parameters error' in 
  let b2,b3 = error==error',error'==error'' in 
  let error = error'' in 
  let error,handler,bmvbdu_true0' = Mvbdu_wrapper.Mvbdu.mvbdu_true parameters handler error in 
  let error, handler, bmvbdu_false0' = Mvbdu_wrapper.Mvbdu.mvbdu_false parameters handler error in 
  let error, handler, bmvbdu_true1' = Mvbdu_wrapper.Mvbdu.mvbdu_unary_true parameters handler error bmvbdu_true0' in
  let error, handler, bmvbdu_true2' = Mvbdu_wrapper.Mvbdu.mvbdu_unary_true parameters handler error bmvbdu_false0' in 
  let error,handler,bmvbdu_false1' = Mvbdu_wrapper.Mvbdu.mvbdu_unary_false parameters handler error bmvbdu_true0' in 
  let error,handler,bmvbdu_false2' = Mvbdu_wrapper.Mvbdu.mvbdu_unary_false parameters handler error bmvbdu_false0' in 
  let error,handler,bmvbdu_false3' = Mvbdu_wrapper.Mvbdu.mvbdu_or parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true3' = Mvbdu_wrapper.Mvbdu.mvbdu_or parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true4' = Mvbdu_wrapper.Mvbdu.mvbdu_or parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true5' = Mvbdu_wrapper.Mvbdu.mvbdu_or parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false4' = Mvbdu_wrapper.Mvbdu.mvbdu_and parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false5' = Mvbdu_wrapper.Mvbdu.mvbdu_and parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true6' = Mvbdu_wrapper.Mvbdu.mvbdu_and parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false6' = Mvbdu_wrapper.Mvbdu.mvbdu_and parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false7' = Mvbdu_wrapper.Mvbdu.mvbdu_xor parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true7' = Mvbdu_wrapper.Mvbdu.mvbdu_xor parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false8' = Mvbdu_wrapper.Mvbdu.mvbdu_xor parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true8' = Mvbdu_wrapper.Mvbdu.mvbdu_xor parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true9' = Mvbdu_wrapper.Mvbdu.mvbdu_nand parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true10' = Mvbdu_wrapper.Mvbdu.mvbdu_nand parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false9' = Mvbdu_wrapper.Mvbdu.mvbdu_nand parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true11' = Mvbdu_wrapper.Mvbdu.mvbdu_nand parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true12' = Mvbdu_wrapper.Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false10' = Mvbdu_wrapper.Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false11' = Mvbdu_wrapper.Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true13' = Mvbdu_wrapper.Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true14' = Mvbdu_wrapper.Mvbdu.mvbdu_nfst parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true15' = Mvbdu_wrapper.Mvbdu.mvbdu_nfst parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false12' = Mvbdu_wrapper.Mvbdu.mvbdu_nfst parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false13' = Mvbdu_wrapper.Mvbdu.mvbdu_nfst parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false14' = Mvbdu_wrapper.Mvbdu.mvbdu_snd parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true16' = Mvbdu_wrapper.Mvbdu.mvbdu_snd parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true17' = Mvbdu_wrapper.Mvbdu.mvbdu_snd parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false15' = Mvbdu_wrapper.Mvbdu.mvbdu_snd parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false16' = Mvbdu_wrapper.Mvbdu.mvbdu_fst parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false17' = Mvbdu_wrapper.Mvbdu.mvbdu_fst parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true18' = Mvbdu_wrapper.Mvbdu.mvbdu_fst parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true19' = Mvbdu_wrapper.Mvbdu.mvbdu_fst parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true20' = Mvbdu_wrapper.Mvbdu.mvbdu_nor parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false18' = Mvbdu_wrapper.Mvbdu.mvbdu_nor parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false19' = Mvbdu_wrapper.Mvbdu.mvbdu_nor parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false20' = Mvbdu_wrapper.Mvbdu.mvbdu_nor parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false21' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false22' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false23' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false24' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true21' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true22' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true23' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true24' = Mvbdu_wrapper.Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true25' = Mvbdu_wrapper.Mvbdu.mvbdu_imply parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true26' = Mvbdu_wrapper.Mvbdu.mvbdu_imply parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true27' = Mvbdu_wrapper.Mvbdu.mvbdu_imply parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false25' = Mvbdu_wrapper.Mvbdu.mvbdu_imply parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_true28' = Mvbdu_wrapper.Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false26' = Mvbdu_wrapper.Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_true29' = Mvbdu_wrapper.Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_true30' = Mvbdu_wrapper.Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false27' = Mvbdu_wrapper.Mvbdu.mvbdu_nimply parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_false28' = Mvbdu_wrapper.Mvbdu.mvbdu_nimply parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false29' = Mvbdu_wrapper.Mvbdu.mvbdu_nimply parameters handler error bmvbdu_true0' bmvbdu_true0' in 
  let error,handler,bmvbdu_true31' = Mvbdu_wrapper.Mvbdu.mvbdu_nimply parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let error,handler,bmvbdu_false30' = Mvbdu_wrapper.Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_false0' bmvbdu_false0' in 
  let error,handler,bmvbdu_true32' = Mvbdu_wrapper.Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_false0' bmvbdu_true0' in
  let error,handler,bmvbdu_false31' = Mvbdu_wrapper.Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_true0' bmvbdu_true0' in
  let error,handler,bmvbdu_false32' = Mvbdu_wrapper.Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_true0' bmvbdu_false0' in
  let list = [4,1; 2,2; 1,3] in 
  let list' = [2,2; 4,1; 1,3] in 
  let error,handler,list__a = Mvbdu_wrapper.Mvbdu.build_association_list parameters handler error list in 
  let error,handler,list__c = Mvbdu_wrapper.Mvbdu.build_reverse_sorted_association_list parameters handler error list in 
  let error,handler,list__a' =Mvbdu_wrapper.Mvbdu.build_association_list parameters handler error list' in 
  let error,handler,list__b' = Mvbdu_wrapper.Mvbdu.build_sorted_association_list parameters handler error list' in 
  let error,handler,mvbdu = Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameters handler error bmvbdu_true0' list__a in 

 (* WRAPPED MVBDU OPTIMIZED *)
  let b0' = Mvbdu_wrapper.Optimized_Mvbdu.is_init () in 
  let error_', handler = Mvbdu_wrapper.Optimized_Mvbdu.init parameters error in 
  let b1' = Mvbdu_wrapper.Optimized_Mvbdu.is_init () in 
  let error_'',handler = Mvbdu_wrapper.Optimized_Mvbdu.init parameters error_' in 
  let b2',b3' = error==error_',error_'==error_'' in 
  let error = error_'' in 
  let error,handler,bmvbdu_true0'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_true parameters handler error in 
  let error, handler, bmvbdu_false0'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_false parameters handler error in 
  let error, handler, bmvbdu_true1'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_unary_true parameters handler error bmvbdu_true0'' in
  let error, handler, bmvbdu_true2'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_unary_true parameters handler error bmvbdu_false0'' in 
  let error,handler,bmvbdu_false1'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_unary_false parameters handler error bmvbdu_true0'' in 
  let error,handler,bmvbdu_false2'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_unary_false parameters handler error bmvbdu_false0'' in 
  let error,handler,bmvbdu_false3'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_or parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true3'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_or parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true4'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_or parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true5'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_or parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false4'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_and parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false5'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_and parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true6'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_and parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false6'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_and parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false7'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_xor parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true7'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_xor parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false8'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_xor parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true8'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_xor parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true9'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nand parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true10'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nand parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false9'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nand parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true11'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nand parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true12'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false10'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false11'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true13'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nsnd parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true14'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nfst parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true15'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nfst parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false12'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nfst parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false13'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nfst parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false14'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_snd parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true16'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_snd parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true17'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_snd parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false15'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_snd parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false16'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_fst parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false17'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_fst parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true18'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_fst parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true19'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_fst parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true20'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nor parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false18'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nor parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false19'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nor parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false20'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nor parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false21'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false22'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false23'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false24'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_false parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true21'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true22'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true23'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true24'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_bi_true parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true25'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_imply parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true26'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_imply parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true27'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_imply parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false25'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_imply parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_true28'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false26'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true29'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_true30'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_rev_imply parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false27'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nimply parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_false28'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nimply parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false29'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nimply parameters handler error bmvbdu_true0'' bmvbdu_true0'' in 
  let error,handler,bmvbdu_true31'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nimply parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let error,handler,bmvbdu_false30'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_false0'' bmvbdu_false0'' in 
  let error,handler,bmvbdu_true32'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_false0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false31'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_true0'' bmvbdu_true0'' in
  let error,handler,bmvbdu_false32'' = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_nrev_imply parameters handler error bmvbdu_true0'' bmvbdu_false0'' in
  let list = [4,1; 2,2; 1,3] in 
  let list' = [2,2; 4,1; 1,3] in 
  let error,handler,list___a = Mvbdu_wrapper.Optimized_Mvbdu.build_association_list parameters handler error list in 
  let error,handler,list___b = Mvbdu_wrapper.Optimized_Mvbdu.build_sorted_association_list parameters handler error list in 
  let error,handler,list___c = Mvbdu_wrapper.Optimized_Mvbdu.build_reverse_sorted_association_list parameters handler error list in 
  let error,handler,list___a' =Mvbdu_wrapper.Optimized_Mvbdu.build_association_list parameters handler error list' in 
  let error,handler,list___b' =Mvbdu_wrapper.Optimized_Mvbdu.build_sorted_association_list parameters handler error list' in 
  let error,handler,list___c' = Mvbdu_wrapper.Optimized_Mvbdu.build_reverse_sorted_association_list parameters handler error list' in 
  let error,handler,mvbdu = Mvbdu_wrapper.Optimized_Mvbdu.mvbdu_redefine parameters handler error bmvbdu_true0'' list___a in 

 (* WRAPPED MVBDU INTERNALIZED *)
  let b0'' = Mvbdu_wrapper.IntMvbdu.is_init () in 
  let () = Mvbdu_wrapper.IntMvbdu.init parameters in 
  let b1'' = Mvbdu_wrapper.IntMvbdu.is_init () in 
  let () = Mvbdu_wrapper.IntMvbdu.init parameters in 
  let bmvbdu_true0''' = Mvbdu_wrapper.IntMvbdu.mvbdu_true () in 
  let bmvbdu_false0''' = Mvbdu_wrapper.IntMvbdu.mvbdu_false () in 
  let bmvbdu_true1''' = Mvbdu_wrapper.IntMvbdu.mvbdu_unary_true bmvbdu_true0''' in
  let bmvbdu_true2''' = Mvbdu_wrapper.IntMvbdu.mvbdu_unary_true bmvbdu_false0''' in 
  let bmvbdu_false1''' = Mvbdu_wrapper.IntMvbdu.mvbdu_unary_false bmvbdu_true0''' in 
  let bmvbdu_false2''' = Mvbdu_wrapper.IntMvbdu.mvbdu_unary_false bmvbdu_false0''' in 
  let bmvbdu_false3''' = Mvbdu_wrapper.IntMvbdu.mvbdu_or bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true3''' = Mvbdu_wrapper.IntMvbdu.mvbdu_or bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true4''' = Mvbdu_wrapper.IntMvbdu.mvbdu_or bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true5''' = Mvbdu_wrapper.IntMvbdu.mvbdu_or bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false4''' = Mvbdu_wrapper.IntMvbdu.mvbdu_and bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false5''' = Mvbdu_wrapper.IntMvbdu.mvbdu_and bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true6''' = Mvbdu_wrapper.IntMvbdu.mvbdu_and bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false6''' = Mvbdu_wrapper.IntMvbdu.mvbdu_and bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false7''' = Mvbdu_wrapper.IntMvbdu.mvbdu_xor bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true7''' = Mvbdu_wrapper.IntMvbdu.mvbdu_xor bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false8''' = Mvbdu_wrapper.IntMvbdu.mvbdu_xor bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true8''' = Mvbdu_wrapper.IntMvbdu.mvbdu_xor bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true9''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nand bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true10''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nand bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false9''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nand bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true11''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nand bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true12''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nsnd bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false10''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nsnd bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false11''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nsnd bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true13''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nsnd bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true14''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nfst bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true15''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nfst bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false12''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nfst bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false13''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nfst bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false14''' = Mvbdu_wrapper.IntMvbdu.mvbdu_snd bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true16''' = Mvbdu_wrapper.IntMvbdu.mvbdu_snd bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true17''' = Mvbdu_wrapper.IntMvbdu.mvbdu_snd bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false15''' = Mvbdu_wrapper.IntMvbdu.mvbdu_snd bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false16''' = Mvbdu_wrapper.IntMvbdu.mvbdu_fst bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false17''' = Mvbdu_wrapper.IntMvbdu.mvbdu_fst bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true18''' = Mvbdu_wrapper.IntMvbdu.mvbdu_fst bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true19''' = Mvbdu_wrapper.IntMvbdu.mvbdu_fst bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true20''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nor bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false18''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nor bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false19''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nor bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false20''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nor bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false21''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_false bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false22''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_false bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false23''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_false bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false24''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_false bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true21''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_true bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true22''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_true bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true23''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_true bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true24''' = Mvbdu_wrapper.IntMvbdu.mvbdu_bi_true bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true25''' = Mvbdu_wrapper.IntMvbdu.mvbdu_imply bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true26''' = Mvbdu_wrapper.IntMvbdu.mvbdu_imply bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true27''' = Mvbdu_wrapper.IntMvbdu.mvbdu_imply bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false25''' = Mvbdu_wrapper.IntMvbdu.mvbdu_imply bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_true28''' = Mvbdu_wrapper.IntMvbdu.mvbdu_rev_imply bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false26''' = Mvbdu_wrapper.IntMvbdu.mvbdu_rev_imply bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_true29''' = Mvbdu_wrapper.IntMvbdu.mvbdu_rev_imply bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_true30''' = Mvbdu_wrapper.IntMvbdu.mvbdu_rev_imply bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false27''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nimply bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_false28''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nimply bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false29''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nimply bmvbdu_true0''' bmvbdu_true0''' in 
  let bmvbdu_true31''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nimply bmvbdu_true0''' bmvbdu_false0''' in
  let bmvbdu_false30''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nrev_imply bmvbdu_false0''' bmvbdu_false0''' in 
  let bmvbdu_true32''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nrev_imply bmvbdu_false0''' bmvbdu_true0''' in
  let bmvbdu_false31''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nrev_imply bmvbdu_true0''' bmvbdu_true0''' in
  let bmvbdu_false32''' = Mvbdu_wrapper.IntMvbdu.mvbdu_nrev_imply bmvbdu_true0''' bmvbdu_false0''' in
  let list = [4,1; 2,2; 1,3] in 
  let list' = [2,2; 4,1; 1,3] in 
  let list____a = Mvbdu_wrapper.IntMvbdu.build_association_list list in 
  let list____c = Mvbdu_wrapper.IntMvbdu.build_reverse_sorted_association_list list in 
  let list____a' =Mvbdu_wrapper.IntMvbdu.build_association_list list' in 
  let _ = Mvbdu_wrapper.IntMvbdu.mvbdu_redefine bmvbdu_true0''' list____a in 

 (* WRAPPED MVBDU INTERNALISED & OPTIMIZED *)
  let b0''' = Mvbdu_wrapper.Optimized_IntMvbdu.is_init () in 
  let () = Mvbdu_wrapper.Optimized_IntMvbdu.init parameters in 
  let b1''' = Mvbdu_wrapper.Optimized_IntMvbdu.is_init () in 
  let () = Mvbdu_wrapper.Optimized_IntMvbdu.init parameters in 
  let bmvbdu_true0'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_true () in 
  let bmvbdu_false0'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_false () in 
  let bmvbdu_true1'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_unary_true bmvbdu_true0'''' in
  let bmvbdu_true2'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_unary_true bmvbdu_false0'''' in 
  let bmvbdu_false1'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_unary_false bmvbdu_true0'''' in 
  let bmvbdu_false2'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_unary_false bmvbdu_false0'''' in 
  let bmvbdu_false3'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_or bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true3'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_or bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true4'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_or bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true5'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_or bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false4'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_and bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false5'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_and bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true6'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_and bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false6'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_and bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false7'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_xor bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true7'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_xor bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false8'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_xor bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true8'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_xor bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true9'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nand bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true10'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nand bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false9'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nand bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true11'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nand bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true12'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nsnd bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false10'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nsnd bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false11'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nsnd bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true13'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nsnd bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true14'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nfst bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true15'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nfst bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false12'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nfst bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false13'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nfst bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false14'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_snd bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true16'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_snd bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true17'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_snd bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false15'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_snd bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false16'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_fst bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false17'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_fst bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true18'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_fst bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true19'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_fst bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true20'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nor bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false18'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nor bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false19'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nor bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false20'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nor bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false21'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_false bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false22'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_false bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false23'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_false bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false24'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_false bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true21'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_true bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true22'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_true bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true23'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_true bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true24'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_bi_true bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true25'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_imply bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true26'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_imply bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true27'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_imply bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false25'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_imply bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_true28'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_rev_imply bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false26'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_rev_imply bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_true29'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_rev_imply bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_true30'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_rev_imply bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false27'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nimply bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_false28'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nimply bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false29'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nimply bmvbdu_true0'''' bmvbdu_true0'''' in 
  let bmvbdu_true31'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nimply bmvbdu_true0'''' bmvbdu_false0'''' in
  let bmvbdu_false30'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nrev_imply bmvbdu_false0'''' bmvbdu_false0'''' in 
  let bmvbdu_true32'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nrev_imply bmvbdu_false0'''' bmvbdu_true0'''' in
  let bmvbdu_false31'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nrev_imply bmvbdu_true0'''' bmvbdu_true0'''' in
  let bmvbdu_false32'''' = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_nrev_imply bmvbdu_true0'''' bmvbdu_false0'''' in
  let list = [4,1; 2,2; 1,3] in 
  let list' = [2,2; 4,1; 1,3] in 
  let list_____a = Mvbdu_wrapper.Optimized_IntMvbdu.build_association_list list in 
  let list_____c = Mvbdu_wrapper.Optimized_IntMvbdu.build_reverse_sorted_association_list list in 
  let list_____a' =Mvbdu_wrapper.Optimized_IntMvbdu.build_association_list list' in 
  let _ = Mvbdu_wrapper.Optimized_IntMvbdu.mvbdu_redefine bmvbdu_true0'''' list_____a in 


  {
    remanent with 
      Sanity_test_sig.error = error ; 
      Sanity_test_sig.mvbdu_handler = handler_0
  },
  ("Mvbdu.001",fun remanent ->
    let b = Mvbdu_core.mvbdu_equal f'' c'' in
    remanent, b, None) :: (List.map (fun (a, b, c) -> a,
      fun remanent -> Mvbdu_sanity.test remanent c b) 
       [
         "Mvbdu.002",a',(true,true,true);
         "Mvbdu.003",b',(true,true,true);
         "Mvbdu.004",c',(true,true,true);
         "Mvbdu.005",d',(true,true,true);
         "Mvbdu.006",e',(true,true,false);
         "Mvbdu.007",f',(true,true,false);
         "Mvbdu.008",g',(false,true,true);
         "Mvbdu.009",h',(false,true,true);
         "Mvbdu.010",i',(true,true,true);           
         "Mvbdu.011",j',(false,true,true);
         "Mvbdu.012",k',(false,true,true);
         "Mvbdu.013",l',(true,false,true);
         
         "Mvbdu.014",a'',(true,true,true);
         "Mvbdu.015",b'',(true,true,true);
         "Mvbdu.016",c'',(true,true,true);
         "Mvbdu.017",d'',(true,true,true);
         "Mvbdu.018",e'',(true,true,true);
         "Mvbdu.019",f'',(true,true,true);
         "Mvbdu.020",g'',(false,true,true);
         "Mvbdu.021",h'',(false,true,true);
         "Mvbdu.022",i'',(true,true,true);
         "Mvbdu.023",j'',(false,true,true);
         "Mvbdu.024",k'',(false,true,true);
         "Mvbdu.025",l'',(true,false,true);
         
         "Mvbdu.026",copy a',(true,false,true);
         "Mvbdu.027",copy b',(true,false,true);
         "Mvbdu.028",copy c',(true,false,true);
         "Mvbdu.029",copy d',(true,false,true);
         "Mvbdu.030",copy e',(true,false,false);
         "Mvbdu.031",copy f',(true,false,false);
         "Mvbdu.032",copy g',(false,false,true);
         "Mvbdu.033",copy h',(false,false,true);
         "Mvbdu.034",copy i',(true,false,true);           
         "Mvbdu.035",copy j',(false,false,true);
         "Mvbdu.036",copy k',(false,false,true);
         "Mvbdu.037",copy l',(true,false,true);
         
         "Mvbdu.038",copy a'',(true,false,true);
         "Mvbdu.039",copy b'',(true,false,true);
         "Mvbdu.040",copy c'',(true,false,true);
         "Mvbdu.041",copy d'',(true,false,true);
         "Mvbdu.042",copy e'',(true,false,true);
         "Mvbdu.043",copy f'',(true,false,true);
         "Mvbdu.044",copy g'',(false,false,true);
         "Mvbdu.045",copy h'',(false,false,true);
         "Mvbdu.046",copy i'',(true,false,true);
         "Mvbdu.047",copy j'',(false,false,true);
         "Mvbdu.048",copy k'',(false,false,true);
         "Mvbdu.049",copy l'',(true,false,true);
         
         "Mvbdu.050",copy_c,(true,false,true);  
         "Mvbdu.051",bmvbdu_true0,(true,true,true);
         "Mvbdu.052",bmvbdu_true1,(true,true,true);
         "Mvbdu.053",bmvbdu_true2,(true,true,true);
         "Mvbdu.054",bmvbdu_true3,(true,true,true);
         "Mvbdu.055",bmvbdu_true4,(true,true,true);
         "Mvbdu.056",bmvbdu_true5,(true,true,true);
         "Mvbdu.057",bmvbdu_true6,(true,true,true);
         "Mvbdu.058",bmvbdu_true7,(true,true,true);
         "Mvbdu.059",bmvbdu_true8,(true,true,true);
         "Mvbdu.060",bmvbdu_true9,(true,true,true);
         "Mvbdu.061",bmvbdu_true10,(true,true,true);
         "Mvbdu.062",bmvbdu_true11,(true,true,true);
         "Mvbdu.063",bmvbdu_true12,(true,true,true);
         "Mvbdu.064",bmvbdu_true13,(true,true,true);
         "Mvbdu.065",bmvbdu_true14,(true,true,true);
         "Mvbdu.066",bmvbdu_true15,(true,true,true);
         "Mvbdu.067",bmvbdu_true16,(true,true,true);
         "Mvbdu.068",bmvbdu_true17,(true,true,true);
         "Mvbdu.069",bmvbdu_true18,(true,true,true);
         "Mvbdu.070",bmvbdu_true19,(true,true,true);
         "Mvbdu.071",bmvbdu_true20,(true,true,true);
         "Mvbdu.072",bmvbdu_true21,(true,true,true);
         "Mvbdu.073",bmvbdu_true22,(true,true,true);
         "Mvbdu.074",bmvbdu_true23,(true,true,true);
         "Mvbdu.075",bmvbdu_true24,(true,true,true);        
         "Mvbdu.076",bmvbdu_true25,(true,true,true);
         "Mvbdu.077",bmvbdu_true26,(true,true,true);
         "Mvbdu.078",bmvbdu_true27,(true,true,true);
         "Mvbdu.079",bmvbdu_true28,(true,true,true);
         "Mvbdu.080",bmvbdu_true29,(true,true,true);
         "Mvbdu.081",bmvbdu_true30,(true,true,true);
         "Mvbdu.082",bmvbdu_true31,(true,true,true);
         "Mvbdu.083",bmvbdu_true32,(true,true,true);         
         "Mvbdu.084",bmvbdu_false0,(true,true,true);
         "Mvbdu.085",bmvbdu_false1,(true,true,true);
         "Mvbdu.086",bmvbdu_false2,(true,true,true);
         "Mvbdu.087",bmvbdu_false3,(true,true,true);
         "Mvbdu.088",bmvbdu_false4,(true,true,true);
         "Mvbdu.089",bmvbdu_false5,(true,true,true);
         "Mvbdu.090",bmvbdu_false6,(true,true,true);
         "Mvbdu.091",bmvbdu_false7,(true,true,true);
         "Mvbdu.092",bmvbdu_false8,(true,true,true);
         "Mvbdu.093",bmvbdu_false9,(true,true,true);
         "Mvbdu.094",bmvbdu_false10,(true,true,true);
         "Mvbdu.095",bmvbdu_false11,(true,true,true);
         "Mvbdu.096",bmvbdu_false12,(true,true,true);
         "Mvbdu.097",bmvbdu_false13,(true,true,true);
         "Mvbdu.098",bmvbdu_false14,(true,true,true);
         "Mvbdu.099",bmvbdu_false15,(true,true,true);
         "Mvbdu.100",bmvbdu_false16,(true,true,true);
         "Mvbdu.101",bmvbdu_false17,(true,true,true);
         "Mvbdu.102",bmvbdu_false18,(true,true,true);
         "Mvbdu.103",bmvbdu_false19,(true,true,true);
         "Mvbdu.104",bmvbdu_false20,(true,true,true);
         "Mvbdu.105",bmvbdu_false21,(true,true,true);
         "Mvbdu.106",bmvbdu_false22,(true,true,true);
         "Mvbdu.107",bmvbdu_false23,(true,true,true);
         "Mvbdu.108",bmvbdu_false24,(true,true,true);
         "Mvbdu.109",bmvbdu_false25,(true,true,true);
         "Mvbdu.110",bmvbdu_false26,(true,true,true);
         "Mvbdu.111",bmvbdu_false27,(true,true,true);
         "Mvbdu.112",bmvbdu_false28,(true,true,true);
         "Mvbdu.113",bmvbdu_false29,(true,true,true);
         "Mvbdu.114",bmvbdu_false30,(true,true,true);
         "Mvbdu.115",bmvbdu_false31,(true,true,true);
         "Mvbdu.116",bmvbdu_false32,(true,true,true);
         "Mvbdu.117",bmvbdu_true33,(true,true,true);
         "Mvbdu.118",l''',(true,true,true);
         
       ])@
    (List.map (fun (a,b) -> a,(fun remanent -> remanent, b == bmvbdu_true0, None))  
       ["true00",bmvbdu_true0;
        "true01",bmvbdu_true1;
        "true02",bmvbdu_true2;
        "true03",bmvbdu_true3;
        "true04",bmvbdu_true4;
        "true05",bmvbdu_true5;
        "true06",bmvbdu_true6;
        "true07",bmvbdu_true7;
        "true08",bmvbdu_true8;
        "true09",bmvbdu_true9;
        "true10",bmvbdu_true10;
        "true11",bmvbdu_true11;
        "true12",bmvbdu_true12;
        "true13",bmvbdu_true13;
        "true14",bmvbdu_true14;
        "true15",bmvbdu_true15;
        "true16",bmvbdu_true16;
        "true17",bmvbdu_true17;
        "true18",bmvbdu_true18;
        "true19",bmvbdu_true19;
        "true20",bmvbdu_true20; 
        "true21",bmvbdu_true21;
        "true22",bmvbdu_true22;
        "true23",bmvbdu_true23;
        "true24",bmvbdu_true24;
        "true25",bmvbdu_true25;
        "true26",bmvbdu_true26;
        "true27",bmvbdu_true27;
        "true28",bmvbdu_true28;
        "true29",bmvbdu_true29;
        "true30",bmvbdu_true30; 
        "true31",bmvbdu_true31;
        "true32",bmvbdu_true32;
        "true33",bmvbdu_true33])@
  (List.map (fun (a,b) -> a,(fun remanent -> remanent, b == bmvbdu_true0', None))      
     [	"true00'",bmvbdu_true0';
        "true01'",bmvbdu_true1';
        "true02'",bmvbdu_true2';
        "true03'",bmvbdu_true3';
        "true04'",bmvbdu_true4';
        "true05'",bmvbdu_true5';
        "true06'",bmvbdu_true6';
        "true07'",bmvbdu_true7';
        "true08'",bmvbdu_true8';
        "true09'",bmvbdu_true9';
        "true10'",bmvbdu_true10';
        "true11'",bmvbdu_true11';
        "true12'",bmvbdu_true12';
        "true13'",bmvbdu_true13';
        "true14'",bmvbdu_true14';
        "true15'",bmvbdu_true15';
        "true16'",bmvbdu_true16';
        "true17'",bmvbdu_true17';
        "true18'",bmvbdu_true18';
        "true19'",bmvbdu_true19';
        "true20'",bmvbdu_true20'; 
        "true21'",bmvbdu_true21';
        "true22'",bmvbdu_true22';
        "true23'",bmvbdu_true23';
        "true24'",bmvbdu_true24';
        "true25'",bmvbdu_true25';
        "true26'",bmvbdu_true26';
        "true27'",bmvbdu_true27';
        "true28'",bmvbdu_true28';
        "true29'",bmvbdu_true29';
        "true30'",bmvbdu_true30'; 
        "true31'",bmvbdu_true31';
        "true32'",bmvbdu_true32';
  ])@
     (List.map (fun (a,b) -> a,(fun remanent -> remanent, b == bmvbdu_true0'', None))      
     [	"true00''",bmvbdu_true0'';
        "true01''",bmvbdu_true1'';
        "true02''",bmvbdu_true2'';
        "true03''",bmvbdu_true3'';
        "true04''",bmvbdu_true4'';
        "true05''",bmvbdu_true5'';
        "true06''",bmvbdu_true6'';
        "true07''",bmvbdu_true7'';
        "true08''",bmvbdu_true8'';
        "true09''",bmvbdu_true9'';
        "true10''",bmvbdu_true10'';
        "true11''",bmvbdu_true11'';
        "true12''",bmvbdu_true12'';
        "true13''",bmvbdu_true13'';
        "true14''",bmvbdu_true14'';
        "true15''",bmvbdu_true15'';
        "true16''",bmvbdu_true16'';
        "true17''",bmvbdu_true17'';
        "true18''",bmvbdu_true18'';
        "true19''",bmvbdu_true19'';
        "true20''",bmvbdu_true20''; 
        "true21''",bmvbdu_true21'';
        "true22''",bmvbdu_true22'';
        "true23''",bmvbdu_true23'';
        "true24''",bmvbdu_true24'';
        "true25''",bmvbdu_true25'';
        "true26''",bmvbdu_true26'';
        "true27''",bmvbdu_true27'';
        "true28''",bmvbdu_true28'';
        "true29''",bmvbdu_true29'';
        "true30''",bmvbdu_true30''; 
        "true31''",bmvbdu_true31'';
        "true32''",bmvbdu_true32'';
  ])@
    (List.map (fun (a,b) -> a,(fun remanent -> remanent, b == bmvbdu_true0''', None))      
     [	"true00'''",bmvbdu_true0''';
        "true01'''",bmvbdu_true1''';
        "true02'''",bmvbdu_true2''';
        "true03'''",bmvbdu_true3''';
        "true04'''",bmvbdu_true4''';
        "true05'''",bmvbdu_true5''';
        "true06'''",bmvbdu_true6''';
        "true07'''",bmvbdu_true7''';
        "true08'''",bmvbdu_true8''';
        "true09'''",bmvbdu_true9''';
        "true10'''",bmvbdu_true10''';
        "true11'''",bmvbdu_true11''';
        "true12'''",bmvbdu_true12''';
        "true13'''",bmvbdu_true13''';
        "true14'''",bmvbdu_true14''';
        "true15'''",bmvbdu_true15''';
        "true16'''",bmvbdu_true16''';
        "true17'''",bmvbdu_true17''';
        "true18'''",bmvbdu_true18''';
        "true19'''",bmvbdu_true19''';
        "true20'''",bmvbdu_true20'''; 
        "true21'''",bmvbdu_true21''';
        "true22'''",bmvbdu_true22''';
        "true23'''",bmvbdu_true23''';
        "true24'''",bmvbdu_true24''';
        "true25'''",bmvbdu_true25''';
        "true26'''",bmvbdu_true26''';
        "true27'''",bmvbdu_true27''';
        "true28'''",bmvbdu_true28''';
        "true29'''",bmvbdu_true29''';
        "true30'''",bmvbdu_true30'''; 
        "true31'''",bmvbdu_true31''';
        "true32'''",bmvbdu_true32''';
  ])@
    (List.map (fun (a,b) -> a,(fun remanent -> remanent, b == bmvbdu_true0'''', None))      
     [	"true00''''",bmvbdu_true0'''';
        "true01''''",bmvbdu_true1'''';
        "true02''''",bmvbdu_true2'''';
        "true03''''",bmvbdu_true3'''';
        "true04''''",bmvbdu_true4'''';
        "true05''''",bmvbdu_true5'''';
        "true06''''",bmvbdu_true6'''';
        "true07''''",bmvbdu_true7'''';
        "true08''''",bmvbdu_true8'''';
        "true09''''",bmvbdu_true9'''';
        "true10''''",bmvbdu_true10'''';
        "true11''''",bmvbdu_true11'''';
        "true12''''",bmvbdu_true12'''';
        "true13''''",bmvbdu_true13'''';
        "true14''''",bmvbdu_true14'''';
        "true15''''",bmvbdu_true15'''';
        "true16''''",bmvbdu_true16'''';
        "true17''''",bmvbdu_true17'''';
        "true18''''",bmvbdu_true18'''';
        "true19''''",bmvbdu_true19'''';
        "true20''''",bmvbdu_true20''''; 
        "true21''''",bmvbdu_true21'''';
        "true22''''",bmvbdu_true22'''';
        "true23''''",bmvbdu_true23'''';
        "true24''''",bmvbdu_true24'''';
        "true25''''",bmvbdu_true25'''';
        "true26''''",bmvbdu_true26'''';
        "true27''''",bmvbdu_true27'''';
        "true28''''",bmvbdu_true28'''';
        "true29''''",bmvbdu_true29'''';
        "true30''''",bmvbdu_true30''''; 
        "true31''''",bmvbdu_true31'''';
        "true32''''",bmvbdu_true32'''';
  ])@ 
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == bmvbdu_false0, None))
       ["false00",bmvbdu_false0;
        "false01",bmvbdu_false1;
        "false02",bmvbdu_false2;
        "false03",bmvbdu_false3;
        "false04",bmvbdu_false4;
        "false05",bmvbdu_false5;
        "false06",bmvbdu_false6;
        "false07",bmvbdu_false7;
        "false08",bmvbdu_false8;
        "false09",bmvbdu_false9;
        "false10",bmvbdu_false10;
        "false11",bmvbdu_false11;
        "false12",bmvbdu_false12;
        "false13",bmvbdu_false13;
        "false14",bmvbdu_false14;
        "false15",bmvbdu_false15;
        "false16",bmvbdu_false16;
        "false17",bmvbdu_false17;
        "false18",bmvbdu_false18;
        "false19",bmvbdu_false19;
        "false20",bmvbdu_false20;
        "false21",bmvbdu_false21;
        "false22",bmvbdu_false22;
        "false23",bmvbdu_false23;
        "false24",bmvbdu_false24;
        "false25",bmvbdu_false25;
        "false26",bmvbdu_false26;
        "false27",bmvbdu_false27;
        "false28",bmvbdu_false28;
        "false29",bmvbdu_false29;
        "false30",bmvbdu_false30;
        "false31",bmvbdu_false31;
        "false32",bmvbdu_false32;])@
     (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == bmvbdu_false0', None))   
	[
	  "false00'",bmvbdu_false0';
        "false01'",bmvbdu_false1';
        "false02'",bmvbdu_false2';
        "false03'",bmvbdu_false3';
        "false04'",bmvbdu_false4';
        "false05'",bmvbdu_false5';
        "false06'",bmvbdu_false6';
        "false07'",bmvbdu_false7';
        "false08'",bmvbdu_false8';
        "false09'",bmvbdu_false9';
        "false10'",bmvbdu_false10';
        "false11'",bmvbdu_false11';
        "false12'",bmvbdu_false12';
        "false13'",bmvbdu_false13';
        "false14'",bmvbdu_false14';
        "false15'",bmvbdu_false15';
        "false16'",bmvbdu_false16';
        "false17'",bmvbdu_false17';
        "false18'",bmvbdu_false18';
        "false19'",bmvbdu_false19';
        "false20'",bmvbdu_false20';
        "false21'",bmvbdu_false21';
        "false22'",bmvbdu_false22';
        "false23'",bmvbdu_false23';
        "false24'",bmvbdu_false24';
        "false25'",bmvbdu_false25';
        "false26'",bmvbdu_false26';
        "false27'",bmvbdu_false27';
        "false28'",bmvbdu_false28';
        "false29'",bmvbdu_false29';
        "false30'",bmvbdu_false30';
        "false31'",bmvbdu_false31';
        "false32'",bmvbdu_false32'
     ])@
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == bmvbdu_false0'', None))   
	[
	  "false00''",bmvbdu_false0'';
        "false01''",bmvbdu_false1'';
        "false02''",bmvbdu_false2'';
        "false03''",bmvbdu_false3'';
        "false04''",bmvbdu_false4'';
        "false05''",bmvbdu_false5'';
        "false06''",bmvbdu_false6'';
        "false07''",bmvbdu_false7'';
        "false08''",bmvbdu_false8'';
        "false09''",bmvbdu_false9'';
        "false10''",bmvbdu_false10'';
        "false11''",bmvbdu_false11'';
        "false12''",bmvbdu_false12'';
        "false13''",bmvbdu_false13'';
        "false14''",bmvbdu_false14'';
        "false15''",bmvbdu_false15'';
        "false16''",bmvbdu_false16'';
        "false17''",bmvbdu_false17'';
        "false18''",bmvbdu_false18'';
        "false19''",bmvbdu_false19'';
        "false20''",bmvbdu_false20'';
        "false21''",bmvbdu_false21'';
        "false22''",bmvbdu_false22'';
        "false23''",bmvbdu_false23'';
        "false24''",bmvbdu_false24'';
        "false25''",bmvbdu_false25'';
        "false26''",bmvbdu_false26'';
        "false27''",bmvbdu_false27'';
        "false28''",bmvbdu_false28'';
        "false29''",bmvbdu_false29'';
        "false30''",bmvbdu_false30'';
        "false31''",bmvbdu_false31'';
        "false32''",bmvbdu_false32''
     ])@
      (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == bmvbdu_false0''', None))   
	[
	  "false00'''",bmvbdu_false0''';
        "false01'''",bmvbdu_false1''';
        "false02'''",bmvbdu_false2''';
        "false03'''",bmvbdu_false3''';
        "false04'''",bmvbdu_false4''';
        "false05'''",bmvbdu_false5''';
        "false06'''",bmvbdu_false6''';
        "false07'''",bmvbdu_false7''';
        "false08'''",bmvbdu_false8''';
        "false09'''",bmvbdu_false9''';
        "false10'''",bmvbdu_false10''';
        "false11'''",bmvbdu_false11''';
        "false12'''",bmvbdu_false12''';
        "false13'''",bmvbdu_false13''';
        "false14'''",bmvbdu_false14''';
        "false15'''",bmvbdu_false15''';
        "false16'''",bmvbdu_false16''';
        "false17'''",bmvbdu_false17''';
        "false18'''",bmvbdu_false18''';
        "false19'''",bmvbdu_false19''';
        "false20'''",bmvbdu_false20''';
        "false21'''",bmvbdu_false21''';
        "false22'''",bmvbdu_false22''';
        "false23'''",bmvbdu_false23''';
        "false24'''",bmvbdu_false24''';
        "false25'''",bmvbdu_false25''';
        "false26'''",bmvbdu_false26''';
        "false27'''",bmvbdu_false27''';
        "false28'''",bmvbdu_false28''';
        "false29'''",bmvbdu_false29''';
        "false30'''",bmvbdu_false30''';
        "false31'''",bmvbdu_false31''';
        "false32'''",bmvbdu_false32'''
     ])@
      (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == bmvbdu_false0'''', None))   
	[
	  "false00''''",bmvbdu_false0'''';
        "false01''''",bmvbdu_false1'''';
        "false02''''",bmvbdu_false2'''';
        "false03''''",bmvbdu_false3'''';
        "false04''''",bmvbdu_false4'''';
        "false05''''",bmvbdu_false5'''';
        "false06''''",bmvbdu_false6'''';
        "false07''''",bmvbdu_false7'''';
        "false08''''",bmvbdu_false8'''';
        "false09''''",bmvbdu_false9'''';
        "false10''''",bmvbdu_false10'''';
        "false11''''",bmvbdu_false11'''';
        "false12''''",bmvbdu_false12'''';
        "false13''''",bmvbdu_false13'''';
        "false14''''",bmvbdu_false14'''';
        "false15''''",bmvbdu_false15'''';
        "false16''''",bmvbdu_false16'''';
        "false17''''",bmvbdu_false17'''';
        "false18''''",bmvbdu_false18'''';
        "false19''''",bmvbdu_false19'''';
        "false20''''",bmvbdu_false20'''';
        "false21''''",bmvbdu_false21'''';
        "false22''''",bmvbdu_false22'''';
        "false23''''",bmvbdu_false23'''';
        "false24''''",bmvbdu_false24'''';
        "false25''''",bmvbdu_false25'''';
        "false26''''",bmvbdu_false26'''';
        "false27''''",bmvbdu_false27'''';
        "false28''''",bmvbdu_false28'''';
        "false29''''",bmvbdu_false29'''';
        "false30''''",bmvbdu_false30'''';
        "false31''''",bmvbdu_false31'''';
        "false32''''",bmvbdu_false32''''
     ])@
    (List.map (fun (a,s) -> a,(fun remanent -> remanent,s,None))
       ["Non initialisation detection (MVBDU)",not b0;
	"Initialisation (MVBDU)",b1;
	"Initialisation detection (MVBDU)",b2;
	"Refuse to reinitialise (MVBDU)",not b3;
	"Non initialisation detection (MVBDU)",not b0';
	"Initialisation (MVBDU)",b1';
	"Initialisation detection (MVBDU)",b2';
	"Refuse to reinitialise (MVBDU)",not b3';
	"Non initialisation detection (MVBDU)",not b0'';
	"Initialisation detection (MVBDU)",b1'';
	"Non initialisation detection (MVBDU)",not b0''';
	"Initialisation detection (MVBDU)",b1''';       
       ])@
    (List.map (fun (a,b,c) -> a, fun remanent -> List_sanity.test remanent c b) 
       [         
         "List.001",list_a,(true,true);
         "List.002",list_b,(false,true);
         "List.003",list_c,(true,true);
         "List.004",list_a',(true,true);
         "List.005",list_b',(false,true);
         "List.006",list_c',(false,true);
       ])@
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == list_a, None))  
       ["List.007",list_a;
        "List.008",list_c;
        "List.009",list_a';
       ])@
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == list__a, None))  
       ["List.010",list__a;
        "List.011",list__c;
        "List.012",list__a';
       ])
    @
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == list___a, None))  
       ["List.013",list___a;
        "List.014",list___c;
        "List.015",list___a';
       ])
    @
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == list____a, None))  
       ["List.016",list____a;
        "List.017",list____c;
        "List.018",list____a';
       ])
  @
    (List.map (fun (a,b) -> a, (fun remanent -> remanent, b == list_____a, None))  
       ["List.019",list_____a;
        "List.020",list_____c;
        "List.021",list_____a';
       ])

