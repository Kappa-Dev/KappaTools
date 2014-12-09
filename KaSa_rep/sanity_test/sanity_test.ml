(**
    * sanity_test.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 03/28/2010
    * Last modification: 11/15/2010
    * * 
    * This library provides a bench of run time tests.
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

  

let test remanent p s1 = 
  let remanent,bool,report = p remanent in 
  let _ = 
      match report with 
        | Some s2 -> Printf.fprintf remanent.Sanity_test_sig.output "%s: %s\n" s1 s2 
        | None -> if bool then Printf.fprintf remanent.Sanity_test_sig.output "%s: %s\n" s1 (if bool then "ok" else "FAIL!")
  in remanent 

  
let remanent parameters = 
  Sanity_test_sig.initial_remanent 
    (Boolean_mvbdu.init_remanent parameters) 
      (fun x -> 
        match x with 
          | true -> 
              (fun error b c d e old_handler -> 
                let old_dictionary = old_handler.Memo_sig.mvbdu_dictionary in 
                let error,output = Boolean_mvbdu.D_mvbdu_skeleton.allocate_uniquely parameters error b c d e old_dictionary in  
                  match output with 
                      | None -> error,None 
                      | Some (i,a,b,new_dic) -> error,Some(i,a,b,Mvbdu_core.update_dictionary old_handler new_dic))
          | false -> 
              (fun error b c d e old_handler -> 
                let old_dictionary = old_handler.Memo_sig.mvbdu_dictionary in 
                let error,output = Boolean_mvbdu.D_mvbdu_skeleton.allocate parameters error b c d e old_dictionary in  
                  match output with 
                      | None -> error,None 
                      | Some (i,a,b,new_dic) -> error,Some(i,a,b,Mvbdu_core.update_dictionary old_handler new_dic)))
          (fun x -> 
        match x with 
          | true -> 
              (fun error b c (d:int List_sig.cell) (e:int -> int List_sig.list) old_handler -> 
                let old_dictionary = old_handler.Memo_sig.list_dictionary in 
                let error,output = Boolean_mvbdu.D_list_skeleton.allocate_uniquely parameters error b c d e old_dictionary in  
                  match output with 
                      | None -> error,None 
                      | Some (i,a,b,new_dic) -> error,Some(i,a,b,List_core.update_dictionary old_handler new_dic))
          | false -> 
              (fun error b c d e old_handler -> 
                let old_dictionary = old_handler.Memo_sig.list_dictionary in 
                let error,output = Boolean_mvbdu.D_list_skeleton.allocate parameters error b c d e old_dictionary in  
                  match output with 
                      | None -> error,None 
                      | Some (i,a,b,new_dic) -> error,Some(i,a,b,List_core.update_dictionary old_handler new_dic)))           
 
let main () =
	let error = Exception.empty_error_handler in    
    let error,parameters,files  = Get_option.get_option error in 
    let _ = Counting_test.test_counting_procedure parameters in  
    let remanent,bdu_test_list = Mvbdu_test.bdu_test (remanent parameters) parameters in 
    let _ = 
      List.fold_left 
        (fun remanent (s,p) -> test remanent p s)
        remanent 
        bdu_test_list in 
      ()

 let _ = main () 