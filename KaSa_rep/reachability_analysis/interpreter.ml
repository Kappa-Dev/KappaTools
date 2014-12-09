(**
    * interpreter.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: Nov 12th 2010
    * Last modification: March 8th 2011
    * 
    * Abstract interpreter for reachability analysis
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Interpreter") message exn (fun () -> default) 
  
                                                           
module Reachability_Interpreter = 
  (functor (Domain:Abstract_domain.Reachability_analysis_abstract_domain) -> 
     struct 
       
       let rec reduce_embedding_via_output_channel error engine_state output_channel  embedding bool = 
         match output_channel
         with 
           | [] -> error,bool,embedding
           | _ -> 
             let error,pre_embedding = Domain.build_abstract_embedding_input_channel error engine_state embedding 
             in  
             let error,bool',post_embedding = Domain.enforce_constraint_embedding error pre_embedding output_channel in 
               reduce_embedding_via_output_channel  error engine_state post_embedding.Domain.embedding_output post_embedding.Domain.post_embedding (bool or bool')  
             
       let rec reduce_state_via_output_channel error engine_state output_channel state bool =  
         match output_channel
         with 
           | [] -> error,bool,state
           | _ -> 
             let error,pre_state = Domain.build_abstract_state_input_channel error engine_state state in  
             let error,bool',post_state = Domain.enforce_constraint_state error pre_state output_channel in 
               reduce_state_via_output_channel error engine_state post_state.Domain.state_output post_state.Domain.post_state (bool or bool')  
           
       let rec reduce_embedding_via_input_channel error engine_state embedding bool  =
           let error,preembedding = Domain.build_abstract_embedding_input_channel error engine_state embedding in 
           let error,bool1,embedding = Domain.reduce error preembedding in 
           let error,bool2,embedding = reduce_embedding_via_output_channel error engine_state embedding.Domain.embedding_output embedding.Domain.post_embedding false in 
           if bool1 or bool2 
           then
             reduce_embedding_via_input_channel error engine_state embedding true  
           else 
             error,bool,embedding
              
       let compute_reachable_states parameter error handler engine_state = 
         let error,wake_up_map = Misc_sa.unsome (Remanent_state_signature.wake_up_map error engine_state) (fun error -> warn parameter error (Some "line 53") Exit (fun _ -> [])) in 
         let nrule = handler.Cckappa_sig.nrules in 
         let blackboard = Array.create nrule false  in 
         let error,init = Domain.init error (Communication_channels.dummy_abstract_state_input_channel engine_state)  in 
         let error,_,init = reduce_state_via_output_channel error engine_state init.Domain.state_output init.Domain.post_state false in 
         let working_list = 
             let rec aux k list = 
               if k<0 
               then 
                   list 
               else
                 aux (k-1) (k::list)
             in   
             aux (nrule-1) []
         in 
         let rec aux state working_list buffer = 
           match 
             working_list,buffer 
           with 
           | [],[] -> error,state 
           | [],_ -> aux state (List.rev buffer) []
           | rule::tail,_ ->  
             let _ = blackboard.(rule)<-false in 
             let error,prestate = Domain.build_abstract_state_input_channel error engine_state state in  
             let error,coarse_embedding = Domain.embed_rule error engine_state rule prestate  in 
             let error,_,refined_embedding = reduce_embedding_via_output_channel error engine_state coarse_embedding.Domain.embedding_output coarse_embedding.Domain.post_embedding false in 
             let error,_,refined_refined_embedding = reduce_embedding_via_input_channel error  engine_state refined_embedding false in 
             let error,is_empty_embedding = Domain.is_empty_embedding error refined_refined_embedding in   
               if 
                 is_empty_embedding 
               then (*bottom*)
                 aux state tail buffer  
               else 
                 let error,refined_refined_pre_embedding = Domain.build_abstract_embedding_input_channel error engine_state refined_refined_embedding in  
                 let error,post_embedding = Domain.apply_rule error engine_state rule refined_refined_pre_embedding in   
                 let error,bool,state = Domain.update_state error post_embedding.Domain.post_embedding state in (*merge with previous result*)
                 if bool   
                 then
                   let to_put_in_the_buffer = wake_up_map rule in  
                   let buffer = 
                       List.fold_left 
                         (fun buffer rule -> 
                             if blackboard.(rule) 
                             then buffer
                             else 
                               begin
                                 blackboard.(rule)<-true ;
                                 rule::buffer
                                 end  
                            )  
                         buffer 
                         to_put_in_the_buffer 
                   in 
                   aux state tail buffer 
                 else 
                   aux state tail buffer 
       in 
       let error,output_state = aux init working_list [] in   
       let error,engine_state = Domain.store_result error output_state engine_state in 
       error,engine_state     
           
     end)
