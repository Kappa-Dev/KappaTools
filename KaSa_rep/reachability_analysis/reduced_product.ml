(**
    * reduced_product.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 12/10/2010
    * Last modification: 14/10/2010
    * * 
    * Reduced product for reachability analysis
    *  
    * Abstract domains used communication channel to implement the reduced product as suggested in 
    *    Patrick Cousot, Radhia Cousot, Jérôme Feret, Laurent Mauborgne, Antoine Miné, David Monniaux,& Xavier Rival.
    *    Combination of Abstractions in the ASTRÉE Static Analyzer.
    *    In 11th Annual Asian Computing Science Conference (ASIAN'06), National Center of Sciences, Tokyo, Japan, December 6—8, 2006. 
    *    LNCS 4435, © Springer-Verlag, Berlin, pp. 272—300. 
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

module Product = 
    (functor (New_Domain:Abstract_domain.Reachability_analysis_abstract_domain) ->
    (functor (Underlying_Domain:Abstract_domain.Reachability_analysis_abstract_domain) -> 
(struct
   type abstract_state = 
     {
       underlying_state:Underlying_Domain.abstract_state;
        new_domain_state:New_Domain.abstract_state
     }
       
   type abstract_embedding = 
     {
       underlying_embedding:Underlying_Domain.abstract_embedding;
        new_domain_embedding:New_Domain.abstract_embedding
     }
    
   type pre_abstract_state = 
      {
        pre_state:abstract_state;
        state_input: Communication_channels.abstract_state_input_channel;
       }
     
   type post_abstract_state = 
      {
        post_state:abstract_state;
        state_output:Communication_channels.abstract_state_output_channel
      } 
      
   type pre_abstract_embedding = 
      {
        pre_embedding:abstract_embedding;
        embedding_input:Communication_channels.abstract_rule_application_input_channel
      } 
     
   type post_abstract_embedding = 
      {
        post_embedding:abstract_embedding;
        embedding_output:Communication_channels.abstract_rule_application_output_channel
      } 
     
   let generic_print f_underlying_domain f_new_domain error log state = 
       let error = f_underlying_domain error log state.underlying_state in 
       f_new_domain error log state.new_domain_state 
   
   let print_xml = generic_print Underlying_Domain.print_xml New_Domain.print_xml
     
   let print_html_desktop =    generic_print Underlying_Domain.print_html_desktop New_Domain.print_html_desktop
     
   let print =    generic_print Underlying_Domain.print New_Domain.print
     
   let store_result error state engine_state = 
     let error,engine_state = Underlying_Domain.store_result error state.underlying_state engine_state in 
     New_Domain.store_result error state.new_domain_state engine_state 
       
   let enforce_constraint_embedding error a l = error,a (*TO DO*)
   let enforce_constraint_state error a l = error,a     (*TO DO*)

   let downgrade_pre_state_underlying state = 
       {Underlying_Domain.pre_state = state.pre_state.underlying_state;
        Underlying_Domain.state_input = state.state_input}
     
   let downgrade_pre_state_new_domain state = 
       {New_Domain.pre_state = state.pre_state.new_domain_state;
        New_Domain.state_input = state.state_input}
   
   let merge_post_state underlying new_domain = 
     {post_state = {underlying_state = underlying.Underlying_Domain.post_state; new_domain_state = new_domain.New_Domain.post_state};
       state_output = 
          List.fold_left
             (fun out_channel elt -> elt::out_channel)
             underlying.Underlying_Domain.state_output 
             new_domain.New_Domain.state_output}
     
   let downgrade_pre_embedding_underlying embedding = 
       {Underlying_Domain.pre_embedding = embedding.pre_embedding.underlying_embedding;
        Underlying_Domain.embedding_input = embedding.embedding_input}
     
   let downgrade_pre_embedding_new_domain embedding = 
       {New_Domain.pre_embedding = embedding.pre_embedding.new_domain_embedding;
        New_Domain.embedding_input = embedding.embedding_input}
   
   let merge_post_embedding  underlying new_domain = 
     {
       post_embedding = 
         {
            underlying_embedding = underlying.Underlying_Domain.post_embedding; 
            new_domain_embedding = new_domain.New_Domain.post_embedding};
       embedding_output = 
           List.fold_left
             (fun out_channel elt -> elt::out_channel)
             underlying.Underlying_Domain.embedding_output 
             new_domain.New_Domain.embedding_output
      }
     
    let init error a  = 
      let error,underlying = Underlying_Domain.init error a in 
      let error,newdomain = New_Domain.init error a in 
       error,
       merge_post_state underlying newdomain 
     
   let embed_rule error engine_state rule_key state =
      let error,underlying = 
           Underlying_Domain.embed_rule 
              error 
              engine_state
              rule_key 
              (downgrade_pre_state_underlying state) 
      in 
      let error,new_domain = 
           New_Domain.embed_rule 
              error 
              engine_state
              rule_key 
              (downgrade_pre_state_new_domain state) in 
       error,merge_post_embedding underlying new_domain  
     
   let empty_embedding =
       {
         underlying_embedding = Underlying_Domain.empty_embedding ; 
         new_domain_embedding = New_Domain.empty_embedding
       }     
     
   let reduce error a = 
    let error,bool_underlying,underlying = Underlying_Domain.reduce error (downgrade_pre_embedding_underlying a) in 
    let error,bot_underlying= Underlying_Domain.is_empty_embedding error underlying.Underlying_Domain.post_embedding in 
    if bot_underlying 
    then 
      error,
      false,
      {
          post_embedding = empty_embedding; 
          embedding_output = []
        }
    else 
      let error,bool_newdomain,new_domain = New_Domain.reduce error (downgrade_pre_embedding_new_domain a) in 
      let error,bot_newdomain = New_Domain.is_empty_embedding error new_domain.New_Domain.post_embedding in 
      if bot_newdomain 
      then 
        error,
        false,
          {
            post_embedding = empty_embedding; 
            embedding_output = []
          }
      else  
        error,
        bool_underlying or bool_newdomain,
        merge_post_embedding underlying new_domain 
        
  let apply_rule error engine_state rule_key a = 
    let error,underlying = Underlying_Domain.apply_rule error engine_state rule_key (downgrade_pre_embedding_underlying a) in 
    let error,new_domain = New_Domain.apply_rule error engine_state rule_key (downgrade_pre_embedding_new_domain a) in 
      error,
      merge_post_embedding underlying new_domain 
    
  let update_state error embedding state = 
    let error,bool1,underlying = Underlying_Domain.update_state error embedding.underlying_embedding state.underlying_state in 
    let error,bool2,new_domain = New_Domain.update_state error embedding.new_domain_embedding state.new_domain_state in 
    error,
    bool1 or bool2,
    {
      underlying_state = underlying;
      new_domain_state = new_domain
    }
   
    
  let is_empty_embedding error embedding = 
      let error,bool1 = Underlying_Domain.is_empty_embedding error embedding.underlying_embedding in 
      if bool1 
      then error,true
      else 
        New_Domain.is_empty_embedding error embedding.new_domain_embedding  
        
        
  let build_abstract_state_input_channel error engine_state a = 
    let error,underlying = Underlying_Domain.build_abstract_state_input_channel error engine_state a.underlying_state in 
    let error,new_domain = 
        New_Domain.enrich_abstract_state_input_channel error 
          {
              New_Domain.pre_state = a.new_domain_state;
              New_Domain.state_input = underlying.Underlying_Domain.state_input} 
    in 
    error,
    {
      pre_state = 
          {
            underlying_state = underlying.Underlying_Domain.pre_state;
            new_domain_state = new_domain.New_Domain.pre_state
          };
      state_input = new_domain.New_Domain.state_input;
    }
    
  let enrich_abstract_state_input_channel error a = 
    let error,underlying = Underlying_Domain.enrich_abstract_state_input_channel error (downgrade_pre_state_underlying a) in 
    let error,new_domain = 
        New_Domain.enrich_abstract_state_input_channel error 
          {
              New_Domain.pre_state = a.pre_state.new_domain_state;
              New_Domain.state_input = underlying.Underlying_Domain.state_input} 
    in 
    error,
    {
      pre_state = 
          {
            underlying_state = underlying.Underlying_Domain.pre_state;
            new_domain_state = new_domain.New_Domain.pre_state
          };
      state_input = new_domain.New_Domain.state_input;
    }
    
   let build_abstract_embedding_input_channel error engine_state a = 
    let error,underlying = Underlying_Domain.build_abstract_embedding_input_channel error engine_state a.underlying_embedding in 
    let error,new_domain = 
        New_Domain.enrich_abstract_embedding_input_channel error 
          {
              New_Domain.pre_embedding = a.new_domain_embedding;
              New_Domain.embedding_input = underlying.Underlying_Domain.embedding_input
          } 
    in 
    error,
    {
      pre_embedding = 
          {
            underlying_embedding = underlying.Underlying_Domain.pre_embedding;
            new_domain_embedding = new_domain.New_Domain.pre_embedding
          };
      embedding_input = new_domain.New_Domain.embedding_input;
    }
    
  let enrich_abstract_embedding_input_channel error a = 
    let error,underlying = Underlying_Domain.enrich_abstract_embedding_input_channel error (downgrade_pre_embedding_underlying a) in 
    let error,new_domain = 
        New_Domain.enrich_abstract_embedding_input_channel error 
          {
              New_Domain.pre_embedding = a.pre_embedding.new_domain_embedding;
              New_Domain.embedding_input = underlying.Underlying_Domain.embedding_input} 
    in 
    error,
    {
      pre_embedding = 
          {
            underlying_embedding = underlying.Underlying_Domain.pre_embedding;
            new_domain_embedding = new_domain.New_Domain.pre_embedding
          };
      embedding_input = new_domain.New_Domain.embedding_input;
    }
    
     
  let enforce_constraint_state error state out_channel = 
    let error,bool_underlying,underlying = Underlying_Domain.enforce_constraint_state  error (downgrade_pre_state_underlying state) out_channel in 
    let error,bool_new_domain,new_domain = New_Domain.enforce_constraint_state error (downgrade_pre_state_new_domain state) out_channel in 
      error,
      bool_underlying or bool_new_domain,
      merge_post_state underlying new_domain    
    
  let enforce_constraint_embedding error embedding out_channel = 
    let error,bool_underlying,underlying = Underlying_Domain.enforce_constraint_embedding  error (downgrade_pre_embedding_underlying embedding) out_channel in 
    let error,bool_new_domain,new_domain = New_Domain.enforce_constraint_embedding error (downgrade_pre_embedding_new_domain embedding) out_channel in 
      error,
      bool_underlying or bool_new_domain,
      merge_post_embedding underlying new_domain  
    
  let store_result error state engine_state = 
    let error,engine_state = Underlying_Domain.store_result error state.underlying_state engine_state in 
     New_Domain.store_result error state.new_domain_state engine_state 
  
end:Abstract_domain.Reachability_analysis_abstract_domain)))