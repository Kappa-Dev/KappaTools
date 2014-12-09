(**
    * abstract_domain.ml
    * 
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 12/10/2010
    * Last modification: 14/10/2010
    * * 
    * Signature for the abstract domains
    *  
    * Abstract domains used communication channel to implement the reduced product as suggested in 
    *    Patrick Cousot, Radhia Cousot, Jérôme Feret, Laurent Mauborgne, Antoine Miné, David Monniaux,& Xavier Rival.
    *    Combination of Abstractions in the ASTRÉE Static Analyzer.
    *    In 11th Annual Asian Computing Science Conference (ASIAN'06), National Center of Sciences, Tokyo, Japan, December 6—8, 2006. 
    *    LNCS 4435, © Springer-Verlag, Berlin, pp. 272—300. 
    *
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

module type Reachability_analysis_abstract_domain =  
  sig
    type abstract_state
    type abstract_embedding 
    
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
        embedding_input: Communication_channels.abstract_rule_application_input_channel;
        }
      
    type post_abstract_embedding = 
      {
        post_embedding:abstract_embedding;
        embedding_output:Communication_channels.abstract_rule_application_output_channel
      } 
      
    val init: Exception.method_handler -> Communication_channels.abstract_state_input_channel -> Exception.method_handler * post_abstract_state
    val embed_rule: Exception.method_handler -> Remanent_state_signature.engine_state -> Remanent_state_signature.rule_key -> pre_abstract_state -> Exception.method_handler *  post_abstract_embedding 
    val apply_rule: Exception.method_handler -> Remanent_state_signature.engine_state -> Remanent_state_signature.rule_key -> pre_abstract_embedding -> Exception.method_handler * post_abstract_embedding 
    val reduce: Exception.method_handler -> pre_abstract_embedding -> Exception.method_handler * bool * post_abstract_embedding  
    val update_state: Exception.method_handler -> abstract_embedding -> abstract_state -> Exception.method_handler * bool * abstract_state
  
    val is_empty_embedding: Exception.method_handler -> abstract_embedding -> Exception.method_handler * bool 
    val empty_embedding: abstract_embedding 
      
    val build_abstract_state_input_channel:  Exception.method_handler -> Remanent_state_signature.engine_state -> abstract_state -> Exception.method_handler * pre_abstract_state  
    val enrich_abstract_state_input_channel: Exception.method_handler -> pre_abstract_state -> Exception.method_handler * pre_abstract_state 
    val build_abstract_embedding_input_channel:  Exception.method_handler -> Remanent_state_signature.engine_state -> abstract_embedding -> Exception.method_handler * pre_abstract_embedding 
    val enrich_abstract_embedding_input_channel: Exception.method_handler -> pre_abstract_embedding -> Exception.method_handler * pre_abstract_embedding 
    
    val enforce_constraint_state:     Exception.method_handler -> pre_abstract_state     -> Communication_channels.abstract_state_output_channel -> Exception.method_handler * bool * post_abstract_state
    val enforce_constraint_embedding: Exception.method_handler -> pre_abstract_embedding -> Communication_channels.abstract_rule_application_output_channel -> Exception.method_handler * bool * post_abstract_embedding
    
    val store_result: Exception.method_handler -> abstract_state -> Remanent_state_signature.engine_state -> Exception.method_handler * Remanent_state_signature.engine_state   
    val print: Exception.method_handler -> out_channel -> abstract_state -> Exception.method_handler  
    val print_xml: Exception.method_handler -> out_channel -> abstract_state -> Exception.method_handler 
    val print_html_desktop: Exception.method_handler -> out_channel -> abstract_state -> Exception.method_handler 
end

module Empty_abstract_domain = 
(struct
  type abstract_state = unit
  type abstract_embedding = unit
    
  type pre_abstract_state = 
    {
        pre_state:abstract_state;
        state_input:Communication_channels.abstract_state_input_channel;
    }
  
  type post_abstract_state = 
    {
        post_state:abstract_state;
        state_output:Communication_channels.abstract_state_output_channel
    } 
  
  type pre_abstract_embedding = 
    {
        pre_embedding:abstract_embedding;
        embedding_input:Communication_channels.abstract_rule_application_input_channel;
    }
    
  type post_abstract_embedding = 
    {
        post_embedding:abstract_embedding;
        embedding_output:Communication_channels.abstract_rule_application_output_channel;
    }
      
  let init error _  = 
      error,
      {
          post_state=();
          state_output=[];
      }
    
  let embed_rule error _ _ _ = 
      error,
      {
          post_embedding=();
          embedding_output=[];
      }
  
  let apply_rule = embed_rule     
  
  let reduce error a = 
    error,
    false, 
    {
      post_embedding=a.pre_embedding;
      embedding_output=[];
      }
    
  let empty_embedding = ()
    
  let is_empty_embedding error a = error,false 
    
  let update_state error embdding state = error,false,state
  
  let build_abstract_state_input_channel error engine_state a = 
    error,
    {
      pre_state = a;
      state_input = Communication_channels.dummy_abstract_state_input_channel engine_state ;
        }
    
  let enrich_abstract_state_input_channel error a = error,a 
  
  let build_abstract_embedding_input_channel error engine_state a = 
     error,
      {
        pre_embedding = a;
        embedding_input = Communication_channels.dummy_rule_application_input_channel engine_state;
         }
    
  let enrich_abstract_embedding_input_channel error a = error,a 
    
  let enforce_constraint_state error state _  = 
      error,
      false,
      {
        post_state = state.pre_state;
        state_output = []
      }
    
    
  let enforce_constraint_embedding error embedding _ = 
      error,
      false,  
      {
        post_embedding = embedding.pre_embedding ;
        embedding_output = []
      }
    
  let store_result error _ a = error,a 
  
  let print error _ _ = error 
  let print_xml error _ _ = error 
  let print_html_desktop error _ _ = error 
    
  end:Reachability_analysis_abstract_domain)