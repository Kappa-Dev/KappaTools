(**
    * communication_channels.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 12/10/2010
    * Last modification: 12/10/2010
    * * 
    * Communication between abstract domains
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

type abstract_state_input_channel = 
    {state_channel_engine_state: Remanent_state_signature.engine_state}
and abstract_state_output_element = Nil 
and abstract_state_output_channel = abstract_state_output_element list 
and abstract_rule_application_input_channel = 
    {embedding_channel_engine_state: Remanent_state_signature.engine_state}
  
and abstract_rule_application_output_element  = Nil
and abstract_rule_application_output_channel = abstract_rule_application_output_element list 
  
let dummy_abstract_state_input_channel engine_state = 
  {
      state_channel_engine_state = engine_state
  }
  
let dummy_rule_application_input_channel engine_state = 
   {  
     embedding_channel_engine_state = engine_state 
   }