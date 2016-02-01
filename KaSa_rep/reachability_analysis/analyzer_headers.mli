(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 30th of January
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche 
  * en Informatique et en Automatique.  
  * All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

type compilation_result = unit

type rule_id = int

type global_static_information =
  compilation_result * Remanent_parameters_sig.parameters

type global_dynamic_information = ()

type event =
  | Check_rule of rule_id

type precondition = unit

type kasa_state = unit

type initial_state = unit

val initialize_global_information:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  compilation_result ->
  Exception.method_handler * global_static_information * global_dynamic_information
    
val dummy_precondition: precondition					      
 
val get_parameter: global_static_information -> Remanent_parameters_sig.parameters

val get_compilation_information: global_static_information -> compilation_result

val get_initial_state: global_static_information -> initial_state list
