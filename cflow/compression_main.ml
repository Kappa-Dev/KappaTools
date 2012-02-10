(**
  * compression_main.ml 
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 19/10/2011
  * Last modification: 19/10/2011
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let debug_mode = true 

let weak_compression env event_list =  
  let refined_event_list = List.map (Kappa_instantiation.Cflow_linker.refine_event (Kappa_instantiation.Cflow_linker.import_env env)) (List.rev event_list) in 
  let _ = 
    if debug_mode 
    then 
      List.iter (Kappa_instantiation.Cflow_linker.print_refined_event stdout (Kappa_instantiation.Cflow_linker.import_env env)) refined_event_list  
  in 
    () 
