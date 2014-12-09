(**
  * get_option.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 18/12/2010
  * Last modification: 19/12/2010
  * * 
  * primitive to parse command-line options 
  *  
  * Copyright 2010 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let options = List.rev []
  
let get_option error = 
  let parameters = Remanent_parameters.get_parameters () in   
  let _ = SuperargTk.parse parameters options FileName.input in 
  error,parameters,!FileName.input 
  
