 (**
  * purity.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 01/02/2011
  * Last modification: 01/02/2011
  * * 
  * Check that the input is written in pure Kappa
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Purity") message exn (fun () -> default) 
  
                                                                                                                                                                       (*module Int_Set_and_Map = Set_and_map.Make (struct type t = int let compare = compare end)*) 
let local_trace = true 
  