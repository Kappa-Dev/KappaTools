  (**
    * indexed_objects.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 18/10/2010
    * Last modification: 18/10/2010
    * * 
    * Type definition for indexed objects
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

type 'a indexed = 
  {id:int;
   value:'a}
  