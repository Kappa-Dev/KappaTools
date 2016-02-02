(**
  * analyzer_sig.mli
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

(* Before properly achieving separation of concepts.
We introduce one monolithic domain that collect everything
(as in the previous analyzer).*)


module Domain =
  sig
    type static_information =
         {
	   global_static_information: Analyzer_headers.global_static_information;	   
	   domain_static_information: unit (* put here the type of the struct that contains all static information as in the previous version of the analysis *)
	 }

    type dynamic_information =
      {
	global_dynamic_information: Analyzer_headers.global_dynamic_information;
	mvbdu_handler: unit; (* put here the type *)
	domain_dynamic_information: unit (* put here the type of the struct that contains the rest of the dynamic information, including the result of the analysis *)
      }

    let get_kappa_handler static = unit (* explain how to extract the handler for kappa expressions from a value of type static_information *)
    let get_mvbdu_handler dynamic = unit (* explain how to extract the handler for mvbdu *)
    let set_mvbdu_handler handler dynamic = dynamic (* explain how to overwritte the previous handler *)

    let initialize static dynamic error =
      error,
      {
	global_static_information=static;
	domain_static_information=()
      }
   							       
    type 'a zeroary =
      static_information
      -> dynamic_information
      -> Exception.method_handler
      -> Exception.method_handler * dynamic_information * 'a

    type ('a, 'b) unary =
      static_information
      -> dynamic_information 
      -> Exception.method_handler
      -> 'a 
      -> Exception.method_handler * dynamic_information * 'b

    type ('a, 'b, 'c) binary =
      static_information
      -> dynamic_information 
      -> Exception.method_handler 
      -> 'a 
      -> 'b 
      -> Exception.method_handler * dynamic_information * 'c

    let add_initial_state static dynamic error state =
      error,dynamic,[]

    let is_enabled static dynamic error r_id =
      error,dynamic,None

    let apply_rule static dynamic error r_id precondition =
      error,dynamic,[]
		      
    let apply_event_list static dynamic error event_list =
      error,dynamic,[]

    let export static dynamic error kasa_state =
      error,dynamic,kasa_state
		      
    let print static dynamic error loggers =
      error,dynamic,()

  end
    
