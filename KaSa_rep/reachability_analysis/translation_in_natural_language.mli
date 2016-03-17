 (**
  * translation_in_natural_language.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016
  * Last modification: 
  * * 
  * Signature for prepreprocessing language ckappa 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

type token

type rename_sites = 
  (Remanent_parameters_sig.parameters ->
   Exception.method_handler -> 
   Ckappa_sig.Site_map_and_set.Map.elt ->
   Exception.method_handler * Ckappa_sig.Site_map_and_set.Map.elt) 
    
val translate: Remanent_parameters_sig.parameters ->
  (*Mvbdu_wrapper.Mvbdu.handler ->*)
  Ckappa_sig.Mvbdu_ckappa_sig.handler ->
  Exception.method_handler ->
  rename_sites -> 
  (*Mvbdu_wrapper.Mvbdu.mvbdu ->*)
  Ckappa_sig.Mvbdu_ckappa_sig.mvbdu ->
  Exception.method_handler * 
    (
      (*Mvbdu_wrapper.Mvbdu.handler*)
      Ckappa_sig.Mvbdu_ckappa_sig.handler * token)

val print: 
  ?beginning_of_sentence:bool ->
  ?prompt_agent_type:bool ->
  ?html_mode:bool ->
  show_dep_with_dimmension_higher_than:int
  -> Remanent_parameters_sig.parameters ->
  Cckappa_sig.kappa_handler ->
  Exception.method_handler ->
  string ->
  Ckappa_sig.c_agent_name ->
  token ->
  Exception.method_handler
	     
