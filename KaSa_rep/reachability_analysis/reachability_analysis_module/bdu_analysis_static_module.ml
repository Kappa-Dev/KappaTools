(**
  * bdu_analysis_main.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 19th of Januaray
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

open Bdu_analysis_module_type

(*******************************************************************************)
(*signature of module*)

module type Bdu_analysis_Static =
sig
  
  type handler = 
    (Boolean_mvbdu.memo_tables,
     Boolean_mvbdu.mvbdu_dic,
     Boolean_mvbdu.association_list_dic,
     Boolean_mvbdu.variables_list_dic,
     bool, int) Memo_sig.handler

  val init_bdu_analysis_static : Exception.method_handler
    -> Exception.method_handler * Bdu_analysis_module_type.bdu_analysis_static

  val site_covering_classes : Remanent_parameters_sig.parameters
    -> Exception.method_handler 
    -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
    -> Exception.method_handler * 
    (int list * int list) Bdu_analysis_module_type.Int2Map_CV.Map.t

  val scan_rule_static : Remanent_parameters_sig.parameters
    -> Exception.method_handler
    -> Cckappa_sig.kappa_handler
    -> int
    -> Cckappa_sig.rule
    -> Covering_classes_type.remanent Covering_classes_type.AgentMap.t
    -> Bdu_analysis_module_type.bdu_analysis_static
    -> Exception.method_handler * Bdu_analysis_module_type.bdu_analysis_static

end

(*******************************************************************************)
(*struture of module*)

module Bdu_analysis_Static =
  (struct
      
    type handler = 
      (Boolean_mvbdu.memo_tables,
       Boolean_mvbdu.mvbdu_dic,
       Boolean_mvbdu.association_list_dic,
       Boolean_mvbdu.variables_list_dic,
       bool, int) Memo_sig.handler

    let init_bdu_analysis_static error =
      let init_covering_classes_id = Int2Map_CV.Map.empty in
      error, 
      {
        store_covering_classes_id = init_covering_classes_id
      }

    let site_covering_classes parameter error covering_classes =
      Bdu_modification_sites_module.site_covering_classes parameter error covering_classes
        
    let scan_rule_static parameter error handler_kappa rule_id rule covering_classes
        store_result=
      let error, store_covering_classes_id = 
        site_covering_classes
          parameter
          error
          covering_classes
      in
      error, 
      {
        store_covering_classes_id = store_covering_classes_id
      }

   end:Bdu_analysis_Static)
