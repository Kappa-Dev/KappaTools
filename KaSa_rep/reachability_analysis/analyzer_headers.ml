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

type compilation_result =
  {
    cc_code       : Cckappa_sig.compil;
    kappa_handler : Cckappa_sig.kappa_handler
  }

type rule_id = int

type global_static_information =
  {
    global_compilation_result : compilation_result;
    global_parameter : Remanent_parameters_sig.parameters;
    global_bdu_common_static : Bdu_analysis_type.bdu_common_static 
  }

type global_dynamic_information =
  {
    mvbdu_handler: Mvbdu_wrapper.Mvbdu.handler
  }

type event =
  | Check_rule of rule_id

type precondition = unit

type kasa_state = unit

type initial_state = Cckappa_sig.enriched_init

let initialize_global_information parameter error mvbdu_handler compilation kappa_handler =
  let init_static = Bdu_analysis_main.init_bdu_common_static in
  error,
  {
    global_compilation_result =
      {
	cc_code = compilation;
	kappa_handler = kappa_handler;
      };
    global_parameter     = parameter;
    global_bdu_common_static = init_static
  },
  {
    mvbdu_handler = mvbdu_handler
  }
    
let dummy_precondition = (():precondition)

let get_parameter static = static.global_parameter

let get_compilation_information static = static.global_compilation_result

let get_kappa_handler static = (get_compilation_information static).kappa_handler

let get_cc_code static = (get_compilation_information static).cc_code
							     
let get_bdu_common_static static = static.global_bdu_common_static

let compute_initial_state error static =
  let parameter = get_parameter static in
  let compil = get_cc_code static in
  let error, init =
    (Int_storage.Nearly_inf_Imperatif.fold
       parameter
       error
       (fun parameter error _ i l -> error, i :: l)
       compil.Cckappa_sig.init
       [])
  in
  error, List.rev init

let get_mvbdu_handler dynamic = dynamic.mvbdu_handler
let set_mvbdu_handler handler dynamic = {dynamic with mvbdu_handler = handler}
