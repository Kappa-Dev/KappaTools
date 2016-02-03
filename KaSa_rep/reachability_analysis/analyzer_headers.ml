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

type global_static_information = compilation_result * Remanent_parameters_sig.parameters

type global_dynamic_information = ()

type event =
  | Check_rule of rule_id

type precondition = unit

type kasa_state = unit

type initial_state = Cckappa_sig.enriched_init

let initialize_global_information parameter error compilation =
  error, (compilation, parameter), ()

let dummy_precondition = (():precondition)

let get_parameter = snd

let get_compilation_information = fst

let get_initial_state _ = []
