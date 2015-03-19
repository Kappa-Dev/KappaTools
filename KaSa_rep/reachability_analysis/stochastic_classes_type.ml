 (**
  * stochastic_classes_type.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of March
  * Last modification: 
  * 
  * Type definitions for the covering classes relations between sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic_classes_type") message exn (fun () -> default)

let local_trace = false

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif

module Stochastic_classes = (*REMOVE*)
  struct
    type t = Union_find.union_find
    let compare = compare
  end

module Dictionary_of_Stochastic_classes =
  Dictionary.Dictionary_of_Ord (Stochastic_classes)
                               
type remanent_dic = (unit, unit) Dictionary_of_Stochastic_classes.dictionary
type remanent = {dic : remanent_dic}

type sites_stochastic_classes = int list AgentMap.t
(*type sites_stochastic_classes = remanent AgentMap.t*)(*FIXME*)

type stochastic_classes =
  {
    stochastic_classes : sites_stochastic_classes
  }
