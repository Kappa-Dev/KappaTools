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

type sites_stochastic_class = int list AgentMap.t

type stochastic_class =
  {
    stochastic_class : sites_stochastic_class
  }

(*Dictionary*)
(*type pair = (int list Int_storage.Nearly_inf_Imperatif.t * int array)

module Stochastic_classes =
  struct
    type t = pair
    let compare = compare
  end

module Dictionary_of_Stochastic_classes = Dictionary.Dictionary_of_Ord (Stochastic_classes)

module Inf_array = Int_storage.Nearly_inf_Imperatif

type dic = (unit, unit) Dictionary_of_Stochastic_classes.dictionary

module Set_list_fst_pair = Set_and_map.Make
                          (struct
                              type t = int
                              let compare = compare
                            end)
                                 
type remanent =
  {
    dic : dic;
    pointer : Set_list_fst_pair.set Inf_array.t
  }
 *)
