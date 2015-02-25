 (**
  * covering_classes_type.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 23th of Feburary
  * Last modification: 
  * 
  * Type definitions for the covering classes relations between the left hand site of a rule and its sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering_classes_type") message exn (fun () -> default)

let local_trace = false
                    
module Label  = Covering_classes_labels.Int_labels
module Labels = Covering_classes_labels.Extensive(Label)

type covering_class = (Cckappa_sig.agent_name * Cckappa_sig.site_name)
                          
module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif
module SiteMap = Int_storage.Extend (AgentMap)(AgentMap)

type sites_covering_classes  = Labels.label_set Int_storage.Quick_Nearly_inf_Imperatif.t SiteMap.t

type covering_classes =
  {
    covering_classes : sites_covering_classes;
  }
