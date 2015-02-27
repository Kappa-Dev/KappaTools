 (**
  * quark.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2011, the 7th of March
  * Last modification: 2014, the 5th of October
  * 
  * Type definitions for the influence relations between rules and sites. 
  *  
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Quark_type") message exn (fun () -> default) 
(*module Int_Set_and_Map = Set_and_map.Make (struct type t = int let compare = compare end)*)

let local_trace = false
 
module Label = Influence_labels.Int_labels 
module Labels = Influence_labels.Extensive(Label)
module Int2Set_and_map = Set_and_map.Make (struct type t = int*int let compare = compare end)
  
type agent_quark = Cckappa_sig.agent_name
type site_quark = (Cckappa_sig.agent_name*Cckappa_sig.site_name*int)

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif 
module SiteMap = Int_storage.Extend (AgentMap)(Int_storage.Extend (AgentMap)(AgentMap))
  
type agents_quarks = Labels.label_set Int_storage.Quick_Nearly_inf_Imperatif.t AgentMap.t  
type sites_quarks = Labels.label_set Int_storage.Quick_Nearly_inf_Imperatif.t SiteMap.t 
  
type quarks = 
  {
    agent_modif_plus:  agents_quarks ;
    agent_modif_minus: agents_quarks ; 
    agent_test: agents_quarks ;
    agent_var_minus: agents_quarks ;
    site_modif_minus: sites_quarks ;
    site_test: sites_quarks ;  
    site_var_minus: sites_quarks ; 
    site_modif_plus: sites_quarks ;
    agent_var_plus : agents_quarks ;
    site_var_plus : sites_quarks ; 
  }

type influence_map = Labels.label_set_couple Int2Set_and_map.map
type influence_maps = 
  {
    wake_up_map: influence_map;
    influence_map: influence_map
  }
