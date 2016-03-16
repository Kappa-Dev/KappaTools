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
(*module Int_Set_and_Map = SetMap.Make (struct type t = int let compare = compare end)*)

let local_trace = false
 
module Label = Influence_labels.Int_labels 

module Labels = Influence_labels.Extensive(Label)

module IntSetMap = 
  SetMap.Make
    (struct
      type t = int
      let compare = compare
     end)

module Int2SetMap =
  SetMap.Make
    (struct 
      type t = int * int 
      let compare = compare
     end)
    
module StringMap =
  SetMap.Make
    (struct 
      type t = string 
      let compare = compare 
     end)
			       
type agent_quark = Ckappa_sig.c_agent_name

type site_quark = (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * int)

module AgentMap = Int_storage.Quick_Nearly_inf_Imperatif (*TODO: remove later*)

(*module Agent_type_quick_nearly_inf_Imperatif =
  Cckappa_sig.Agent_type_quick_nearly_inf_Imperatif*)

(*module SiteMap = Int_storage.Extend (AgentMap)(Int_storage.Extend (AgentMap)(AgentMap))*)
module SiteMap =
  Int_storage.Extend (Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif)
    (Int_storage.Extend (Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif)(AgentMap))

(*module DeadSiteMap= Int_storage.Extend (AgentMap)(AgentMap)*)

module DeadSiteMap= Int_storage.Extend
  (Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif)(Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif)

(*type agents_quarks = Labels.label_set Int_storage.Quick_Nearly_inf_Imperatif.t AgentMap.t*)

type agents_quarks =
  Labels.label_set
    Int_storage.Quick_Nearly_inf_Imperatif.t
    Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.t  
    
type sites_quarks = Labels.label_set Int_storage.Quick_Nearly_inf_Imperatif.t SiteMap.t 
  
type quarks = 
  {
     dead_agent: Labels.label_set 
     Int_storage.Quick_Nearly_inf_Imperatif.t  
     StringMap.Map.t ;
     dead_sites: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t
       Cckappa_sig.KaSim_Site_map_and_set.Map.t
       Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.t ;
     dead_states: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t 
       DeadSiteMap.t;
     dead_agent_plus: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t  
       StringMap.Map.t ;
     dead_sites_plus: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t
       Cckappa_sig.KaSim_Site_map_and_set.Map.t
       Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.t ;
     dead_states_plus: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t  
       DeadSiteMap.t;
     dead_agent_minus: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t  
       StringMap.Map.t ;
     dead_sites_minus: Labels.label_set
       Int_storage.Quick_Nearly_inf_Imperatif.t
       Cckappa_sig.KaSim_Site_map_and_set.Map.t 
       Ckappa_sig.Agent_type_quick_nearly_inf_Imperatif.t ;
     dead_states_minus: Labels.label_set 
       Int_storage.Quick_Nearly_inf_Imperatif.t
       DeadSiteMap.t;
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

type influence_map = Labels.label_set_couple Int2SetMap.Map.t

type influence_maps = 
  {
    wake_up_map: influence_map;
    influence_map: influence_map
  }
