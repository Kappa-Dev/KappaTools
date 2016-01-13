(**
  * bdu_analysis_type.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 15th of July
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Int_storage
open Fifo
open Cckappa_sig

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_analysis_type") message exn (fun () -> default)

let local_trace = false

module AgentMap = Quick_Nearly_inf_Imperatif

(************************************************************************************)
(*static information*)

(*half break side effect*)

module Int2Map_HalfBreak_effect =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

module Int2Map_Remove_effect =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

(*potential partner side effects*)
module Int2Map_potential_effect =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

(************************************************************************************)
(*views that are tested and modified with agent_id*)

module Int2Map_Modif =
  SetMap.Make (
    struct
      (*agent_id, agent_type, site*)
      type t = int * int * int
      let compare = compare
    end)

(*views that are tested and modified without agent_id*)

module Int2Map_Test_Modif =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

module Project2_modif =
  SetMap.Proj (Int2Map_Modif)(Int2Map_Test_Modif)

(************************************************************************************)
(*dynamic information*)

(*dynamic contact map*)

module Int2Map_CV =
  SetMap.Make (
    struct
      (*agent_type, site*)
      type t = int * int
      let compare = compare
    end)

(*TODO:new function having information of type string*)
(*module Int2Map_CV_map =
  SetMap.Make (
    struct
      (*agent_type, agent_name_string, site*)
      type t = int * string * int * Ckappa_sig.site
      let compare = compare
    end)*)

(*full contact map*)

module Int2Map_CM_state = 
  SetMap.Make (
    struct
      (*agent_type, site, state*)
      type t = int * int * int
      let compare = compare
    end
  )

(*syntactic contact map *)
(*module Map_second_agent_bind =
  SetMap.Make (
    struct
      type t = int * int
      let compare = compare
    end )*)

module Int2Map_pair =
  SetMap.Make (
    struct
      type t = int * int
      let compare = compare
    end )

(*module Int2Map_syn =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end )*)

module Set_triple =
  SetMap.Make (
    struct
      type t = int * int * int
      let compare = compare
    end )

module Set_pair =
  SetMap.Make (
    struct
      type t = (int * int * int) * (int * int * int)
      let compare = compare
    end )

(*list of rules to awake when the state of a site is modified and tested*)

module Int2Map_CV_Modif = 
  SetMap.Make (
    struct
      (*agent_type,site_type, covering_class_id*)
      type t = int * int
      let compare = compare
    end)

(*todo*)
module Int2Map_CM_Syntactic =
  SetMap.Make 
    (struct 
        type t = Set_triple.Set.t
        let compare = compare
      end)

(************************************************************************************)
(*type bdu_build_map:
  local information of views that are tested, created and modified with new indexes*)

(*creation*)
module Map_creation_bdu =
  SetMap.Make (
    struct
      type t = int * int * int
      let compare = compare
    end)

(*with projection*)
module Map_final_creation_bdu =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

module Map_agent_type_creation_bdu =
  SetMap.Make (
    struct
      type t = int * int (*agent_type, cv_id*)
      let compare = compare
    end)

module Project2bdu_creation =
  SetMap.Proj2 (Map_creation_bdu)(Map_final_creation_bdu)(Map_agent_type_creation_bdu)

(************************************************************************************)
(*test*)

module Map_test_bdu =
  SetMap.Make (
    struct
      type t = int * int * int * int
      let compare = compare
    end)

(*projection*)
module Map_final_test_bdu =
  SetMap.Make (
    struct
      type t = int (*rule_id*)
      let compare = compare
    end)

module Map_agent_id_test_bdu =
  SetMap.Make (
    struct
      type t = int (*agent_id*)
      let compare = compare
    end)

module Project2bdu_test =
  SetMap.Proj2 (Map_test_bdu)(Map_final_test_bdu)(Map_agent_id_test_bdu)

(************************************************************************************)
(*TODO:projection for views used in is_enable*)

module Map_rule_id_views =
  SetMap.Make
    (struct
      type t = int
      let compare = compare
     end)
    
module Map_triple_views =
  SetMap.Make
    (struct 
      type t = int * int * int
      let compare = compare
     end)
    
module Project2_bdu_views =
  SetMap.Proj2 (Map_test_bdu) (Map_rule_id_views) (Map_triple_views)

(************************************************************************************)
(*modification*)

module Map_modif_list =
  SetMap.Make (
    struct
      type t = int * int * int * int
      let compare = compare
    end)

module Map_final_modif_list =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

module Map_agent_id_modif_list =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

module Project2bdu_modif =
  SetMap.Proj2 (Map_modif_list)(Map_final_modif_list)(Map_agent_id_modif_list)

(************************************************************************************)
(*potential side effect*)

module Map_potential_bdu =
  SetMap.Make (
    struct
      type t = int * int * int
      let compare = compare
    end)

(*with projection*)
module Map_final_potential_bdu =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

module Map_agent_type_potential_bdu =
  SetMap.Make (
    struct
      type t = int * int (*agent_type, cv_id*)
      let compare = compare
    end)

module Project2bdu_potential =
  SetMap.Proj2 (Map_potential_bdu)(Map_final_potential_bdu)(Map_agent_type_potential_bdu)

(************************************************************************************)
(*list of potential side effect (site free)*)

module Map_potential_list =
  SetMap.Make (
    struct
      type t = int * int * int
      let compare = compare
    end)

(*with projection*)
module Map_final_potential_list =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

module Map_agent_type_potential_list =
  SetMap.Make (
    struct
      type t = int 
      let compare = compare
    end)

module Project2bdu_potential_list =
  SetMap.Proj2 (Map_potential_list)(Map_final_potential_list)(Map_agent_type_potential_list)


(************************************************************************************)
(*fixpoint iteration*)

module Map_bdu_update =
  SetMap.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*build a pair of site_address map*)
(*module Map_site_address =
  SetMap.Make (
    struct
      type t = site_address * site_address
      let compare = compare
    end)

module Map_test_bond =
  SetMap.Make (
    struct
      type t = int (*rule_id*)
      let compare = compare
    end)*)

(************************************************************************************)
(*static information*)

type half_break_action = 
  (int list * (int * int) list) Int2Map_HalfBreak_effect.Map.t

(*do not consider the case where site has state free.*)
type remove_action =
  (int list * int list) Int2Map_Remove_effect.Map.t

(*potential side effects*)
type free_partner = (int * int) list Int2Map_potential_effect.Map.t
type bind_partner = (int * int) list Int2Map_potential_effect.Map.t

type potential_partner_free = free_partner
type potential_partner_bind = bind_partner

type bdu_analysis_static =
  {
    store_covering_classes_id : (int list * int list) Int2Map_CV.Map.t;
    store_side_effects        : half_break_action * remove_action;
    store_potential_side_effects : potential_partner_free *  potential_partner_bind;
    (* views that are tested and modificated with agent_id*)
    store_modification_sites  :
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    store_test_sites :
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    store_test_modification_sites :
      (int list * Site_map_and_set.Set.t) Int2Map_Modif.Map.t;
    (*views that are tested and modificated without agent_id, will be used in
      update function*)
    store_modif_map      : (int list * Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t;
    store_test_map       : (int list * Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t;
    store_test_modif_map : (int list * Site_map_and_set.Set.t) Int2Map_Test_Modif.Map.t;
  }

(************************************************************************************)
(*dynamic information*)

type wl_int = IntWL.WSetMap.elt list * IntWL.WSetMap.elt list * IntWL.WSetMap.Set.t

type bdu_analysis_dynamic =
  {
    store_contact_map_full     : Set_triple.Set.t Int2Map_CM_state.Map.t;
    (*syntactic contact map included initial bonds*)
    store_syn_contact_map_full : Set_triple.Set.t Int2Map_CM_Syntactic.Map.t;
    store_covering_classes_modification_update : (*update(c)*)
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t;
    (*update(c) with side effect information*)
    store_covering_classes_modification_side_effects :
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t;
    (*final update function*)
    store_covering_classes_modification_update_full :
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t;
  }

(************************************************************************************)
(*build covering classes with new indexes*)

type bdu_build =
  {
    store_remanent_triple: ((int * int list * Site_map_and_set.Set.t) list) AgentMap.t;
    store_wl_creation: wl_int;
    store_bdu_test_restriction_map:  Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t;
    store_proj_bdu_test_restriction_map: 
      Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_id_test_bdu.Map.t Map_final_test_bdu.Map.t;
    store_bdu_creation_restriction_map: Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t;
    store_proj_bdu_creation_restriction_map: 
      Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_type_creation_bdu.Map.t
      Map_final_creation_bdu.Map.t;
    store_bdu_init_restriction_map: Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t;
    store_modif_list_restriction_map: 
      Mvbdu_wrapper.Mvbdu.hconsed_association_list list Map_modif_list.Map.t;
    store_proj_modif_list_restriction_map: 
      Mvbdu_wrapper.Mvbdu.hconsed_association_list list Map_agent_id_modif_list.Map.t
      Map_final_modif_list.Map.t;
    (*potential partner of side effects*)
    store_bdu_potential_effect_restriction_map : 
      Mvbdu_wrapper.Mvbdu.mvbdu Map_potential_bdu.Map.t;
    store_proj_bdu_potential_restriction_map :
      Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_type_potential_bdu.Map.t
      Map_final_potential_bdu.Map.t;
    store_potential_list_restriction_map :
      Mvbdu_wrapper.Mvbdu.hconsed_association_list list Map_potential_list.Map.t;
    store_proj_potential_list_restriction_map :
      Mvbdu_wrapper.Mvbdu.hconsed_association_list list 
      Map_agent_type_potential_list.Map.t Map_final_potential_list.Map.t;
    store_proj_bdu_views :
      Mvbdu_wrapper.Mvbdu.mvbdu Map_triple_views.Map.t Map_rule_id_views.Map.t;
  }

(************************************************************************************)
(*main*)

type bdu_analysic =
    {
      store_bdu_analysis_static  : bdu_analysis_static;
      store_bdu_analysis_dynamic : bdu_analysis_dynamic;
      store_bdu_build            : bdu_build;
    }
