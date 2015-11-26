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

(*syntactic contact map*)

module Int2Map_CM_state = 
  SetMap.Make (
    struct
      (*agent_type, site, state*)
      type t = int * int * int
      let compare = compare
    end
  )

(*list of rules to awake when the state of a site is modified and tested*)

module Int2Map_CV_Modif = 
  SetMap.Make (
    struct
      (*agent_id, agent_type, covering_class_id*)
      type t = int * int
      let compare = compare
    end)


(************************************************************************************)
(*type bdu_build_map:
  local information of views that are tested, created and modified with new indexes*)

module Map_creation =
  SetMap.Make (
    struct
      type t = int * int * int
      let compare = compare
    end)

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

module Project2bdu_creation =
  SetMap.Proj (Map_creation_bdu)(Map_final_creation_bdu)

module Map_creation_bdu_ag =
  SetMap.Make (
    struct
      type t = int (*agent_type*)
      let compare = compare
    end)

(*Test rule*)
module Map_test =
  SetMap.Make (
    struct
      type t = int * int * int * int
      let compare = compare
    end)

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
      type t = int
      let compare = compare
    end)

module Project2bdu_test =
  SetMap.Proj (Map_test_bdu)(Map_final_test_bdu)

(*modification*)

module Map_modif =
  SetMap.Make (
    struct
      type t = int * int * int * int
      let compare = compare
    end)

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

module Map_modif_ag =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)


module Project2bdu_modif =
  SetMap.Proj (Map_modif_list)(Map_final_modif_list)

(************************************************************************************)
(*fixpoint iteration*)

module Map_bdu_update =
  SetMap.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*build a pair of site_address map*)

module Map_site_address =
  SetMap.Make (
    struct
      type t = site_address * site_address
      let compare = compare
    end)

module Map_test_bond =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

(************************************************************************************)
(*static information*)

type half_break_action = 
  (int list * (int * int) list) Int2Map_HalfBreak_effect.Map.t

(*do not consider the case where site has state free.*)
type remove_action =
  (int list * int list) Int2Map_Remove_effect.Map.t

type bdu_analysis_static =
  {
    store_covering_classes_id : (int list * int list) Int2Map_CV.Map.t;
    store_side_effects        : half_break_action * remove_action;
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
    store_contact_map : 
    (*TODO: combine contact map and modification update into a product type*)
    (int list * (int * int * int) list) Int2Map_CM_state.Map.t;
    store_covering_classes_modification_update : (*update(c)*)
      (int list * Site_map_and_set.Set.t) Int2Map_CV_Modif.Map.t;
    store_wl_creation : wl_int;
  }

(************************************************************************************)
(*build covering classes with new indexes*)

type bdu_build =
  {
    store_remanent_triple       : ((int * int list * Site_map_and_set.Set.t) list) AgentMap.t;
    store_remanent_test_map     : (int list * (int * int) list) Map_test.Map.t;
    store_remanent_creation_map : (int list * (int * int) list) Map_creation.Map.t;
    store_remanent_modif_map    : (int list * (int * int) list) Map_modif.Map.t;
  }

(************************************************************************************)
(*build covering classes in map and bdu with new indexes for site_type*)

type bdu_build_map =
  {
    store_creation_bdu_map : 
    (int list * (int * Mvbdu_wrapper.Mvbdu.mvbdu) list) Map_creation_bdu.Map.t;
    store_test_bdu_map     :
      (int list * (int * Mvbdu_wrapper.Mvbdu.mvbdu) list) Map_test_bdu.Map.t;
    store_modif_list_map   :
      (int list * (int * (int * int) list) list) Map_modif_list.Map.t;
    (*projection map of creation with rule_id*)
    store_final_creation_bdu_map :
      (int list * (int * Mvbdu_wrapper.Mvbdu.mvbdu) list) Map_final_creation_bdu.Map.t;
    store_final_test_bdu_map :
      (int list * (int * Mvbdu_wrapper.Mvbdu.mvbdu) list) Map_final_test_bdu.Map.t;
    store_final_modif_list_map :
      (int list * (int * (int * int) list) list) Map_final_modif_list.Map.t
  }

(************************************************************************************)
(*fixpoint*)

type bdu_fixpoint =
  {
    store_test_has_bond_rhs : (int list * Map_site_address.Set.t) Map_test_bond.Map.t;
    store_bdu_update_map    : (int list * Mvbdu_wrapper.Mvbdu.mvbdu list) Map_bdu_update.Map.t;
    store_triple_test : ((int list * Mvbdu_wrapper.Mvbdu.mvbdu) Map_creation_bdu_ag.Map.t *
                           (int list * (int * int) list) Map_modif_ag.Map.t) (*REMOVE*)
  }

(************************************************************************************)
(*main*)

type bdu_analysic =
    {
      store_bdu_analysis_static  : bdu_analysis_static;
      store_bdu_analysis_dynamic : bdu_analysis_dynamic;
      store_bdu_build            : bdu_build;
      store_bdu_build_map        : bdu_build_map;
      store_bdu_fixpoint         : bdu_fixpoint
    }
