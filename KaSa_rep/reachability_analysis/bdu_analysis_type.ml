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
open Set_and_map

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_analysis_type") message exn (fun () -> default)

let local_trace = false

(************************************************************************************)
(*module type*)

module AgentMap = Quick_Nearly_inf_Imperatif

(*module of covering classes*)

module Int2Map_CV =
  Set_and_map.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*------------------------------------------------------------------------------*)
(*module type of contact map with state*)

module Int2Map_CM_state =
  Set_and_map.Make (
    struct
      type t = int * int * int
      let compare = compare
    end
  )

(*------------------------------------------------------------------------------*)
(*module type of modification site*)

module Int2Map_Modif =
  Set_and_map.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*------------------------------------------------------------------------------*)
(*half break side effect module*)

module Int2Map_HalfBreak_effect =
  Set_and_map.Make (
    struct
      type t = int * int
      let compare = compare
    end)

module Int2Map_Remove_effect =
  Set_and_map.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*------------------------------------------------------------------------------*)
(*update function of covering classes and modification sites*)

module Int2Map_CV_Modif =
  Set_and_map.Make (
    struct
      type t = int * int * int
      let compare = compare
    end)

(************************************************************************************)

(*type site_bdu  = (List_sig.variable * Cckappa_sig.state_index) list *
  ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
    Boolean_mvbdu.list_dic, bool, int)
      Memo_sig.handler * bool Mvbdu_sig.mvbdu array)*)


(************************************************************************************)
(*static information*)

type half_break_action = 
  (int list * (int * int) list) Int2Map_HalfBreak_effect.map

(*do not consider the case where site has state free.*)
type remove_action =
  (int list * int list) Int2Map_Remove_effect.map

type bdu_analysis_static =
  {
    store_covering_classes_id : (int list * int list) Int2Map_CV.map;
    store_side_effects        : half_break_action * remove_action;
    store_creation_sites      : (int list * Site_map_and_set.set) Int2Map_Modif.map;
    store_modification_sites_without_creation : (int list * Site_map_and_set.set)
      Int2Map_Modif.map;
    store_modification_sites  :
      (int list * Site_map_and_set.set) Int2Map_Modif.map;
    store_test_sites :
      (int list * Site_map_and_set.set) Int2Map_Modif.map;
    store_test_modification_sites : 
      (int list * Site_map_and_set.set) Int2Map_Modif.map;
    store_test_modification_without_creation : (*use in update*)
      (int list * Site_map_and_set.set) Int2Map_Modif.map;
  }

(************************************************************************************)
(*dynamic*)

type wl_int = IntWL.WSet.elt list * IntWL.WSet.elt list * IntWL.WSet.set

type bdu_analysis_dynamic =
  {
    store_contact_map      : 
    (*TODO: combine contact map and modification update into a product type*)
    (int list * (int * int * int) list) Int2Map_CM_state.map;
    store_covering_classes_modification_update :
      (int list * Site_map_and_set.set) Int2Map_CV_Modif.map;
    store_update :
      (int list * Site_map_and_set.set) Int2Map_CV_Modif.map *
      (int list * Site_map_and_set.set) Int2Map_CV_Modif.map *
      (int list * Site_map_and_set.set) Int2Map_CV_Modif.map *
      (int list * Site_map_and_set.set) Int2Map_CV_Modif.map;
    store_wl_update          : wl_int AgentMap.t;
    store_wl_creation        : wl_int AgentMap.t;
    store_wl_creation_update : wl_int AgentMap.t
  }

(************************************************************************************)
(*build bdu type*)

type bdu_build =
  {
    store_remanent_test : ((int * int list * Site_map_and_set.set) list) AgentMap.t;
    store_test_restriction : (int * int Site_map_and_set.map) list AgentMap.t
  }

(************************************************************************************)
(*main*)

type bdu_analysic =
    {
      store_bdu_analysis_static  : bdu_analysis_static;
      store_bdu_analysis_dynamic : bdu_analysis_dynamic;
      store_bdu_build            : bdu_build;
    }
