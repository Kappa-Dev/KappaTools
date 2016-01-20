(**
  * bdu_analysis_main.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 19th of Januaray
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU analysis") message exn (fun () -> default)  

let trace = false

open Cckappa_sig
open Int_storage

(*******************************************************************************)
(*type abstraction*)

module AgentMap = Quick_Nearly_inf_Imperatif

module Set_triple =
  Map_wrapper.Make
    (SetMap.Make (
      struct
        type t = int * int * int
        let compare = compare
      end))

(*full contact map*)

module Int2Map_CM_state =
  Map_wrapper.Make 
    (SetMap.Make
       (struct 
         type t = int * int * int
         let compare = compare
        end))

(*syntactic contact map*)

module Int2Map_CM_Syntactic =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = Set_triple.Set.t
         let compare = compare
        end))

(*list of rules to awake when the state of a site is modified and tested*)

module Int2Map_CV_Modif =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int
         let compare = compare
        end))

(*******************************************************************************)

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
