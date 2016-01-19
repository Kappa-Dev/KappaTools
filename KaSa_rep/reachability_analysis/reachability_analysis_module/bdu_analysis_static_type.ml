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

(*******************************************************************************)
(*type abstraction*)

module Int2Map_CV =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int
         let compare = compare
        end))

(*******************************************************************************)
(*half break side effect*)

module Int2Map_HalfBreak_effect =
  Map_wrapper.Make 
    (SetMap.Make
       (struct 
         type t = int * int
         let compare = compare
        end))

type half_break_action = 
  (int list * (int * int) list) Int2Map_HalfBreak_effect.Map.t

(*******************************************************************************)
(*do not consider the case where site has state free.*)

module Int2Map_Remove_effect =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int
         let compare = compare
        end))

type remove_action =
  (int list * int list) Int2Map_Remove_effect.Map.t

(*******************************************************************************)
(*potential partner side effects*)

module Int2Map_potential_effect =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int
         let compare = compare
        end))

type free_partner = (int * int) list Int2Map_potential_effect.Map.t
type bind_partner = (int * int) list Int2Map_potential_effect.Map.t

type potential_partner_free = free_partner
type potential_partner_bind = bind_partner

(************************************************************************************)
(*views that are tested and modified with agent_id*)

(*views that are tested and modified without agent_id*)

module Int2Map_Modif =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int * int
         let compare = compare
        end))

module Int2Map_Test_Modif =
  Map_wrapper.Make
    (SetMap.Make
       (struct 
         type t = int * int
         let compare = compare
        end))

module Project2_modif =
  Map_wrapper.Proj (Int2Map_Modif) (Int2Map_Test_Modif)

(*******************************************************************************)

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
