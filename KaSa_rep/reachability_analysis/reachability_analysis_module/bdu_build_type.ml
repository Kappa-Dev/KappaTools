(**
  * bdu_analysis_type.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2016, the 20th of January
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Fifo
open Int_storage

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_analysis_type") message exn (fun () -> default)

let local_trace = false

(************************************************************************************)
(*type abstraction*)

module AgentMap = Quick_Nearly_inf_Imperatif

type wl_int = IntWL.WSetMap.elt list * IntWL.WSetMap.elt list * IntWL.WSetMap.Set.t

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
      type t = int
      let compare = compare
    end)

module Map_agent_id_test_bdu =
  SetMap.Make (
    struct
      type t = int
      let compare = compare
    end)

module Project2bdu_test =
  SetMap.Proj2 (Map_test_bdu)(Map_final_test_bdu)(Map_agent_id_test_bdu)


(************************************************************************************)
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
      type t = int * int
      let compare = compare
    end)

module Project2bdu_creation =
  SetMap.Proj2 (Map_creation_bdu)(Map_final_creation_bdu)(Map_agent_type_creation_bdu)

(************************************************************************************)
(*init*)

module Map_init_bdu =
  Map_wrapper.Make
    (SetMap.Make
       (struct
         type t = int * int
         let compare = compare
        end))

(************************************************************************************)
(*modification*)

module Map_modif_list =
  Map_wrapper.Make
    (SetMap.Make (
      struct
        type t = int * int * int * int
        let compare = compare
      end))

(************************************************************************************)
(*potential side effect*)

module Map_potential_bdu =
  SetMap.Make (
    struct
      type t = int * int * int * int
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
      type t = int * int * int
      let compare = compare
    end)

module Project2bdu_potential = (*REMOVE?*)
  SetMap.Proj2 (Map_potential_bdu)(Map_final_potential_bdu)(Map_agent_type_potential_bdu)


(************************************************************************************)
(*projection for views used in is_enable*)

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

type bdu_build =
  {
    store_remanent_triple: ((int * int list * Site_map_and_set.Set.t) list) AgentMap.t;
    store_wl_creation: wl_int;
    store_bdu_test_restriction_map: Mvbdu_wrapper.Mvbdu.mvbdu Map_test_bdu.Map.t;
    store_proj_bdu_test_restriction_map: 
      Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_id_test_bdu.Map.t Map_final_test_bdu.Map.t;
    store_bdu_creation_restriction_map: Mvbdu_wrapper.Mvbdu.mvbdu Map_creation_bdu.Map.t;
    store_proj_bdu_creation_restriction_map: 
      Mvbdu_wrapper.Mvbdu.mvbdu Map_agent_type_creation_bdu.Map.t
      Map_final_creation_bdu.Map.t;
    store_bdu_init_restriction_map: Mvbdu_wrapper.Mvbdu.mvbdu Map_init_bdu.Map.t;
    store_modif_list_restriction_map: 
      Mvbdu_wrapper.Mvbdu.hconsed_association_list Map_modif_list.Map.t;
    (*potential partner of side effects*)
    store_bdu_potential_effect_restriction_map : 
      (Mvbdu_wrapper.Mvbdu.mvbdu* Mvbdu_wrapper.Mvbdu.hconsed_association_list)
      Map_potential_bdu.Map.t;
    store_proj_bdu_potential_restriction_map :
      (Mvbdu_wrapper.Mvbdu.mvbdu * Mvbdu_wrapper.Mvbdu.hconsed_association_list)
      Map_agent_type_potential_bdu.Map.t Map_final_potential_bdu.Map.t;
    store_proj_bdu_views :
      Mvbdu_wrapper.Mvbdu.mvbdu Map_triple_views.Map.t Map_rule_id_views.Map.t;
  }
