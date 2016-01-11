(**
    * ode_fragmentation.ml
    * openkappa
    * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2015, the 9th of Apirl
    * Last modification: 
    * * 
    * ODE fragmentation
    * 
    *  
    * Copyright 2010,2011 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)

open Int_storage
open Cckappa_sig
open Printf

let warn parameter mh message exn default =
  Exception.warn parameter mh (Some "ODE fragmentation type") message exn
                 (fun () -> default)

let trace = false

(************************************************************************************)
(*TYPE*)

module AgentMap = Quick_Nearly_inf_Imperatif

module Internal_flow_map =
  SetMap.Make (
    struct 
      type t = int
      let compare = compare
    end)

module External_flow_map =
  SetMap.Make (
    struct
      type t = int * int
      let compare = compare
    end)

type ode_frag =
    {
      store_sites_modified_set            : Site_map_and_set.Set.t AgentMap.t;
      store_sites_bond_pair_set           : 
        Site_map_and_set.Set.t AgentMap.t * Site_map_and_set.Set.t AgentMap.t;
      store_sites_bond_pair_set_external  : 
        Site_map_and_set.Set.t AgentMap.t * Site_map_and_set.Set.t AgentMap.t;
      store_sites_lhs                     : int list AgentMap.t;
      store_sites_anchor_set              : 
        Site_map_and_set.Set.t AgentMap.t * Site_map_and_set.Set.t AgentMap.t;
      store_internal_flow                 : 
        (int list * Site_map_and_set.Set.t) Internal_flow_map.Map.t *
        (int list * Site_map_and_set.Set.t) Internal_flow_map.Map.t;
      store_external_flow                 :
        (Site_map_and_set.Set.t * Site_map_and_set.Set.t * Site_map_and_set.Set.t)
        External_flow_map.Map.t;
    }
