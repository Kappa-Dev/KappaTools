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

open Int_storage
open Cckappa_sig
open Dictionary

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering_classes_type") message exn (fun () -> default)

let local_trace = false

module AgentMap = Quick_Nearly_inf_Imperatif

type 'a map      = 'a Site_map_and_set.map
type set         = Site_map_and_set.set

(*state information: (site, state)*)
type state_list = (int * int) list

(*BDU*)
type bdu_redefine =  bool Mvbdu_sig.mvbdu

type bdu_handler_list  = 
    ((Boolean_mvbdu.memo_tables, 
     Boolean_mvbdu.mvbdu_dic,
     Boolean_mvbdu.list_dic,
     bool,
     int) Memo_sig.handler * bdu_redefine) list 

type bdu_handler  =
    ((Boolean_mvbdu.memo_tables, 
     Boolean_mvbdu.mvbdu_dic,
     Boolean_mvbdu.list_dic,
     bool, 
     int) Memo_sig.handler * bdu_redefine)

type covering_classes =
  {
    store_modified_map     : int map AgentMap.t;
    store_covering_classes : (int * state_list) list list AgentMap.t
                             * state_list AgentMap.t;
    store_creation         : ((int * int) list * bdu_handler) AgentMap.t;
    store_bdu_test         : ((int * int) list * bdu_handler_list) AgentMap.t
  }

type bdu_collection = (*TODO*)
    {
      store_bdu_covering_classes : (bdu_handler * int) list AgentMap.t
    }

(************************************************************************************)
(* DICTIONARY for covering classes *)

(*------------------------------------------------------------------------------*)
(* TYPE REMANENT:
   key(t): int; 'a t = infinite array of list(#id) *)

module Inf_array = Nearly_inf_Imperatif

module Covering_class =
  struct
    type t = (int * int) list
    let compare = compare
  end

module Modified_class =
  struct
    type t = int list
    let compare = compare
  end

(*Dictionary*)

module Dictionary_of_Covering_class = Dictionary_of_Ord (Covering_class)
module Dictionary_of_Modified_class = Dictionary_of_Ord (Modified_class)

type pair_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type index_dic  = (unit, unit) Dictionary_of_Covering_class.dictionary
type test_dic   = (unit, unit) Dictionary_of_Covering_class.dictionary
type modif_dic  = (unit, unit) Dictionary_of_Modified_class.dictionary

type remanent =
    {
      store_pointer_backward    : set Inf_array.t;
      store_dic                 : pair_dic;
      store_new_index_dic       : index_dic;
      store_test_new_index_dic  : test_dic;
      store_modif_new_index_dic : modif_dic;
    }
