(**
  * bdu_analysis_type.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
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
  MapExt.Make (
    struct
      type t = int * int
      let compare = compare
    end)


(*module type of contact map with state*)

module Int2Map_CM_state =
  MapExt.Make (
    struct
      type t = int * int * int
      let compare = compare
    end
  )

(*module type of contact map without state*)

module BSet =
  Set_and_map.Make (
    struct
      type t = int
      let compare = compare
    end)

module Int2Map_CM_Set =
  MapExt.Make (
    struct
      type t = int * BSet.set * int (*agent * site set * agent'*)
      let compare = compare
    end)
 
(*module type of modification site*)

module Int2Map_Modif =
  MapExt.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*half break side effect module*)

module Int2Map_HalfBreak_effect =
  MapExt.Make (
    struct
      type t = int * int
      let compare = compare
    end)

module Int2Map_Remove_effect =
  MapExt.Make (
    struct
      type t = int * int
      let compare = compare
    end)

(*update function of covering classes and modification sites*)
module Int2Map_CV_Modif =
  MapExt.Make (
    struct
      type t = int * int * int
      let compare = compare
    end)

(************************************************************************************)

type site_bdu  = (List_sig.variable * Cckappa_sig.state_index) list *
  ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
    Boolean_mvbdu.list_dic, bool, int)
      Memo_sig.handler * bool Mvbdu_sig.mvbdu array)

type wl_int = IntWL.WSet.elt list * IntWL.WSet.elt list * IntWL.WSet.set

type half_break_action = 
  (int list * (int * int) list) Int2Map_HalfBreak_effect.t

(*do not consider the case where site has state free.*)
type remove_action =
  (int list * int list) Int2Map_Remove_effect.t

type bdu_analysic =
    {
      store_creation_rule    : (int list * wl_int * Cckappa_sig.rule array) AgentMap.t;
      store_creation         : site_bdu AgentMap.t;
      (*static information*)
      store_side_effects     : half_break_action * remove_action;
      store_covering_classes_id : (int list * int list) Int2Map_CV.t;
      store_modification_sites  : (int list * int list) Int2Map_Modif.t; 
      (*------------------------------------------------------------------------------*)
      (*dynamic information*)
      store_contact_map      :
        (int list * (int * int * int) list) Int2Map_CM_state.t;
      store_binding_rhs_set :
        (int list * BSet.set) Int2Map_CM_Set.t * 
        (int list * BSet.set) Int2Map_CM_Set.t;
      store_binding_dual_rhs :
        (int list * (int * int * int) list) Int2Map_CM_state.t *
        (int list * (int * int * int) list) Int2Map_CM_state.t ;
      store_covering_classes_modification_update :
        (int list * int list) Int2Map_CV_Modif.t;
      (*store_update :
        (int list * int list) Int2Map_CV_Modif.t *
        (int list * int list) Int2Map_CV_Modif.t *
        (int list * int list) Int2Map_CV_Modif.t *
        (int list * int list) Int2Map_CV_Modif.t;*)
    (*bdu fixpoint iteration*)
      (*store_fixpoint : (wl_int * Cckappa_sig.rule array) AgentMap.t*)
    }
