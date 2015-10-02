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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_analysis_type") message exn (fun () -> default)

let local_trace = false

module AgentMap = Quick_Nearly_inf_Imperatif

(*contact map with state*)
module Int2Map =
  MapExt.Make (
    struct
      type t = Cckappa_sig.agent_name * Cckappa_sig.site_name * Cckappa_sig.state_index
      let compare = compare
    end
  )

(*contact map without state*)
module Int2Map_pair =
  MapExt.Make (
    struct
      type t = Cckappa_sig.agent_name * Cckappa_sig.site_name
      let compare = compare
    end
  )

type site_bdu  = (List_sig.variable * Cckappa_sig.state_index) list *
  ((Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
    Boolean_mvbdu.list_dic, bool, int)
      Memo_sig.handler * bool Mvbdu_sig.mvbdu array)

type wl_int = IntWL.WSet.elt list * IntWL.WSet.elt list * IntWL.WSet.set

type half_break_action =
  (IntWL.WSet.elt * Cckappa_sig.site_name * Cckappa_sig.state_index) list AgentMap.t

type remove_action =
  (Fifo.IntWL.WSet.elt * Cckappa_sig.Site_map_and_set.key * bool option) list AgentMap.t *
    (Fifo.IntWL.WSet.elt * Cckappa_sig.Site_map_and_set.key) list AgentMap.t

type bdu_analysic =
    {
      store_creation_rule    : (int list * wl_int * Cckappa_sig.rule array) AgentMap.t;
      store_creation         : site_bdu AgentMap.t;
      store_side_effects     : half_break_action * remove_action;
      store_modification_sites :
        (IntWL.WSet.elt * Cckappa_sig.site_name * Cckappa_sig.state_index) list AgentMap.t;
      store_covering_classes_modified_sites:
        (IntWL.WSet.elt * Cckappa_sig.site_name * Cckappa_sig.state_index) list AgentMap.t;
      (*contact map*)
      store_contact_map      :
        ((Cckappa_sig.agent_name list) *
            (Cckappa_sig.agent_name * Cckappa_sig.site_name *
               Cckappa_sig.state_index) list) Int2Map.t;
      store_binding_rhs      :
        ((Cckappa_sig.agent_name list) *
            (Cckappa_sig.agent_name * Cckappa_sig.site_name) list) Int2Map_pair.t;
      store_binding_dual     :
        (Cckappa_sig.agent_name list *
           (Cckappa_sig.agent_name * Cckappa_sig.site_name *
              Cckappa_sig.state_index) list) Int2Map.t;
    }
