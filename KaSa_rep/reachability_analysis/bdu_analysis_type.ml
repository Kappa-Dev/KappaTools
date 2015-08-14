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

type pair_site = (int * int) list

type bdu = bool Mvbdu_sig.mvbdu

type handler = (Boolean_mvbdu.memo_tables, Boolean_mvbdu.mvbdu_dic,
	        Boolean_mvbdu.list_dic, bool, int) Memo_sig.handler

type handler_bdu = handler * bdu
   
type site_bdu  = pair_site * handler_bdu
      
type triple_wl = BduWlist.WSet.elt list * BduWlist.WSet.elt list * BduWlist.WSet.set

type test_wl = pair_site * handler * triple_wl

(*create type set for bdu_iteration*)

module Bdu_iterate = Set_and_map.Make
  (struct
    type t = bdu 
    let compare = compare
   end)

type set = Bdu_iterate.set

type bdu_analysic =
    {
      store_creation  : site_bdu AgentMap.t;
      store_iteration : test_wl AgentMap.t * test_wl AgentMap.t * (handler * set) AgentMap.t;
    }
