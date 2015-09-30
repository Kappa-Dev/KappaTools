(**
  * bdu_side_effects.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 30th of September
  * Last modification: 
  * 
  * Compute the relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Bdu_analysis_type
open Cckappa_sig
open Int_storage

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU modification sites") message exn (fun () -> default)

let trace = false

(************************************************************************************)
(* return a list of rule_id has sites that are modified.
   For example:
   'r0' A() ->
   'r1' A(x) ->
   'r2' A(x!B.x) -> A(x)
   'r3' A(x), B(x) -> A(x!1), B(x!1)
   'r4' A(x,y), C(x) -> A(x, y!1), C(x!1)

   result:
   - A(x): [r2; r3]
   - A(y): [r4]
   - B(x): [r3]
   - C(x): [r4]
*)

let collect_modification_sites parameter error rule_id diff_direct store_result =
  AgentMap.fold parameter error
    (fun parameter error agent_id site_modif store_result ->
      if Site_map_and_set.is_empty_map site_modif.agent_interface
      then error, store_result
      else
        let agent_type = site_modif.agent_name in
        let triple_list =
          Site_map_and_set.fold_map
            (fun site port current_list ->
              let state = Bdu_creation.int_of_port port in
              let list =
                (rule_id, site, state) :: current_list
              in
              list
            ) site_modif.agent_interface []
        in
        (*old*)
        let error, old_list =
          match AgentMap.unsafe_get parameter error agent_type store_result with
          | error, None -> error, []
          | error, Some l -> error, l
        in
        (*new*)
        let new_list = List.concat [triple_list; old_list] in
        (*store*)
        let error, store_result =
          AgentMap.set
            parameter
            error
            agent_type
            (List.rev new_list)
            store_result
        in
        error, store_result 
    ) diff_direct store_result
