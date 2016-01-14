(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 26th of October
  * Last modification: 
  * 
  * Compute the relations between the left hand site of a rule and its sites.
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

(*convert a list of covering class, new_index of covering class into a set*)

open Covering_classes_type
open Cckappa_sig
open Site_map_and_set

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Covering classes") message exn
                 (fun () -> default)                

let trace = false

(************************************************************************************)
(*function mapping a list of covering class, return triple
  (list of new_index, map1, map2)
  For example:
  - intput : covering_class_list [0;1]
  - output : 
  {new_index_of_covering_class: [1;2],
  map1 (from 0->1, 1->2),
  map2 (from 1->0, 2->1)
  }
*)

let new_index_pair_map parameter error l =
  let rec aux acc k map1 map2 error =
    match acc with
    | [] -> error, (map1, map2)
    | h :: tl ->
      let error,map1 = Map.add parameter error h k map1 in
      let error,map2 = Map.add parameter error k h map2 in
      aux tl (k+1) map1 map2 error 
  in
  let error',(map1,map2) = aux l 1 Map.empty Map.empty error in
  let error = Exception.check warn parameter error error' (Some "line 49") Exit in
  error,(map1,map2)
 

(************************************************************************************)
(*convert a list to a set*)

let list2set parameter error list =
  let error',list =
    List.fold_left
      (fun (error, current_set) elt ->
       let error, add_set =
	 Set.add parameter error elt current_set
       in
       error, add_set
      ) (error, Set.empty) list
  in
  let error = Exception.check warn parameter error error' (Some "line 66") Exit in
  error,list 

(************************************************************************************)

let collect_remanent_list2set parameter error handler_kappa store_remanent  =
  let error, init = AgentMap.create parameter error 0 in
  AgentMap.fold parameter error 
    (fun paramter error agent_type remanent store_result ->
     (*-------------------------------------------------------------------------*)
     (*get covering classes dictionary*)
     let store_dic = remanent.store_dic in
     let error, (id_list, site_set_list) =
       Dictionary_of_Covering_class.fold
	 (fun list _ index (error, (index_list, current_list)) ->
	  let error, list2set =
	    list2set parameter error list
	  in
	  let list_set = list2set :: current_list in
	  let list_index = index :: index_list in
	  error, (List.rev list_index, List.rev list_set)
	 ) store_dic (error, ([], []))
     in
      (*-------------------------------------------------------------------------*)
      (*store a mapping function from a list of covering class into a list
        of new index and a pair of map*)
     let error, (id_list_map, list_of_map) =
       Dictionary_of_Covering_class.fold
         (fun list _ index (error, (index_list, current_list)) ->
           let error, store_map =
             new_index_pair_map parameter error list
           in
           let new_list = store_map :: current_list in
           let index_list = index :: index_list in
           error, (List.rev index_list, List.rev new_list)
          ) store_dic (error, ([], []))
     in
     (*-------------------------------------------------------------------------*)
     (*print covering classes*)
     let _ =
       if Remanent_parameters.get_dump_reachability_analysis_covering_classes parameter
       then
         (*let _ = Format.printf "Reachability analysis potential dependencies...@." in *)
         let parameter =
           Remanent_parameters.update_prefix parameter ""
         in
         if Remanent_parameters.get_trace parameter
         then
           let error, agent_string = 
             try
               Handler.string_of_agent parameter error handler_kappa agent_type
             with
               _ -> warn parameter error (Some "line 118") Exit (string_of_int agent_type)
           in
           List.iter (fun id ->
             List.iter (fun site_set ->
               let _ =
                 Printf.fprintf stdout 
                   "Potential dependencies between sites:\nagent_type:%i:%s:covering_class_id:%i\n" agent_type agent_string id
               in
               Site_map_and_set.Set.iter (fun site_type ->
                 let error, site_string =
                   try
                     Handler.string_of_site parameter error handler_kappa 
                       agent_type site_type
                   with
                     _ -> warn parameter error (Some "line 132") Exit 
                       (string_of_int site_type)
                 in
                 Printf.fprintf stdout "site_type:%i:%s\n" site_type site_string
               ) site_set
             ) site_set_list
           ) id_list
         else ()
     in
     (*-------------------------------------------------------------------------*)
     (*print mapping with new indexes of site_type*)
     let _ =
        if Remanent_parameters.get_dump_reachability_analysis_covering_classes parameter
        then
          let parameter =
            Remanent_parameters.update_prefix parameter ""
          in
          if Remanent_parameters.get_trace parameter
          then
            let error, agent_string = 
              try
                Handler.string_of_agent parameter error handler_kappa agent_type
              with
                _ -> warn parameter error (Some "line 155") Exit (string_of_int agent_type)
            in
            List.iter (fun id ->
              let _ =
                Printf.fprintf stdout
                  "Mapping between global identifier of sites (per agent) and local identifier of sites (per covering classes):\nagent_type:%i:%s:covering_class_id:%i\n" 
                  agent_type agent_string id
              in
              List.iter (fun (map1, map2) ->
                let _ =
                  Site_map_and_set.Map.iter
                    (fun site site_new ->
                      let error, site_string =
                        try
                          Handler.string_of_site parameter error handler_kappa
                            agent_type site
                        with
                          _ -> warn parameter error (Some "line 172") Exit 
                            (string_of_int site)
                      in
                      Printf.fprintf stdout
                        "Global:site_type:%i:%s  => Local:site_type':%i:%s\n" 
                        site site_string site_new site_string
                    ) map1
                in
                Site_map_and_set.Map.iter
                  (fun site_new site ->
                    let error, site_string =
                      try
                        Handler.string_of_site parameter error handler_kappa agent_type site
                      with
                        _ -> warn parameter error (Some "line 186") Exit 
                          (string_of_int site)
                    in
                    Printf.fprintf stdout 
                      "Local:site_type':%i:%s  => Global:site_type:%i:%s\n"
                      site_new site_string site site_string
                  ) map2
              ) list_of_map
            ) id_list_map
          else ()
     in
     (*-------------------------------------------------------------------------*)
     (*store*)
     let error, store_result =
       AgentMap.set
         parameter
         error
         agent_type
         ((id_list, site_set_list),
          (id_list_map, list_of_map))
         store_result
     in
     error, store_result
    ) store_remanent init
