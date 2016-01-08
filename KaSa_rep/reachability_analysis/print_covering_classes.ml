(**
  * print_covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 26th of June
  * Last modification: 
  * 
  * Print the relations between the left hand site of a rule and its sites.
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Covering_classes_type
open Int_storage
open Covering_classes_new_index

(*------------------------------------------------------------------------------*)

let print_site_type l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | (x,_) :: tl ->
        fprintf stdout "site_type:%i\n" x;
        aux tl
  in aux l

let print_site_list l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | x :: tl ->
        fprintf stdout "site_type:%i\n" x;
        aux tl
  in aux l

(*------------------------------------------------------------------------------*)
(*print new index*)

let print_new_index_dic parameter error elt_id store_index =
  Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt index _ _ ->
      let _ = 
        fprintf stdout
          "Potential dependencies between sites:New-index:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_site_list index
      in
      error
    ) store_index
    
(*------------------------------------------------------------------------------*)
(*print test with new index*)

let print_test_new_index_dic parameter error elt_id store_test =(*REMOVE*)
  Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt value_index _ _ ->
      let _ = fprintf stdout
        "Potential dependencies between sites:New-index:TEST:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_site_list value_index
      in
      error
    ) store_test

(*------------------------------------------------------------------------------*)
(*print modified site (action) with new index*)

let print_modified_dic parameter error elt_id store_modif =(*REMOVE*)
  Dictionary_of_Modified_class.print
    parameter
    error
    (fun parameter error elt l _ _ ->
      let _ =
        fprintf stdout 
          "Potential dependencies between sites:New-index:MODIFICATION-:Covering_class_id:%i:class_id:%i:\n"
          elt_id elt;
        print_site_list l
      in
      error
    ) store_modif

(*------------------------------------------------------------------------------*)

let print_dic_and_new_index parameter error store_index store_test store_modif store_dic =
  Dictionary_of_Covering_class.print
    parameter
    error
    (fun parameter error elt_id list _ _ ->
      let _ =
        let _ =
        (*print covering class in dictionary*)
          printf "Potential dependencies between sites:Covering_class_id:%i:\n"
            elt_id; 
          print_site_list list
        in
        (*print new_index for covering class*)
        print_new_index_dic
          parameter
          error 
          elt_id 
          store_index 
      in
      (*REMOVE*)
      (*print site that is tested with its new_index*)
      (*let _ = print_test_new_index_dic
        parameter 
        error 
        elt_id
        store_test 
        in*)
      (*print site that is modified with its new_index*)
      (*print_modified_dic 
        parameter
        error 
        elt_id 
        store_modif
        in*)
      error)
    store_dic

(*------------------------------------------------------------------------------*)

let print_value_site parameter error elt site value_site = (*REMOVE*)
  Quick_Nearly_inf_Imperatif.print
    error
    (fun error parameter value_site_list ->
      let _ =
        let rec aux_value acc' =
          match acc' with
            | [] -> acc'
            | vsite :: tl' ->
              let _ =
                fprintf stdout 
                  "Potential dependencies between sites:New-index:Covering_class_id:%i:"
                  elt;
                match vsite with
                  | Ckappa_sig.Internal s ->
		    fprintf stdout "site_modified:%i->%s(internal state)\n"
                      site s
	          | Ckappa_sig.Binding s ->
		    fprintf stdout "site_modified:%i->%s(binding state)\n"
                      site s                                     
              in aux_value tl'
        in aux_value value_site_list
      in
      error
    ) parameter value_site


(************************************************************************************)
(*MAIN PRINT*)

(*let print_result parameter error result_remanent =
  if Remanent_parameters.get_do_site_dependencies parameter
  then
    let _ = Format.printf "\nPotential dependencies ....@." in
    let parameter =
      Remanent_parameters.update_prefix parameter ""
    in
    AgentMap.print
      error
      (fun error parameter remanent ->
        let _ =
        (*------------------------------------------------------------------------------*)
        (* number of covering classes*)
          let number =
            number_of_covering_classes 
              parameter
              error
              remanent.store_dic
          in
          let _ = fprintf stdout
            "Potential dependencies between sites:Number of covering classes:%i\n" number
          in
        (*------------------------------------------------------------------------------*)
        (*print covering class and theirs new-index*)
          let _ =
            print_dic_and_new_index parameter error
              remanent.store_new_index_dic
              remanent.store_test_new_index_dic
              remanent.store_modif_new_index_dic
              remanent.store_dic
          in
        (*------------------------------------------------------------------------------*)
          error
        in
        error) parameter result_remanent
  else error*)
