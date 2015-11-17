(**
  * print_bdu_fixpoint.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 13th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Bdu_analysis_type
open Print_bdu_build_map

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)

let print_bdu_creation_array parameter error result =
  AgentMap.print error
    (fun error parameter array ->
      let _ =
        Array.iteri (fun index bdu_creation ->
          let _ =
            fprintf stdout "index of this array:%i\n" index;
            print_bdu parameter error bdu_creation
          in
          ()
        ) array
      in
      error
    ) parameter result

(************************************************************************************)

let print_bdu_test_array parameter error result =
  AgentMap.print error
    (fun error parameter array ->
      let _ =
        Array.iteri (fun index bdu_test ->
          let _ =
            fprintf stdout "index of this array:%i\n" index;
            print_bdu parameter error bdu_test
          in
          ()
        ) array
      in
      error
    ) parameter result

(************************************************************************************)
(*main print*)

let print_bdu_fixpoint parameter error result =
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "\n------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "* Fixpoint iteration :\n";
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "- An array of bdu_creation:\n";
    print_bdu_creation_array
      parameter
      error
      result.store_bdu_creation_array    
  in
  let _ =
    fprintf (Remanent_parameters.get_log parameter)
      "------------------------------------------------------------\n";
    fprintf (Remanent_parameters.get_log parameter)
      "- An array of bdu_test:\n";
    print_bdu_test_array
      parameter
      error
      result.store_bdu_test_array    
  in
  error
