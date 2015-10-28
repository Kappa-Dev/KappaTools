(**
  * print_bdu_analysis.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 28th of October
  * Last modification: 
  * 
  * Print relations between sites in the BDU data structures
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Printf
open Memo_sig
open Cckappa_sig
open Remanent_parameters_sig
open Bdu_analysis_type
open Print_bdu_analysis_static
open Print_bdu_analysis_dynamic
open Print_bdu_build

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "BDU creation") message exn (fun () -> default)  

let trace = false

(************************************************************************************)
(*fixpoint iteration*)


(************************************************************************************)
(*MAIN PRINT*)

let print_result parameter error result =
  let parameter = Remanent_parameters.update_prefix parameter "agent_type_" in
  let _ =
    print_result_static
      parameter
      error 
      result.store_bdu_analysis_static
  in
  let _ =
    print_result_dynamic
      parameter
      error
      result.store_bdu_analysis_dynamic
  in
  let _ =
    print_bdu_build
      parameter
      error
      result.store_bdu_build
  in
  error
