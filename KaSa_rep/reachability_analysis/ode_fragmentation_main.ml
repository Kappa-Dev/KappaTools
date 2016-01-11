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
open Ode_fragmentation_type
open Ode_fragmentation
open Print_ode_fragmentation

let warn parameter mh message exn default =
  Exception.warn parameter mh (Some "ODE fragmentation main") message exn
                 (fun () -> default)

let trace = false

(************************************************************************************)     
(*MAIN*)

let ode_fragmentation parameter error handler_kappa compiled =
  let error, result =
    scan_rule_set parameter error handler_kappa compiled 
  in
  (*print*)
  (*let _ =
    if (Remanent_parameters.get_trace parameter) || trace
    then
      print_result parameter error handler_kappa result
    else
      error
  in*)
  error, result
