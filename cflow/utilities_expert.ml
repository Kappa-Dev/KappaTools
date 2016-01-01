(**
 * utilities.ml 
 *      
 * Creation:                      <2015-03-28 feret>
 * Last modification: Time-stamp: <2015-12-16 09:14:46 feret>
 * 
 * API for causal compression (for expert)
 * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Jean Krivine, Université Paris-Diderot, CNRS   
 *  
 * Copyright 2015 Institut National de Recherche en Informatique  * et en Automatique.  All rights reserved.  
 * This file is distributed under the terms of the 
 * GNU Library General Public License *)

let debug_mode = false
let remove_obs_before parameter handler log_info error last_info trace = error,log_info,trace		   
let fold_over_the_causal_past_of_observables_with_a_progress_bar_while_reshaking_the_trace
      parameter ~shall_we_compute:always ~shall_we_compute_profiling_information:always 
      handler log_info error
      always never
      init_score 
      score_before
      stop_before      
      score_after
      stop_after
      merge_score
      n_first
      global_trace_simplification
      f
      store_result 
      trace
      table 
  =
  let f parameter ?(shall_we_compute=always) ?(shall_we_compute_profiling_information=always) handler log_info error trace info (last_info,stop_next,thresh_before,thresh_after,counter,table) =
    if stop_next
    then error,log_info,Stop.stop (last_info,table)
    else 
      let error,log_info,before = score_before parameter log_info error trace in
      let error,log_info,stop = stop_before parameter log_info error before thresh_before in
      if stop
      then error,log_info,Stop.stop (last_info,table)
      else
	begin
	  let error,log_info,trace = f parameter handler log_info error trace in
	  let error,log_info,after = score_after parameter log_info error trace in 
	  let error,log_info,stop = stop_after parameter log_info error after thresh_after in
	  let error,log_info,table = store_result parameter handler log_info error trace info table in
	  let last_info = Some info in 
	  let error,log_info,thresh_before,thresh_after =
	    if counter > n_first
	    then 
	      error,log_info,thresh_before,thresh_after    
	    else
	      let error,log_info,thresh_before = merge_score parameter handler log_info error thresh_before before in
	      let error,log_info,thresh_after  = merge_score parameter handler log_info error thresh_after  after  in
	      error,log_info,thresh_before,thresh_after
	  in
	  error,log_info,Stop.success (last_info,stop,thresh_before,thresh_after,succ counter,table)
	end
  in 
  let rec aux log_info error trace (table:unit) =
    if Utilities.has_obs trace
    then
      begin
	let error,log_info,trace = global_trace_simplification 0 (error,log_info,trace) in 	
	let error,log_info,output =
	  Utilities.fold_over_the_causal_past_of_observables_with_a_progress_bar
	    parameter ~shall_we_compute:always ~shall_we_compute_profiling_information:always 
	    handler log_info error
	    always never
	    f
	    trace
	    (None,false,None,None,0,table)
	in
	Stop.success_or_stop
	  (fun (_,_,_,_,_,output) -> error,log_info,output)
	  (fun (last_info,(table:unit)) -> 
	   let error,log_info,trace = remove_obs_before parameter handler log_info error last_info trace in
	   aux log_info error trace table)
	  output
      end
    else
      error,log_info,table
  in
  aux log_info error trace table 
