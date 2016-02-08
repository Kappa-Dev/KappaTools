(**
  * bdu_fixpoint_iteration.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 9th of October
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open Cckappa_sig
open Bdu_analysis_type
open SetMap
open Mvbdu_sig
open Boolean_mvbdu
open Memo_sig
open Site_map_and_set
open Covering_classes_type
open Bdu_build
open Fifo
open Printf
open Mvbdu_wrapper

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Bdu_fixpoint_iteration") message exn
    (fun () -> default)

let local_trace = false

(*let dump_channel parameter  f =
  if local_trace
    || Remanent_parameters.get_trace parameter
  then f (Remanent_parameters.get_logger parameter)*)

let dump_formatter parameter  f =
  if local_trace
    || Remanent_parameters.get_trace parameter
  then f (Remanent_parameters.get_logger parameter)

(************************************************************************************)
(** [dump_view_diff handler_bdu old_bdu new_bdu] returns a pretty print of
    the xor of the two input bdus according to its agent and the covering
    class that its belong to. The list of sites in each covering class is
    computed by their news indexes. This function also convert a xor_bdu in
    bdu data structure into a list type. The printer will print the format
    of bdu and the format that easy to read.*)

let dump_view_diff parameter handler_kappa handler_bdu error
    site_correspondence agent_type cv_id old_bdu new_bdu =
  if local_trace
    || Remanent_parameters.get_dump_reachability_analysis_diff parameter
    || Remanent_parameters.get_trace parameter
  then
    let prefix = Remanent_parameters.get_prefix parameter in
    let error, handler_bdu, bdu_diff =
      Mvbdu_wrapper.Mvbdu.mvbdu_xor parameter handler_bdu error old_bdu new_bdu
    in
    (*-----------------------------------------------------------------------*)
    let error, agent_string =
      try
        Handler.string_of_agent parameter error handler_kappa agent_type
      with
        _ -> warn parameter error (Some "line 56") Exit (string_of_int agent_type)
    in
    (*-----------------------------------------------------------------------*)
    (*list of sites in a covering class*)
    let error, site_correspondence =
      match AgentMap.get parameter error agent_type site_correspondence with
      | error, None -> warn parameter error (Some "73") Exit []
      | error, Some a -> error, a
    in
    let error, site_correspondence =
      let rec aux list =
	match list with
	| [] -> warn parameter error (Some "line 68") Exit []
	| (h, list, _) :: _ when h = cv_id -> error, list
	| _ :: tail -> aux tail
      in aux site_correspondence
    in
    (*-----------------------------------------------------------------------*)
    (*build a pair of coresspondence map:
      - map1: global -> local; map2: local -> global*)
    let error, (map1, map2) =
      Bdu_build.new_index_pair_map parameter error site_correspondence
    in
    (*-----------------------------------------------------------------------*)
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter)  in
    (*-----------------------------------------------------------------------*)
    let error, handler_bdu =
      if local_trace
        || Remanent_parameters.get_trace parameter
      then
	let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "%sINTENSIONAL DESCRIPTION:" prefix
        in
	let () =
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
	in
        (*print bdu different: this will print in a format of bdu*)
	let () =
          Mvbdu_wrapper.Mvbdu.print parameter bdu_diff
        in
        (*print a list of relations: this will print in a format readable*)
	let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "%sEXTENSIONAL DESCRIPTION:" prefix
        in
	let () =
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
	in
	error, handler_bdu
      else
	error, handler_bdu
    in
    (*this is a function to convert a bdu of diff into a list.
      return a pair: (bdu, and a pair of (site, state) list of list)*)
    let error, handler_bdu, list =
      Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler_bdu error bdu_diff
    in
    (*-----------------------------------------------------------------------*)
    (*print function for extentional description*)
    let error =
      List.fold_left
	(fun error l ->
	 let error, bool =
	   List.fold_left
	     (fun (error, bool) (site_type, state) ->
               let error, site_type =
                 match Map.find_option parameter error site_type map2 with
                 | error, None -> warn parameter error (Some "line 100") Exit (-1)
                 | error, Some i -> error, i
               in
               (*-----------------------------------------------------------------------*)
	       let error, site_string =
		 try
                   Handler.string_of_site parameter error handler_kappa
		     agent_type site_type
		 with
		   _ -> warn parameter error (Some "line 136")
                     Exit (string_of_int site_type)
	       in
	       let error, state_string =
                 try
		   Handler.string_of_state_fully_deciphered parameter error handler_kappa
		     agent_type site_type state
		 with
		   _ -> warn parameter error (Some "line 146") Exit (string_of_int state)
               in
               (*-----------------------------------------------------------------------*)
               let () =
		 if bool
                 then Loggers.fprintf (Remanent_parameters.get_logger parameter) ","
		 else Loggers.fprintf (Remanent_parameters.get_logger parameter)
                   "%s%s(" prefix agent_string
               in
	       let () =
                 Loggers.fprintf (Remanent_parameters.get_logger parameter)
		   "%s%s" site_string state_string
               in
               error, true
             )
	     (error, false) l
	 in
         (*-----------------------------------------------------------------------*)
	 let () =
	   if bool
           then
	     let () =
	       Loggers.fprintf (Remanent_parameters.get_logger parameter) ")" in
	     Loggers.print_newline (Remanent_parameters.get_logger parameter)
	 in error)
	error list
    in error, handler_bdu
  else error, handler_bdu

(************************************************************************************)
(** [dump_valuation handler_bdu valuation] returns a pretty print of a
    valuation, according to its agent and the covering class that its
    belong to. The list of sites in each covering class is computed by
    their new indexes. This function also convert a valuation in bdu data
    structure into a list type. The printer will print the format of bdu
    and the format that easy to read*)

let dump_valuation parameter handler_kappa handler_bdu error
    site_correspondence agent_type cv_id valuation =
  if local_trace
    || Remanent_parameters.get_dump_reachability_analysis_diff parameter
    || Remanent_parameters.get_trace parameter
  then
    let prefix = Remanent_parameters.get_prefix parameter in
    (*-----------------------------------------------------------------------*)
    let error, agent_string =
      try
        Handler.string_of_agent parameter error handler_kappa agent_type
      with
        _ -> warn parameter error (Some "line 56") Exit (string_of_int agent_type)
    in
    (*-----------------------------------------------------------------------*)
    (*get a list of sites in a covering class*)
    let error, site_correspondence =
      match AgentMap.get parameter error agent_type site_correspondence with
      | error, None -> warn parameter error (Some "line 202") Exit []
      | error, Some a -> error, a
    in
    let error, site_correspondence =
      let rec aux list =
	match list with
	| [] -> warn parameter error (Some "line 68") Exit []
	| (h, list, _) :: _ when h = cv_id -> error, list
	| _ :: tail -> aux tail
      in aux site_correspondence
    in
    (*-----------------------------------------------------------------------*)
    let error, (map1, map2) =
      Bdu_build.new_index_pair_map parameter error site_correspondence
    in
    (*-----------------------------------------------------------------------*)
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    (*-----------------------------------------------------------------------*)
    let error, handler_bdu =
      if local_trace
        || Remanent_parameters.get_trace parameter
      then
	let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "%sINTENSIONAL DESCRIPTION:" prefix
        in
	let () =
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
	in
	let () =
          Mvbdu_wrapper.Mvbdu.print_association_list
            parameter valuation in
	let () =
          Loggers.fprintf (Remanent_parameters.get_logger parameter)
            "%sEXTENSIONAL DESCRIPTION:" prefix
	in
	let () =
	  Loggers.print_newline (Remanent_parameters.get_logger parameter)
        in
	error, handler_bdu
      else
	error, handler_bdu
    in
    (*-----------------------------------------------------------------------*)
    (*build a list*)
    let error, handler_bdu, list =
      Mvbdu_wrapper.Mvbdu.extensional_of_association_list
        parameter handler_bdu error valuation
    in
    (*-----------------------------------------------------------------------*)
    let error, bool =
      List.fold_left
	(fun (error, bool) (site_type, state) ->
          let error, site_type =
            match Map.find_option parameter error site_type map2 with
            | error, None -> warn parameter error (Some "262") Exit (-1)
            | error, Some i -> error, i
          in
          (*-----------------------------------------------------------------------*)
	  let error, site_string =
	    try
              Handler.string_of_site parameter error handler_kappa
		agent_type site_type
	    with
	      _ -> warn parameter error (Some "line 136") Exit (string_of_int site_type)
	  in
	  let error, state_string =
            try
	      Handler.string_of_state_fully_deciphered parameter error handler_kappa
		agent_type site_type state
	    with
	      _ -> warn parameter error (Some "line 146") Exit (string_of_int state)
          in
          (*-----------------------------------------------------------------------*)
          let () =
	    if bool
            then Loggers.fprintf (Remanent_parameters.get_logger parameter) ","
	    else Loggers.fprintf (Remanent_parameters.get_logger parameter)
              "%s%s(" prefix agent_string
          in
	  let () =
            Loggers.fprintf (Remanent_parameters.get_logger parameter)
	      "%s%s" site_string state_string
          in
          error, true
        )
	(error, false)
	list
    in
    let () =
      if bool
      then
	let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) ")" in
	let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
	()
    in
    error,handler_bdu
  else
    error,handler_bdu

(************************************************************************************)
(** [dump_cv_label site_correspondence] returns a pretty print of each
    covering class (the information of a list of sites) of each agent. *)

let dump_cv_label bool parameter handler_kappa error site_correspondence agent_type cv_id =
  if local_trace
    || Remanent_parameters.get_trace parameter
    || bool
  then
    let log = Remanent_parameters.get_logger parameter in
    let prefix = Remanent_parameters.get_prefix parameter in
    (*-----------------------------------------------------------------------*)
    let error, agent_string =
      try
        Handler.string_of_agent parameter error handler_kappa agent_type
      with
        _ -> warn parameter error (Some "line 56") Exit (string_of_int agent_type)
    in
    (*-----------------------------------------------------------------------*)
    let error, site_correspondence =
      AgentMap.get parameter error agent_type site_correspondence
    in
    let error, site_correspondence =
      match site_correspondence with
      | None -> warn parameter error (Some "line 187") Exit []
      | Some a -> error, a
    in
    (*get a list of sites in a covering class *)
    let error, site_correspondence =
      let rec aux list =
	match list with
	| [] -> warn parameter error (Some "line 195") Exit []
	| (h, list, _) :: _ when h = cv_id -> error, list
	| _ :: tail -> aux tail
      in aux site_correspondence
    in
    (*-----------------------------------------------------------------------*)
    (*output*)
    let () = Loggers.fprintf log "%s %s(" prefix agent_string in
    let error, _ =
      List.fold_left
	(fun (error, bool) site_type ->
	    let error, site_string =
	      try
                Handler.string_of_site parameter error handler_kappa
		  agent_type site_type
	      with
		_ -> warn parameter error (Some "line 210") Exit (string_of_int site_type)
	    in
	    let () =
	      Loggers.fprintf log "%s%s" (if bool then "," else "") site_string
	    in
	    error, true)
	(error, false) site_correspondence
    in
    let () = Loggers.fprintf log ")" in
    let () = Loggers.print_newline log  in
    error
  else
    error

(************************************************************************************)
(** [update handler_bdu bdu_test list_a bdu_X] returns the union of the bdu
    after redefine (the bdu redefine is the result of the intersection of
    bdu at a fixpoint with bdu that is tested with a list modified) with
    the bdu at a fixpoint. *)

(*Xn intersection with bdu_test and modif and then union with X_n*)

let compute_bdu_update_aux parameter handler error bdu_test list_a bdu_X =
  (*do the intersection X_n and bdu_test*)
  let error, handler, bdu_inter =
    Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_X bdu_test
  in
  (*redefine with modification list*)
  let error, handler, bdu_redefine =
    Mvbdu_wrapper.Mvbdu.mvbdu_redefine parameter handler error bdu_inter list_a
  in
  (*do the union of bdu_redefine and bdu_X*)
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_redefine bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(** [update_views handler_bdu bdu_test list_a bdu_X] is the computation of
    a bdu fixpoint when its views is tested. *)

let compute_bdu_update_views parameter handler error bdu_test list_a bdu_X =
  let error, handler, bdu_result =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(** [update_creation handler_bdu bdu_creation bdu_X] is the computaion of a
    bdu fixpoint when exists creation action. *)

let compute_bdu_update_creation parameter handler error bdu_creation bdu_X =
  let error, handler, bdu_result =
    Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_creation bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(** [update_side_effects handler_bdu bdu_test list_a bdu_X] is the
    computation of a bdu fixpoint when exists side effects properties. *)

let compute_bdu_update_side_effects parameter handler error bdu_test list_a bdu_X =
  let error, handler, bdu_result =
    compute_bdu_update_aux
      parameter
      handler
      error
      bdu_test
      list_a
      bdu_X
  in
  error, handler, bdu_result

(************************************************************************************)
(** [update_wl list_of_rules wl] returns a working list contents a list of
    rules that is tested/modified/both, according to its agent and the
    covering class that its belong to. This function also print out the
    information which rule should be investigated at each step of the
    iteration. *)

let add_update_to_wl ?title:(title="") parameter error handler_kappa compiled
    site_correspondence agent_type cv_id
    store_covering_classes_modification_update_full wl =
  (*-----------------------------------------------------------------------*)
  (*get a set of sites that are needed to add into working list*)
  let error, (_, s1) =
    match Int2Map_CV_Modif.Map.find_option_without_logs parameter error (agent_type, cv_id)
      store_covering_classes_modification_update_full
    with
    | error, None -> error, ([], Site_map_and_set.Set.empty)
    | error, Some (l, s) -> error, (l, s)
  in
  (*-----------------------------------------------------------------------*)
  (*print working list information*)
  let error =
    if local_trace
      || Remanent_parameters.get_dump_reachability_analysis_wl parameter
    then
      begin
	let log = Remanent_parameters.get_logger parameter in
        (*-----------------------------------------------------------------------*)
	let error, agent_string =
	  try
            Handler.string_of_agent parameter error handler_kappa agent_type
          with
            _ -> warn parameter error (Some "line 460") Exit (string_of_int agent_type)
	in
        (*-----------------------------------------------------------------------*)
        (*dump covering class label*)
	let error =
	  if title <> ""
	  then
	    let parameter_cv = Remanent_parameters.update_prefix parameter ("\t"^title) in
	    let error =
              (*true: print the site type inside covering class*)
              dump_cv_label true parameter_cv handler_kappa error
                site_correspondence agent_type cv_id
            in
	    error
	  else
	    error
	in
        (*-----------------------------------------------------------------------*)
	let () =
          Site_map_and_set.Set.iter (fun rule_id ->
	    (*mapping rule_id of type int -> string*)
	    let error, rule_id_string =
	      try
		Handler.string_of_rule parameter error handler_kappa
		  compiled rule_id
	      with
		_ -> warn parameter error (Some "line 250") Exit (string_of_int rule_id)
	    in
	    let tab =
	      if title = "" then "\t" else "\t\t"
	    in
	    let () = Loggers.fprintf log "%s%s(%s) should be investigated "
              (Remanent_parameters.get_prefix parameter) tab rule_id_string in
	    let () = Loggers.print_newline log in ())
	    s1 in
	error
      end
    else error
  in
  (*-----------------------------------------------------------------------*)
  Site_map_and_set.Set.fold (fun rule_id (error, wl) ->
    let error, wl = IntWL.push parameter error rule_id wl in
    error, wl
  ) s1 (error, wl)

(************************************************************************************)
(** [views_creation_test_potienal rule_id views creation test potential] returns 
    a map of bdu depending on the kind of agent at each rule. *)

let collect_map_views_creation_test_potential parameter error rule_id
    store_proj_bdu_views
    store_proj_bdu_creation_restriction_map
    store_proj_bdu_potential_restriction_map
    =
  let error, bdu_proj_views =
    match Map_rule_id_views.Map.find_option rule_id store_proj_bdu_views with
    | None -> error, Map_triple_views.Map.empty
    | Some m -> error, m
  in
  let error, bdu_creation_map =
    match Map_final_creation_bdu.Map.find_option rule_id
      store_proj_bdu_creation_restriction_map
    with
    | None -> error, Map_agent_type_creation_bdu.Map.empty
    | Some map -> error, map
  in
  let error, bdu_potential_map =
    match Map_final_potential_bdu.Map.find_option rule_id
      store_proj_bdu_potential_restriction_map
    with
    | None -> error, Map_agent_type_potential_bdu.Map.empty
    | Some map -> error, map
  in
  error, (bdu_proj_views, bdu_creation_map, (*bdu_test_map,*) bdu_potential_map)

(************************************************************************************)
(** [is_enable rule_id views map] returns true if the intersection of bdu
    of views that is tested and bdu at a fixpoint is not equal to the bdu
    of branch false at each rule, according to its agent and covering
    class. *)

exception False of Exception.method_handler * Mvbdu_wrapper.Mvbdu.handler
(* TO DO, do the thing cleanly with a forall in wrapped map module *)

let is_enable parameter handler error bdu_false rule_id
    bdu_proj_views store_bdu_update_map =
  try
    let error,handler =
      Map_triple_views.Map.fold
	(fun (agent_id, agent_type, cv_id) bdu_test (error, handler)->
	  let error, bdu_X =
            match Map_bdu_update.Map.find_option_without_logs parameter error
	      (agent_type, cv_id) store_bdu_update_map
            with
            | error, None -> error, bdu_false
            | error, Some bdu -> error, bdu
          in
         (*do the intersection of bdu_test and bdu_X*)
          let error, handler, bdu_inter =
            Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error bdu_test bdu_X
          in
         (*check is enable*)
          if Mvbdu_wrapper.Mvbdu.equal bdu_inter bdu_false
          then raise (False (error, handler))
          else error, handler
	) bdu_proj_views (error, handler)
    in error, handler, true
  with False (error, handler) -> error, handler, false

(************************************************************************************)
(** [new_views handler_bdu map wl] returns the new views information. *)

let compute_new_views parameter handler error
    handler_kappa
    compiled
    site_correspondence
    agent_type
    cv_id
    is_new_view
    store_covering_classes_modification_update_full
    wl_tl
    store_result =
  let error, (handler, new_wl, store_result) =
    if is_new_view
    then
      let error, new_wl =
        add_update_to_wl
	  parameter
          error
          handler_kappa
          compiled
	  site_correspondence
	  agent_type
	  cv_id
          store_covering_classes_modification_update_full
          wl_tl
      in
      error, (handler, new_wl, store_result)
    else
      error, (handler, wl_tl, store_result)
  in
  error, (handler, new_wl, store_result)

(************************************************************************************)
(** [views_enabled handler_bdu rule_id map] returns the result of a views
    that is enabled at each rule according to its agent and the covering
    class that its belong to. It deals with three different aspects: when
    the views is tested, when it has creation action and when side effects
    occurs. Add a new views when the old views is different than the
    current views. If it is a new views also print the information of the
    views difference at each iteration. *)

let compute_views_enabled parameter handler error
    handler_kappa
    compiled
    correspondence
    bdu_true
    bdu_false
    rule_id
    bdu_creation_map
    modif_list_map
    bdu_and_list_potential_map
    wl_tl
    store_covering_classes_modification_update_full
    bdu_proj_views
    store_bdu_update_map =
  (*-----------------------------------------------------------------------*)
  (* add_link should collect the list/set of (agent_type,cv_id) for which
     something has changed, so that add_update_to_wl can focus on these
     pairs *)
  let add_link parameter handler error correspondence (agent_type, cv_id)
      bdu_update store_result =
    let error, bdu_old =
      match Map_bdu_update.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, bdu_false
      | error, Some old -> error, old
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_update bdu_old
    in
    (*-----------------------------------------------------------------------*)
    if Mvbdu_wrapper.Mvbdu.equal bdu_union bdu_old
    then
      error, handler, false, store_result
    else
      (*print different views*)
      let parameter_view_diff = Remanent_parameters.update_prefix parameter "\t" in
      let error, handler =
        dump_view_diff parameter_view_diff handler_kappa
          handler error correspondence agent_type cv_id bdu_old bdu_union
      in
      (*add bdu union inside the result, return true if it discovers a new views*)
      let error, store_result =
        Map_bdu_update.Map.add_or_overwrite parameter error
          (agent_type, cv_id) bdu_union store_result
      in
      error, handler, true, store_result
  in
  (*-----------------------------------------------------------------------*)
  (*deal with views*)
  let parameter_cv =
    Remanent_parameters.update_prefix parameter "\t\tUpdating the views for"
  in
  let parameter_views = Remanent_parameters.update_prefix parameter "\t\t\t" in
  let error, (handler, wl_tl, store_result) =
    Map_triple_views.Map.fold
      (fun (agent_id, agent_type, cv_id) _ (error, (handler, wl_tl, store_result)) ->
        (*-----------------------------------------------------------------------*)
        (*print covering class label. Ex: A(x!,y!)*)
	let error =
          dump_cv_label (Remanent_parameters.get_dump_reachability_analysis_diff parameter)
            parameter_cv handler_kappa error correspondence agent_type cv_id
        in
	(*-----------------------------------------------------------------------*)
        let error, bdu_X =
          match Map_bdu_update.Map.find_option_without_logs parameter error
            (agent_type, cv_id) store_result
          with
          | error, None -> error, bdu_false
          | error, Some bdu -> error, bdu
        in
        let error, bdu_test =
          match Map_triple_views.Map.find_option
            (agent_id, agent_type, cv_id) bdu_proj_views
          with
          | None -> error, bdu_true
          | Some bdu -> error, bdu
        in
	let error, handler, bdu_update =
          match Map_modif_list.Map.find_option_without_logs parameter error
            (agent_id, agent_type, rule_id, cv_id) modif_list_map
	  with
          | error, None -> error, handler, bdu_X
	  | error, Some list_a ->
            compute_bdu_update_views
	      parameter_views
	      handler
	      error
	      bdu_test
	      list_a
	      bdu_X
        in
        (*-----------------------------------------------------------------------*)
        let error, handler, is_new_view, store_result =
          add_link parameter_views handler error correspondence
            (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, new_wl, store_result) =
          compute_new_views
            parameter_views
            handler
            error
            handler_kappa
            compiled
	    correspondence
	    agent_type
	    cv_id
            is_new_view
            store_covering_classes_modification_update_full
            wl_tl
            store_result
        in
        error, (handler, new_wl, store_result)
      )
      bdu_proj_views
      (error, (handler, wl_tl, store_bdu_update_map))
  in
  (*-----------------------------------------------------------------------*)
  (*start to deal with agent creation*)
  let error, (handler, wl_tl, store_result) =
    Map_agent_type_creation_bdu.Map.fold
      (fun (agent_type, cv_id) bdu_creation (error, (handler, wl_tl, store_result)) ->
       let error, bdu_X =
	  match Map_bdu_update.Map.find_option_without_logs parameter error
            (agent_type, cv_id) store_result
	  with
	  | error, None -> error, bdu_false
	  | error, Some bdu -> error, bdu
	in
        let error, handler, bdu_update =
          compute_bdu_update_creation
            parameter_views
            handler
            error
            bdu_creation
            bdu_X
	in
	let error, handler, is_new_view, store_result =
          add_link parameter_views handler error correspondence
            (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, wl_tl, store_result) =
          compute_new_views
            parameter_views
            handler
            error
            handler_kappa
            compiled
	    correspondence
	    agent_type
	    cv_id
            is_new_view
            store_covering_classes_modification_update_full
            wl_tl
            store_result
        in
        error, (handler, wl_tl, store_result)
      )
      bdu_creation_map
      (error, (handler, wl_tl, store_result))
  in
  (*-----------------------------------------------------------------------*)
  (*start fo deal with side effects*)
  let error, (handler, wl_tl, store_result) =
    Map_agent_type_potential_bdu.Map.fold
      (* JF: to do use a fold2 *)
      (fun (agent_type, new_site_id, cv_id) (bdu_test, list)
        (error, (handler, wl_tl, store_result)) ->
        let error, bdu_X =
	  match Map_bdu_update.Map.find_option_without_logs parameter error
            (agent_type, cv_id) store_result
	  with
	  | error, None -> error, bdu_false
	  | error, Some bdu -> error, bdu
	in
	let error, handler, bdu_update =
	  compute_bdu_update_side_effects
	    parameter_views
	    handler
	    error
	    bdu_test
	    list
	    bdu_X
        in
        let error, handler, is_new_view, store_result =
	  add_link parameter_views handler error correspondence
            (agent_type, cv_id) bdu_update store_result
        in
        (*-----------------------------------------------------------------------*)
        let error, (handler, wl_tl, store_result) =
	  compute_new_views
	    parameter_views
	    handler
	    error
	    handler_kappa
	    compiled
	    correspondence
	    agent_type
	    cv_id
	    is_new_view
	    store_covering_classes_modification_update_full
	    wl_tl
	    store_result
	in
	error, (handler, wl_tl, store_result)
      )
      bdu_and_list_potential_map
      (error, (handler, wl_tl, store_result))
  in
  error, (handler, wl_tl, store_result)

(************************************************************************************)
(** [fixpoint_init handler_bdu wl dead_rule map] starts the iteration 

fixpoint iteration with/without initial state*)

(*REMOVE this function later*)
let store_bdu_fixpoint_init_map parameter handler error handler_kappa bdu_false
    site_correspondence_in_covering_classes
    store_bdu_init_restriction_map =
  let log = Remanent_parameters.get_logger parameter in
  let add_link parameter handler error correspondence (agent_type, cv_id)
      bdu store_result =
    let error, bdu_old =
      match Map_bdu_update.Map.find_option_without_logs parameter error
        (agent_type, cv_id) store_result
      with
      | error, None -> error, bdu_false
      | error, Some bdu -> error, bdu
    in
    let error, handler, bdu_union =
      Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
      (*JF: this is a computation, thus you have to pass the handler *)
    in
    let parameter_views = Remanent_parameters.update_prefix parameter "\t" in
      (*print bdu different in views*)
    let error, handler =
      dump_view_diff parameter_views handler_kappa handler error
        correspondence agent_type cv_id bdu_old bdu_union
    in
    let error, result_map =
      Map_bdu_update.Map.add_or_overwrite parameter error
        (agent_type, cv_id) bdu_union store_result
    in
    error, handler, result_map
  in
  (*-----------------------------------------------------------------------*)
  (*in case having initial state the bdu_iter will be the union of bdu_init
    and bdu_iter*)
  let error, bool, handler, store_bdu_fixpoint_init_map =
    Map_init_bdu.Map.fold
      (fun (agent_type, cv_id) bdu (error, bool, handler, store_result) ->
	let () =
	  if not bool
	    &&
	      (local_trace
	       || Remanent_parameters.get_dump_reachability_analysis_diff parameter
	       || Remanent_parameters.get_trace parameter)
	  then
	    let () = Loggers.fprintf log "\tViews in initial state" in
	    let () = Loggers.print_newline log in
	    let () = Loggers.print_newline log in
	    ()
	in
	let error, handler, store_result =
          add_link parameter handler error
            site_correspondence_in_covering_classes 
            (agent_type, cv_id) bdu store_result
        in
        error, true, handler, store_result
      )
      store_bdu_init_restriction_map
      (error, false, handler, Map_bdu_update.Map.empty)
  in
  error, bool, handler, store_bdu_fixpoint_init_map
    
(*********************************************************************************)

let collect_bdu_fixpoint_with_init parameter handler error
    handler_kappa
    compiled
    bdu_true
    bdu_false
    site_correspondence_in_covering_classes
    (wl_creation:Fifo.IntWL.WSet.elt list * Fifo.IntWL.WSet.elt list *
       Fifo.IntWL.WSet.t)
    store_proj_bdu_creation_restriction_map
    modif_list_map
    store_proj_bdu_potential_restriction_map
    store_proj_bdu_views
    store_covering_classes_modification_update_full
    store_bdu_init_restriction_map
    dead_rule_array
    =
  (*-----------------------------------------------------------------------*)
    let log = Remanent_parameters.get_logger parameter in
    let add_link parameter handler error correspondence (agent_type, cv_id)
        bdu store_result =
      let error, bdu_old =
	match Map_bdu_update.Map.find_option_without_logs parameter error
          (agent_type, cv_id) store_result
	with
	| error, None -> error, bdu_false
	| error, Some bdu -> error, bdu
      in
      let error, handler, bdu_union =
	Mvbdu_wrapper.Mvbdu.mvbdu_or parameter handler error bdu_old bdu
      (*JF: this is a computation, thus you have to pass the handler *)
      in
      let parameter_views = Remanent_parameters.update_prefix parameter "\t" in
      (*print bdu different in views*)
      let error, handler =
	dump_view_diff parameter_views handler_kappa handler error
          correspondence agent_type cv_id bdu_old bdu_union
      in
      let error, result_map =
	Map_bdu_update.Map.add_or_overwrite parameter error
          (agent_type, cv_id) bdu_union store_result
      in
      error, handler, result_map
    in
  (*-----------------------------------------------------------------------*)
  (*in case having initial state the bdu_iter will be the union of bdu_init
    and bdu_iter*)
  let error, bool, handler, store_bdu_fixpoint_init_map =
    Map_init_bdu.Map.fold
      (fun (agent_type, cv_id) bdu (error, bool, handler, store_result) ->
	let () =
	  if not bool
	    &&
	      (local_trace
	       || Remanent_parameters.get_dump_reachability_analysis_diff parameter
	       || Remanent_parameters.get_trace parameter)
	  then
	    let () = Loggers.fprintf log "\tViews in initial state" in
	    let () = Loggers.print_newline log in
	    let () = Loggers.print_newline log in
	    ()
	in
	let error, handler, store_result =
          add_link parameter handler error
            site_correspondence_in_covering_classes (agent_type, cv_id) bdu store_result
        in
        error, true, handler, store_result
      )
      store_bdu_init_restriction_map
      (error, false, handler, Map_bdu_update.Map.empty)
  in
  let () =
    if not bool
      &&
	(local_trace
	 || Remanent_parameters.get_dump_reachability_analysis_diff parameter
	 || Remanent_parameters.get_trace parameter)
    then
      let () =
	Loggers.fprintf log "\tInitial state is empty"
      in
      let () = Loggers.print_newline log in
      let () = Loggers.print_newline log in
      ()
  in
  let () =
    if
      local_trace
      || Remanent_parameters.get_trace parameter
      || Remanent_parameters.get_dump_reachability_analysis_wl parameter
    then
      let () = Loggers.fprintf log "\tWake-up rules" in
      let () = Loggers.print_newline log in
      ()
  in
  (*-----------------------------------------------------------------------*)
  (*add update(c) into working list*)
  let error, wl_init_creation =
    Map_bdu_update.Map.fold
      (fun (agent_type, cv_id) _ (error, wl_init_creation) ->
	add_update_to_wl
	  ~title:"Dealing with"
	  parameter
          error
          handler_kappa
          compiled
	  site_correspondence_in_covering_classes
	  agent_type
          cv_id
          store_covering_classes_modification_update_full
          wl_init_creation)
      store_bdu_fixpoint_init_map
      (error, wl_creation)
  in
  (*-----------------------------------------------------------------------*)
  (*iterate function*)
  let rec aux acc_wl (error, handler, store_bdu_fixpoint_init_map, dead_rule_array) =
    if IntWL.is_empty acc_wl
    then
      error, (handler, store_bdu_fixpoint_init_map, dead_rule_array)
    else
      (*-----------------------------------------------------------------------*)
      (*pop the first element (rule_id) in this working list*)
      let error, (rule_id_op, wl_tl) = IntWL.pop parameter error acc_wl in
      match rule_id_op with
      | None ->
        warn parameter error (Some "888") Exit
          (handler, store_bdu_fixpoint_init_map, dead_rule_array)
      | Some rule_id ->
        (*----------------------------------------------------------------------*)
        (*output of rule that is enabled*)
    	let _ =
          if
	    local_trace
	    || (Remanent_parameters.get_dump_reachability_analysis_iteration parameter)
	    || (Remanent_parameters.get_trace parameter)
          then
            (*mapping rule_id of type int -> string*)
            let error, rule_id_string =
              try
                Handler.string_of_rule parameter error handler_kappa
		  compiled rule_id
              with
                _ -> warn parameter error (Some "line 795") Exit
		  (string_of_int rule_id)
            in
	    let () = Loggers.print_newline log in
            let () = Loggers.fprintf log "\tApplying %s:" rule_id_string in
	    let () = Loggers.print_newline log in
	     ()
        in
        (*--------------------------------------------------------------------*)
        let error,
          (bdu_proj_views, bdu_creation_map, bdu_and_list_potential_map) =
          collect_map_views_creation_test_potential
            parameter
            error
            rule_id
            store_proj_bdu_views
            store_proj_bdu_creation_restriction_map
            store_proj_bdu_potential_restriction_map
        in
        (*--------------------------------------------------------------------*)
        (*is for all bdu_test satisfy a covering_class?*)
        let error, handler, is_enable =
          is_enable
            parameter
            handler
            error
            bdu_false
            rule_id
            bdu_proj_views
            store_bdu_fixpoint_init_map
        in
        (*-----------------------------------------------------------------------*)
        begin
          if is_enable
          then
            (*dead_rule_array:when it is an enable rule set dead_rule_array is true*)
            let dead_rule_array =
              dead_rule_array.(rule_id) <- true;
              dead_rule_array
            in
            (*-----------------------------------------------------------------------*)
            (*output of rule that is enabled*)
            let _ =
              if
		local_trace
		|| Remanent_parameters.get_trace parameter
		|| Remanent_parameters.get_dump_reachability_analysis_iteration parameter
              then
		let () = Loggers.fprintf log "\t\tthe precondition is satisfied" in
		let () = Loggers.print_newline log in ()
            in
            (*-----------------------------------------------------------------------*)
            let error, (handler, new_wl, store_new_result) =
              compute_views_enabled
                parameter
                handler
                error
                handler_kappa
                compiled
		site_correspondence_in_covering_classes
                bdu_true
                bdu_false
		rule_id
                bdu_creation_map
                modif_list_map
                bdu_and_list_potential_map
                wl_tl
                store_covering_classes_modification_update_full
                bdu_proj_views
                store_bdu_fixpoint_init_map
            in
            aux new_wl (error, handler, store_new_result, dead_rule_array)
          else
            (*-----------------------------------------------------------------------*)
            (*output of rule that is disabled*)
            let _ =
              if local_trace
		 || (Remanent_parameters.get_dump_reachability_analysis_iteration parameter)
		 || (Remanent_parameters.get_trace parameter)
              then
                let () = Loggers.fprintf log "\t\tthe predcondition is not satisfied yet" in
		let () = Loggers.print_newline log in ()
            in
            (*-----------------------------------------------------------------------*)
            aux wl_tl (error, handler, store_bdu_fixpoint_init_map, dead_rule_array)
        end
  in
  (*start with init_map and union with initial state*)
  aux wl_init_creation (error, handler, store_bdu_fixpoint_init_map, dead_rule_array)

(************************************************************************************)
(*final fixpoint iteration*)

let collect_bdu_fixpoint_map parameter handler error
    handler_kappa
    compiled
    site_correspondence_in_covering_classes
    wl_creation
    store_proj_bdu_creation_restriction_map
    store_proj_modif_list_restriction_map
    store_proj_bdu_potential_restriction_map
    store_proj_bdu_views
    store_covering_classes_modification_update_full
    store_bdu_init_restriction_map
    init_dead_rule_array
  =
  let error, handler, bdu_false =
    Mvbdu_wrapper.Mvbdu.mvbdu_false parameter handler error
  in
  let error, handler, bdu_true =
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error
  in
  (*-----------------------------------------------------------------------*)
  (*fixpoint*)
  let error, (handler, store_bdu_fixpoint_map, dead_rule_array) =
     collect_bdu_fixpoint_with_init
       parameter
       handler
       error
       handler_kappa
       compiled
       bdu_true
       bdu_false
       site_correspondence_in_covering_classes
       wl_creation
       store_proj_bdu_creation_restriction_map
       store_proj_modif_list_restriction_map
       store_proj_bdu_potential_restriction_map
       store_proj_bdu_views
       store_covering_classes_modification_update_full
       store_bdu_init_restriction_map
       init_dead_rule_array
  in
  error, (handler, store_bdu_fixpoint_map, dead_rule_array)

(************************************************************************************)
(*BACKUP: DO NOT DELETE ME*)

(*side effects in the case of half break*)

(*check the reverse binding: B.x - A.x *)

(*let store_new_result_hb_map
    parameter
    error
    half_break_map
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
 let add_link (agent_type, cv_id) rule_id_effect store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty)
        (agent_type, cv_id) store_result in
    let errorm, current_set =
      Site_map_and_set.Set.add parameter error rule_id_effect old
    in
    let error, new_set =
      Site_map_and_set.Set.union parameter error current_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
        (l, new_set) store_result
    in
    error, result
 in
 Int2Map_HalfBreak_effect.Map.fold
   (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
     List.fold_left (fun (error, store_result) (rule_id_eff, state) ->
       Int2Map_syn.Map.fold
         (fun rule_id set (error, store_result) ->
           Set_pair.Set.fold
             (fun ((agent_type1, site_type1, state1),(agent_type2,site_type2, state2))
               (error, store_result) ->
                 if state = state2
                 then
                   if agent_type1 = agent_type_eff &&
                     site_type1 = site_type_eff
                   then
                     Int2Map_CV_Modif.Map.fold
                       (fun (agent_type, cv_id)
                         (l', rule_id_set) (error, store_result) ->
                           Site_map_and_set.Set.fold
                             (fun rule_id_update (error, current_result) ->
                               if agent_type = agent_type2
                               then
                                 let error, result =
                                   add_link (agent_type, cv_id) rule_id_eff
                                     current_result
                                 in
                                 error, result
                               else
                                 error, current_result
                             ) rule_id_set (error, store_result)
                       ) store_covering_classes_modification_update
                       (error, store_result)
                   else
                     error, store_result
                 else
                   error, store_result
             ) set (error, store_result)
         ) store_contact_map (error, store_result)
     ) (error, store_result) l2
   ) half_break_map (error, store_result_map)
 *)

(************************************************************************************)
(*side effects in the case of remove*)

(*let store_new_result_remove_map
  parameter
  error
  remove_map
  store_covering_classes_modification_update
  store_contact_map
  store_result_map
    =
  let add_link (agent_type, cv_id) rule_id_effect store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
	([], Site_map_and_set.Set.empty)
        (agent_type, cv_id) store_result in
    let errorm, current_set =
      Site_map_and_set.Set.add parameter error rule_id_effect old
    in
    let error, new_set =
      Site_map_and_set.Set.union parameter error current_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id)
        (l, new_set) store_result
    in
    error, result
  in
  Int2Map_Remove_effect.Map.fold
    (fun (agent_type_eff, site_type_eff) (l1, l2) (error, store_result) ->
      List.fold_left (fun (error, store_result) rule_id_eff ->
        Int2Map_syn.Map.fold
          (fun rule_id set (error, store_result) ->
            Set_pair.Set.fold
              (fun ((agent_type1, site_type1, state1),(agent_type2, site_type2, state2))
                (error, store_result) ->
                  if agent_type1 = agent_type_eff &&
                    site_type1 = site_type_eff
                  then
                    let error, store_result =
                      Int2Map_CV_Modif.Map.fold
                        (fun (agent_type, cv_id) (l', rule_id_set)
                          (error, store_result) ->
                            let error, store_result =
                              Site_map_and_set.Set.fold
                                (fun rule_id_update (error, current_result) ->
                                  if agent_type = agent_type2
                                  then
                                    add_link (agent_type, cv_id) rule_id_eff current_result
                                  else
                                    error, current_result
                                ) rule_id_set (error, store_result)
                            in
                            error, store_result
                        ) store_covering_classes_modification_update
                        (error, store_result)
                    in
                    error, store_result
                  else
                    error, store_result
              ) set (error, store_result)
          ) store_contact_map (error, store_result)
      ) (error, store_result) l2
    ) remove_map (error, store_result_map)
  *)

(************************************************************************************)
(*combine the result of half break and remove*)

(*let collect_hb_remove_map parameter error
    store_side_effects
    store_covering_classes_modification_update
    store_contact_map
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
        ([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let error, union =
      Site_map_and_set.Set.union parameter error
        rule_id_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, union) store_result
    in
    error, result
  in
  let (half_break_map, remove_map) = store_side_effects in
  let error, store_hb_map =
    store_new_result_hb_map
      parameter
      error
      half_break_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  let error, store_remove_map =
    store_new_result_remove_map
      parameter
      error
      remove_map
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  Int2Map_CV_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found
      in
      error
    )
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, cv_id) (l1, s1) (l2, s2) store_result ->
      let error', union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 331") Exit in
      let error, store_result =
        add_link error (agent_type, cv_id) union store_result
      in
      error, store_result
    )
    store_hb_map
    store_remove_map
    store_result_map
  *)

(************************************************************************************)
(*combine the result of update(c) and half break and remove side
  effects. This is final update function for side effect*)

(*let collect_update_hb_remove_map parameter error
    store_side_effects
    store_contact_map
    store_covering_classes_modification_update
    store_result_map
    =
  let add_link error (agent_type, cv_id) rule_id_set store_result =
    let (l, old) =
      Int2Map_CV_Modif.Map.find_default
        ([], Site_map_and_set.Set.empty) (agent_type, cv_id) store_result
    in
    let error, union =
      Site_map_and_set.Set.union parameter error
        rule_id_set old
    in
    let result =
      Int2Map_CV_Modif.Map.add (agent_type, cv_id) (l, union) store_result
    in
    error, result
  in
  let error, store_hb_remove_map =
    collect_hb_remove_map
      parameter
      error
      store_side_effects
      store_covering_classes_modification_update
      store_contact_map
      Int2Map_CV_Modif.Map.empty
  in
  Int2Map_CV_Modif.Map.fold2_with_logs
    (fun parameter error str str_opt exn ->
      let error, _ = warn parameter error str_opt exn Not_found
      in
      error
    )
    parameter
    error
    (*exists in 'a t*)
    (fun parameter error (agent_type, cv_id) (l1, s1) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s1 store_result
      in
      error, store_result
    )
    (*exists in 'b t*)
    (fun parameter error (agent_type, cv_id) (l2, s2) store_result ->
      let error, store_result =
        add_link error (agent_type, cv_id) s2 store_result
      in
      error, store_result
    )
    (*exists in both*)
    (fun parameter error (agent_type, cv_id) (l1, s1) (l2, s2) store_result ->
      let error', union = Site_map_and_set.Set.union parameter error s1 s2 in
      let error = Exception.check warn parameter error error' (Some "line 331") Exit in
      let error, store_result =
        add_link error (agent_type, cv_id) union store_result
      in
      error, store_result
    )
    store_hb_remove_map
    store_covering_classes_modification_update
    store_result_map
  *)
(************************************************************************************)
(*compute view that has new view and new bond*)

(*let compute_new_views parameter handler error
    agent_type
    cv_id
    is_new_view
    store_covering_classes_modification_update_full
    wl_tl
    store_result =
  let error, (handler, new_wl, store_result) =
    if is_new_view
    then
      let error, new_wl =
        add_update_to_wl
          parameter
          error
	  agent_type
	  cv_id
          store_covering_classes_modification_update_full
          wl_tl
      in
      error, (handler, new_wl, store_result)
    (*REMARK: will be used later when changed to module and use dynamic contact map*)
    (* begin
       if is_new_bond
       then
       let error, new_wl =
       add_update_to_wl
       parameter
       error
       agent_type
       cv_id
       store_new_result_map
       wl_tl
       in
       error, (handler, new_wl, store_result)
       else
       let error, new_wl =
       add_update_to_wl
       parameter
       error
       agent_type
       cv_id
       store_covering_classes_modification_update
       wl_tl
       in
       error, (handler, new_wl, store_result)
       end*)
    else
      error, (handler, wl_tl, store_result)
  in
  error, (handler, new_wl, store_result)*)
