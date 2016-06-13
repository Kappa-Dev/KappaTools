(**
  * export_KaSim.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: December, the 9th of 2014
  * Last modification: December, the 9th of 2014
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Export_to_KaSim")
    message exn (fun () -> default)

(*******************************************************************************)
(*type abstraction*)

type accuracy_level = Low | Medium | High | Full

module AccuracySetMap =
  SetMap.Make
    (struct
      type t = accuracy_level
      let compare a b =
        match
          a,b
        with
        | Low,Low -> 0
        | Low,_ -> -1
        | _,Low -> 1
        | Medium,Medium -> 0
        | Medium,_ -> -1
        | _,Medium -> 1
        | High, High -> 0
        | High,_ -> -1
        | _,High -> 1
        | Full,Full -> 0

      let print f = function
        | Full -> Format.fprintf f "Full"
        | High -> Format.fprintf f "High"
        | Medium -> Format.fprintf f "Medium"
        | Low -> Format.fprintf f "Low"
    end)

module AccuracyMap = AccuracySetMap.Map

type rule_id = int
type var_id =  int
type compilation = Cckappa_sig.compil

type influence_node =
| Rule of rule_id
| Var of var_id

module InfluenceNodeSetMap =
  SetMap.Make
    (struct
      type t = influence_node
      let compare = compare
      let print f = function
        | Rule r -> Format.fprintf f "Rule %i" r
        | Var r -> Format.fprintf f "Var %i" r
     end)

module InfluenceNodeMap = InfluenceNodeSetMap.Map

type location =
  | Direct of int
  | Side_effect of int
type 'a pair = 'a * 'a

type influence_map =
  {
    positive: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
    negative: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
  }

(*******************************************************************************)
(*module signatures*)

module type Export_to_KaSim =
  sig

    type state
    type contact_map =
      ((string list) * (string*string) list) Mods.StringMap.t Mods.StringMap.t

    val init:
      called_from:Remanent_parameters_sig.called_from ->
      ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil ->
      state

    val flush_errors: state -> state
    val get_contact_map:
      ?accuracy_level:accuracy_level -> state -> state * contact_map
    val get_influence_map:
      ?accuracy_level:accuracy_level -> state -> state * influence_map
    val get_signature: state -> state * Signature.s
    val get_most_accurate_contact_map:
      state -> contact_map option
    val get_most_accurate_influence_map: state -> influence_map option
    val dump_influence_map: accuracy_level -> state -> unit
    val dump_contact_map: accuracy_level -> state -> unit
    val dump_signature: state -> unit
    val dump_errors: state -> unit
    val dump_errors_light: state -> unit

  end

(*******************************************************************************)
(*structures of module*)

module Export_to_KaSim =
  (struct

    let string_of_influence_node x =
      match x with
      | Rule i -> "Rule "^(string_of_int i)
      | Var i -> "Var "^(string_of_int i)

    let print_influence_map parameters influence_map =
      Loggers.fprintf (Remanent_parameters.get_logger parameters) "Influence map:" ;
      Loggers.print_newline (Remanent_parameters.get_logger parameters);
      InfluenceNodeMap.iter
        (fun x y ->
           InfluenceNodeMap.iter
             (fun y labellist ->
                let () =
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    " %s->%s"
                    (string_of_influence_node x)
                    (string_of_influence_node y)
                in
                let () =
                  Loggers.print_newline
                    (Remanent_parameters.get_logger parameters) in
                ())
             y)
        influence_map.positive;
      InfluenceNodeMap.iter
        (fun x y ->
           InfluenceNodeMap.iter
             (fun y labellist ->
                let () =
                  Loggers.fprintf
                    (Remanent_parameters.get_logger parameters)
                    " %s-|%s"
                    (string_of_influence_node x) (string_of_influence_node y) in
                let () = Loggers.print_newline
                    (Remanent_parameters.get_logger parameters) in
                ())
             y)
        influence_map.negative;
      Loggers.print_newline
        (Remanent_parameters.get_logger parameters)

    let print_contact_map parameters contact_map =
      Loggers.fprintf (Remanent_parameters.get_logger parameters)  "Contact map: ";
      Loggers.print_newline (Remanent_parameters.get_logger parameters) ;
      Mods.StringMap.iter
	(fun x ->
	 Mods.StringMap.iter
           (fun y (l1,l2) ->
            if l1<>[]
            then
              begin
		let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s@%s: " x y in
		let _ = List.fold_left
			  (fun bool x ->
			   (if bool then
                              Loggers.fprintf (Remanent_parameters.get_logger parameters) ", ");
			   Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" x;
			   true)
			  false l1
		in
		Loggers.print_newline (Remanent_parameters.get_logger parameters)
              end
            else ();
            List.iter
              (fun (z,t) ->
               Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s@%s--%s@%s" x y z t;
               Loggers.print_newline (Remanent_parameters.get_logger parameters)
              ) l2
           )
	)
        contact_map

    (*-------------------------------------------------------------------------------*)
    (*type abstraction*)

    type contact_map = ((string list) * (string*string) list) Mods.StringMap.t Mods.StringMap.t

    type errors = Exception.method_handler

    type state =
      {
        parameters    : Remanent_parameters_sig.parameters  ;
        handler       : Cckappa_sig.kappa_handler ;
        compilation   : compilation ;
        internal_influence_map:
          (Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t *
           Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t)
            AccuracyMap.t ;
        influence_map : influence_map AccuracyMap.t;
        contact_map   : contact_map AccuracyMap.t ;
        signature     : Signature.s option;
        errors        : Exception.method_handler ;
      }

    (*-------------------------------------------------------------------------------*)
    (*operations of module signatuares*)

    let init ~called_from compil =
      let errors = Exception.empty_error_handler in
      let parameters =
        Remanent_parameters.get_parameters ~called_from () in
      let () =
        match called_from with
        | Remanent_parameters_sig.Internalised -> assert false
        | Remanent_parameters_sig.KaSim
        | Remanent_parameters_sig.JS
        | Remanent_parameters_sig.KaSa -> ()
      in
      let parameters_compil =
        Remanent_parameters.update_call_stack
          parameters Preprocess.local_trace (Some "Prepreprocess.translate_compil")
      in
      let errors,refined_compil =
        Prepreprocess.translate_compil parameters_compil errors compil
      in
      let parameters_list_tokens =
        Remanent_parameters.update_call_stack
          parameters List_tokens.local_trace (Some "List_tokens.scan_compil")
      in
      let errors,handler =
        List_tokens.scan_compil parameters_list_tokens errors refined_compil
      in
      let parameters_sig =
        Remanent_parameters.update_prefix parameters "Signature:"
      in
      let errors =
        if Remanent_parameters.get_trace parameters || Print_handler.trace
        then Print_handler.print_handler parameters_sig errors handler
        else errors
      in
      let parameters_c_compil =
        Remanent_parameters.update_call_stack
          parameters Preprocess.local_trace (Some "Preprocess.translate_c_compil")
      in
      let errors, handler, c_compil =
        Preprocess.translate_c_compil
          parameters_c_compil errors handler refined_compil
      in
      {
        handler       = handler ;
        compilation   = c_compil ;
        parameters    = parameters ;
        contact_map   = AccuracyMap.empty;
        internal_influence_map = AccuracyMap.empty;
        influence_map = AccuracyMap.empty;
        signature     = None;
        errors        = errors
      }

    let flush_errors state =
      {
        state with
          errors = Exception.empty_error_handler
      }

    let compute_raw_contact_map state =
      let sol        = ref Mods.StringMap.empty in
      let handler    = state.handler in
      let parameters = state.parameters in
      let error      = state.errors in
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "+ Compute the contact map"
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters) in
      (*----------------------------------------------------------------*)
      let add_link (a,b) (c,d) sol =
	let sol_a = Mods.StringMap.find_default Mods.StringMap.empty a sol in
        let l,old = Mods.StringMap.find_default ([],[]) b sol_a in
        Mods.StringMap.add a (Mods.StringMap.add b (l,((c,d)::old)) sol_a) sol
      in
      (*----------------------------------------------------------------*)
      let add_internal_state (a,b) c sol =
        match c with
        | Ckappa_sig.Binding _ -> sol
        | Ckappa_sig.Internal state ->
	   let sol_a = Mods.StringMap.find_default Mods.StringMap.empty a sol in
           let old,l = Mods.StringMap.find_default ([],[]) b sol_a in
           Mods.StringMap.add a (Mods.StringMap.add b (state::old,l) sol_a) sol
      in
      (*----------------------------------------------------------------*)
      let simplify_site site =
        match site with
        | Ckappa_sig.Binding site_name
        | Ckappa_sig.Internal site_name -> site_name
      in
      (*----------------------------------------------------------------*)
      let error =
        Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.iter
          parameters error
          (fun parameters error (i,j) s  ->
             let error,ag =
               Handler.translate_agent parameters error handler i
             in
             let error,site =
               Handler.translate_site parameters error handler i j
             in
             let site = simplify_site site in
             let error =
               Ckappa_sig.Dictionary_of_States.iter
                 parameters error
                 (fun parameters error s state  () () ->
                    let () =
                      sol := add_internal_state (ag,site) state (!sol)
                    in
                    error)
                 s
             in
             error)
          handler.Cckappa_sig.states_dic
      in
      (*----------------------------------------------------------------*)
      let sol = !sol in
      let error, sol =
        Ckappa_sig.Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
          parameters error
          (fun parameters error (i, (j , k)) (i', j', k') sol ->
             let error, ag_i =
               Handler.translate_agent parameters error handler i
             in
             let error, site_j =
               Handler.translate_site parameters error handler i j
             in
             let site_j = simplify_site site_j in
             let error, ag_i' =
               Handler.translate_agent parameters error handler i'
             in
             let error, site_j' =
               Handler.translate_site parameters error handler i' j'
             in
             let site_j' = simplify_site site_j' in
             let sol = add_link (ag_i,site_j) (ag_i',site_j') sol in
             error, sol)
          handler.Cckappa_sig.dual sol
      in
      let sol =
        Mods.StringMap.map (Mods.StringMap.map (fun (l,x) -> List.rev l,x)) sol
      in
      {state
       with contact_map = AccuracyMap.add Low sol state.contact_map ;
            errors = error }

    let convert_label a =
      if a<0 then Side_effect (-(a+1))
      else Direct a

    let convert_id x nrules =
      if x<nrules
      then
        Rule x
      else
        Var (x-nrules)

    let convert_influence_map influence nrules  =
        Ckappa_sig.PairRule_setmap.Map.fold
          (fun (x,y) list map ->
             let x = convert_id (Ckappa_sig.int_of_rule_id x) nrules in
             let y = convert_id (Ckappa_sig.int_of_rule_id y) nrules in
             let old =
               match
                 InfluenceNodeMap.find_option x map
               with
               | None -> InfluenceNodeMap.empty
               | Some x -> x
             in
             let list =
               Quark_type.Labels.convert_label_set_couple list
             in
             let list =
               List.rev_map
                 (fun (a,b) -> convert_label a,convert_label b)
                 (List.rev list)
             in
             InfluenceNodeMap.add x
               (InfluenceNodeMap.add y list old)
               map
          )
          influence
          InfluenceNodeMap.empty

    let compute_raw_influence_map state =
      let parameters = state.parameters in
      let error = state.errors in
      let handler = state.handler in
      let compil = state.compilation in
      let nrules = Handler.nrules parameters error handler in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "+ Generating the raw influence map"
      in
      let () =
        Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      let parameters_quark =
        Remanent_parameters.update_call_stack
          parameters Quark.local_trace (Some "Quark.quarkify")
      in
      let parameters_quark =
        Remanent_parameters.update_prefix parameters_quark "Quarks:"
      in
      let error,quark_map =
        Quark.quarkify parameters_quark error handler compil
      in
      let parameters_influence_map =
        Remanent_parameters.update_prefix
          parameters "Influence_map:"
      in
      let error,wake_up_map,inhibition_map =
        Influence_map.compute_influence_map parameters_influence_map
          error handler quark_map nrules
      in
      {
        state
        with
          errors = error ;
          influence_map =
            AccuracyMap.add
              Low
              {
                positive = convert_influence_map wake_up_map nrules ;
                negative = convert_influence_map inhibition_map nrules ;
              }
              state.influence_map;
          internal_influence_map =
            AccuracyMap.add
              Low
              (wake_up_map,inhibition_map)
              state.internal_influence_map
      }

    let rec get_raw_influence_map state =
      match
        AccuracyMap.find_option
          Low
          state.internal_influence_map
      with
      | None ->
        let state = compute_raw_influence_map state in
        get_raw_influence_map state
      | Some map ->
        state,
        map

    let compute_intermediary_influence_map state =
      let state,(wake_up_map,inhibition_map) =
        get_raw_influence_map state
      in
      let parameters = state.parameters in
      let error = state.errors in
      let handler = state.handler in
      let compil = state.compilation in
      let nrules = Handler.nrules parameters error handler in
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "+ Refining the influence map"
      in
      let () = Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      let error,wake_up_map =
        Algebraic_construction.filter_influence
          parameters error handler compil wake_up_map true
      in
      let error,inhibition_map =
        Algebraic_construction.filter_influence
          parameters error handler compil inhibition_map false
      in
      {
        state
        with
          errors = error ;
          influence_map =
            AccuracyMap.add
              Medium
              {
                positive = convert_influence_map wake_up_map nrules ;
                negative = convert_influence_map inhibition_map nrules ;
              }
              state.influence_map;
          internal_influence_map =
            AccuracyMap.add
              Medium
              (wake_up_map,inhibition_map)
              state.internal_influence_map
      }


    let compute_contact_map accuracy_level state =
      match
        accuracy_level
      with
      | Low -> compute_raw_contact_map state
      | Medium | High | Full -> compute_raw_contact_map state

    let correct_accuracy_level_in_contact_map =
      function
      | Low -> Low
      | Medium -> Low
      | High -> Low
      | Full -> Low

    let rec get_contact_map ?accuracy_level:(accuracy_level=Low) state =
      let accuracy_level =
        correct_accuracy_level_in_contact_map accuracy_level
      in
      match
        AccuracyMap.find_option accuracy_level state.contact_map
      with
      | Some x -> state,x
      | None ->
        get_contact_map
          ~accuracy_level
          (compute_contact_map accuracy_level state)

    let correct_accuracy_level_in_influence_map =
          function
          | Low -> Low
          | Medium -> Medium
          | High -> Medium
          | Full -> Medium

    let compute_influence_map accuracy_level state =
      match
        accuracy_level
      with
      | Low ->
        compute_raw_influence_map state
      | Medium | High | Full ->
        compute_intermediary_influence_map state

    let rec get_influence_map ?accuracy_level:(accuracy_level=Low) state =
      let accuracy_level =
        correct_accuracy_level_in_influence_map accuracy_level
      in
      match
        AccuracyMap.find_option accuracy_level state.influence_map
      with
      | Some x -> state,x
      | None ->
        get_influence_map
          ~accuracy_level
          (compute_influence_map accuracy_level state)

    let compute_signature state =
      let state,l = get_contact_map state in
      let l =
        Mods.StringMap.fold
          (fun a interface list ->
           (Location.dummy_annot a ,
            Mods.StringMap.fold
              (fun x (states,_binding) acc ->
               {
                 Ast.port_nme = Location.dummy_annot x ;
                 Ast.port_int =
                   List.rev_map
                     (fun s -> Location.dummy_annot s)
                     (List.rev states);
                 Ast.port_lnk = Location.dummy_annot Ast.FREE}::acc)
	      interface [])::list)
          l [] in
      {state with signature = Some (Signature.create l)}

    let rec get_signature state =
      match state.signature with
      | Some x -> state,x
      | None -> get_signature (compute_signature state)

    let find_most_precise map =
      match
        AccuracyMap.max_key map
      with
      | None -> None
      | Some key ->
        AccuracyMap.find_option key map

    let get_most_accurate_contact_map state =
      find_most_precise state.contact_map

    let get_most_accurate_influence_map state =
      find_most_precise state.influence_map

    let dump_influence_map accuracy state =
      match
        AccuracyMap.find_option accuracy state.influence_map
      with
      | None -> ()
      | Some influence_map ->
        print_influence_map state.parameters influence_map

    let dump_contact_map accuracy state =
      match
        AccuracyMap.find_option accuracy state.contact_map
      with
      | None -> ()
      | Some contact_map ->
        print_contact_map state.parameters contact_map

    let dump_signature state =
      match state.signature with
      | None -> ()
      | Some signature -> ()

    let dump_errors state =
      Exception.print state.parameters state.errors

    let dump_errors_light state =
      Exception.print_errors_light_for_kasim state.parameters state.errors

   end:Export_to_KaSim)
