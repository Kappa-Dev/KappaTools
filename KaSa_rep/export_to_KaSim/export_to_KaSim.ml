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

type rule_id = string
type var_id = string

type compilation = Cckappa_sig.compil
type influence_node =
  | Rule of rule_id
  | Var of var_id

module InfluenceNodeSetMap =
  SetMap.Make (struct type t = influence_node let compare = compare end)
module InfluenceNodeMap = InfluenceNodeSetMap.Map
module String2SetMap =
  SetMap.Make (struct type t = string*string let compare = compare end)
module String2Map = String2SetMap.Map
module StringMap = Mods.StringMap

type influence_map =
    {
      positive: influence_node InfluenceNodeMap.t ;
      negative: influence_node InfluenceNodeMap.t ;
    }

module type Export_to_KaSim =
  sig
    type state
    val init:
      ((string Location.annot) * Ast.port list,
       Ast.mixture, string, Ast.rule) Ast.compil -> state
    val get_influence_map: state -> state * influence_map
    val get_contact_map:
      Format.formatter -> state -> state * (string list * (string*string) list) String2Map.t
    val get_signature:
      Format.formatter -> state -> state * Signature.s

    val dump_errors: state -> unit
    val dump_errors_light: state -> unit
    val flush_errors: state -> state

    val dump_influence_map: state -> unit
    val dump_contact_map: state -> unit
    val dump_signature: state -> unit
  end

module Export_to_KaSim =
  (struct
    let string_of_influence_node x =
      match x with
      | Rule i -> "Rule "^i
      | Var i -> "Var "^i

    let print_influence_map parameters influence_map =
      Printf.fprintf parameters.Remanent_parameters_sig.log "Influence map: \n";
      InfluenceNodeMap.iter
	(fun x y ->
	 Printf.fprintf parameters.Remanent_parameters_sig.log"  %s->%s \n"
			(string_of_influence_node x) (string_of_influence_node y))
	influence_map.positive;
      InfluenceNodeMap.iter
	(fun x y ->
	 Printf.fprintf parameters.Remanent_parameters_sig.log " %s-|%s \n"
			(string_of_influence_node x) (string_of_influence_node y))
	influence_map.negative;
      Printf.fprintf parameters.Remanent_parameters_sig.log "\n"

    let print_contact_map parameters contact_map =
      Printf.fprintf parameters.Remanent_parameters_sig.log  "Contact map: \n";
      String2Map.iter
	(fun (x,y) (l1,l2) ->
	    if l1<>[]
	    then
	      begin
		let _ = Printf.fprintf parameters.Remanent_parameters_sig.log "%s@%s: " x y in 
		let _ = List.fold_left
			  (fun bool x ->
			   (if bool then 
			       Printf.fprintf parameters.Remanent_parameters_sig.log ", ");
			    Printf.fprintf parameters.Remanent_parameters_sig.log "%s" x;
			   true)
			  false l1 in
		Printf.fprintf parameters.Remanent_parameters_sig.log "\n"
	      end
	    else ();
	    List.iter
	      (fun (z,t) ->
		Printf.fprintf parameters.Remanent_parameters_sig.log "%s@%s--%s@%s\n" x y z t 
	      ) l2
	)
	contact_map

    type contact_map = ((string list) * (string*string) list) String2Map.t

    type errors = Exception.method_handler

    type state =
	{
	  parameters: Remanent_parameters_sig.parameters  ;
	  handler: Cckappa_sig.kappa_handler ;
	  compilation: compilation ;
	  influence_map:influence_map option ;
	  contact_map:contact_map  option ;
	  signature: Signature.s option;
	  errors: Exception.method_handler ;
	}

    let init compil  =
      let errors = Exception.empty_error_handler in
      let parameters = Remanent_parameters.get_parameters () in   
      let parameters_compil =
	Remanent_parameters.update_call_stack
	  parameters Preprocess.local_trace (Some "Prepreprocess.translate_compil") in
      let errors,refined_compil =
	Prepreprocess.translate_compil parameters_compil errors compil in
      let parameters_list_tokens =
	Remanent_parameters.update_call_stack
	  parameters List_tokens.local_trace (Some "List_tokens.scan_compil") in
      let errors,handler =
	List_tokens.scan_compil parameters_list_tokens errors refined_compil in
      let parameters_sig =
	Remanent_parameters.update_prefix parameters "Signature:" in
      let errors =
	if Remanent_parameters.get_trace parameters || Print_handler.trace
	then Print_handler.print_handler parameters_sig errors handler
	else errors
      in
      let parameters_c_compil =
	Remanent_parameters.update_call_stack
	  parameters Preprocess.local_trace (Some "Preprocess.translate_c_compil") in
      let errors,handler,c_compil =
	Preprocess.translate_c_compil
	  parameters_c_compil errors handler refined_compil in

	{
	  handler = handler ;
	  compilation = c_compil ;
	  parameters = parameters ;
	  contact_map = None;
	  influence_map = None;
	  signature = None;
	  errors=errors
	}

    let flush_errors state =
      {state with errors = Exception.empty_error_handler}

    let compute_contact_map f state =
      let sol = ref String2Map.empty in
      let handler = state.handler in
      let parameters = state.parameters in
      let error = state.errors in
      let _ = Format.fprintf f "+ Compute the contact map@," in
      let add_link (a,b) (c,d) sol =
	let l,old =
	  String2Map.find_default ([],[]) (a,b) sol
	in
	String2Map.add (a,b) (l,((c,d)::old)) sol
      in
      let add_internal_state (a,b) c sol =
	match c
	with
	| Ckappa_sig.Binding _ -> sol
	| Ckappa_sig.Internal state ->
	   let old,l =
	     String2Map.find_default ([],[]) (a,b) sol
	   in
	   String2Map.add (a,b) (state::old,l) sol
      in
      let simplify_site site =
        match site with
	| Ckappa_sig.Binding site_name
	| Ckappa_sig.Internal site_name -> site_name
      in
      let _ =
	Ckappa_sig.Dictionary_of_agents.print
          parameters error
          (fun parameters error i agent_name () () ->
           let error,site_dic =
	     Misc_sa.unsome
	       (Int_storage.Nearly_inf_Imperatif.get
		  parameters error i handler.Cckappa_sig.sites)
	       (fun error ->
		warn parameters error (Some "line 103") Exit
		     (Ckappa_sig.Dictionary_of_sites.init ()))
           in
           let error =
	     Ckappa_sig.Dictionary_of_sites.print
	       parameters error
	       (fun parameters_dot error j site () () ->
                let _ =
                  sol:=String2Map.add (agent_name,simplify_site site) ([],[]) (!sol)
                in
                error)
	       site_dic
           in
	   error)
      in
      let error =
	Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.iter
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
	     Cckappa_sig.Dictionary_of_States.print
	       parameters error
	       (fun parameters error s state  () () ->
		let () =
		  sol:=add_internal_state (ag,site) state (!sol)
		in
		error)
	       s
	   in
	   error)
	  handler.Cckappa_sig.states_dic
      in
      let sol = !sol in
      let error,sol =
        Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.fold
          parameters error
          (fun parameters error (i,(j,k)) (i',j',k') sol ->
	   let error,ag_i =
	     Handler.translate_agent parameters error handler i
	   in
	   let error,site_j =
	     Handler.translate_site parameters error handler i j
	   in
	   let site_j = simplify_site site_j in
	   let error,ag_i' =
	     Handler.translate_agent parameters error handler i'
	   in
	   let error,site_j' =
	     Handler.translate_site parameters error handler i' j'
	   in
	   let site_j' = simplify_site site_j' in
	   let sol = add_link (ag_i,site_j) (ag_i',site_j') sol in
           error,sol)
          handler.Cckappa_sig.dual sol
      in
      let sol =
	String2Map.map (fun (l,x) -> List.rev l,x) sol
      in
      {state with contact_map = Some sol ; errors = error }

    let compute_influence_map state = state
      
    let rec get_contact_map f state =
      match state.contact_map with
      | Some x -> state,x
      | None ->
	 get_contact_map f (compute_contact_map f state)

    let rec get_influence_map state =
      match state.influence_map with
      | Some x -> state,x
      | None ->
	 get_influence_map (compute_influence_map state)

     let compute_signature f state = 
      let state,contact_map = get_contact_map f state in 
      let add a x states map = 
	let old = 
	    StringMap.find_default [] a map 
	in 
	StringMap.add a ((x,states)::old) map
      in 
      let l = 
	String2Map.fold 
	  (fun (a,x) (states,binding) map -> 
	    add a x (states,binding) map
	  )
	  contact_map
	  StringMap.empty
      in 
      let l = 
	StringMap.fold 
	  (fun a interface list -> 
	    (Location.dummy_annot a ,
	     
	     List.rev_map 
	       (fun (x,(states,binding)) -> 
		 { 
		   Ast.port_nme = Location.dummy_annot x ;
		   Ast.port_int = 
		     List.rev_map
		       (fun s -> Location.dummy_annot s)
		       (List.rev states);
		   Ast.port_lnk = Location.dummy_annot Ast.FREE})
	       (List.rev interface))::list)
	  l 
	  []
      in 
      {state with signature = Some (Signature.create l)}


	
    let rec get_signature f state =
      match state.signature with
      | Some x -> state,x
      | None -> get_signature f (compute_signature f state)
	   
    let dump_influence_map state =
      match state.influence_map with
      | None -> ()
      | Some influence_map ->
	 print_influence_map state.parameters influence_map

    let dump_contact_map state =
	match state.contact_map with
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
