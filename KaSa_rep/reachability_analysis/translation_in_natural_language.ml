let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Translation_in_natural_language.ml") message exn (fun () -> default)  

let trace = false

type token =
  | Range of int * int list
  | Equiv of (int * int) * (int * int)
  | Imply of (int * int) * (int * int)
  | Partition of (int * (int * token list) list)
  | No_known_translation of (int * int) list list

type rename_sites =   (Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.Site_map_and_set.Map.elt -> Exception.method_handler * Cckappa_sig.Site_map_and_set.Map.elt) 

let non_relational parameter handler error mvbdu = 
  let error, handler, list = 
    Mvbdu_wrapper.Mvbdu.mvbdu_cartesian_abstraction parameter handler error mvbdu 
  in
  let error, handler, mvbdu_true = 
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error
  in
  let error, handler, recomposition = 
    List.fold_left
      (fun (error,handler,conjunct) term ->
	  Mvbdu_wrapper.Mvbdu.mvbdu_and parameter handler error conjunct term)
      (error, handler, mvbdu_true) list
  in
  error, handler, Mvbdu_wrapper.Mvbdu.equal mvbdu recomposition

let try_partitioning parameter handler error rename_site_inverse mvbdu = 
  let error, handler, mvbdu_true =
    Mvbdu_wrapper.Mvbdu.mvbdu_true parameter handler error
  in
  let error, handler, var_hconsed_list =
    Mvbdu_wrapper.Mvbdu.variables_list_of_mvbdu parameter handler error mvbdu
  in
  let error, handler, var_list =
    Mvbdu_wrapper.Mvbdu.extensional_of_variables_list parameter handler error var_hconsed_list
  in
  let rec aux l (error,handler) =
    match 
      l
    with 
    | [] -> error, handler, None
    | head::tail ->
      let error, handler, singleton = Mvbdu_wrapper.Mvbdu.build_variables_list parameter handler error [head] in
      let error, hnadler, mvbdu_ref = Mvbdu_wrapper.Mvbdu.mvbdu_project_abstract_away parameter handler error mvbdu singleton in
      let error, handler, proj_in = Mvbdu_wrapper.Mvbdu.mvbdu_project_keep_only parameter handler error mvbdu singleton in
      let error, handler, list_asso = Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error proj_in in
      let error, range = 
	let rec aux2 list (error,output) = 
	  match 
	    list
	  with
	  | [] -> (error,output)
	  | [(x,i)]::tail when x=head -> 
	      aux2 tail (error,(i::output))
	  | _::tail -> 
	    aux2 tail (warn parameter error (Some "line 54") Exit output)
	in aux2 list_asso (error,[])
      in
      let rec aux3 list (error,handler,output) =
	match 
	  list
	with 
	| [] -> error, handler, Some output
	| h::t -> 
	  begin
	    let error,handler,select = 
	      Mvbdu_wrapper.Mvbdu.build_association_list
		parameter handler error [head,h]
	    in
	    let error,handler,mvbdu_case = 
	      Mvbdu_wrapper.Mvbdu.mvbdu_redefine 
		parameter handler error mvbdu_true select
	    in
	    let error,handler,case = 
	      Mvbdu_wrapper.Mvbdu.mvbdu_and 
		parameter handler error mvbdu_case mvbdu
	    in
	    let error,handler,bool =
	      non_relational parameter handler error case
	    in
	    if bool 
	    then 
	      let error,handler,away =
		Mvbdu_wrapper.Mvbdu.mvbdu_project_abstract_away
		  parameter handler error case singleton
	      in
	      if 
		Mvbdu_wrapper.Mvbdu.equal away mvbdu_ref
	      then
		aux3 t (error,handler,output)
	      else
	       	let error,handler,list = 
		  Mvbdu_wrapper.Mvbdu.mvbdu_cartesian_abstraction parameter handler error away
		in	
		let error,handler,list =
		  List.fold_left
		    (fun (error,handler,list) elt ->
		      let error,handler,elt =
			Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error elt
		      in
		      begin
			let error,var_list_opt = 
			  match 
			    elt
			  with 
			  | [] | []::_ | ((_,_)::_::_)::_ 
			  -> error,None
			  | [(a,b)]::q -> 
			    begin
				let rec aux4 q output = 
				  match q
				  with 
				  | [] -> error,Some (a,output)
				  | [(c,d)]::q when c=a -> 
				      aux4 q (d::output)
				  | _ -> error,None
				in aux4 q [b]
			      end
			in
			match
			  var_list_opt
			with
			| None -> 
			  let error,() = warn parameter error (Some "line 127") Exit () in
			  error,handler,list
			| Some (a,l) ->
			  let error,a' = rename_site_inverse parameter error a in
			  (error,handler, 
			     ((Range (a',l))::list))
		      end)
		    (error,handler,[])
		    (List.rev list)
		in
		aux3 t (error,handler,((h,list)::output))
	    else 
	      error,handler,None
	  end
      in 
      let error, handler, output = aux3 range (error,handler,[]) in
      match output with 
      | None -> 
	aux tail (error,handler)
      | Some l -> 
	let error,head = rename_site_inverse parameter error head in
	error,handler,Some (head,l)
  in
  aux var_list (error,handler)
    


let translate parameter handler error rename_site_inverse mvbdu =
  let error, handler, list = 
    Mvbdu_wrapper.Mvbdu.extensional_of_mvbdu parameter handler error mvbdu 
  in
  let error,list =
    List.fold_left
      (fun (error,list) elt1 ->
       let error,elt1 =
	 List.fold_left
	   (fun (error,list) (elt2,asso) ->
	    let error,elt2 = rename_site_inverse parameter error elt2 in
	    error,(elt2,asso)::list
	   )
	   (error,[]) (List.rev elt1)
       in
       error,elt1::list)
      (error,[])
      (List.rev list)
  in
  if Remanent_parameters.get_use_natural_language parameter
  then
    begin
       let error, handler, vars =
	 Mvbdu_wrapper.Mvbdu.variables_list_of_mvbdu parameter handler error mvbdu
       in
       let error, handler, var_list =
	 Mvbdu_wrapper.Mvbdu.extensional_of_variables_list parameter handler error vars
       in
       let error, var_list =
	 List.fold_left
	   (fun (error,list) elt ->
	    let error,elt = rename_site_inverse parameter error elt in
	    error,elt::list)
	   (error,[])
	   (List.rev var_list)
       in
       match
	 var_list
       with
       | [] -> error,(handler, No_known_translation list) (* OK if the agent has no sites *)
       | [x] ->
	  let error,list =
	    List.fold_left
	      (fun (error,list) elt ->
	       match elt with [a,b] when a=x -> error,b::list
			    | _ -> warn parameter error (Some "line 128") Exit list)
	      (error,[])
	      list
	  in
	  error, (handler, Range (x,list))
       | [x;y] ->
	  begin
	    match
	      list
	    with
	    | [] | [_] -> warn parameter error (Some "line 138") Exit (handler, No_known_translation list)
	    | [[site1,state1;site2,state2];[site1',_;site2',_]] ->
	       begin
		 if site1 = site1' && site2 = site2'
		 then
		   error, (handler, Equiv ((site1, state1), (site2, state2)))
		 else
		   warn parameter error (Some "line 144") Exit (handler, No_known_translation list)
	       end
	    | [[site1,state1;site2,state2];[site1',state1';site2',state2'];[site1'',state1'';site2'',state2'']] ->
	       begin
		 if site1 = site1' && site1 = site1'' && site2 = site2' && site2 = site2''
		 then
		   if state1 = state1'
		   then  error, (handler, Imply ((site1,state1''),(site2,state2'')))
		   else if state1 = state1''
		   then  error, (handler, Imply ((site1,state1'),(site2,state2')))
		   else if state1' = state1''
		   then error, (handler, Imply ((site1,state1),(site2,state2)))
		   else error, (handler, No_known_translation list)
		 else
		   warn parameter error (Some "line 159") Exit (handler, No_known_translation list)
	       end
	    | _ -> error, (handler, No_known_translation list)
	  end
       | _ -> 
	 begin
	   let error, handler, output = 
	     try_partitioning parameter handler error rename_site_inverse mvbdu 
	   in
	   match 
	     output
	   with 
	   | None -> error, (handler, No_known_translation list)
	   | Some (var,l) -> 
	     error,
	     (handler, 
	     Partition (var, l))
	 end
    end
  else
    error, (handler, No_known_translation list)
    
let rec print ?beginning_of_sentence:(beggining=true) ?prompt_agent_type:(prompt_agent_type=true) ~show_dep_with_dimmension_higher_than:dim_min parameter handler_kappa error agent_string agent_type translation =
  let cap s = if beggining then String.capitalize s else s in
  let in_agent s = if prompt_agent_type then " in agent "^s^" " else "" in
  let in_agent_comma s = if prompt_agent_type then "in agent "^s^", " else "" in
  let in_agent_colon s = if prompt_agent_type then "in agent "^s^": " else "" in 
  let error, () =
    match
      translation
    with
    | Range (site_type,state_list) ->
      begin
	if dim_min <= 1
	then
	  let error', site_string =
	    Handler.string_of_site parameter error handler_kappa
	      agent_type site_type
	  in 
          let error = Exception.check warn parameter error error' (Some "line 111") Exit in
	  let rec aux list error =
	    match list
	    with
	    | [] -> warn parameter error (Some "line 282") Exit ()
	    | [state] ->
	      let error', state_string =
		Handler.string_of_state_fully_deciphered parameter error
		  handler_kappa agent_type site_type state
	      in
	      let error = Exception.check warn parameter error error' (Some "line 121") Exit in
	      error, Printf.fprintf (Remanent_parameters.get_log parameter) " and %s.\n" state_string
	    | state::tail ->
	      let error', state_string =
		Handler.string_of_state_fully_deciphered parameter error
		  handler_kappa agent_type site_type state
	      in
	      let error = Exception.check warn parameter error error' (Some "line 128") Exit in
	      let () = Printf.fprintf (Remanent_parameters.get_log parameter) " %s," state_string in
	      aux tail error
	  in
	  match
	    state_list
	  with
	  | [] -> warn parameter error (Some "line 305") Exit ()
	  | [state] ->
	    let error', state_string =
	      Handler.string_of_state_fully_deciphered parameter error
		handler_kappa agent_type site_type state
	    in
	    let error = Exception.check warn parameter error error' (Some "line 141") Exit in
	    error, 
	    Printf.fprintf 
	      (Remanent_parameters.get_log parameter) 
	      "%s%she state of the site %s%sis always %s.\n" 
	      (Remanent_parameters.get_prefix parameter) (cap "t") site_string (in_agent agent_string) state_string
	  | [state1;state2] ->
	    let error', state_string1 =
	      Handler.string_of_state_fully_deciphered parameter error
		handler_kappa agent_type site_type state1
	    in
	    let error = Exception.check warn parameter error error' (Some "line 149") Exit in
	    let error', state_string2 =
	      Handler.string_of_state_fully_deciphered parameter error
		handler_kappa agent_type site_type state2
	    in
	    let error = Exception.check warn parameter error error' (Some "line 154") Exit in
	    error, Printf.fprintf (Remanent_parameters.get_log parameter) 
	      "%s%she state of the site %s%sranges over %s and %s.\n" 
	      (Remanent_parameters.get_prefix parameter) (cap "t") site_string (in_agent agent_string) state_string1 state_string2
	  | list ->
	    let () = Printf.fprintf (Remanent_parameters.get_log parameter)
	      "%s%she state of the site %s%sranges over" 
	      (Remanent_parameters.get_prefix parameter) (cap "t") site_string (in_agent agent_string)
	    in
	    aux list error
	else error,()
      end
   | Equiv ((site1,state1),(site2,state2)) ->
     if dim_min <= 2
     then
       begin
	 let error', site_string1 =
	   Handler.string_of_site parameter error handler_kappa
	     agent_type site1
	 in
	 let error = Exception.check warn parameter error error' (Some "line 172") Exit in
	 let error', state_string1 =
	   Handler.string_of_state_fully_deciphered parameter error
	     handler_kappa agent_type site1 state1
	 in
	 let error = Exception.check warn parameter error error' (Some "line 177") Exit in
	 let error', site_string2 =
	   Handler.string_of_site parameter error handler_kappa
	     agent_type site2
	 in
	 let error = Exception.check warn parameter error error' (Some "line 182") Exit in
	 let error', state_string2 =
	   Handler.string_of_state_fully_deciphered parameter error
	     handler_kappa agent_type site2 state2
	 in
	 let error = Exception.check warn parameter error error' (Some "line 187") Exit in
	 error,
	 Printf.fprintf (Remanent_parameters.get_log parameter) "%s%sthe state of the site %s is %s, if and only if, the state of the site %s is %s.\n" (Remanent_parameters.get_prefix parameter) (cap (in_agent_comma agent_string)) site_string1 state_string1 site_string2 state_string2
       end
     else
       error,()
   | Imply ((site1,state1),(site2,state2)) ->
     if dim_min <= 2
     then
       begin
	 let error', site_string1 =
	   Handler.string_of_site parameter error handler_kappa agent_type site1
	 in
	 let error = Exception.check warn parameter error error' (Some "line 200") Exit in
	 let error', state_string1 =
	   Handler.string_of_state_fully_deciphered parameter error
	     handler_kappa agent_type site1 state1
	 in
	 let error = Exception.check warn parameter error error' (Some "line 204") Exit in
	 let error', site_string2 =
	   Handler.string_of_site parameter error handler_kappa agent_type site2
	 in
	 let error = Exception.check warn parameter error error' (Some "line 208") Exit in
	 let error', state_string2 =
	   Handler.string_of_state_fully_deciphered parameter error
	     handler_kappa agent_type site2 state2 in
	 let error = Exception.check warn parameter error error' (Some "line 212") Exit in
	 error,
	 Printf.fprintf (Remanent_parameters.get_log parameter) "%s%sthe state of the site %s is %s whenever the state of the site %s is %s.\n"  (Remanent_parameters.get_prefix parameter) (cap (in_agent_comma agent_string)) site_string2 state_string2 site_string1 state_string1
       end
     else
       error,()
   | Partition (v, list) -> 
     let () = 
       Printf.fprintf (Remanent_parameters.get_log parameter) 
	 "%s%s\n" (Remanent_parameters.get_prefix parameter) (cap (in_agent_colon agent_string)) 
     in
     let error, site_string = 
	 Handler.string_of_site parameter error handler_kappa agent_type v 
     in
     let parameter = Remanent_parameters.update_prefix parameter "\t" in
     let error = 
       List.fold_left
	 (fun error (a,list) -> 
	   let error, state_string =
	       Handler.string_of_state_fully_deciphered parameter error handler_kappa agent_type v a
	   in
	   let () = Printf.fprintf (Remanent_parameters.get_log parameter) "%swhen the state of the site %s is equal to %s, then:\n" (Remanent_parameters.get_prefix parameter) site_string state_string in
	   let parameter = Remanent_parameters.update_prefix parameter "\t+ " in
	   List.fold_left
	     (fun error token -> 
	       print ~beginning_of_sentence:false ~prompt_agent_type:false ~show_dep_with_dimmension_higher_than:0 parameter  handler_kappa error agent_string agent_type token) 
	     error list)
	 error list
     in error,()
   | No_known_translation list ->
     begin
       match list
       with
       | [] -> error,()
       | head::_ ->
	 let n = List.length head in
	 if List.length head >= dim_min
	 then 
	   let () = Printf.fprintf (Remanent_parameters.get_log parameter) "%s%s, the sites " (Remanent_parameters.get_prefix parameter) (cap (in_agent_comma agent_string)) in
	   let () = Printf.fprintf (Remanent_parameters.get_log parameter) "... " in
	   let () = Printf.fprintf (Remanent_parameters.get_log parameter) "are entangled by the following %i-d relationship:\n" n in
	   let parameter = Remanent_parameters.update_prefix parameter "\t" in
	   List.fold_left
	     (fun error l ->
	       let error, bool =
		 List.fold_left
		   (fun (error, bool) (site_type, state) ->
		     let error', site_string =
		       Handler.string_of_site parameter error handler_kappa
			 agent_type site_type
		     in
		     let error = Exception.check warn parameter error error' (Some "line 235") Exit in
		     let error, state_string =
		       Handler.string_of_state_fully_deciphered parameter error
			 handler_kappa agent_type site_type state
		     in
		     let error = Exception.check warn parameter error error' (Some "line 240") Exit in
		     (*-----------------------------------------------------------*)
		     let () =
		       if bool 
		       then Printf.fprintf (Remanent_parameters.get_log parameter) ","
		       else Printf.fprintf (Remanent_parameters.get_log parameter) "%s%s(" (Remanent_parameters.get_prefix parameter) agent_string
                     in
		     let () = Printf.fprintf (Remanent_parameters.get_log parameter) 
		       "%s%s" site_string state_string
		     in
		     error,true
		   )
		   (error,false) l
	       in
		(*-----------------------------------------------------------*)
	       let () = 
		 if bool 
		 then Printf.fprintf (Remanent_parameters.get_log parameter) ")\n"
	       in error)
	     error list,()
	 else
	   error,()
     end	     
 in
 error
