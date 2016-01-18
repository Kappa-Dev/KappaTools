let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Translation_in_natural_language.ml") message exn (fun () -> default)  

let trace = false

type token =
  | Range of int * int list
  | Equiv of (int * int) * (int * int)
  | Imply of (int * int) * (int * int)
  | No_known_translation of (int * int) list list
type rename_sites =   (Remanent_parameters_sig.parameters -> Exception.method_handler -> Cckappa_sig.Site_map_and_set.Map.elt -> Exception.method_handler * Cckappa_sig.Site_map_and_set.Map.elt) 

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
       | _ -> error, (handler, No_known_translation list)
    end
  else
    error, (handler, No_known_translation list)
    
let print ~show_dep_with_dimmension_higher_than:dim_min parameter handler_kappa error agent_string agent_type translation =
 let error, () =
   match
     translation
   with
   | Range (site_type,state_list) ->
      begin
	if dim_min <= 1
	then
	  let error, site_string =
	    try 
	      Handler.string_of_site parameter error handler_kappa
				     agent_type site_type
	    with
	      _ ->
              warn parameter error (Some "line 273") Exit
                   (string_of_int site_type)
	  in
	  let rec aux list error =
	    match list
	    with
	    | [] -> warn parameter error (Some "line 282") Exit ()
	    | [state] ->
	       let error, state_string =
			 try
			   Handler.string_of_state_fully_deciphered parameter error
								    handler_kappa agent_type site_type state
			 with
			   _ -> warn parameter error (Some "line 290") Exit 
				     (string_of_int state)
		       in
		       error, Printf.fprintf (Remanent_parameters.get_log parameter) " and %s.\n" state_string
	    | state::tail ->
	       let error, state_string =
		 try
		   Handler.string_of_state_fully_deciphered parameter error
							    handler_kappa agent_type site_type state
		 with
		   _ -> warn parameter error (Some "line 300") Exit 
			     (string_of_int state)
	       in
	       let () = Printf.fprintf (Remanent_parameters.get_log parameter) " %s," state_string in
	       aux tail error
	  in
	  match
		    state_list
	  with
	  | [] -> warn parameter error (Some "line 305") Exit ()
	  | [state] ->
	     let error, state_string =
	       try
		 Handler.string_of_state_fully_deciphered parameter error
							  handler_kappa agent_type site_type state
	       with
		 _ -> warn parameter error (Some "line 313") Exit 
			   (string_of_int state)
	     in
	     error, Printf.fprintf (Remanent_parameters.get_log parameter) 
				   "The state of the site %s in agent %s is always %s.\n" site_string agent_string state_string
	  | [state1;state2] ->
	     let error, state_string1 =
	       try
		 Handler.string_of_state_fully_deciphered parameter error
							  handler_kappa agent_type site_type state1
	       with
		 _ -> warn parameter error (Some "line 323") Exit 
			   (string_of_int state1)
	     in
	     let error, state_string2 =
	       try
		 Handler.string_of_state_fully_deciphered parameter error
							  handler_kappa agent_type site_type state2
	       with
		 _ -> warn parameter error (Some "line 331") Exit 
			   (string_of_int state2)
	     in
	     error, Printf.fprintf (Remanent_parameters.get_log parameter) 
				   "The state of the site %s in agent %s ranges over %s and %s.\n" site_string agent_string state_string1 state_string2
	  | list ->
	     let () = Printf.fprintf (Remanent_parameters.get_log parameter)
				     "The state of the site %s in agent %s ranges over" site_string agent_string
	     in
	     aux list error
	else error,()
      end
   | Equiv ((site1,state1),(site2,state2)) ->
      if dim_min <= 2
      then
	begin
	  let error, site_string1 =
	    try 
	      Handler.string_of_site parameter error handler_kappa
				     agent_type site1
	    with
	      _ ->
	      warn parameter error (Some "line 348") Exit
                   (string_of_int site1)
	  in
	  let error, state_string1 =
	    try
	      Handler.string_of_state_fully_deciphered parameter error
						       handler_kappa agent_type site1 state1
	    with
	      _ -> warn parameter error (Some "line 357") Exit 
			(string_of_int state1)
	  in
	  let error, site_string2 =
	    try 
	      Handler.string_of_site parameter error handler_kappa
				     agent_type site2
	    with
	      _ ->
              warn parameter error (Some "line 365") Exit
                   (string_of_int site2)
	  in
	  let error, state_string2 =
	    try
	      Handler.string_of_state_fully_deciphered parameter error
						       handler_kappa agent_type site2 state2
	    with
	      _ -> warn parameter error (Some "line 373") Exit 
			(string_of_int state2)
	  in
	  error,
	  Printf.fprintf (Remanent_parameters.get_log parameter) "In agent %s, the state of the site %s is %s, if and only if, the state of the site %s is %s.\n" agent_string site_string1 state_string1 site_string2 state_string2
	end
      else
	error,()
   | Imply ((site1,state1),(site2,state2)) ->
      if dim_min <= 2
      then
	begin
	  let error, site_string1 =
	    try 
	      Handler.string_of_site parameter error handler_kappa agent_type site1
	    with
	      _ ->
              warn parameter error (Some "line 348") Exit (string_of_int site1)
	  in
	  let error, state_string1 =
	    try
	      Handler.string_of_state_fully_deciphered parameter error
						       handler_kappa agent_type site1 state1
		    with
		      _ -> warn parameter error (Some "line 357") Exit (string_of_int state1)
		  in
		  let error, site_string2 =
		    try 
		      Handler.string_of_site parameter error handler_kappa agent_type site2
		    with
		      _ ->
                      warn parameter error (Some "line 365") Exit (string_of_int site2)
		  in
		  let error, state_string2 =
		    try
		      Handler.string_of_state_fully_deciphered parameter error
							       handler_kappa agent_type site2 state2
		    with
		      _ -> warn parameter error (Some "line 373") Exit 	(string_of_int state2)
		  in
		  error,
		  Printf.fprintf (Remanent_parameters.get_log parameter) "In agent %s, the state of the site %s is %s whenever the state of the site %s is %s.\n" agent_string site_string2 state_string2 site_string1 state_string1
		  end
		else
		  error,()
   | No_known_translation list ->
      begin
	match list
	with
	| [] -> error,()
	| head::_ ->
	   if List.length head >= dim_min
	   then 
	     List.fold_left
	       (fun error l ->
		let error, bool =
		  List.fold_left
		    (fun (error, bool) (site_type, state) ->
		     let error, site_string =
		       try 
			 Handler.string_of_site parameter error handler_kappa
						agent_type site_type
		       with
			 _ ->
			 warn parameter error (Some "line 147") Exit
			      (string_of_int site_type)
		     in
		     let error, state_string =
		       try
			 Handler.string_of_state_fully_deciphered parameter error
								  handler_kappa agent_type site_type state
		       with
			 _ -> warn parameter error (Some "line 146") Exit 
				   (string_of_int state)
		     in
		     (*-----------------------------------------------------------*)
		     let () =
		       if bool 
		       then Printf.fprintf (Remanent_parameters.get_log parameter) ","
		       else Printf.fprintf (Remanent_parameters.get_log parameter) "%s(" agent_string
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
