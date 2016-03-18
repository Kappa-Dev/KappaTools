(**
   * algebraic_construction.ml
   * openkappa
   * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
   * 
   * Creation: September, the 27th of 2015 
   * Last modification: September, the 27th of 2015
   * * 
   * algebraic check for the influence map.
   *  
   * Copyright 2015 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
  Exception.warn parameters mh (Some "algebraic_construction.sig") message exn (fun () -> default) 

exception False of Exception.method_handler 
    
let check parameters error handler mixture1 mixture2 (i,j) =
  let add (n1,n2) error to_do (inj1,inj2) =
    let im1 = 
      (*Quark_type.IntSetMap.Map.find_option*)
      Ckappa_sig.Agent_id_setmap.Map.find_option
        n1
        inj1
    in
    error,match im1
      with
      | Some n2' when n2 = n2' -> Some (to_do, inj1, inj2)
      | Some _ -> None
      | None ->
        begin
	  let im2 = 
            (*Quark_type.IntSetMap.Map.find_option*)
            Ckappa_sig.Agent_id_setmap.Map.find_option
              n2
              inj2
          in
	  match im2
	  with Some _ -> None
	  | None ->
	    let inj1 = 
              (*Quark_type.IntSetMap.Map.add*)
              Ckappa_sig.Agent_id_setmap.Map.add
                n1
                n2
                inj1 
            in
	    let inj2 =
              (*Quark_type.IntSetMap.Map.add*)
              Ckappa_sig.Agent_id_setmap.Map.add
                n2
                n1 
                inj2 
            in
	    Some ((n1, n2) :: to_do, inj1, inj2)
        end 
  in

  let rec check_agent error to_do already_done =
    match to_do with
    | [] -> error,true
    | (h1,h2) :: t when (Ckappa_sig.int_of_agent_id h1) < 0 ||
        (Ckappa_sig.int_of_agent_id h2) < 0 ->
      check_agent error t already_done
    | (h1,h2)::t ->
      begin
        (* check agent type *)
        let error,view1 = 
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get 
            parameters error h1 mixture1.Cckappa_sig.views 
        in
        let error,view2 = 
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get
            parameters error h2 mixture2.Cckappa_sig.views 
        in
        let error,bonds1 = 
         (*Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get*)
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters error h1 mixture1.Cckappa_sig.bonds 
        in 
        let error,bonds2 = 
         (*Int_storage.Quick_Nearly_inf_Imperatif.unsafe_get*)
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters error h2 mixture2.Cckappa_sig.bonds
        in
        check_interface error view1 view2 bonds1 bonds2 t already_done 
      end

  and deal_with error iter2 ag1 ag2 bonds1 bonds2 (to_do,already_done) =
    let bonds1 =
      match bonds1 with Some bonds1 -> bonds1 | None -> Ckappa_sig.Site_map_and_set.Map.empty
    in 
    let bonds2 =
      match bonds2 with Some bonds2 -> bonds2 | None -> Ckappa_sig.Site_map_and_set.Map.empty
    in 
    let error,bool = 
      try
	let error = 
	  iter2
	    parameters error
	    (fun _ error _ port1 port2 ->
	      let range1 = port1.Cckappa_sig.site_state in
	      let range2 = port2.Cckappa_sig.site_state in
	      if not (range1.Cckappa_sig.max < range2.Cckappa_sig.min || 
                        range2.Cckappa_sig.max < range1.Cckappa_sig.min)
	      then error
	      else raise (False error))
	    ag1.Cckappa_sig.agent_interface
	    ag2.Cckappa_sig.agent_interface
	in error, true
      with
	False error -> error, false
    in
    if bool 
    then
      try
	let error,(to_do,already_done) =
	  Ckappa_sig.Site_map_and_set.Map.fold2_sparse parameters error 
	    (fun _ error _ port1 port2 (to_do,already_done) ->
	      if port1.Cckappa_sig.site = port2.Cckappa_sig.site
	      then
		match
		  add
		    (port1.Cckappa_sig.agent_index,
		     port2.Cckappa_sig.agent_index)
		    error
		    to_do
		    already_done
		with
		| error,None -> raise (False error)
		| error,Some (todo,inj1,inj2) -> (error,(todo,(inj1,inj2)))
	      else
		raise (False error) 
	    )
	    bonds1
	    bonds2
	    (to_do,already_done)
	in error,(true,(to_do,already_done))
      with
	False error -> error,(false,(to_do,already_done))
    else
      error,(bool,(to_do,already_done))


  and check_interface error ag1 ag2 bonds1 bonds2 to_do already_done =

    let error,(bool,(to_do,already_done)) =
      match
	ag1,ag2
      with
      | None,_ | _,None -> 
        warn parameters error (Some ("Should not scan empty agents...")) Exit (true,(to_do,already_done))
      | Some ag1,Some ag2 ->
	begin
	  match ag1
	  with 
	  | Cckappa_sig.Ghost->
            warn parameters error (Some "Should not scan ghost agents...") Exit (true,(to_do,already_done))
	  | Cckappa_sig.Unknown_agent _ -> raise (False error) 
	  | Cckappa_sig.Dead_agent (ag1,_,l11,l12) ->
	    begin
	      match ag2 with
	      | Cckappa_sig.Unknown_agent _ -> raise (False error)
	      | Cckappa_sig.Ghost -> warn parameters error 
                (Some "Should not scan ghost agents...") Exit (true,(to_do,already_done))
	      | Cckappa_sig.Dead_agent (ag2,s2,l21,l22) ->
		begin
		  let error,(bool,(to_do,already_done)) =
		    deal_with error
		      (fun parameter error ->
			Ckappa_sig.Site_map_and_set.Map.iter2
			  parameter error 
			  (fun parameter error site _ ->
			    if Ckappa_sig.Site_map_and_set.Map.mem site l22 ||
                              Ckappa_sig.Site_map_and_set.Map.mem site l21
			    then raise (False error)
			    else error)
			  (fun parameter error site _  ->
			    if Ckappa_sig.Site_map_and_set.Map.mem site l12 ||
                              Ckappa_sig.Site_map_and_set.Map.mem site l22
			    then raise (False error)
			    else error))
		      ag1
		      ag2
		      bonds1
		      bonds2
		      (to_do,already_done) in
		     (* to do check consistency of dead sites *)
 		  error,(true,(to_do,already_done))
		end
	      | Cckappa_sig.Agent ag2 ->
		deal_with error
		  (fun parameter error ->
		    Ckappa_sig.Site_map_and_set.Map.iter2
		      parameter error 
		      (fun _ error _ _ -> error)
		      (fun parameter error site _  ->
			if Ckappa_sig.Site_map_and_set.Map.mem site l11 ||
                          Ckappa_sig.Site_map_and_set.Map.mem site l12
			then raise (False error)
			else error))
		  ag1
		  ag2
		  bonds1
		  bonds2
		  (to_do,already_done)
		  
	    end 
	  | Cckappa_sig.Agent ag1 ->
	    begin
	      match ag2 with
	      | Cckappa_sig.Unknown_agent _ -> raise (False error)
	      | Cckappa_sig.Ghost -> warn parameters error 
                (Some "Should not scan ghost agents...") Exit (true,(to_do,already_done))
	      | Cckappa_sig.Dead_agent (ag2,_,l21,l22) ->
		begin
		  begin
		    deal_with error
		      (fun parameter error ->
			Ckappa_sig.Site_map_and_set.Map.iter2
			  parameter error 
			  (fun parameter error site _ ->
			    if Ckappa_sig.Site_map_and_set.Map.mem site l22 || 
                              Ckappa_sig.Site_map_and_set.Map.mem site l21
			    then raise (False error)
			    else error)
			  (fun _ error _ _  -> error))
		      ag1
		      ag2
		      bonds1
		      bonds2
		      (to_do,already_done)
		  end
		end
	      | Cckappa_sig.Agent ag2 -> 
                deal_with error Ckappa_sig.Site_map_and_set.Map.iter2_sparse
                  ag1 ag2 bonds1 bonds2 (to_do,already_done)
	       (* begin let bonds1 = match bonds1 with Some bonds1 ->
	     	  bonds1 | None -> Cckappa_sig.Site_map_and_set.Map.empty
	     	  in let bonds2 = match bonds2 with Some bonds2 -> bonds2
	     	  | None -> Cckappa_sig.Site_map_and_set.Map.empty in let
	     	  error,bool = try let error =
	     	  Cckappa_sig.Site_map_and_set.Map.iter2_sparse parameters
	     	  error (fun _ error _ port1 port2 -> let range1 =
	     	  port1.Cckappa_sig.site_state in let range2 =
	     	  port2.Cckappa_sig.site_state in if not
	     	  (range1.Cckappa_sig.max < range2.Cckappa_sig.min ||
	     	  range2.Cckappa_sig.max < range1.Cckappa_sig.min) then
	     	  error else raise (False error))
	     	  ag1.Cckappa_sig.agent_interface
	     	  ag2.Cckappa_sig.agent_interface in error,true with False
	     	  error -> error,false in if bool then try let
	     	  error,(to_do,already_done) =
	     	  Cckappa_sig.Site_map_and_set.Map.fold2_sparse parameters
	     	  error (fun _ error _ port1 port2 (to_do,already_done) ->
	     	  if port1.Cckappa_sig.site = port2.Cckappa_sig.site then
	     	  match add (port1.Cckappa_sig.agent_index,
	     	  port2.Cckappa_sig.agent_index) error to_do already_done
	     	  with | error,None -> raise (False error) | error,Some
	     	  (todo,inj1,inj2) -> (error,(todo,(inj1,inj2))) else
	     	  raise (False error) ) bonds1 bonds2 (to_do,already_done)
	     	  in error,(true,(to_do,already_done)) with False error ->
	     	  error,(false,(to_do,already_done)) else
	     	  error,(bool,(to_do,already_done)) end*)
	    end 
	end
    in
    if bool
    then
      check_agent error to_do already_done
    else
      error,false
  in
  let error,ouput = add (i,j) error [] 
    (*(Quark_type.IntSetMap.Map.empty,Quark_type.IntSetMap.Map.empty)*)
    (Ckappa_sig.Agent_id_setmap.Map.empty, Ckappa_sig.Agent_id_setmap.Map.empty)
  in
  match ouput
  with
    None -> warn parameters error (Some "Missing rule") Exit (false)
  | Some(todo,inj1,inj2) -> check_agent error [i,j] (inj1,inj2)
    
exception Pass of Exception.method_handler 
    
let filter_influence parameters error handler compilation map bool =
  let nrules = Handler.nrules parameters error handler in
  let get_var v =
    match snd (v.Cckappa_sig.e_variable) with 
    | Ast.KAPPA_INSTANCE(mixture), _ -> error,mixture
    | _ -> 
      let error,() = warn parameters error (Some "Composite observable") Exit ()
      in raise (Pass error)
  in 
  let get_lhs r =
    r.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_lhs
  in
  let get_rhs r =
    r.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_rhs
  in
  let get_bool = if bool then get_rhs else get_lhs in 

  let check_influence_rule_mixt error rule1 mixt  pos =
    let updt_pos ((x:Ckappa_sig.c_agent_id), (y:Ckappa_sig.c_agent_id)) =
      (if bool && (Ckappa_sig.int_of_agent_id x) >= rule1.Cckappa_sig.e_rule_c_rule.Cckappa_sig.prefix 
       then Ckappa_sig.agent_id_of_int
          (
            (Ckappa_sig.int_of_agent_id x) + rule1.Cckappa_sig.e_rule_c_rule.Cckappa_sig.delta
          )
       else x
      ),
      y
    in
    check 
      parameters
      error
      handler 
      (get_bool rule1) 
      mixt 
      (updt_pos pos)
  in
  (*Quark_type.Int2SetMap.Map.fold*)
  Ckappa_sig.PairRule_setmap.Map.fold
    (fun (a,b) couple (error,map') ->
      try 
        begin 
	  let error,rule1 =
           (*Int_storage.Nearly_inf_Imperatif.get*)
            Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
              parameters 
              error
              a 
              compilation.Cckappa_sig.rules 
          in
	  let error,r1 =
	    match rule1
	    with
	    | None ->
	      let error,() = warn parameters error (Some "Missing rule") Exit ()
	      in raise (Pass error)
	    | Some r -> error,r
	  in
	  let error,mixt =
	    if
	      (Ckappa_sig.int_of_rule_id b) < nrules
	    then
	      begin
	        let error,rule2 =
                 (*Int_storage.Nearly_inf_Imperatif.get*)
                  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
                    parameters 
                    error
                    b 
                    compilation.Cckappa_sig.rules
                in 
	        match rule2 with
	        | None ->
		  let error,() = warn parameters error 
                    (Some ("Missing rule"^ (Ckappa_sig.string_of_rule_id b))) Exit ()
		  in raise (Pass error)
	        | Some r -> error, get_lhs r 
	      end
	    else
	      begin
	        let error,var = 
                 (*Int_storage.Nearly_inf_Imperatif.get*)
                  Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
                    parameters 
                    error
                    (Ckappa_sig.rule_id_of_int ((Ckappa_sig.int_of_rule_id b) - nrules))
                    compilation.Cckappa_sig.variables
                in
	        match var with
	        | None ->
		  let error,() = warn parameters error
                    (Some ("Missing var" ^ 
                              (string_of_int ((Ckappa_sig.int_of_rule_id b) - nrules)))) Exit ()
		  in raise (Pass error)
	        | Some v -> get_var v
	      end
	  in 
	  let error,couple' =
	    try
	      let error,couple' =
	        Quark_type.Labels.filter_couple
		  parameters
		  error
		  handler 
		  (fun error a b  ->
		    check_influence_rule_mixt error
		      r1
		      mixt
		      (Ckappa_sig.agent_id_of_int a,
                       Ckappa_sig.agent_id_of_int b)
                  )
		  couple
	      in
	      error,couple'
	    with Exit -> error,couple
	  in 
	  if Quark_type.Labels.is_empty_couple couple'
	  then  error,map'
	  else error,
           (*Quark_type.Int2SetMap.Map.add*)
            Ckappa_sig.PairRule_setmap.Map.add
              (a,b) couple' map'
        end
      with Pass error -> (error,map')
    )
    map 
    (error, 
     Ckappa_sig.PairRule_setmap.Map.empty
    (*Quark_type.Int2SetMap.Map.empty*)
    )
