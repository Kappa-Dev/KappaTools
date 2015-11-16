let several_internal_states pos =
  raise (ExceptionDefn.Malformed_Decl
	   ("In a pattern, a site cannot have several internal state ",pos))

let several_occurence_of_site agent_name (na,pos) =
  raise (ExceptionDefn.Malformed_Decl
	   ("Site '"^na^"' occurs more than once in this agent '"^agent_name^"'",pos))

(*
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice
*)
let mixture sigs mix =
  let check_a_link (one,two as acc) = function
    | (Ast.LNK_TYPE _ | Ast.LNK_SOME | Ast.LNK_ANY | Ast.FREE),_ -> acc
    | Ast.LNK_VALUE i, pos ->
       if Mods.IntSet.mem i two then
	 raise (ExceptionDefn.Malformed_Decl
		  ("This is the third occurence of link '"^string_of_int i
		   ^"' in the same mixture.",pos))
       else match Mods.IntMap.pop i one with
	    | None,one' -> (Mods.IntMap.add i pos one',two)
	    | Some _,one' -> (one',Mods.IntSet.add i two) in
  let (last_one,_) =
    List.fold_left
      (fun acc ((agent_name,_ as ag),pl) ->
       let ag_id = Signature.num_of_agent ag sigs in
       let si = Signature.get sigs ag_id in
       fst @@
	 List.fold_left
	   (fun (acc,pset) p ->
	    let pset' = Mods.StringSet.add (fst p.Ast.port_nme) pset in
	    let () = if pset == pset' then
		       several_occurence_of_site agent_name p.Ast.port_nme in
	    let s_id = Signature.num_of_site ~agent_name p.Ast.port_nme si in
	    let () = match p.Ast.port_int with
	      | [int] -> ignore (Signature.num_of_internal_state s_id int si)
	      | [] -> ()
	      | _ :: (_,pos) :: _ -> several_internal_states pos in
	    (check_a_link acc p.Ast.port_lnk,pset')) (acc,Mods.StringSet.empty) pl)
      (Mods.IntMap.empty,Mods.IntSet.empty) mix in
  match Mods.IntMap.root last_one with
  | None -> ()
  | Some (i,pos) ->
     raise (ExceptionDefn.Malformed_Decl
	      ("The link '"^string_of_int i^"' occurs only one time in the mixture.",
	       pos))

let rec ast_alg_expr sigs tok algs = function
  | Ast.BIN_ALG_OP (_,x,y),_ ->
     let () = ast_alg_expr sigs tok algs x in ast_alg_expr sigs tok algs y
  | Ast.UN_ALG_OP (_,x),_ -> ast_alg_expr sigs tok algs x
  | (Ast.STATE_ALG_OP _ | Ast.CONST _ | Ast.TMAX | Ast.EMAX | Ast.PLOTNUM),_ -> ()
  | Ast.OBS_VAR v,pos -> ignore (NamedDecls.elt_id ~kind:"variable" algs (v,pos))
  | Ast.TOKEN_ID t,pos -> ignore (NamedDecls.elt_id ~kind:"token" tok (t,pos))
  | Ast.KAPPA_INSTANCE m,_ -> mixture sigs m

let rec bool_expr sigs tok algs = function
  | (Ast.TRUE | Ast.FALSE),_ -> ()
  | Ast.BOOL_OP (_,x,y),_ ->
     let () = bool_expr sigs tok algs x in bool_expr sigs tok algs y
  | Ast.COMPARE_OP (_,x,y),_ ->
     let () = ast_alg_expr sigs tok algs x in ast_alg_expr sigs tok algs y

let rule sigs tok algs r =
  let () = mixture sigs r.Ast.lhs in
  let tk =
    List.iter (fun (va,na) ->
	       let () = ast_alg_expr sigs tok algs va in
	       ignore (NamedDecls.elt_id ~kind:"token" tok na)) in
  let () = tk r.Ast.rm_token in
  let () = mixture sigs r.Ast.rhs in
  let () = tk r.Ast.add_token in
  let () = ast_alg_expr sigs tok algs r.Ast.k_def in
  let kop = function
    | None -> ()
    | Some k_op -> ast_alg_expr sigs tok algs k_op in
  let () =
    match r.Ast.k_un with
    | None -> ()
    | Some (k_un,k_op) ->
       let () = ast_alg_expr sigs tok algs k_un in
       kop k_op in
  kop r.Ast.k_op

let print_expr sigs tok algs = function
  | Ast.Str_pexpr _,_ -> ()
  | Ast.Alg_pexpr x,pos -> ast_alg_expr sigs tok algs (x,pos)

let modif_expr sigs tok algs = function
  | (Ast.INTRO (how,(who,_)) | Ast.DELETE (how,(who,_))) ->
     let () = ast_alg_expr sigs tok algs how in
     mixture sigs who
  | Ast.UPDATE (v,how) ->
     let () = ignore (NamedDecls.elt_id ~kind:"variable" algs v) in
     ast_alg_expr sigs tok algs how
  | Ast.UPDATE_TOK (t,how) ->
     let () = ignore (NamedDecls.elt_id ~kind:"token" tok t) in
     ast_alg_expr sigs tok algs how
  | (Ast.STOP (p,_) | Ast.SNAPSHOT (p,_) | Ast.FLUX (p,_) | Ast.FLUXOFF (p,_)) ->
     List.iter (print_expr sigs tok algs) p
  | Ast.PLOTENTRY -> ()
  | Ast.PRINT (p,p',_) ->
     let () = List.iter (print_expr sigs tok algs) p in
     List.iter (print_expr sigs tok algs) p'
  | Ast.CFLOWLABEL (_,_s) -> ()
  | Ast.CFLOWMIX (_,(m,_)) -> mixture sigs m

let perturbation sigs tok algs ((pre,mods,post),_) =
  let () = bool_expr sigs tok algs pre in
  let () = List.iter (modif_expr sigs tok algs) mods in
  match post with
  | None -> ()
  | Some post -> bool_expr sigs tok algs post

let init sigs tok algs = function
  | Ast.INIT_MIX (how,(who,_)) ->
     let () = ast_alg_expr sigs tok algs how in
     mixture sigs who
  | Ast.INIT_TOK (how,t) ->
     let () = ast_alg_expr sigs tok algs how in
     ignore (NamedDecls.elt_id ~kind:"token" tok t)

let compil sigs tok algs c =
  let () =
    List.iter (fun (_,expr) -> ast_alg_expr sigs tok algs expr)
	      c.Ast.variables in
  let () =
    List.iter (fun (_,(r,_)) -> rule sigs tok algs r) c.Ast.rules in
  let () =
    List.iter (fun expr -> ast_alg_expr sigs tok algs expr)
	      c.Ast.observables in
  let () =
    List.iter (fun (_,ini,_) -> init sigs tok algs ini)
	      c.Ast.init in
  List.iter (perturbation sigs tok algs) c.Ast.perturbations
