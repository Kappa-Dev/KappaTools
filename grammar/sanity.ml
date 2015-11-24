let several_internal_states pos =
  raise (ExceptionDefn.Malformed_Decl
	   ("In a pattern, a site cannot have several internal state ",pos))

let several_occurence_of_site agent_name (na,pos) =
  raise (ExceptionDefn.Malformed_Decl
	   ("Site '"^na^"' occurs more than once in this agent '"^agent_name^"'",pos))

let not_enough_specified agent_name (na,pos) =
  raise (ExceptionDefn.Malformed_Decl
	   ("The link status of agent '"^agent_name^"', site '"^na
	    ^"' on the right hand side is underspecified",pos))

(*
- agent exists
- sites exist
- unique site occurence / agent
- internal_states exist
- unique internal_state / site
- links appear exactly twice
*)
let mixture_part is_creation sigs mix acc =
  let check_a_link agent_name port_name (one,two as acc) = function
    | (Ast.LNK_TYPE _ | Ast.LNK_SOME | Ast.LNK_ANY),_ ->
      if is_creation then
	not_enough_specified agent_name port_name
      else acc
    | Ast.FREE,_ -> acc
    | Ast.LNK_VALUE (i,()), pos ->
       if Mods.IntSet.mem i two then
	 raise (ExceptionDefn.Malformed_Decl
		  ("This is the third occurence of link '"^string_of_int i
		   ^"' in the same mixture.",pos))
       else match Mods.IntMap.pop i one with
	    | None,one' -> (Mods.IntMap.add i pos one',two)
	    | Some _,one' -> (one',Mods.IntSet.add i two) in
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
	  (check_a_link agent_name p.Ast.port_nme acc p.Ast.port_lnk,pset'))
	(acc,Mods.StringSet.empty) pl) acc mix

let mixture_final_check (last_one,_) =
  match Mods.IntMap.root last_one with
  | None -> ()
  | Some (i,pos) ->
    raise (ExceptionDefn.Malformed_Decl
	     ("The link '"^string_of_int i^"' occurs only one time in the mixture.",
	      pos))

let mixture is_creation sigs mix =
  mixture_final_check
    (mixture_part is_creation sigs mix (Mods.IntMap.empty,Mods.IntSet.empty))

let rec ast_alg_expr sigs tok algs = function
  | Ast.BIN_ALG_OP (_,x,y),_ ->
     let () = ast_alg_expr sigs tok algs x in ast_alg_expr sigs tok algs y
  | Ast.UN_ALG_OP (_,x),_ -> ast_alg_expr sigs tok algs x
  | (Ast.STATE_ALG_OP _ | Ast.CONST _ | Ast.TMAX | Ast.EMAX | Ast.PLOTNUM),_ -> ()
  | Ast.OBS_VAR v,pos -> ignore (NamedDecls.elt_id ~kind:"variable" algs (v,pos))
  | Ast.TOKEN_ID t,pos -> ignore (NamedDecls.elt_id ~kind:"token" tok (t,pos))
  | Ast.KAPPA_INSTANCE m,_ -> mixture false sigs m

let rec bool_expr sigs tok algs = function
  | (Ast.TRUE | Ast.FALSE),_ -> ()
  | Ast.BOOL_OP (_,x,y),_ ->
     let () = bool_expr sigs tok algs x in bool_expr sigs tok algs y
  | Ast.COMPARE_OP (_,x,y),_ ->
     let () = ast_alg_expr sigs tok algs x in ast_alg_expr sigs tok algs y

let rule sigs tok algs r =
  let rec check_rhs lhs rhs acc =
    match lhs,rhs with
    | ((lag_na,_),lag_p)::lt, ((rag_na,_),rag_p as ag)::rt
      when String.compare lag_na rag_na = 0 &&
	Ast.no_more_site_on_right true lag_p rag_p ->
     check_rhs lt rt (ag::acc)
  | _, added ->
    let out = mixture_part
      false sigs acc (Mods.IntMap.empty,Mods.IntSet.empty) in
    mixture_final_check (mixture_part true sigs added out) in
  let () = mixture false sigs r.Ast.lhs in
  let tk =
    List.iter (fun (va,na) ->
	       let () = ast_alg_expr sigs tok algs va in
	       ignore (NamedDecls.elt_id ~kind:"token" tok na)) in
  let () = tk r.Ast.rm_token in
  let () = check_rhs r.Ast.lhs r.Ast.rhs [] in
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
  | Ast.INTRO (how,(who,_)) ->
     let () = ast_alg_expr sigs tok algs how in
     mixture true sigs who
  | Ast.DELETE (how,(who,_)) ->
     let () = ast_alg_expr sigs tok algs how in
     mixture false sigs who
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
  | Ast.CFLOWMIX (_,(m,_)) -> mixture false sigs m

let perturbation sigs tok algs ((pre,mods,post),_) =
  let () = bool_expr sigs tok algs pre in
  let () = List.iter (modif_expr sigs tok algs) mods in
  match post with
  | None -> ()
  | Some post -> bool_expr sigs tok algs post

let init sigs tok algs = function
  | Ast.INIT_MIX (how,(who,_)) ->
     let () = ast_alg_expr sigs tok algs how in
     mixture true sigs who
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
