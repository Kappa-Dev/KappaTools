open Alg_expr

let print_ast_link f = function
  | Ast.FREE -> ()
  | Ast.LNK_TYPE ((p,_), (a,_)) -> Format.fprintf f "!%s.%s" p a
  | Ast.LNK_ANY -> Format.fprintf f "?"
  | Ast.LNK_SOME -> Format.fprintf f "!_"
  | Ast.LNK_VALUE i -> Format.fprintf f "!%i" i

let print_ast_internal f l =
  Pp.list Pp.empty (fun f (x,_) -> Format.fprintf f "~%s" x) f l

let print_ast_port f p =
  Format.fprintf f "%s%a%a" (fst p.Ast.port_nme)
		 print_ast_internal p.Ast.port_int
		 print_ast_link (fst p.Ast.port_lnk)

let print_ast_agent f ((ag_na,_),l) =
  Format.fprintf f "%s(%a)" ag_na
		 (Pp.list (fun f -> Format.fprintf f ",") print_ast_port) l

let print_ast_mix f m = Pp.list Pp.comma print_ast_agent f m

let rec print_ast_alg f = function
  | Ast.EMAX -> Format.fprintf f "[Emax]"
  | Ast.PLOTNUM -> Format.fprintf f "[p]"
  | Ast.TMAX -> Format.fprintf f "[Tmax]"
  | Ast.CONST n -> Nbr.print f n
  | Ast.OBS_VAR lab -> Format.fprintf f "'%s'" lab
  | Ast.KAPPA_INSTANCE ast ->
     Format.fprintf f "|%a|" print_ast_mix ast
  | Ast.TOKEN_ID tk -> Format.fprintf f "|%s|" tk
  | Ast.STATE_ALG_OP op -> Term.print_state_alg_op f op
  | Ast.BIN_ALG_OP (op, (a,_), (b,_)) ->
     Format.fprintf f "(%a %a %a)"
		    print_ast_alg a Term.print_bin_alg_op op print_ast_alg b
  |Ast.UN_ALG_OP (op, (a,_)) ->
    Format.fprintf f "(%a %a)" Term.print_un_alg_op op print_ast_alg a

let rec print_bool p_alg f = function
  | Ast.TRUE -> Format.fprintf f "[true]"
  | Ast.FALSE -> Format.fprintf f "[false]"
  | Ast.BOOL_OP (op,(a,_), (b,_)) ->
     Format.fprintf f "(%a %a %a)" (print_bool p_alg) a
		    Term.print_bool_op op (print_bool p_alg) b
  | Ast.COMPARE_OP (op,(a,_), (b,_)) ->
     Format.fprintf f "(%a %a %a)"
		    p_alg a Term.print_compare_op op p_alg b

let print_ast_bool = print_bool print_ast_alg

let rec ast_alg_has_mix = function
  | Ast.BIN_ALG_OP (_, a, b), _ -> ast_alg_has_mix a || ast_alg_has_mix b
  | Ast.UN_ALG_OP (_, a), _  -> ast_alg_has_mix a
  | (Ast.STATE_ALG_OP _ | Ast.OBS_VAR _ | Ast.TOKEN_ID _ | Ast.CONST _ |
     Ast.TMAX | Ast.EMAX | Ast.PLOTNUM), _ -> false
  | Ast.KAPPA_INSTANCE _, _ -> true

type ('a,'b) contractible = NO of 'a
			  | MAYBE of 'a * Term.bin_alg_op * 'a * 'b
			  | YES of 'b

let rec compile_alg var_map tk_map ?max_allowed_var
		    domain (alg,pos) =
  let rec_call domain x =
    match compile_alg var_map tk_map ?max_allowed_var domain x with
    | (domain',(ALG_VAR _ | TOKEN_ID _ | UN_ALG_OP _ | STATE_ALG_OP _
		| KAPPA_INSTANCE _ as alg,_)) -> (domain', NO alg)
    | (domain',(CONST n,_)) -> (domain',YES n)
    | (domain',(BIN_ALG_OP (op, (x,_), (CONST y,_)) as alg,_)) ->
       (domain',MAYBE(alg,op,x,y))
    | (domain',(BIN_ALG_OP _ as alg,_)) -> (domain',NO alg)
  in
  match alg with
  | Ast.KAPPA_INSTANCE ast ->
     begin
       match domain with
       | Some (origin,contact_map,domain) ->
	  let domain',ccs =
	    Snip.connected_components_sum_of_ambiguous_mixture
	    contact_map domain ?origin ast in
	  let ccs' = List.map fst ccs in
	  (Some (origin,contact_map,domain'), (KAPPA_INSTANCE ccs',pos))
       | None ->
	  raise (ExceptionDefn.Internal_Error
		   ("Theoritically pure alg_expr has a misture",pos))
     end
  | Ast.OBS_VAR lab ->
     let i =
       try Mods.StringMap.find lab var_map with
       | Not_found ->
	  raise (ExceptionDefn.Malformed_Decl
		   (lab ^" is not a declared variable",pos))
     in
     let () = match max_allowed_var with
       | Some j when j < i ->
	  raise (ExceptionDefn.Malformed_Decl
		   ("Reference to not yet defined '"^lab ^"' is forbidden.",
		    pos))
       | None | Some _ -> ()
     in (domain,(ALG_VAR i,pos))
  | Ast.TOKEN_ID tk_nme ->
     let i =
       try Mods.StringMap.find tk_nme tk_map
       with Not_found ->
	 raise (ExceptionDefn.Malformed_Decl
		  (tk_nme ^ " is not a declared token",pos))
     in (domain,(TOKEN_ID i,pos))
  | Ast.STATE_ALG_OP (op) -> (domain,(STATE_ALG_OP (op),pos))
  | Ast.CONST n -> (domain,(CONST n,pos))
  | Ast.EMAX ->
     let getMaxEventValue =
       match !Parameter.maxEventValue with
       | Some n -> Nbr.I n
       | None -> Format.eprintf "[emax] constant is evaluated to infinity@.";
		 Nbr.F infinity in
     (domain,(CONST getMaxEventValue,pos))
  | Ast.TMAX ->
     let getMaxTimeValue = match !Parameter.maxTimeValue with
       | Some t -> Nbr.F t
       | None -> Format.eprintf "[tmax] constant is evaluated to infinity@.";
		 Nbr.F infinity in
     (domain,(CONST getMaxTimeValue,pos))
  | Ast.PLOTNUM ->
	  let getPointNumberValue = Nbr.I !Parameter.pointNumberValue in
	  (domain,(CONST getPointNumberValue,pos))
  | Ast.BIN_ALG_OP (op, (a,pos1), (b,pos2)) ->
     begin match rec_call domain (a,pos1) with
	   | (domain',YES n1) ->
	      begin
		match rec_call domain' (b,pos2) with
		| (domain'',YES n2) ->
		   (domain'',(CONST (Nbr.of_bin_alg_op op n1 n2),pos))
		| (domain'',( NO y | MAYBE (y,_,_,_) )) ->
		   (domain'',
		    (BIN_ALG_OP (op, (CONST n1,pos1), (y,pos2)),pos))
	      end
	   | (domain',( NO x | MAYBE (x,_,_,_) )) ->
	      match rec_call domain' (b,pos2) with
	      | (domain'',YES n2) ->
		 (domain'',
		  (BIN_ALG_OP (op, (x,pos1), (CONST n2,pos2)),pos))
	      | (domain'',( NO y | MAYBE (y,_,_,_) )) ->
		 (domain'',(BIN_ALG_OP (op, (x,pos1), (y,pos2)),pos))
     end
  | Ast.UN_ALG_OP (op,(a,pos1)) ->
     begin match rec_call domain (a,pos1) with
	   | (domain',YES n) ->
	      (domain',(CONST (Nbr.of_un_alg_op op n),pos))
	   | (domain',(NO x | MAYBE (x,_,_,_))) ->
	      (domain',(UN_ALG_OP (op,(x,pos1)),pos))
     end

let compile_pure_alg var_map tk_map (alg,pos) =
  snd @@ compile_alg var_map tk_map None (alg,pos)

let compile_alg ?origin var_map tk_map ?max_allowed_var
		contact_map domain (alg,pos) =
  match compile_alg var_map tk_map ?max_allowed_var
		    (Some (origin,contact_map,domain)) (alg,pos) with
  | Some (_, _,domain),alg -> domain,alg
  | None, _ -> failwith "domain has been lost in Expr.compile_alg"

let rec compile_bool var_map tk_map contact_map domain = function
  | Ast.TRUE,pos -> (domain,(Ast.TRUE,pos))
  | Ast.FALSE,pos -> (domain,(Ast.FALSE,pos))
  | Ast.BOOL_OP (op,a,b), pos ->
     begin match compile_bool var_map tk_map contact_map domain a, op with
	   | (_,(Ast.TRUE,_)), Term.OR -> (domain,(Ast.TRUE,pos))
	   | (_,(Ast.FALSE,_)), Term.AND -> (domain,(Ast.FALSE,pos))
	   | (_,(Ast.TRUE,_)), Term.AND
	   | (_,(Ast.FALSE,_)), Term.OR ->
	      compile_bool var_map tk_map contact_map domain b
	   | (domain',
	      ((Ast.BOOL_OP _ | Ast.COMPARE_OP _) ,_ as a') as out1),_ ->
	      match compile_bool var_map tk_map contact_map domain' b, op with
	      | (_,(Ast.TRUE,_)), Term.OR -> (domain,(Ast.TRUE,pos))
	      | (_,(Ast.FALSE,_)), Term.AND -> (domain,(Ast.FALSE,pos))
	      | (_,(Ast.TRUE,_)), Term.AND
	      | (_,(Ast.FALSE,_)), Term.OR -> out1
	      | (domain'',
		 ((Ast.BOOL_OP _ | Ast.COMPARE_OP _),_ as b')),_ ->
		 (domain'',(Ast.BOOL_OP (op,a',b'),pos))
     end
  | Ast.COMPARE_OP (op,a,b),pos ->
     let (domain',a') =
       compile_alg var_map tk_map contact_map domain a in
     let (domain'',b') =
       compile_alg var_map tk_map contact_map domain' b in
     match a',b' with
     | (CONST n1,_), (CONST n2,_) ->
	(domain'',
	 ((if Nbr.of_compare_op op n1 n2 then Ast.TRUE else Ast.FALSE),pos))
     | (( BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | ALG_VAR _
	  | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_), _ ->
	(domain'',(Ast.COMPARE_OP (op,a',b'), pos))

let rec has_time_dep (in_t,_,deps as vars_deps) = function
  | (BIN_ALG_OP (_, a, b),_) ->
     has_time_dep vars_deps a||has_time_dep vars_deps b
  | (UN_ALG_OP (_, a),_) -> has_time_dep vars_deps a
  | ((KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_) -> false
  | (STATE_ALG_OP Term.TIME_VAR,_) -> true
  | (STATE_ALG_OP (Term.CPUTIME | Term.EVENT_VAR| Term.NULL_EVENT_VAR
		  | Term.PROD_EVENT_VAR),_) -> false
  | (ALG_VAR i,_) ->
     let rec aux j =
       Term.DepSet.mem (Term.ALG j) in_t ||
	 Term.DepSet.exists
	   (function Term.ALG k -> aux k
		   | (Term.RULE _ | Term.PERT _) -> false) deps.(j) in
     aux i

let rec stops_of_bool_expr vars_deps = function
    | Ast.TRUE | Ast.FALSE -> []
    | Ast.BOOL_OP (op,(a,_),(b,_)) ->
       let st1 = stops_of_bool_expr vars_deps a in
       let st2 = stops_of_bool_expr vars_deps b in
       (match op,st1,st2 with
	| _, [], _ -> st2
	| _, _, [] -> st1
	| Term.OR, n1, n2 -> n1 @ n2
	| Term.AND, _, _ -> raise ExceptionDefn.Unsatisfiable
       )
    | Ast.COMPARE_OP (op,(a1,_ as a),(b1,_ as b)) ->
       match op with
       | Term.EQUAL when has_time_dep vars_deps a||has_time_dep vars_deps b ->
	  begin match a1,b1 with
		| STATE_ALG_OP (Term.TIME_VAR), CONST n
		| CONST n, STATE_ALG_OP (Term.TIME_VAR) -> [n]
		| ( BIN_ALG_OP _ | UN_ALG_OP _ | ALG_VAR _
		    | STATE_ALG_OP (Term.CPUTIME | Term.EVENT_VAR | Term.TIME_VAR
				    | Term.NULL_EVENT_VAR | Term.PROD_EVENT_VAR)
		    | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ ->
		   raise ExceptionDefn.Unsatisfiable
	  end
       | (Term.EQUAL | Term.SMALLER | Term.GREATER | Term.DIFF) -> []
