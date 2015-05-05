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

type alg_expr =
    BIN_ALG_OP of
      Term.bin_alg_op * alg_expr Term.with_pos * alg_expr Term.with_pos
  | UN_ALG_OP of Term.un_alg_op * alg_expr Term.with_pos
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of (int * Connected_component.cc list list)
  | TOKEN_ID of int
  | CONST of Nbr.t

type ('a,'b) contractible = NO of 'a
			  | MAYBE of 'a * Term.bin_alg_op * 'a * 'b
			  | YES of 'b

let rec compile_alg ?label var_map tk_map ?max_allowed_var
		    contact_map domain
		    (fr_mix_id,mix_l as mixs) (alg,pos) =
  let rec_call domain mixs x =
    match compile_alg var_map tk_map ?max_allowed_var contact_map domain mixs x with
    | (domain',mixs', (ALG_VAR _ | TOKEN_ID _ | UN_ALG_OP _ | STATE_ALG_OP _
		       | KAPPA_INSTANCE _ as alg,_)) -> (domain',mixs', NO alg)
    | (domain',mixs',(CONST n,_)) -> (domain',mixs',YES n)
    | (domain',mixs',(BIN_ALG_OP (op, (x,_), (CONST y,_)) as alg,_)) ->
       (domain',mixs',MAYBE(alg,op,x,y))
    | (domain',mixs',(BIN_ALG_OP _ as alg,_)) -> (domain',mixs',NO alg)
  in
  match alg with
  | Ast.KAPPA_INSTANCE ast ->
     let domain',ccs =
       Snip.connected_components_sum_of_ambiguous_mixture
	 contact_map domain ast in
     (domain',
      (succ fr_mix_id,(label,ast)::mix_l), (KAPPA_INSTANCE (fr_mix_id,ccs),pos))
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
     in (domain,mixs,(ALG_VAR i,pos))
  | Ast.TOKEN_ID tk_nme ->
     let i =
       try Mods.StringMap.find tk_nme tk_map
       with Not_found ->
	 raise (ExceptionDefn.Malformed_Decl
		  (tk_nme ^ " is not a declared token",pos))
     in (domain,mixs,(TOKEN_ID i,pos))
  | Ast.STATE_ALG_OP (op) -> (domain,mixs,(STATE_ALG_OP (op),pos))
  | Ast.CONST n -> (domain,mixs,(CONST n,pos))
  | Ast.EMAX ->
     let getMaxEventValue =
       match !Parameter.maxEventValue with
       | Some n -> Nbr.I n
       | None -> Format.eprintf "[emax] constant is evaluated to infinity@.";
		 Nbr.F infinity in
     (domain,mixs,(CONST getMaxEventValue,pos))
  | Ast.TMAX ->
     let getMaxTimeValue = match !Parameter.maxTimeValue with
       | Some t -> Nbr.F t
       | None -> Format.eprintf "[tmax] constant is evaluated to infinity@.";
		 Nbr.F infinity in
     (domain,mixs,(CONST getMaxTimeValue,pos))
  | Ast.PLOTNUM ->
	  let getPointNumberValue = Nbr.I !Parameter.pointNumberValue in
	  (domain,mixs,(CONST getPointNumberValue,pos))
  | Ast.BIN_ALG_OP (op, (a,pos1), (b,pos2)) ->
     begin match rec_call domain mixs (a,pos1) with
	   | (domain',mixs',YES n1) ->
	      begin
		match rec_call domain' mixs' (b,pos2) with
		| (domain'',mixs'',YES n2) ->
		   (domain'',mixs'',(CONST (Nbr.of_bin_alg_op op n1 n2),pos))
		| (domain'',mixs'',( NO y | MAYBE (y,_,_,_) )) ->
		   (domain'',mixs'',
		    (BIN_ALG_OP (op, (CONST n1,pos1), (y,pos2)),pos))
	      end
	   | (domain',mixs',( NO x | MAYBE (x,_,_,_) )) ->
	      match rec_call domain' mixs' (b,pos2) with
	      | (domain'',mixs'',YES n2) ->
		 (domain'', mixs'',
		  (BIN_ALG_OP (op, (x,pos1), (CONST n2,pos2)),pos))
	      | (domain'',mixs'',( NO y | MAYBE (y,_,_,_) )) ->
		 (domain'',mixs'',(BIN_ALG_OP (op, (x,pos1), (y,pos2)),pos))
     end
  | Ast.UN_ALG_OP (op,(a,pos1)) ->
     begin match rec_call domain mixs (a,pos1) with
	   | (domain',mixs',YES n) ->
	      (domain',mixs',(CONST (Nbr.of_un_alg_op op n),pos))
	   | (domain',mixs',(NO x | MAYBE (x,_,_,_))) ->
	      (domain',mixs',(UN_ALG_OP (op,(x,pos1)),pos))
     end

let rec compile_bool var_map tk_map contact_map domain mixs = function
  | Ast.TRUE,pos -> (domain,mixs,(Ast.TRUE,pos))
  | Ast.FALSE,pos -> (domain,mixs,(Ast.FALSE,pos))
  | Ast.BOOL_OP (op,a,b), pos ->
     begin match compile_bool var_map tk_map contact_map domain mixs a, op with
	   | (_,_,(Ast.TRUE,_)), Term.OR -> (domain,mixs,(Ast.TRUE,pos))
	   | (_,_,(Ast.FALSE,_)), Term.AND -> (domain,mixs,(Ast.FALSE,pos))
	   | (_,_,(Ast.TRUE,_)), Term.AND
	   | (_,_,(Ast.FALSE,_)), Term.OR ->
	      compile_bool var_map tk_map contact_map domain mixs b
	   | (domain',mixs',
	      ((Ast.BOOL_OP _ | Ast.COMPARE_OP _) ,_ as a') as out1),_ ->
	      match compile_bool var_map tk_map contact_map domain' mixs' b, op with
	      | (_,_,(Ast.TRUE,_)), Term.OR -> (domain,mixs,(Ast.TRUE,pos))
	      | (_,_,(Ast.FALSE,_)), Term.AND -> (domain,mixs,(Ast.FALSE,pos))
	      | (_,_,(Ast.TRUE,_)), Term.AND
	      | (_,_,(Ast.FALSE,_)), Term.OR -> out1
	      | (domain'',mixs'',
		 ((Ast.BOOL_OP _ | Ast.COMPARE_OP _),_ as b')),_ ->
		 (domain'',mixs'',(Ast.BOOL_OP (op,a',b'),pos))
     end
  | Ast.COMPARE_OP (op,a,b),pos ->
     let (domain',mixs',a') =
       compile_alg var_map tk_map contact_map domain mixs a in
     let (domain'',mixs'',b') =
       compile_alg var_map tk_map contact_map domain' mixs' b in
     match a',b' with
     | (CONST n1,_), (CONST n2,_) ->
	(domain'',mixs'',
	 ((if Nbr.of_compare_op op n1 n2 then Ast.TRUE else Ast.FALSE),pos))
     | (( BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | ALG_VAR _
	  | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_), _ ->
	(domain'',mixs'',(Ast.COMPARE_OP (op,a',b'), pos))

let add_dep el s = Term.DepSet.add el s
let rec aux_dep s = function
  | BIN_ALG_OP (_, (a,_), (b,_)) -> aux_dep (aux_dep s a) b
  | UN_ALG_OP (_, (a,_)) -> aux_dep s a
  | STATE_ALG_OP op -> add_dep (Term.dep_of_state_alg_op op) s
  | ALG_VAR i -> add_dep (Term.ALG i) s
  | KAPPA_INSTANCE (i,_) -> add_dep (Term.KAPPA i) s
  | TOKEN_ID i -> add_dep (Term.TOK i) s
  | CONST _ -> s
let deps_of_alg_expr alg = aux_dep Term.DepSet.empty alg

let rec deps_of_bool_expr = function
    | Ast.TRUE | Ast.FALSE -> Term.DepSet.empty,[]
    | Ast.BOOL_OP (op,(a,_),(b,_)) ->
       let (s1,st1) = deps_of_bool_expr a in
       let (s2,st2) = deps_of_bool_expr b in
       (Term.DepSet.union s1 s2,
	match op,st1,st2 with
	| _, [], _ -> st2
	| _, _, [] -> st1
	| Term.OR, n1, n2 -> n1 @ n2
	| Term.AND, _, _ -> raise ExceptionDefn.Unsatisfiable
       )
    | Ast.COMPARE_OP (op,(a,_),(b,_)) ->
       let s = aux_dep (aux_dep Term.DepSet.empty a) b in
       match op with
       | Term.EQUAL when Term.DepSet.mem Term.TIME s ->
	  begin match a,b with
		| STATE_ALG_OP (Term.TIME_VAR), CONST n
		| CONST n, STATE_ALG_OP (Term.TIME_VAR) -> (s, [n])
		| ( BIN_ALG_OP _ | UN_ALG_OP _ | ALG_VAR _
		    | STATE_ALG_OP (Term.CPUTIME | Term.EVENT_VAR | Term.TIME_VAR
				    | Term.NULL_EVENT_VAR | Term.PROD_EVENT_VAR)
		    | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ ->
		   raise ExceptionDefn.Unsatisfiable
	  end
       | (Term.EQUAL | Term.SMALLER | Term.GREATER | Term.DIFF) -> (s, [])
