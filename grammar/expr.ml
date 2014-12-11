let rec print_ast_alg f = function
  | Ast.EMAX -> Format.fprintf f "[Emax]"
  | Ast.PLOTNUM -> Format.fprintf f "[p]"
  | Ast.TMAX -> Format.fprintf f "[Tmax]"
  | Ast.CONST n -> Nbr.print f n
  | Ast.OBS_VAR lab -> Format.fprintf f "'%s'" lab
  | Ast.KAPPA_INSTANCE ast ->
     Format.pp_print_string f "|#no printer for mixture, sorry#|"
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
  | KAPPA_INSTANCE of int
  | TOKEN_ID of int
  | CONST of Nbr.t

type ('a,'b) contractible = NO of 'a
			  | MAYBE of 'a * Term.bin_alg_op * 'a * 'b
			  | YES of 'b

let rec compile_alg ?label var_map tk_map ?max_allowed_var
		    (fr_mix_id,mix_l as mixs) (alg,(beg_pos,_ as pos)) =
  let rec_call mixs x =
    match compile_alg var_map tk_map ?max_allowed_var mixs x with
    | (mixs', (ALG_VAR _ | TOKEN_ID _ | UN_ALG_OP _ | STATE_ALG_OP _
		     | KAPPA_INSTANCE _ as alg,_)) -> (mixs', NO alg)
    | (mixs',(CONST n,_)) -> (mixs',YES n)
    | (mixs',(BIN_ALG_OP (op, (x,_), (CONST y,_)) as alg,_)) ->
       (mixs',MAYBE(alg,op,x,y))
    | (mixs',(BIN_ALG_OP _ as alg,_)) -> (mixs',NO alg)
  in
  match alg with
  | Ast.KAPPA_INSTANCE ast ->
     ((succ fr_mix_id,(label,ast)::mix_l), (KAPPA_INSTANCE fr_mix_id,pos))
  | Ast.OBS_VAR lab ->
     let i =
       try Mods.StringMap.find lab var_map with
       | Not_found ->
	  raise (ExceptionDefn.Semantics_Error
		   (Tools.pos_of_lex_pos beg_pos,
		    lab ^" is not a declared variable"))
     in
     let () = match max_allowed_var with
       | Some j when j < i ->
	  raise (ExceptionDefn.Semantics_Error
		   (Tools.pos_of_lex_pos beg_pos,
		    "Reference to not yet defined '"^lab ^"' is forbidden."))
       | None | Some _ -> ()
     in(mixs,(ALG_VAR i,pos))
  | Ast.TOKEN_ID tk_nme ->
     let i =
       try Mods.StringMap.find tk_nme tk_map
       with Not_found ->
	 raise (ExceptionDefn.Semantics_Error
		  (Tools.pos_of_lex_pos beg_pos,tk_nme ^ " is not a declared token"))
     in (mixs,(TOKEN_ID i,pos))
  | Ast.STATE_ALG_OP (op) -> (mixs,(STATE_ALG_OP (op),pos))
  | Ast.CONST n -> (mixs,(CONST n,pos))
  | Ast.EMAX -> (mixs,(CONST (Nbr.getMaxEventValue ()),pos))
  | Ast.TMAX -> (mixs,(CONST (Nbr.getMaxTimeValue ()),pos))
  | Ast.PLOTNUM -> (mixs,(CONST (Nbr.getPointNumberValue ()),pos))
  | Ast.BIN_ALG_OP (op, (a,pos1), (b,pos2)) ->
     begin match rec_call mixs (a,pos1) with
	   | (mixs',YES n1) ->
	      begin
		match rec_call mixs' (b,pos2) with
		| (mixs'',YES n2) ->
		   (mixs'',(CONST (Nbr.of_bin_alg_op op n1 n2),pos))
		| (mixs'',( NO y | MAYBE (y,_,_,_) )) ->
		   (mixs'',
		    (BIN_ALG_OP (op, (CONST n1,pos1), (y,pos2)),pos))
	      end
	   | (mixs',( NO x | MAYBE (x,_,_,_) )) ->
	      match rec_call mixs' (b,pos2) with
	      | (mixs'',YES n2) ->
		 (mixs'', (BIN_ALG_OP (op, (x,pos1), (CONST n2,pos2)),pos))
	      | (mixs'',( NO y | MAYBE (y,_,_,_) )) ->
		 (mixs'',(BIN_ALG_OP (op, (x,pos1), (y,pos2)),pos))
     end
  | Ast.UN_ALG_OP (op,(a,pos1)) ->
     begin match rec_call mixs (a,pos1) with
	   | (mixs',YES n) ->
	      (mixs',(CONST (Nbr.of_un_alg_op op n),pos))
	   | (mixs',(NO x | MAYBE (x,_,_,_))) ->
	      (mixs',(UN_ALG_OP (op,(x,pos1)),pos))
     end

let rec compile_bool var_map tk_map mixs = function
  | Ast.TRUE,pos -> (mixs,(Ast.TRUE,pos))
  | Ast.FALSE,pos -> (mixs,(Ast.FALSE,pos))
  | Ast.BOOL_OP (op,a,b), pos ->
     begin match compile_bool var_map tk_map mixs a, op with
	   | (_,(Ast.TRUE,_)), Term.OR -> (mixs,(Ast.TRUE,pos))
	   | (_,(Ast.FALSE,_)), Term.AND -> (mixs,(Ast.FALSE,pos))
	   | (_,(Ast.TRUE,_)), Term.AND
	   | (_,(Ast.FALSE,_)), Term.OR -> compile_bool var_map tk_map mixs b
	   | (mixs',a' as out1),_ ->
	      match compile_bool var_map tk_map mixs' b, op with
	      | (_,(Ast.TRUE,_)), Term.OR -> (mixs,(Ast.TRUE,pos))
	      | (_,(Ast.FALSE,_)), Term.AND -> (mixs,(Ast.FALSE,pos))
	      | (_,(Ast.TRUE,_)), Term.AND
	      | (_,(Ast.FALSE,_)), Term.OR -> out1
	      | (mixs'',b'),_ -> (mixs'',(Ast.BOOL_OP (op,a',b'),pos))
     end
  | Ast.COMPARE_OP (op,a,b),pos ->
     let (mixs',a') = compile_alg var_map tk_map mixs a in
     let (mixs'',b') = compile_alg var_map tk_map mixs' b in
     match a',b' with
     | (CONST n1,_), (CONST n2,_) ->
	(mixs'',
	 ((if Nbr.of_compare_op op n1 n2 then Ast.TRUE else Ast.FALSE),pos))
     | _, _ -> (mixs'',(Ast.COMPARE_OP (op,a',b'), pos))

let add_dep el s = Term.DepSet.add el s
let rec aux_dep s = function
  | BIN_ALG_OP (op, (a,_), (b,_)) -> aux_dep (aux_dep s a) b
  | UN_ALG_OP (op, (a,_)) -> aux_dep s a
  | STATE_ALG_OP op -> add_dep (Term.dep_of_state_alg_op op) s
  | ALG_VAR i -> add_dep (Term.ALG i) s
  | KAPPA_INSTANCE i -> add_dep (Term.KAPPA i) s
  | TOKEN_ID i -> add_dep (Term.TOK i) s
  | CONST _ -> s
let deps_of_alg_expr alg = aux_dep Term.DepSet.empty alg

let rec deps_of_bool_expr = function
    | Ast.TRUE | Ast.FALSE -> Term.DepSet.empty,None
    | Ast.BOOL_OP (op,(a,_),(b,_)) ->
       let (s1,st1) = deps_of_bool_expr a in
       let (s2,st2) = deps_of_bool_expr b in
       (Term.DepSet.union s1 s2,
	match op,st1,st2 with
	| _, None, _ -> st2
	| _, _, None -> st1
	| Term.OR, Some n1, Some n2 -> Some (Nbr.min n1 n2)
	| Term.AND, Some n1, Some n2 ->
	   if Nbr.is_equal n1 n2 then st1 else raise ExceptionDefn.Unsatisfiable
       )
    | Ast.COMPARE_OP (op,(a,_),(b,_)) ->
       let s = aux_dep (aux_dep Term.DepSet.empty a) b in
       match op with
       | Term.EQUAL when Term.DepSet.mem Term.TIME s ->
	  begin match a,b with
		| STATE_ALG_OP (Term.TIME_VAR), CONST n
		| CONST n, STATE_ALG_OP (Term.TIME_VAR) -> (s, Some n)
		| _, _ -> raise ExceptionDefn.Unsatisfiable
	  end
       | _ -> (s, None)
