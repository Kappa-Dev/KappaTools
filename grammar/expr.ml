let rec print_ast_alg f = function
  | Ast.EMAX -> Printf.fprintf f "[Emax]"
  | Ast.TMAX -> Printf.fprintf f "[Tmax]"
  | Ast.CONST n -> Nbr.print f n
  | Ast.OBS_VAR lab -> Printf.fprintf f "'%s'" lab
  | Ast.KAPPA_INSTANCE ast ->
     Printf.fprintf f "|#no printer for mixture, sorry#|"
  | Ast.TOKEN_ID tk -> Printf.fprintf f "|%s|" tk
  | Ast.STATE_ALG_OP op -> Term.print_state_alg_op f op
  | Ast.BIN_ALG_OP (op, (a,_), (b,_)) ->
     Printf.fprintf f "(%a %a %a)"
		    print_ast_alg a Term.print_bin_alg_op op print_ast_alg b
  |Ast.UN_ALG_OP (op, (a,_)) ->
   Printf.fprintf f "(%a %a)" Term.print_un_alg_op op print_ast_alg a
let rec ast_alg_to_string () = function
  | Ast.EMAX -> Printf.sprintf "[Emax]"
  | Ast.TMAX -> Printf.sprintf "[Tmax]"
  | Ast.CONST n -> Nbr.to_string n
  | Ast.OBS_VAR lab -> Printf.sprintf "'%s'" lab
  | Ast.KAPPA_INSTANCE ast ->
     Printf.sprintf "|#no printer for mixture, sorry#|"
  | Ast.TOKEN_ID tk -> Printf.sprintf "|%s|" tk
  | Ast.STATE_ALG_OP op -> Term.state_alg_op_to_string () op
  | Ast.BIN_ALG_OP (op, (a,_), (b,_)) ->
     Printf.sprintf "(%a %a %a)" ast_alg_to_string a
		    Term.bin_alg_op_to_string op ast_alg_to_string b
  | Ast.UN_ALG_OP (op, (a,_)) ->
     Printf.sprintf "(%a %a)" Term.un_alg_op_to_string op ast_alg_to_string a

let rec print_bool p_alg f = function
  | Ast.TRUE -> Printf.fprintf f "[true]"
  | Ast.FALSE -> Printf .fprintf f "[false]"
  | Ast.BOOL_OP (op,(a,_), (b,_)) ->
     Printf.fprintf f "(%a %a %a)" (print_bool p_alg) a
		    Term.print_bool_op op (print_bool p_alg) b
  | Ast.COMPARE_OP (op,(a,_), (b,_)) ->
     Printf.fprintf f "(%a %a %a)"
		    p_alg a Term.print_compare_op op p_alg b
let rec bool_to_string alg_to_str () = function
  | Ast.TRUE -> Printf.sprintf "[true]"
  | Ast.FALSE -> Printf .sprintf "[false]"
  | Ast.BOOL_OP (op, (a,_), (b,_)) ->
     Printf.sprintf "(%a %a %a)" (bool_to_string alg_to_str) a
		    Term.bool_op_to_string op (bool_to_string alg_to_str) b
  | Ast.COMPARE_OP (op, (a,_), (b,_)) ->
     Printf.sprintf "(%a %a %a)"
		    alg_to_str a Term.compare_op_to_string op alg_to_str b

let print_ast_bool = print_bool print_ast_alg
let ast_bool_to_string = bool_to_string ast_alg_to_string

type alg_expr =
    BIN_ALG_OP of Term.bin_alg_op * alg_expr Term.with_pos * alg_expr Term.with_pos
  | UN_ALG_OP of Term.un_alg_op * alg_expr Term.with_pos
  | STATE_ALG_OP of Term.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of int
  | TOKEN_ID of int
  | CONST of Nbr.t

type ('a,'b) contractible = NO of 'a
			  | MAYBE of 'a * Term.bin_alg_op * 'a * 'b
			  | YES of 'b

let rec aux_compil env mixs (alg,(beg_pos,_ as pos)) =
  let rec_call env mixs x =
    match aux_compil env mixs x with
    | (env', mixs', (ALG_VAR _ | TOKEN_ID _ | UN_ALG_OP _ | STATE_ALG_OP _
		     | KAPPA_INSTANCE _ as alg,_)) -> (env', mixs', NO alg)
    | (env',mixs',(CONST n,_)) -> (env',mixs',YES n)
    | (env',mixs',(BIN_ALG_OP (op, (x,_), (CONST y,_)) as alg,_)) ->
       (env',mixs',MAYBE(alg,op,x,y))
    | (env',mixs',(BIN_ALG_OP _ as alg,_)) -> (env',mixs',NO alg)
  in
  match alg with
  | Ast.KAPPA_INSTANCE ast ->
     let (env', id) =
       Environment.declare_var_kappa None env in
     (env', (id,ast)::mixs, (KAPPA_INSTANCE id,pos))
  | Ast.OBS_VAR lab ->
     let i,_ =
       try Environment.num_of_alg lab env with
       | Not_found ->
	  raise (ExceptionDefn.Semantics_Error
		   (Tools.pos_of_lex_pos beg_pos,
		    lab ^" is not a declared variable"))
     in (env,mixs,(ALG_VAR i,pos))
  | Ast.TOKEN_ID tk_nme ->
     let i =
       try Environment.num_of_token tk_nme env
       with Not_found ->
	 raise (ExceptionDefn.Semantics_Error
		  (Tools.pos_of_lex_pos beg_pos,tk_nme ^ " is not a declared token"))
     in (env,mixs,(TOKEN_ID i,pos))
  | Ast.STATE_ALG_OP (op) -> (env,mixs,(STATE_ALG_OP (op),pos))
  | Ast.CONST n -> (env,mixs,(CONST n,pos))
  | Ast.EMAX -> (env,mixs,(CONST (Parameter.getMaxEventValue ()),pos))
  | Ast.TMAX ->
     (env,mixs,(CONST (Parameter.getMaxTimeValue ()),pos))
  | Ast.BIN_ALG_OP (op, (a,pos1), (b,pos2)) ->
     begin match rec_call env mixs (a,pos1) with
	   | (env',mixs',YES n1) ->
	      begin
		match rec_call env' mixs' (b,pos2) with
		| (env'',mixs'',YES n2) ->
		   (env'',mixs'',(CONST (Nbr.of_bin_alg_op op n1 n2),pos))
		| (env'',mixs'',( NO y | MAYBE (y,_,_,_) )) ->
		   (env'',mixs'',
		    (BIN_ALG_OP (op, (CONST n1,pos1), (y,pos2)),pos))
	      end
	   | (env',mixs',( NO x | MAYBE (x,_,_,_) )) ->
	      match rec_call env' mixs' (b,pos2) with
	      | (env'',mixs'',YES n2) ->
		 (env'',mixs'',
		  (BIN_ALG_OP (op, (x,pos1), (CONST n2,pos2)),pos))
	      | (env'',mixs'',( NO y | MAYBE (y,_,_,_) )) ->
		 (env'',mixs'',(BIN_ALG_OP (op, (x,pos1), (y,pos2)),pos))
     end
  | Ast.UN_ALG_OP (op,(a,pos1)) ->
     begin match rec_call env mixs (a,pos1) with
	   | (env',mixs',YES n) ->
	      (env',mixs',(CONST (Nbr.of_un_alg_op op n),pos))
	   | (env',mixs',(NO x | MAYBE (x,_,_,_))) ->
	      (env',mixs',(UN_ALG_OP (op,(x,pos1)),pos))
     end

let compile_alg env alg_pos =
  aux_compil env [] alg_pos

let compile_bool env bool_pos =
  let rec aux env mixs = function
    | Ast.TRUE,pos -> (env,mixs,(Ast.TRUE,pos))
    | Ast.FALSE,pos -> (env,mixs,(Ast.FALSE,pos))
    | Ast.BOOL_OP (op,a,b), pos ->
       begin match aux env mixs a, op with
	     | (_,_,(Ast.TRUE,_)), Term.OR -> (env,mixs,(Ast.TRUE,pos))
	     | (_,_,(Ast.FALSE,_)), Term.AND -> (env,mixs,(Ast.FALSE,pos))
	     | (_,_,(Ast.TRUE,_)), Term.AND
	     | (_,_,(Ast.FALSE,_)), Term.OR -> aux env mixs b
	     | (env',mixs',a' as out1),_ ->
		match aux env' mixs' b, op with
		| (_,_,(Ast.TRUE,_)), Term.OR -> (env,mixs,(Ast.TRUE,pos))
		| (_,_,(Ast.FALSE,_)), Term.AND -> (env,mixs,(Ast.FALSE,pos))
		| (_,_,(Ast.TRUE,_)), Term.AND
		| (_,_,(Ast.FALSE,_)), Term.OR -> out1
		| (env'',mixs'',b'),_ -> (env'',mixs'',(Ast.BOOL_OP (op,a',b'),pos))
       end
    | Ast.COMPARE_OP (op,a,b),pos ->
       let (env',mixs',a') = aux_compil env mixs a in
       let (env'',mixs'',b') = aux_compil env' mixs' b in
       match a',b' with
       | (CONST n1,_), (CONST n2,_) ->
	  (env'',mixs'',
	   ((if Nbr.of_compare_op op n1 n2 then Ast.TRUE else Ast.FALSE),pos))
       | _, _ -> (env'',mixs'',(Ast.COMPARE_OP (op,a',b'), pos))
  in aux env [] bool_pos
