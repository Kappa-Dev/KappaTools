type t =
    BIN_ALG_OP of Operator.bin_alg_op * t Location.annot * t Location.annot
  | UN_ALG_OP of Operator.un_alg_op * t Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of int
  | KAPPA_INSTANCE of Connected_component.cc array list
  | TOKEN_ID of int
  | CONST of Nbr.t

let rec add_dep (in_t,in_e,toks_d,out as x) d = function
  | BIN_ALG_OP (_, a, b), _ -> add_dep (add_dep x d a) d b
  | UN_ALG_OP (_, a), _ -> add_dep x d a
  | ALG_VAR j, _ ->
     let () = out.(j) <- Operator.DepSet.add d out.(j) in
     x
  | (KAPPA_INSTANCE _ | CONST _), _ -> x
  | TOKEN_ID i, _ ->
    let () = toks_d.(i) <- Operator.DepSet.add d toks_d.(i) in
    x
  | STATE_ALG_OP op, _ ->
     match op with
     | (Operator.EMAX_VAR | Operator.TMAX_VAR | Operator.PLOTNUM) -> x
     | Operator.TIME_VAR -> (Operator.DepSet.add d in_t,in_e,toks_d,out)
     | (Operator.CPUTIME | Operator.EVENT_VAR | Operator.NULL_EVENT_VAR) ->
	(in_t,Operator.DepSet.add d in_e,toks_d,out)

let setup_alg_vars_rev_dep toks vars =
  let in_t = Operator.DepSet.empty in
  let in_e = Operator.DepSet.empty in
  let toks_d = Array.make (NamedDecls.size toks) Operator.DepSet.empty in
  let out = Array.make (Array.length vars) Operator.DepSet.empty in
  Tools.array_fold_lefti
    (fun i x (_,y) -> add_dep x (Operator.ALG i) y) (in_t,in_e,toks_d,out) vars

let rec propagate_constant updated_vars counter vars = function
  | BIN_ALG_OP (op,a,b),pos as x ->
     (match propagate_constant updated_vars counter vars a,
	    propagate_constant updated_vars counter vars b with
      | (CONST c1,_),(CONST c2,_) -> CONST (Nbr.of_bin_alg_op op c1 c2),pos
      | ((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
	  | TOKEN_ID _ | ALG_VAR _ | CONST _),_),
	((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
	  | TOKEN_ID _ | ALG_VAR _ | CONST _),_) -> x)
  | UN_ALG_OP (op,a),pos as x ->
     (match propagate_constant updated_vars counter vars a with
      | CONST c,_ -> CONST (Nbr.of_un_alg_op op c),pos
      | (BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
	 | TOKEN_ID _ | ALG_VAR _),_ -> x)
  | STATE_ALG_OP (Operator.EMAX_VAR),pos ->
     CONST
       (match Counter.max_events counter with
	| Some n -> Nbr.I n
	| None ->
	   let () =
	     ExceptionDefn.warning
	       ~pos (fun f -> Format.pp_print_string
				f "[Emax] constant is evaluated to infinity") in
	   Nbr.F infinity),pos
  | STATE_ALG_OP (Operator.TMAX_VAR),pos ->
     CONST
       (match Counter.max_time counter with
	| Some t -> Nbr.F t
	| None ->
	   let () =
	     ExceptionDefn.warning
	       ~pos (fun f -> Format.pp_print_string
				f "[Tmax] constant is evaluated to infinity") in
	   Nbr.F infinity),pos
  | STATE_ALG_OP (Operator.PLOTNUM),pos ->
     CONST (Nbr.I (Counter.plot_points counter)),pos
  | STATE_ALG_OP (Operator.CPUTIME | Operator.TIME_VAR | Operator.EVENT_VAR
		  | Operator.NULL_EVENT_VAR),_ as x -> x
  | ALG_VAR i,pos as x ->
     (if List.mem i updated_vars then x
      else match vars.(i) with
	   | _,((CONST _ | ALG_VAR _ as y),_) -> y,pos
	   | _,((BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | KAPPA_INSTANCE _
		 | TOKEN_ID _),_) -> x)
  | (KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_ as x -> x

let rec propagate_constant_bool updated_vars counter vars = function
  | (Ast.TRUE | Ast.FALSE),_ as x -> x
  | Ast.BOOL_OP (op,a,b),pos ->
     begin match propagate_constant_bool updated_vars counter vars a, op with
	   | (Ast.TRUE,_), Operator.OR -> Ast.TRUE,pos
	   | (Ast.FALSE,_), Operator.AND -> Ast.FALSE,pos
	   | (Ast.TRUE,_), Operator.AND
	   | (Ast.FALSE,_), Operator.OR ->
	      propagate_constant_bool updated_vars counter vars b
	   | ((Ast.BOOL_OP _ | Ast.COMPARE_OP _),_ as a'),_ ->
	      match propagate_constant_bool updated_vars counter vars b, op with
	      | (Ast.TRUE,_), Operator.OR -> Ast.TRUE,pos
	      | (Ast.FALSE,_), Operator.AND -> Ast.FALSE,pos
	      | (Ast.TRUE,_), Operator.AND
	      | (Ast.FALSE,_), Operator.OR -> a'
	      | ((Ast.BOOL_OP _ | Ast.COMPARE_OP _),_ as b'),_ ->
		 Ast.BOOL_OP (op,a',b'),pos
     end
  | Ast.COMPARE_OP (op,a,b),pos ->
     let a' = propagate_constant updated_vars counter vars a in
     let b' = propagate_constant updated_vars counter vars b in
     match a',b' with
     | (CONST n1,_), (CONST n2,_) ->
	(if Nbr.of_compare_op op n1 n2 then Ast.TRUE,pos else Ast.FALSE,pos)
     | (( BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | ALG_VAR _
	  | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_), _ ->
	Ast.COMPARE_OP (op,a',b'),pos

let rec has_time_dep (in_t,_,_,deps as vars_deps) = function
  | (BIN_ALG_OP (_, a, b),_) ->
     has_time_dep vars_deps a||has_time_dep vars_deps b
  | (UN_ALG_OP (_, a),_) -> has_time_dep vars_deps a
  | ((KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_) -> false
  | (STATE_ALG_OP Operator.TIME_VAR,_) -> true
  | (STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR |
		   Operator.NULL_EVENT_VAR | Operator.EMAX_VAR |
		   Operator.TMAX_VAR | Operator.PLOTNUM),_) -> false
  | (ALG_VAR i,_) ->
     let rec aux j =
       Operator.DepSet.mem (Operator.ALG j) in_t ||
	 Operator.DepSet.exists
	   (function Operator.ALG k -> aux k
		   | (Operator.RULE _ | Operator.PERT _) -> false) deps.(j) in
     aux i

let rec stops_of_bool_expr vars_deps = function
    | Ast.TRUE | Ast.FALSE -> []
    | Ast.BOOL_OP (op,(a,_),(b,_)) ->
       let st1 = stops_of_bool_expr vars_deps a in
       let st2 = stops_of_bool_expr vars_deps b in
       (match op,st1,st2 with
	| _, [], _ -> st2
	| _, _, [] -> st1
	| Operator.OR, n1, n2 -> n1 @ n2
	| Operator.AND, _, _ -> raise ExceptionDefn.Unsatisfiable
       )
    | Ast.COMPARE_OP (op,(a1,_ as a),(b1,_ as b)) ->
       match op with
       | Operator.EQUAL when has_time_dep vars_deps a||has_time_dep vars_deps b ->
	  begin match a1,b1 with
		| STATE_ALG_OP (Operator.TIME_VAR), CONST n
		| CONST n, STATE_ALG_OP (Operator.TIME_VAR) -> [n]
		| ( BIN_ALG_OP _ | UN_ALG_OP _ | ALG_VAR _
		    | STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR |
				    Operator.TIME_VAR | Operator.NULL_EVENT_VAR |
				    Operator.EMAX_VAR |Operator.TMAX_VAR |
				    Operator.PLOTNUM)
		    | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ ->
		   raise ExceptionDefn.Unsatisfiable
	  end
       | (Operator.EQUAL | Operator.SMALLER | Operator.GREATER | Operator.DIFF) -> []
