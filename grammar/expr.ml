open Alg_expr

type ('a,'b) contractible = NO of 'a
			  | MAYBE of 'a * Operator.bin_alg_op * 'a * 'b
			  | YES of 'b

let rec compile_alg counter domain  (alg,pos) =
  let rec_call domain x =
    match compile_alg counter domain x with
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
	  (Some (origin,contact_map,domain'),
	   (KAPPA_INSTANCE (List.map fst ccs),pos))
       | None ->
	  raise (ExceptionDefn.Internal_Error
		   ("Theoritically pure alg_expr has a mixture",pos))
     end
  | Ast.OBS_VAR i -> (domain,(ALG_VAR i,pos))
  | Ast.TOKEN_ID i -> (domain,(TOKEN_ID i,pos))
  | Ast.STATE_ALG_OP (op) -> (domain,(STATE_ALG_OP (op),pos))
  | Ast.CONST n -> (domain,(CONST n,pos))
  | Ast.EMAX ->
     let getMaxEventValue =
       match Counter.max_events counter with
       | Some n -> Nbr.I n
       | None ->
	  ExceptionDefn.warning
	    ~pos (fun f -> Format.pp_print_string
			     f "[Emax] constant is evaluated to infinity");
	  Nbr.F infinity in
     (domain,(CONST getMaxEventValue,pos))
  | Ast.TMAX ->
     let getMaxTimeValue = match Counter.max_time counter with
       | Some t -> Nbr.F t
       | None ->
	  ExceptionDefn.warning
	    ~pos (fun f -> Format.pp_print_string
			     f "[Tmax] constant is evaluated to infinity");
		  Nbr.F infinity in
     (domain,(CONST getMaxTimeValue,pos))
  | Ast.PLOTNUM ->
	  let getPointNumberValue = Nbr.I (Counter.plot_points counter) in
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

let compile_pure_alg counter (alg,pos) =
  snd @@ compile_alg counter None (alg,pos)

let compile_alg ?origin contact_map counter domain (alg,pos) =
  match compile_alg counter (Some (origin,contact_map,domain)) (alg,pos) with
  | Some (_, _,domain),alg -> domain,alg
  | None, _ -> failwith "domain has been lost in Expr.compile_alg"

let rec compile_bool contact_map counter domain = function
  | Ast.TRUE,pos -> (domain,(Ast.TRUE,pos))
  | Ast.FALSE,pos -> (domain,(Ast.FALSE,pos))
  | Ast.BOOL_OP (op,a,b), pos ->
     begin match compile_bool contact_map counter domain a, op with
	   | (_,(Ast.TRUE,_)), Operator.OR -> (domain,(Ast.TRUE,pos))
	   | (_,(Ast.FALSE,_)), Operator.AND -> (domain,(Ast.FALSE,pos))
	   | (_,(Ast.TRUE,_)), Operator.AND
	   | (_,(Ast.FALSE,_)), Operator.OR ->
	      compile_bool contact_map counter domain b
	   | (domain',
	      ((Ast.BOOL_OP _ | Ast.COMPARE_OP _) ,_ as a') as out1),_ ->
	      match compile_bool contact_map counter domain' b, op with
	      | (_,(Ast.TRUE,_)), Operator.OR -> (domain,(Ast.TRUE,pos))
	      | (_,(Ast.FALSE,_)), Operator.AND -> (domain,(Ast.FALSE,pos))
	      | (_,(Ast.TRUE,_)), Operator.AND
	      | (_,(Ast.FALSE,_)), Operator.OR -> out1
	      | (domain'',
		 ((Ast.BOOL_OP _ | Ast.COMPARE_OP _),_ as b')),_ ->
		 (domain'',(Ast.BOOL_OP (op,a',b'),pos))
     end
  | Ast.COMPARE_OP (op,a,b),pos ->
     let (domain',a') =
       compile_alg contact_map counter domain a in
     let (domain'',b') =
       compile_alg contact_map counter domain' b in
     match a',b' with
     | (CONST n1,_), (CONST n2,_) ->
	(domain'',
	 ((if Nbr.of_compare_op op n1 n2 then Ast.TRUE else Ast.FALSE),pos))
     | (( BIN_ALG_OP _ | UN_ALG_OP _ | STATE_ALG_OP _ | ALG_VAR _
	  | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_), _ ->
	(domain'',(Ast.COMPARE_OP (op,a',b'), pos))

let rec has_time_dep (in_t,_,_,deps as vars_deps) = function
  | (BIN_ALG_OP (_, a, b),_) ->
     has_time_dep vars_deps a||has_time_dep vars_deps b
  | (UN_ALG_OP (_, a),_) -> has_time_dep vars_deps a
  | ((KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _),_) -> false
  | (STATE_ALG_OP Operator.TIME_VAR,_) -> true
  | (STATE_ALG_OP (Operator.CPUTIME | Operator.EVENT_VAR|
		   Operator.NULL_EVENT_VAR),_) -> false
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
				    Operator.TIME_VAR | Operator.NULL_EVENT_VAR)
		    | KAPPA_INSTANCE _ | TOKEN_ID _ | CONST _), _ ->
		   raise ExceptionDefn.Unsatisfiable
	  end
       | (Operator.EQUAL | Operator.SMALLER | Operator.GREATER | Operator.DIFF) -> []
