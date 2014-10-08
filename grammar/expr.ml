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
