open Ast

let rec print_ast_alg f = function
  | Ast.EMAX -> Printf.fprintf f "[Emax]"
  | Ast.TMAX -> Printf.fprintf f "[Tmax]"
  | Ast.CONST n -> Nbr.print f n
  | Ast.OBS_VAR lab -> Printf.fprintf f "'%s'" lab
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
  | Ast.TOKEN_ID tk -> Printf.sprintf "|%s|" tk
  | Ast.STATE_ALG_OP op -> Term.state_alg_op_to_string () op
  | Ast.BIN_ALG_OP (op, (a,_), (b,_)) ->
     Printf.sprintf "(%a %a %a)" ast_alg_to_string a
		    Term.bin_alg_op_to_string op ast_alg_to_string b
  | Ast.UN_ALG_OP (op, (a,_)) ->
     Printf.sprintf "(%a %a)" Term.un_alg_op_to_string op ast_alg_to_string a

let rec print_bool f = function
  | Ast.TRUE -> Printf.fprintf f "[true]"
  | Ast.FALSE -> Printf .fprintf f "[false]"
  | Ast.AND ((a,_), (b,_)) ->
     Printf.fprintf f "(%a && %a)" print_bool a print_bool b
  | Ast.OR ((a,_), (b,_)) ->
     Printf.fprintf f "(%a || %a)" print_bool a print_bool b
  | Ast.GREATER ((a,_), (b,_)) ->
     Printf.fprintf f "(%a > %a)" print_ast_alg a print_ast_alg b
  | Ast.SMALLER ((a,_), (b,_)) ->
     Printf.fprintf f "(%a < %a)" print_ast_alg a print_ast_alg b
  | Ast.EQUAL ((a,_), (b,_)) ->
     Printf.fprintf f "(%a = %a)" print_ast_alg a print_ast_alg b
  | Ast.DIFF ((a,_), (b,_)) ->
     Printf.fprintf f "(%a != %a)" print_ast_alg a print_ast_alg b

let rec bool_to_string () = function
  | Ast.TRUE -> Printf.sprintf "[true]"
  | Ast.FALSE -> Printf .sprintf "[false]"
  | Ast.AND ((a,_), (b,_)) ->
     Printf.sprintf "(%a && %a)" bool_to_string a bool_to_string b
  | Ast.OR ((a,_), (b,_)) ->
     Printf.sprintf "(%a || %a)" bool_to_string a bool_to_string b
  | Ast.GREATER ((a,_), (b,_)) ->
     Printf.sprintf "(%a > %a)" ast_alg_to_string a ast_alg_to_string b
  | Ast.SMALLER ((a,_), (b,_)) ->
     Printf.sprintf "(%a < %a)" ast_alg_to_string a ast_alg_to_string b
  | Ast.EQUAL ((a,_), (b,_)) ->
     Printf.sprintf "(%a = %a)" ast_alg_to_string a ast_alg_to_string b
  | Ast.DIFF ((a,_), (b,_)) ->
     Printf.sprintf "(%a != %a)" ast_alg_to_string a ast_alg_to_string b
