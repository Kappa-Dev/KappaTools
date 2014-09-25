open Ast

let rec print_alg f = function
  | EMAX -> Printf.fprintf f "[Emax]"
  | TMAX -> Printf.fprintf f "[Tmax]"
  | CONST n -> Nbr.print f n
  | CPUTIME -> Printf.fprintf f "[Tsim]"
  | OBS_VAR lab -> Printf.fprintf f "'%s'" lab
  | TOKEN_ID tk -> Printf.fprintf f "|%s|" tk
  | TIME_VAR -> Printf.fprintf f "[T]"
  | EVENT_VAR -> Printf.fprintf f "[E]"
  | NULL_EVENT_VAR -> Printf.fprintf f "[E-]"
  | PROD_EVENT_VAR -> Printf.fprintf f "[E+]"
  | DIV ((a,_), (b,_)) -> Printf.fprintf f "(%a / %a)" print_alg a print_alg b
  | SUM ((a,_), (b,_)) -> Printf.fprintf f "(%a + %a)" print_alg a print_alg b
  | MULT ((a,_), (b,_)) -> Printf.fprintf f "(%a * %a)" print_alg a print_alg b
  | MINUS ((a,_), (b,_)) -> Printf.fprintf f "(%a - %a)" print_alg a print_alg b
  | POW ((a,_), (b,_)) -> Printf.fprintf f "(%a ^ %a)" print_alg a print_alg b
  | MODULO ((a,_), (b,_)) ->
     Printf.fprintf f "(%a [mod] %a)" print_alg a print_alg b
  | MAX ((a,_), (b,_)) ->
     Printf.fprintf f "([max] %a %a)" print_alg a print_alg b
  | MIN ((a,_), (b,_)) ->
     Printf.fprintf f "([min] %a %a)" print_alg a print_alg b
  | COSINUS (a,_) -> Printf.fprintf f "[cos] %a" print_alg a
  | SINUS (a,_) -> Printf.fprintf f "[sin] %a" print_alg a
  | TAN (a,_) -> Printf.fprintf f "[tan] %a" print_alg a
  | EXP (a,_) -> Printf.fprintf f "[exp] %a" print_alg a
  | SQRT (a,_) -> Printf.fprintf f "[sqrt] %a" print_alg a
  | INT (a,_) -> Printf.fprintf f "[int] %a" print_alg a
  | LOG (a,_) -> Printf.fprintf f "[log] %a" print_alg a
  | UMINUS (a,_) -> Printf.fprintf f "(- %a)" print_alg a

let rec alg_to_string () = function
  | EMAX -> Printf.sprintf "[Emax]"
  | TMAX -> Printf.sprintf "[Tmax]"
  | CONST n -> Nbr.to_string n
  | CPUTIME -> Printf.sprintf "[Tsim]"
  | OBS_VAR lab -> Printf.sprintf "'%s'" lab
  | TOKEN_ID tk -> Printf.sprintf "|%s|" tk
  | TIME_VAR -> Printf.sprintf "[T]"
  | EVENT_VAR -> Printf.sprintf "[E]"
  | NULL_EVENT_VAR -> Printf.sprintf "[E-]"
  | PROD_EVENT_VAR -> Printf.sprintf "[E+]"
  | DIV ((a,_), (b,_)) ->
     Printf.sprintf "(%a / %a)" alg_to_string a alg_to_string b
  | SUM ((a,_), (b,_)) ->
     Printf.sprintf "(%a + %a)" alg_to_string a alg_to_string b
  | MULT ((a,_), (b,_)) ->
     Printf.sprintf "(%a * %a)" alg_to_string a alg_to_string b
  | MINUS ((a,_), (b,_)) ->
     Printf.sprintf "(%a - %a)" alg_to_string a alg_to_string b
  | POW ((a,_), (b,_)) ->
     Printf.sprintf "(%a ^ %a)" alg_to_string a alg_to_string b
  | MODULO ((a,_), (b,_)) ->
     Printf.sprintf "(%a [mod] %a)" alg_to_string a alg_to_string b
  | MAX ((a,_), (b,_)) ->
     Printf.sprintf "([max] %a %a)" alg_to_string a alg_to_string b
  | MIN ((a,_), (b,_)) ->
     Printf.sprintf "([min] %a %a)" alg_to_string a alg_to_string b
  | COSINUS (a,_) -> Printf.sprintf "[cos] %a" alg_to_string a
  | SINUS (a,_) -> Printf.sprintf "[sin] %a" alg_to_string a
  | TAN (a,_) -> Printf.sprintf "[tan] %a" alg_to_string a
  | EXP (a,_) -> Printf.sprintf "[exp] %a" alg_to_string a
  | SQRT (a,_) -> Printf.sprintf "[sqrt] %a" alg_to_string a
  | INT (a,_) -> Printf.sprintf "[int] %a" alg_to_string a
  | LOG (a,_) -> Printf.sprintf "[log] %a" alg_to_string a
  | UMINUS (a,_) -> Printf.sprintf "(- %a)" alg_to_string a

let rec print_bool f = function
  | TRUE -> Printf.fprintf f "[true]"
  | FALSE -> Printf .fprintf f "[false]"
  | AND ((a,_), (b,_)) ->
     Printf.fprintf f "(%a && %a)" print_bool a print_bool b
  | OR ((a,_), (b,_)) ->
     Printf.fprintf f "(%a || %a)" print_bool a print_bool b
  | GREATER ((a,_), (b,_)) ->
     Printf.fprintf f "(%a > %a)" print_alg a print_alg b
  | SMALLER ((a,_), (b,_)) ->
     Printf.fprintf f "(%a < %a)" print_alg a print_alg b
  | EQUAL ((a,_), (b,_)) ->
     Printf.fprintf f "(%a = %a)" print_alg a print_alg b
  | DIFF ((a,_), (b,_)) ->
     Printf.fprintf f "(%a != %a)" print_alg a print_alg b

let rec bool_to_string () = function
  | TRUE -> Printf.sprintf "[true]"
  | FALSE -> Printf .sprintf "[false]"
  | AND ((a,_), (b,_)) ->
     Printf.sprintf "(%a && %a)" bool_to_string a bool_to_string b
  | OR ((a,_), (b,_)) ->
     Printf.sprintf "(%a || %a)" bool_to_string a bool_to_string b
  | GREATER ((a,_), (b,_)) ->
     Printf.sprintf "(%a > %a)" alg_to_string a alg_to_string b
  | SMALLER ((a,_), (b,_)) ->
     Printf.sprintf "(%a < %a)" alg_to_string a alg_to_string b
  | EQUAL ((a,_), (b,_)) ->
     Printf.sprintf "(%a = %a)" alg_to_string a alg_to_string b
  | DIFF ((a,_), (b,_)) ->
     Printf.sprintf "(%a != %a)" alg_to_string a alg_to_string b
