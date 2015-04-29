open Mods

let value_state_alg_op counter ?(time=Counter.time counter) = function
  | Term.CPUTIME -> Nbr.F (Sys.time ())
  | Term.TIME_VAR -> Nbr.F time
  | Term.EVENT_VAR -> Nbr.I (Counter.event counter+Counter.null_event counter)
  | Term.NULL_EVENT_VAR -> Nbr.I (Counter.null_event counter)
  | Term.PROD_EVENT_VAR ->Nbr.I (Counter.event counter)

type alg_stack_element =
  | TO_EXEC_ALG of Term.bin_alg_op * Expr.alg_expr
  | TO_EXEC_COMP of Term.compare_op * Expr.alg_expr
  | TO_EXEC_BOOL of Term.bool_op * Expr.alg_expr Ast.bool_expr
  | TO_COMPUTE_ALG of Term.bin_alg_op * Nbr.t
  | TO_COMPUTE_COMP of Term.compare_op * Nbr.t
  | TO_COMPUTE_UN of Term.un_alg_op

let rec exec_alg :
type a. Counter.t -> ?time:float -> get_alg:(int -> Expr.alg_expr) ->
     get_mix:((int * Connected_component.t list list) -> Nbr.t) -> get_tok:(int -> Nbr.t) ->
     (Counter.t -> ?time:float -> get_alg:(int -> Expr.alg_expr) ->
      get_mix:((int * Connected_component.t list list) -> Nbr.t) -> get_tok:(int -> Nbr.t) ->
      Nbr.t -> alg_stack_element list -> a) ->
     Expr.alg_expr -> alg_stack_element list -> a =
    fun counter ?time ~get_alg ~get_mix ~get_tok with_value alg sk ->
    match alg with
    | Expr.BIN_ALG_OP (op,(a,_),(b,_)) ->
       exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value
		a (TO_EXEC_ALG (op,b)::sk)
    | Expr.UN_ALG_OP (op,(a,_)) ->
       exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value
		a (TO_COMPUTE_UN op::sk)
    | Expr.STATE_ALG_OP (op) ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok
		  (value_state_alg_op counter ?time op) sk
    | Expr.ALG_VAR i ->
       exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value
		(get_alg i) sk
    | Expr.KAPPA_INSTANCE (i,ccs) ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok
		  (get_mix (i,ccs)) sk
    | Expr.TOKEN_ID i ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok
		  (get_tok i) sk
    | Expr.CONST n ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok n sk

let rec with_value_alg counter ?time ~get_alg ~get_mix ~get_tok n = function
  | [] -> n
  | TO_EXEC_ALG (op,alg) :: sk ->
     exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value_alg
	      alg (TO_COMPUTE_ALG (op,n)::sk)
  | TO_COMPUTE_ALG (op,n1) :: sk ->
     with_value_alg counter ?time ~get_alg ~get_mix ~get_tok
		    (Nbr.of_bin_alg_op op n1 n) sk
  | TO_COMPUTE_UN op :: sk ->
     with_value_alg counter ?time ~get_alg ~get_mix ~get_tok
		    (Nbr.of_un_alg_op op n) sk
  | (TO_COMPUTE_COMP _ | TO_EXEC_COMP _ | TO_EXEC_BOOL _)
    :: _ -> failwith "type error in with_value_alg"
and with_value_alg_bool counter ?time ~get_alg ~get_mix ~get_tok n = function
  | TO_EXEC_ALG (op,alg) :: sk ->
     exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value_alg_bool
	      alg (TO_COMPUTE_ALG (op,n)::sk)
  | TO_COMPUTE_ALG (op,n1) :: sk ->
     with_value_alg_bool counter ?time ~get_alg ~get_mix ~get_tok
			 (Nbr.of_bin_alg_op op n1 n) sk
  | TO_COMPUTE_UN op :: sk ->
     with_value_alg_bool counter ?time ~get_alg ~get_mix ~get_tok
			 (Nbr.of_un_alg_op op n) sk
  | TO_EXEC_COMP (op,alg) :: sk ->
     exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value_alg_bool
	      alg (TO_COMPUTE_COMP (op,n)::sk)
  | TO_COMPUTE_COMP (op,n1) :: sk ->
     with_value_bool counter ?time ~get_alg ~get_mix ~get_tok
		     (Nbr.of_compare_op op n1 n) sk
  | TO_EXEC_BOOL _ :: _ -> failwith "type error in with_value_alg_bool"
  | [] -> failwith "type error in with_value_alg_bool"
and exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr sk =
  match expr with
  | Ast.TRUE -> with_value_bool counter ?time ~get_alg ~get_mix ~get_tok true sk
  | Ast.FALSE ->
     with_value_bool counter ?time ~get_alg ~get_mix ~get_tok false sk
  | Ast.BOOL_OP (op,(a,_),(b,_)) ->
     exec_bool counter ?time ~get_alg ~get_mix ~get_tok
	       a (TO_EXEC_BOOL (op,b) :: sk)
  | Ast.COMPARE_OP (op,(a,_),(b,_)) ->
     exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value_alg_bool
	      a (TO_EXEC_COMP (op,b) :: sk)
and with_value_bool counter ?time ~get_alg ~get_mix ~get_tok b = function
  | [] -> b
  | TO_EXEC_BOOL (Term.OR,_) :: sk when b ->
     with_value_bool counter ?time ~get_alg ~get_mix ~get_tok true sk
  | TO_EXEC_BOOL (Term.AND,_) :: sk when not b ->
     with_value_bool counter ?time ~get_alg ~get_mix ~get_tok false sk
  | TO_EXEC_BOOL ((Term.OR | Term.AND),expr) :: sk ->
     exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr sk
  | (TO_EXEC_ALG _ | TO_EXEC_COMP _ | TO_COMPUTE_ALG _ | TO_COMPUTE_COMP _
     | TO_COMPUTE_UN _) :: _ -> failwith "type error in with_value_bool"

let value_bool counter ?time ~get_alg ~get_mix ~get_tok expr =
  exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr []
let value_alg counter ?time ~get_alg ~get_mix ~get_tok alg =
  exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value_alg alg []
