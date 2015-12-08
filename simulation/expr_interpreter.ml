let value_state_alg_op counter ?(time=Counter.current_time counter) = function
  | Operator.CPUTIME -> Nbr.F (Sys.time ())
  | Operator.TIME_VAR -> Nbr.F time
  | Operator.EVENT_VAR -> Nbr.I (Counter.current_event counter)
  | Operator.NULL_EVENT_VAR -> Nbr.I (Counter.nb_null_event counter)

type alg_stack_element =
  | TO_EXEC_ALG of Operator.bin_alg_op * Alg_expr.t
  | TO_EXEC_COMP of Operator.compare_op * Alg_expr.t
  | TO_EXEC_BOOL of Operator.bool_op * Alg_expr.t Ast.bool_expr
  | TO_COMPUTE_ALG of Operator.bin_alg_op * Nbr.t
  | TO_COMPUTE_COMP of Operator.compare_op * Nbr.t
  | TO_COMPUTE_UN of Operator.un_alg_op

let rec exec_alg :
type a. Counter.t -> ?time:float -> get_alg:(int -> Alg_expr.t) ->
     get_mix:(Connected_component.t array list -> Nbr.t) ->
     get_tok:(int -> Nbr.t) ->
     (Counter.t -> ?time:float -> get_alg:(int -> Alg_expr.t) ->
      get_mix:(Connected_component.t array list -> Nbr.t) ->
      get_tok:(int -> Nbr.t) ->
      Nbr.t -> alg_stack_element list -> a) ->
     Alg_expr.t -> alg_stack_element list -> a =
    fun counter ?time ~get_alg ~get_mix ~get_tok with_value alg sk ->
    match alg with
    | Alg_expr.BIN_ALG_OP (op,(a,_),(b,_)) ->
       exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value
		a (TO_EXEC_ALG (op,b)::sk)
    | Alg_expr.UN_ALG_OP (op,(a,_)) ->
       exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value
		a (TO_COMPUTE_UN op::sk)
    | Alg_expr.STATE_ALG_OP (op) ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok
		  (value_state_alg_op counter ?time op) sk
    | Alg_expr.ALG_VAR i ->
       exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value
		(get_alg i) sk
    | Alg_expr.KAPPA_INSTANCE ccs ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok
		  (get_mix ccs) sk
    | Alg_expr.TOKEN_ID i ->
       with_value counter ?time ~get_alg ~get_mix ~get_tok
		  (get_tok i) sk
    | Alg_expr.CONST n ->
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
  | TO_EXEC_BOOL (Operator.OR,_) :: sk when b ->
     with_value_bool counter ?time ~get_alg ~get_mix ~get_tok true sk
  | TO_EXEC_BOOL (Operator.AND,_) :: sk when not b ->
     with_value_bool counter ?time ~get_alg ~get_mix ~get_tok false sk
  | TO_EXEC_BOOL ((Operator.OR | Operator.AND),expr) :: sk ->
     exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr sk
  | (TO_EXEC_ALG _ | TO_EXEC_COMP _ | TO_COMPUTE_ALG _ | TO_COMPUTE_COMP _
     | TO_COMPUTE_UN _) :: _ -> failwith "type error in with_value_bool"

let value_bool counter ?time ~get_alg ~get_mix ~get_tok expr =
  exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr []
let value_alg counter ?time ~get_alg ~get_mix ~get_tok alg =
  exec_alg counter ?time ~get_alg ~get_mix ~get_tok with_value_alg alg []
