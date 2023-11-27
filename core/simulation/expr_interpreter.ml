(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let value_state_alg_op counter ?(time = Counter.current_time counter) = function
  | Operator.CPUTIME -> Nbr.F (Sys.time ())
  | Operator.TIME_VAR -> Nbr.F time
  | Operator.EVENT_VAR -> Nbr.I (Counter.current_event counter)
  | Operator.NULL_EVENT_VAR -> Nbr.I (Counter.nb_null_event counter)
  | Operator.EMAX_VAR ->
    (match Counter.max_events counter with
    | Some n -> Nbr.I n
    | None -> Nbr.F infinity)
  | Operator.TMAX_VAR ->
    (match Counter.max_time counter with
    | Some t -> Nbr.F t
    | None -> Nbr.F infinity)

type (_, _) stack =
  | RETURN : ('a, 'a) stack
  | TO_EXEC_ALG :
      Operator.bin_alg_op * Primitives.alg_expr * (Nbr.t, 'a) stack
      -> (Nbr.t, 'a) stack
  | TO_EXEC_COMP :
      Operator.compare_op * Primitives.alg_expr * (bool, 'a) stack
      -> (Nbr.t, 'a) stack
  | TO_EXEC_IF :
      Primitives.alg_expr * Primitives.alg_expr * (Nbr.t, 'a) stack
      -> (bool, 'a) stack
  | TO_EXEC_BOOL :
      Operator.bin_bool_op
      * (Pattern.id array list, int) Alg_expr.bool
      * (bool, 'a) stack
      -> (bool, 'a) stack
  | TO_COMPUTE_ALG :
      Operator.bin_alg_op * Nbr.t * (Nbr.t, 'a) stack
      -> (Nbr.t, 'a) stack
  | TO_COMPUTE_COMP :
      Operator.compare_op * Nbr.t * (bool, 'a) stack
      -> (Nbr.t, 'a) stack
  | TO_COMPUTE_UN : Operator.un_alg_op * (Nbr.t, 'a) stack -> (Nbr.t, 'a) stack
  | TO_COMPUTE_BOOL : Operator.un_bool_op * (bool, 'a) stack -> (bool, 'a) stack

let rec exec_alg :
    type a.
    Counter.t ->
    ?time:float ->
    get_alg:(int -> Primitives.alg_expr) ->
    get_mix:(Pattern.id array list -> Nbr.t) ->
    get_tok:(int -> Nbr.t) ->
    (Pattern.id array list, int) Alg_expr.e ->
    (Nbr.t, a) stack ->
    a =
 fun counter ?time ~get_alg ~get_mix ~get_tok alg sk ->
  match alg with
  | Alg_expr.BIN_ALG_OP (op, (a, _), (b, _)) ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok a
      (TO_EXEC_ALG (op, b, sk))
  | Alg_expr.UN_ALG_OP (op, (a, _)) ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok a (TO_COMPUTE_UN (op, sk))
  | Alg_expr.STATE_ALG_OP op ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok
      (value_state_alg_op counter ?time op)
      sk
  | Alg_expr.ALG_VAR i ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok (get_alg i) sk
  | Alg_expr.KAPPA_INSTANCE ccs ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok (get_mix ccs) sk
  | Alg_expr.TOKEN_ID i ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok (get_tok i) sk
  | Alg_expr.CONST n -> with_value counter ?time ~get_alg ~get_mix ~get_tok n sk
  | Alg_expr.IF ((cond, _), (yes, _), (no, _)) ->
    exec_bool counter ?time ~get_alg ~get_mix ~get_tok cond
      (TO_EXEC_IF (yes, no, sk))
  | Alg_expr.DIFF_TOKEN _ | Alg_expr.DIFF_KAPPA_INSTANCE _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("Cannot evalutate derivatives in expression", Loc.dummy))

and exec_bool :
    type a.
    Counter.t ->
    ?time:float ->
    get_alg:(int -> Primitives.alg_expr) ->
    get_mix:(Pattern.id array list -> Nbr.t) ->
    get_tok:(int -> Nbr.t) ->
    (Pattern.id array list, int) Alg_expr.bool ->
    (bool, a) stack ->
    a =
 fun counter ?time ~get_alg ~get_mix ~get_tok expr sk ->
  match expr with
  | Alg_expr.TRUE -> with_value counter ?time ~get_alg ~get_mix ~get_tok true sk
  | Alg_expr.FALSE ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok false sk
  | Alg_expr.UN_BOOL_OP (op, (a, _)) ->
    exec_bool counter ?time ~get_alg ~get_mix ~get_tok a
      (TO_COMPUTE_BOOL (op, sk))
  | Alg_expr.BIN_BOOL_OP (op, (a, _), (b, _)) ->
    exec_bool counter ?time ~get_alg ~get_mix ~get_tok a
      (TO_EXEC_BOOL (op, b, sk))
  | Alg_expr.COMPARE_OP (op, (a, _), (b, _)) ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok a
      (TO_EXEC_COMP (op, b, sk))

and with_value :
    type a b.
    Counter.t ->
    ?time:float ->
    get_alg:(int -> Primitives.alg_expr) ->
    get_mix:(Pattern.id array list -> Nbr.t) ->
    get_tok:(int -> Nbr.t) ->
    a ->
    (a, b) stack ->
    b =
 fun counter ?time ~get_alg ~get_mix ~get_tok n -> function
  | RETURN -> n
  | TO_EXEC_ALG (op, alg, sk) ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok alg
      (TO_COMPUTE_ALG (op, n, sk))
  | TO_COMPUTE_ALG (op, n1, sk) ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok
      (Nbr.of_bin_alg_op op n1 n)
      sk
  | TO_COMPUTE_UN (op, sk) ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok (Nbr.of_un_alg_op op n)
      sk
  | TO_EXEC_COMP (op, alg, sk) ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok alg
      (TO_COMPUTE_COMP (op, n, sk))
  | TO_COMPUTE_COMP (op, n1, sk) ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok
      (Nbr.of_compare_op op n1 n)
      sk
  | TO_EXEC_IF (yes, no, sk) ->
    exec_alg counter ?time ~get_alg ~get_mix ~get_tok
      (if n then
         yes
       else
         no)
      sk
  | TO_EXEC_BOOL (Operator.OR, _, sk) when n ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok true sk
  | TO_EXEC_BOOL (Operator.AND, _, sk) when not n ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok false sk
  | TO_EXEC_BOOL ((Operator.OR | Operator.AND), expr, sk) ->
    exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr sk
  | TO_COMPUTE_BOOL (Operator.NOT, sk) ->
    with_value counter ?time ~get_alg ~get_mix ~get_tok (not n) sk

let value_bool counter ?time ~get_alg ~get_mix ~get_tok expr =
  exec_bool counter ?time ~get_alg ~get_mix ~get_tok expr RETURN

let value_alg counter ?time ~get_alg ~get_mix ~get_tok alg =
  exec_alg counter ?time ~get_alg ~get_mix ~get_tok alg RETURN
