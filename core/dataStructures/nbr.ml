(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = F of float | I of int | I64 of Int64.t

let cast_bin_op ~op_f ?op_i ?op_i64 x y =
  match x, y with
  | F x, F y -> F (op_f x y)
  | I x, F y -> F (op_f (float_of_int x) y)
  | F x, I y -> F (op_f x (float_of_int y))
  | I x, I y ->
    (match op_i with
    | None -> F (op_f (float_of_int x) (float_of_int y))
    | Some op_i -> I (op_i x y))
  | I x, I64 y ->
    (match op_i64 with
    | None -> F (op_f (float_of_int x) (Int64.to_float y))
    | Some op_i64 -> I64 (op_i64 (Int64.of_int x) y))
  | I64 x, I y ->
    (match op_i64 with
    | None -> F (op_f (Int64.to_float x) (float_of_int y))
    | Some op_i64 -> I64 (op_i64 x (Int64.of_int y)))
  | I64 x, I64 y ->
    (match op_i64 with
    | None -> F (op_f (Int64.to_float x) (Int64.to_float y))
    | Some op_i64 -> I64 (op_i64 x y))
  | F x, I64 y -> F (op_f x (Int64.to_float y))
  | I64 x, F y -> F (op_f (Int64.to_float x) y)

let cast_un_op ?op_f ?op_i ?op_i64 x =
  match x with
  | F x ->
    (match op_f with
    | Some op_f -> F (op_f x)
    | None ->
      (match op_i with
      | None -> invalid_arg "cast_un"
      | Some op_i -> I (op_i (int_of_float x))))
  | I64 x ->
    (match op_i64 with
    | Some op_i64 -> I64 (op_i64 x)
    | None ->
      (match op_f with
      | None -> invalid_arg "cast_un_op"
      | Some op_f -> F (op_f (Int64.to_float x))))
  | I x ->
    (match op_i with
    | Some op_i -> I (op_i x)
    | None ->
      (match op_f with
      | None -> invalid_arg "cast_un_op"
      | Some op_f -> F (op_f (float_of_int x))))

let compare n1 n2 =
  match n1, n2 with
  | F x, F y -> Stdlib.compare x y
  | I x, I y -> Stdlib.compare x y
  | F x, I y -> Stdlib.compare x (float_of_int y)
  | I x, F y -> Stdlib.compare (float_of_int x) y
  | I x, I64 y -> Stdlib.compare (Int64.of_int x) y
  | I64 x, I64 y -> Stdlib.compare x y
  | I64 x, I y -> Stdlib.compare x (Int64.of_int y)
  | F x, I64 y -> Stdlib.compare x (Int64.to_float y)
  | I64 x, F y -> Stdlib.compare (Int64.to_float x) y

let is_greater n1 n2 = compare n1 n2 > 0
let is_smaller n1 n2 = compare n1 n2 < 0
let is_equal n1 n2 = compare n1 n2 = 0
let add n1 n2 = cast_bin_op ~op_f:( +. ) ~op_i:( + ) ~op_i64:Int64.add n1 n2
let sub n1 n2 = cast_bin_op ~op_f:( -. ) ~op_i:( - ) ~op_i64:Int64.sub n1 n2
let mult n1 n2 = cast_bin_op ~op_f:( *. ) ~op_i:( * ) ~op_i64:Int64.mul n1 n2
let min n1 n2 = cast_bin_op ~op_f:min ~op_i:min ~op_i64:min n1 n2
let max n1 n2 = cast_bin_op ~op_f:max ~op_i:max ~op_i64:max n1 n2

let rem n1 n2 =
  cast_bin_op ~op_i:( mod ) ~op_i64:Int64.rem ~op_f:mod_float n1 n2

let internal_div n1 n2 =
  cast_bin_op ~op_i:( / ) ~op_i64:Int64.div ~op_f:( /. ) n1 n2

let succ n = cast_un_op ~op_f:(( +. ) 1.) ~op_i:succ ~op_i64:Int64.succ n
let pred n = cast_un_op ~op_f:(fun x -> x -. 1.) ~op_i:pred ~op_i64:Int64.pred n
let neg n = cast_un_op ~op_f:( ~-. ) ~op_i:( ~- ) ~op_i64:Int64.neg n

let to_float n =
  match n with
  | I x -> Some (float_of_int x)
  | I64 x -> Some (Int64.to_float x)
  | F x ->
    (match classify_float x with
    | FP_zero | FP_normal | FP_subnormal -> Some x
    | FP_infinite | FP_nan -> None)

let to_int n =
  match n with
  | F x -> int_of_float x
  | I x -> x
  | I64 x -> Int64.to_int x (*Might exceed thebiggest 32 bits integer*)

let zero = I 0

let is_zero = function
  | I64 x -> x = Int64.zero
  | I x -> x = 0
  | F x -> Tools.float_is_zero x

let one = I 1

let is_strictly_positive = function
  | F x -> x > 0.
  | I x -> x > 0
  | I64 x -> x > Int64.zero

let pos_pow n1 n2 =
  cast_bin_op ~op_f:( ** ) ~op_i:Tools.pow ~op_i64:Tools.pow64 n1 n2

let pow x n =
  if is_zero n || is_strictly_positive n then
    pos_pow x n
  else (
    match to_float x with
    | Some x -> pos_pow (F (1. /. x)) (neg n)
    | None -> F nan
  )

let print f = function
  | F x -> Format.fprintf f "%s" (string_of_float x)
  | I64 x -> Format.fprintf f "%Ld" x
  | I x -> Format.fprintf f "%d" x

let pretty_print f = function
  | F x -> Format.fprintf f "%g" x
  | I64 x -> Format.fprintf f "%Ld" x
  | I x -> Format.fprintf f "%d" x

let print_option f = function
  | I x -> Format.fprintf f "%d" x
  | I64 x -> Format.fprintf f "%Ld" x
  | F x ->
    (match classify_float x with
    | FP_zero | FP_normal | FP_subnormal ->
      Format.fprintf f "%s" (string_of_float x)
    | FP_infinite | FP_nan -> ())

let to_string = function
  | F x -> string_of_float x
  | I64 x -> Int64.to_string x
  | I x -> string_of_int x

let rec iteri f x n =
  if is_strictly_positive n then
    iteri f (f n x) (pred n)
  else
    x

let rec maybe_iteri f x n =
  if is_strictly_positive n then (
    match f n x with
    | None -> x
    | Some x' -> maybe_iteri f x' (pred n)
  ) else
    x

let of_string x =
  try I (int_of_string x) with Failure _ -> F (float_of_string x)

let to_yojson = function
  | I x -> `Int x
  | I64 x -> `String (Int64.to_string x)
  | F x ->
    (match classify_float x with
    | FP_zero | FP_normal | FP_subnormal -> `Float x
    | FP_infinite | FP_nan -> `String (string_of_float x))

let of_yojson = function
  | `Int x -> I x
  | `Float x -> F x
  | `String n as x ->
    (try I64 (Int64.of_string n)
     with Failure _ ->
       (try F (float_of_string n)
        with Failure _ ->
          raise (Yojson.Basic.Util.Type_error ("Not an Nbr", x))))
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an Nbr", x))

let write_t ob f = Yojson.Basic.to_buffer ob (to_yojson f)

let string_of_t ?(len = 1024) x =
  let ob = Buffer.create len in
  write_t ob x;
  Buffer.contents ob

let read_t p lb = of_yojson (Yojson.Basic.from_lexbuf ~stream:true p lb)
let t_of_string s = read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let of_bin_alg_op = function
  | Operator.MULT -> mult
  | Operator.SUM -> add
  | Operator.DIV ->
    fun x y ->
      if (not (is_zero y)) && is_zero (rem x y) then
        internal_div x y
      else
        cast_bin_op ~op_f:( /. ) x y
  | Operator.MINUS -> sub
  | Operator.MODULO -> rem
  | Operator.MIN -> min
  | Operator.MAX -> max
  | Operator.POW -> pow

let of_un_alg_op = function
  | Operator.LOG -> fun x -> cast_un_op ~op_f:log x
  | Operator.SQRT -> fun x -> cast_un_op ~op_f:sqrt x
  | Operator.EXP -> fun x -> cast_un_op ~op_f:exp x
  | Operator.SINUS -> fun x -> cast_un_op ~op_f:sin x
  | Operator.COSINUS -> fun x -> cast_un_op ~op_f:cos x
  | Operator.TAN -> fun x -> cast_un_op ~op_f:tan x
  | Operator.INT ->
    fun x -> cast_un_op ~op_i:(fun n -> n) ~op_i64:(fun n -> n) x
  | Operator.UMINUS -> neg

let of_compare_op = function
  | Operator.GREATER -> is_greater
  | Operator.SMALLER -> is_smaller
  | Operator.EQUAL -> is_equal
  | Operator.DIFF -> fun v v' -> not (is_equal v v')
