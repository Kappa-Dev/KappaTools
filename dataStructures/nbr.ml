type t = F of float | I of int | I64 of Int64.t

let cast_bin_op ~op_f ?op_i ?op_i64 x y =
  match (x,y) with
  | (F x, F y) -> F (op_f x y)
  | (I x, F y) -> F (op_f (float_of_int x) y)
  | (F x, I y) -> F (op_f x (float_of_int y))
  | (I x, I y) ->
     begin
       match op_i with
       | None -> F (op_f (float_of_int x) (float_of_int y))
       | Some op_i -> I (op_i x y)
     end
  | (I x, I64 y) ->
     begin
       match op_i64 with
       | None -> F (op_f (float_of_int x) (Int64.to_float y))
       | Some op_i64 -> I64 (op_i64 (Int64.of_int x) y)
     end
  | (I64 x, I y) ->
     begin
       match op_i64 with
       | None -> F (op_f (Int64.to_float x) (float_of_int y))
       | Some op_i64 -> I64 (op_i64 x (Int64.of_int y))
     end
  | (I64 x, I64 y) ->
     begin
       match op_i64 with
       | None -> F (op_f (Int64.to_float x) (Int64.to_float y))
       | Some op_i64 -> I64 (op_i64 x y)
     end
  | (F x, I64 y) -> F (op_f x (Int64.to_float y))
  | (I64 x, F y) -> F (op_f (Int64.to_float x) y)

let cast_un_op ?op_f ?op_i ?op_i64 x =
  match x with
  | F x ->
     begin
       match op_f with
       | Some op_f -> F (op_f x)
       | None -> match op_i with None -> invalid_arg "cast_un"
			       | Some op_i -> I (op_i (int_of_float x))
     end
  | I64 x ->
     begin
       match op_i64 with
       | Some op_i64 -> I64 (op_i64 x)
       | None -> match op_f with None -> invalid_arg "cast_un_op"
			       | Some op_f -> F (op_f (Int64.to_float x))
     end
  | I x ->
     match op_i with
     | Some op_i -> I (op_i x)
     | None -> match op_f with None -> invalid_arg "cast_un_op"
			     | Some op_f -> F (op_f (float_of_int x))

let compare n1 n2 =
  match n1,n2 with
  | (F x, F y) -> Pervasives.compare x y
  | (I x, I y) -> Pervasives.compare x y
  | (F x, I y) -> Pervasives.compare x (float_of_int y)
  | (I x, F y) -> Pervasives.compare (float_of_int x) y
  | (I x, I64 y) -> Pervasives.compare (Int64.of_int x) y
  | (I64 x, I64 y) -> Pervasives.compare x y
  | (I64 x, I y) -> Pervasives.compare x (Int64.of_int y)
  | (F x, I64 y) -> Pervasives.compare x (Int64.to_float y)
  | (I64 x, F y) -> Pervasives.compare (Int64.to_float x) y

let is_greater n1 n2 = compare n1 n2 > 0
let is_smaller n1 n2 = compare n1 n2 < 0
let is_equal n1 n2 = compare n1 n2=0

let add n1 n2 = cast_bin_op ~op_f:(+.) ~op_i:(+) ~op_i64:Int64.add n1 n2
let sub n1 n2 = cast_bin_op ~op_f:(-.) ~op_i:(-) ~op_i64:Int64.sub n1 n2
let mult n1 n2 = cast_bin_op ~op_f:( *.) ~op_i:( * ) ~op_i64:Int64.mul n1 n2
let min n1 n2 = cast_bin_op ~op_f:min ~op_i:min ~op_i64:min n1 n2
let max n1 n2 = cast_bin_op ~op_f:max ~op_i:max ~op_i64:max n1 n2

let succ n = cast_un_op ~op_f:((+.) 1.) ~op_i:succ ~op_i64:Int64.succ n
let pred n = cast_un_op ~op_f:((-.) 1.) ~op_i:pred ~op_i64:Int64.pred n
let neg n = cast_un_op ~op_f:(~-.) ~op_i:(~-) ~op_i64:Int64.neg n

let to_float n =
  match n with
  | F x -> x
  | I x -> float_of_int x
  | I64 x -> Int64.to_float x

let to_int n =
  match n with
  | F x -> (int_of_float x)
  | I x -> x
  | I64 x -> Int64.to_int x (*Might exceed thebiggest 32 bits integer*)

let is_zero = function
  | I64 x -> x = Int64.zero
  | I x -> x = 0
  | F x -> match classify_float x with
	   | FP_zero -> true
	   | FP_normal | FP_subnormal |FP_infinite | FP_nan -> false

let is_strictly_positive = function
  | F x -> x > 0.
  | I x -> x > 0
  | I64 x -> x > Int64.zero

let print f = function
  | F x -> Format.fprintf f "%E" x
  | I64 x -> Format.fprintf f "%Ld" x
  | I x -> Format.fprintf f "%d" x

let to_string = function
  | F x -> Printf.sprintf "%E" x
  | I64 x -> Printf.sprintf "%Ld" x
  | I x -> Printf.sprintf "%d" x

let rec iteri f x n =
  if is_strictly_positive n then iteri f (f n x) (pred n) else x

let of_bin_alg_op = function
  | Term.MULT -> mult
  | Term.SUM -> add
  | Term.DIV -> fun x y -> cast_bin_op ~op_f:(/.) x y
  | Term.MINUS -> sub
  | Term.POW ->
     cast_bin_op ~op_f:( ** ) ~op_i:Tools.pow ~op_i64:Tools.pow64
  | Term.MODULO ->
     cast_bin_op ~op_i:(mod)  ~op_i64:Int64.rem
		 ~op_f:(fun a b ->
			float_of_int
			  (int_of_float a mod int_of_float b))
  | Term.MIN -> min
  | Term.MAX -> max

let of_un_alg_op = function
  | Term.LOG -> fun x -> cast_un_op ~op_f:log x
  | Term.SQRT -> fun x -> cast_un_op ~op_f:sqrt x
  | Term.EXP -> fun x -> cast_un_op ~op_f:exp x
  | Term.SINUS -> fun x -> cast_un_op ~op_f:sin x
  | Term.COSINUS -> fun x -> cast_un_op ~op_f:cos x
  | Term.TAN -> fun x -> cast_un_op ~op_f:tan x
  | Term.INT ->
     fun x -> cast_un_op ~op_i:(fun n -> n) ~op_i64:(fun n -> n) x
  | Term.UMINUS -> neg

let of_compare_op = function
  | Term.GREATER -> is_greater
  | Term.SMALLER -> is_smaller
  | Term.EQUAL -> is_equal
  | Term.DIFF -> fun v v' -> not (is_equal v v')

let getMaxEventValue () =
  match !Parameter.maxEventValue with
  | Some n -> I n
  | None -> Format.eprintf "[emax] constant is evaluated to infinity@.";
	    F infinity

let getMaxTimeValue () =
  match !Parameter.maxTimeValue with
  | Some t -> F t
  | None -> Format.eprintf "[tmax] constant is evaluated to infinity@.";
	    F infinity

let getPointNumberValue () = I !Parameter.pointNumberValue
