(**
    * intinf.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 01/10/2010
    * Last modification: Time-stamp: <Jul 02 2016>
    * *
    * unboundend integer library, with infinity
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

type 'a intinf = Int of 'a | Infinity

let equal a b =
  match a, b with
  | Infinity, Infinity -> true
  | Infinity, _ | _, Infinity -> false
  | Int a, Int b -> Big_int.eq_big_int a b

let f_gen_zeroary f = Int (f ())

let f_gen_unary f x =
  match x with
  | Infinity -> Infinity
  | Int x -> Int (f x)

let f_gen_binary f x y =
  match x, y with
  | Infinity, _ | _, Infinity -> Infinity
  | Int i, Int j -> Int (f i j)

type bi_intinf = Big_int.big_int intinf

let bi_add = f_gen_binary Big_int.add_big_int
let bi_mult = f_gen_binary Big_int.mult_big_int
let big_zero = Big_int.zero_big_int
let big_two = Big_int.unit_big_int
let infty = Infinity
let bi_one = Int (Big_int.big_int_of_int 1)
let bi_zero = Int (Big_int.big_int_of_int 0)
let bi_of_int i = Int (Big_int.big_int_of_int i)

let bi_print_int stdout i =
  match i with
  | Int i -> Printf.fprintf stdout "%s" (Big_int.string_of_big_int i)
  | Infinity -> Printf.fprintf stdout "+oo"

let bi_string_of i =
  match i with
  | Int i -> Big_int.string_of_big_int i
  | Infinity -> "+oo"

let bi_n_n_plus_1_divided_by_2 n =
  match n with
  | Int i when Big_int.eq_big_int (Big_int.mod_big_int i big_two) big_zero ->
    Int
      (Big_int.mult_big_int
         (Big_int.div_big_int i big_two)
         (Big_int.succ_big_int i))
  | Int i ->
    let succ = Big_int.succ_big_int i in
    Int (Big_int.mult_big_int i (Big_int.div_big_int succ big_two))
  | Infinity -> Infinity
