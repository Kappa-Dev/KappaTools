val generic_zeroary :
  ('a ->
  ('b -> 'b -> int) ->
  ((int, 'c) Mvbdu_sig.precell, 'd) Mvbdu_sig.premvbdu ->
  'd Mvbdu_sig.cell ->
  (int -> 'd Mvbdu_sig.mvbdu) ->
  'e ->
  Exception_without_parameter.method_handler * ('f * 'g * 'h * 'e) option) ->
  'e ->
  ('i ->
  'a * (('d Mvbdu_sig.mvbdu, 'd) Mvbdu_sig.precell, 'd) Mvbdu_sig.premvbdu) ->
  'i ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler * ('e * 'h option)

val generic_unary :
  (Exception_without_parameter.method_handler ->
  ('f -> 'f -> int) ->
  ((int, 'i) Mvbdu_sig.precell, 'a) Mvbdu_sig.premvbdu ->
  'a Mvbdu_sig.cell ->
  (int -> 'a Mvbdu_sig.mvbdu) ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'a Mvbdu_sig.cell
    * 'a Mvbdu_sig.mvbdu
    * ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler)
    option) ->
  ( 'a,
    'b,
    'c,
    'd,
    'e,
    (('a Mvbdu_sig.mvbdu, 'a) Mvbdu_sig.precell, 'a) Mvbdu_sig.premvbdu,
    'g,
    'h )
  Memo_sig.unary_memoized_fun ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'a Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler * 'a Mvbdu_sig.mvbdu option)

val generic_binary :
  (Exception_without_parameter.method_handler ->
  ('f -> 'f -> int) ->
  ((int, 'i) Mvbdu_sig.precell, 'a) Mvbdu_sig.premvbdu ->
  'a Mvbdu_sig.cell ->
  (int -> 'a Mvbdu_sig.mvbdu) ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'a Mvbdu_sig.cell
    * 'a Mvbdu_sig.mvbdu
    * ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler)
    option) ->
  ( 'a,
    'b,
    'c,
    'd,
    'e,
    (('a Mvbdu_sig.mvbdu, 'a) Mvbdu_sig.precell, 'a) Mvbdu_sig.premvbdu,
    'g,
    'h )
  Memo_sig.binary_memoized_fun ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'a Mvbdu_sig.mvbdu ->
  'a Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler * 'a Mvbdu_sig.mvbdu option)

val generic_unary_other :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ( 'c,
    'e,
    'f,
    'g,
    'h,
    'j ->
    'j Mvbdu_sig.mvbdu ->
    'k
    * ('k ->
      Exception_without_parameter.method_handler
      * (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu),
    'j Mvbdu_sig.mvbdu * 'j Mvbdu_sig.mvbdu,
    'd,
    'i )
  Memo_sig.memoized_fun ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'j Mvbdu_sig.mvbdu ->
  'j Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val clean_head :
  'a ->
  ('b, 'c, 'd, 'e, 'f, 'g, 'b Mvbdu_sig.mvbdu, 'h, 'i) Memo_sig.memoized_fun ->
  (Remanent_parameters_sig.parameters ->
  ('h, 'c, 'd, 'e, 'f, 'b, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'b Mvbdu_sig.mvbdu ->
  'b Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (('h, 'c, 'd, 'e, 'f, 'b, 'i) Memo_sig.handler * 'b Mvbdu_sig.mvbdu option)) ->
  ('h, 'c, 'd, 'e, 'f, 'b, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'b Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (('h, 'c, 'd, 'e, 'f, 'b, 'i) Memo_sig.handler * 'b Mvbdu_sig.mvbdu option)

val keep_head_only :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ('c, 'e, 'f, 'g, 'h, 'j, 'c Mvbdu_sig.mvbdu, 'd, 'i) Memo_sig.memoized_fun ->
  (Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  'c Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val redefine_range :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ( 'c,
    'e,
    'f,
    'g,
    'h,
    ( 'c,
      'e,
      'f,
      'g,
      'h,
      (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu,
      'd,
      'i )
    Memo_sig.reset,
    'c Mvbdu_sig.mvbdu * (int option * int option) List_sig.list,
    'd,
    'i )
  Memo_sig.memoized_fun ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  'c Mvbdu_sig.mvbdu ->
  (int option * int option) List_sig.list ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val redefine :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ( 'c,
    'e,
    'f,
    'g,
    'h,
    ( 'c,
      'e,
      'f,
      'g,
      'h,
      (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu,
      'd,
      'i )
    Memo_sig.reset,
    'c Mvbdu_sig.mvbdu * int List_sig.list,
    'd,
    'i )
  Memo_sig.memoized_fun ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  'c Mvbdu_sig.mvbdu ->
  int List_sig.list ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val monotonicaly_rename :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ( 'c,
    'e,
    'f,
    'g,
    'h,
    'j,
    'c Mvbdu_sig.mvbdu * int List_sig.list,
    'd,
    'i )
  Memo_sig.memoized_fun ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  'c Mvbdu_sig.mvbdu ->
  int List_sig.list ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val project_keep_only :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ( 'c,
    'e,
    'f,
    'g,
    'h,
    ( 'c,
      'e,
      'f,
      'g,
      'h,
      (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu,
      'd,
      'i )
    Memo_sig.reset,
    'c Mvbdu_sig.mvbdu * 'j List_sig.list,
    'd,
    'i )
  Memo_sig.memoized_fun ->
  (Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)) ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  'c Mvbdu_sig.mvbdu ->
  'j List_sig.list ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val project_abstract_away :
  (Exception_without_parameter.method_handler ->
  ('a -> 'a -> int) ->
  ((int, 'b) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  'c Mvbdu_sig.cell ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * (int
    * 'c Mvbdu_sig.cell
    * 'c Mvbdu_sig.mvbdu
    * ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler)
    option) ->
  ( 'c,
    'e,
    'f,
    'g,
    'h,
    ( 'c,
      'e,
      'f,
      'g,
      'h,
      (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu,
      'd,
      'i )
    Memo_sig.reset,
    'c Mvbdu_sig.mvbdu * 'j List_sig.list,
    'd,
    'i )
  Memo_sig.memoized_fun ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  ('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler ->
  'c Mvbdu_sig.mvbdu ->
  'j List_sig.list ->
  Exception_without_parameter.method_handler
  * (('d, 'e, 'f, 'g, 'h, 'c, 'i) Memo_sig.handler * 'c Mvbdu_sig.mvbdu option)

val mvbdu_identity : 'a -> 'b -> 'c -> 'd -> 'c * ('a * 'd option)
val mvbdu_constant : 'a -> 'b -> 'c -> 'd -> 'e -> 'd * ('b * 'a option)

val recursive_not_memoize :
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a) ->
  ('b, 'c, 'd, 'e, 'f, 'a, 'g, 'h, 'i) Memo_sig.memoized_fun

val memoize_no_fun :
  (('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler -> 'i) ->
  ('i ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  'a Mvbdu_sig.mvbdu ->
  'i ->
  Exception_without_parameter.method_handler
  * (('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler * 'a Mvbdu_sig.mvbdu option)) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  'a Mvbdu_sig.mvbdu ->
  'a Mvbdu_sig.mvbdu ->
  'i ->
  Exception_without_parameter.method_handler * 'i) ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.unary_memoized_fun

val memoize_binary_no_fun :
  (('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler -> 'i) ->
  ('i ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  'a Mvbdu_sig.mvbdu * 'a Mvbdu_sig.mvbdu ->
  'i ->
  Exception_without_parameter.method_handler
  * (('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler * 'a Mvbdu_sig.mvbdu option)) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('g, 'b, 'c, 'd, 'e, 'a, 'h) Memo_sig.handler ->
  'a Mvbdu_sig.mvbdu * 'a Mvbdu_sig.mvbdu ->
  'a Mvbdu_sig.mvbdu ->
  'i ->
  Exception_without_parameter.method_handler * 'i) ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.binary_memoized_fun

val not_recursive_not_memoize_unary :
  (Exception_without_parameter.method_handler ->
  'a ->
  Exception_without_parameter.method_handler
  * (('b Mvbdu_sig.mvbdu, 'b) Mvbdu_sig.precell, 'b) Mvbdu_sig.premvbdu
  * 'c Mvbdu_sig.mvbdu option) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'd) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('e -> 'e -> int) ->
  ((int, 'f) Mvbdu_sig.precell, 'b) Mvbdu_sig.premvbdu ->
  (('b Mvbdu_sig.mvbdu, 'b) Mvbdu_sig.precell, 'b) Mvbdu_sig.premvbdu ->
  (int -> 'b Mvbdu_sig.mvbdu) ->
  ('g, 'h, 'i, 'j, 'k, 'c, 'l) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * ('m
    * 'n
    * 'c Mvbdu_sig.mvbdu
    * ('g, 'h, 'i, 'j, 'k, 'c, 'l) Memo_sig.handler)
    option) ->
  ('c, 'h, 'i, 'j, 'k, 'd, 'a, 'g, 'l) Memo_sig.memoized_fun

val recursive_memoize :
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'a) ->
  (('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler -> 'i) ->
  ('i ->
  ('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler ->
  ('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler ->
  'j ->
  'i ->
  Exception_without_parameter.method_handler
  * (('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler * 'g Mvbdu_sig.mvbdu option)) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ('b, 'c, 'd, 'e, 'f, 'g, 'h) Memo_sig.handler ->
  'j ->
  'g Mvbdu_sig.mvbdu ->
  'i ->
  Exception_without_parameter.method_handler * 'i) ->
  ('g, 'c, 'd, 'e, 'f, 'a, 'j, 'b, 'h) Memo_sig.memoized_fun

val not_recursive_binary :
  (Exception_without_parameter.method_handler ->
  'a ->
  'b ->
  Exception_without_parameter.method_handler
  * (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu
  * 'd Mvbdu_sig.mvbdu option) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'e) ->
  (Exception_without_parameter.method_handler ->
  ('f -> 'f -> int) ->
  ((int, 'g) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  (('c Mvbdu_sig.mvbdu, 'c) Mvbdu_sig.precell, 'c) Mvbdu_sig.premvbdu ->
  (int -> 'c Mvbdu_sig.mvbdu) ->
  ('h, 'i, 'j, 'k, 'l, 'd, 'm) Memo_sig.handler ->
  Exception_without_parameter.method_handler
  * ('n
    * 'o
    * 'd Mvbdu_sig.mvbdu
    * ('h, 'i, 'j, 'k, 'l, 'd, 'm) Memo_sig.handler)
    option) ->
  ('d, 'i, 'j, 'k, 'l, 'e, 'a * 'b, 'h, 'm) Memo_sig.memoized_fun

val id_of_mvbdu : 'a Mvbdu_sig.mvbdu -> int

val a :
  (Exception_without_parameter.method_handler ->
  ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler ->
  'c ->
  Exception_without_parameter.method_handler
  * ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler
  * (('j Mvbdu_sig.mvbdu, 'j) Mvbdu_sig.precell, 'j) Mvbdu_sig.premvbdu
  * 'h Mvbdu_sig.mvbdu option) ->
  (Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  'e) ->
  (('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler ->
  'k ->
  'l ->
  'c ->
  'h Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler) ->
  (('k ->
   'l ->
   'c ->
   'h Mvbdu_sig.mvbdu ->
   Exception_without_parameter.method_handler
   * ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler) ->
  ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler ->
  ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler) ->
  (Exception_without_parameter.method_handler ->
  'c ->
  ('k ->
  'l ->
  'c ->
  'h Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler) ->
  Exception_without_parameter.method_handler * 'h Mvbdu_sig.mvbdu option) ->
  (Exception_without_parameter.method_handler ->
  'c ->
  'h Mvbdu_sig.mvbdu ->
  ('k ->
  'l ->
  'c ->
  'h Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler) ->
  Exception_without_parameter.method_handler
  * ('k ->
    'l ->
    'c ->
    'h Mvbdu_sig.mvbdu ->
    Exception_without_parameter.method_handler
    * ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler)) ->
  (Exception_without_parameter.method_handler ->
  ('m -> 'm -> int) ->
  ((int, 'n) Mvbdu_sig.precell, 'j) Mvbdu_sig.premvbdu ->
  (('j Mvbdu_sig.mvbdu, 'j) Mvbdu_sig.precell, 'j) Mvbdu_sig.premvbdu ->
  (int -> 'j Mvbdu_sig.mvbdu) ->
  ('a, 'b, 'd, 'f, 'g, 'h, 'i) Memo_sig.handler ->
  'k * ('o * 'p * 'h Mvbdu_sig.mvbdu * 'l) option) ->
  ('h, 'b, 'd, 'f, 'g, 'e, 'c, 'a, 'i) Memo_sig.memoized_fun
