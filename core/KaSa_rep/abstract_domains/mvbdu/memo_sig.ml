(**
   * mvbdu_sig.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 22/11/2010
   * Last modification: Time-stamp: <Dec 31 2016>
   * *
   * Signature for memoized function
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type ('bool, 'mvbdu_dic, 'blist, 'rlist, 'vlist, 'c, 'd, 'memo_tables, 'int, 'h) memoized_fun = {
  f:
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'c;
  store:
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
     
    ('memo_tables, 'mvbdu_dic, 'blist, 'rlist, 'vlist, 'bool, 'int) handler ->
    'd ->
    'h ->
    Exception.exceptions_caught_and_uncaught
    * ('memo_tables, 'mvbdu_dic, 'blist, 'rlist, 'vlist, 'bool, 'int) handler;
  get:
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ('memo_tables, 'mvbdu_dic, 'blist, 'rlist, 'vlist, 'bool, 'int) handler ->
    'd ->
    Exception.exceptions_caught_and_uncaught
    * (('memo_tables, 'mvbdu_dic, 'blist, 'rlist, 'vlist, 'bool, 'int) handler * 'h option);
}

and ('memo_tables, 'mvbdu_dic, 'blist, 'rlist, 'vlist, 'bool, 'int) handler = {
  data: 'memo_tables;
  mvbdu_dictionary: 'mvbdu_dic;
  association_list_dictionary: 'blist;
  variables_list_dictionary: 'vlist;
  range_list_dictionary: 'rlist;
  print_cell: Remanent_parameters_sig.parameters -> 'bool Mvbdu_sig.cell -> unit;
  print_skel:
    Remanent_parameters_sig.parameters -> 'bool Mvbdu_sig.skeleton -> unit;
  print_mvbdu: Remanent_parameters_sig.parameters -> 'bool Mvbdu_sig.mvbdu -> unit;
}

type 'a pair = 'a * 'a

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    'a ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'c),
    'a Mvbdu_sig.mvbdu,
    'd,
    'e,
    'a Mvbdu_sig.mvbdu )
  memoized_fun

type ('a,
       'b,
       'blist,
       'rlist,
       'vlist,
       'c,
       'd,
       'e,
       'f)
     unary_with_threshold_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    'a ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'c),
    int * 'a Mvbdu_sig.mvbdu,
    'd,
    'e,
    'f )
  memoized_fun

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) binary_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    ('a ->
    Exception.exceptions_caught_and_uncaught
    * ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun)
    pair,
    'a Mvbdu_sig.mvbdu * 'a Mvbdu_sig.mvbdu,
    'd,
    'e,
    'a Mvbdu_sig.mvbdu )
  memoized_fun

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_other_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    'a ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'a Mvbdu_sig.cell),
    'd * 'a Mvbdu_sig.mvbdu,
    'c,
    'e,
    'a Mvbdu_sig.mvbdu )
  memoized_fun

type ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'memo_tables, 'e) reset = {
  empty_range_list:
    Exception.exceptions_caught_and_uncaught
    * ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'memo_tables, 'e) unary_memoized_fun;
  empty_association_list:
    Exception.exceptions_caught_and_uncaught
    * ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'memo_tables, 'e) unary_memoized_fun;
  empty_variables_list:
    Exception.exceptions_caught_and_uncaught
    * ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'memo_tables, 'e) unary_memoized_fun;
  leaf:
    'bool ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'bool Mvbdu_sig.cell);
  clean_head:
    Exception.exceptions_caught_and_uncaught
    * ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'memo_tables, 'e) unary_memoized_fun;
    
  height: Exception.exceptions_caught_and_uncaught
    * (bool, 'b, 'blist, 'rlist, 'vlist,
    bool ->
    Exception_without_parameter.exceptions_caught_and_uncaught *
    (Exception_without_parameter.exceptions_caught_and_uncaught ->
     Exception_without_parameter.exceptions_caught_and_uncaught *
     ('bool, bool) Mvbdu_sig.premvbdu),
    bool Mvbdu_sig.mvbdu, 'memo_tables, 'c, int)
   memoized_fun;
  
    width: Exception.exceptions_caught_and_uncaught
    * (bool, 'b, 'blist, 'rlist, 'vlist,
    bool ->
    Exception_without_parameter.exceptions_caught_and_uncaught *
    (Exception_without_parameter.exceptions_caught_and_uncaught ->
     Exception_without_parameter.exceptions_caught_and_uncaught *
     ('bool, bool) Mvbdu_sig.premvbdu),
    bool Mvbdu_sig.mvbdu, 'memo_tables, 'c, int)
   memoized_fun;
  
  build_false:
    int ->
    int ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'bool Mvbdu_sig.cell);
  build_true:
    int ->
    int ->
    'bool Mvbdu_sig.mvbdu ->
    'bool Mvbdu_sig.mvbdu ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'bool Mvbdu_sig.cell);
}
