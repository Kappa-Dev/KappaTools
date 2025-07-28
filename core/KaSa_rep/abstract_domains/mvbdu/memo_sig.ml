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

type ('mvbdu_leave_data,
       'mvbdu_dic,
       'blist,
       'rlist,
       'vlist,
       'function_type,
       'd,
       'memo_tables,
       'int,
       'mvbdu)
     memoized_fun = {
  f:
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    'function_type;
  store:
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ( 'memo_tables,
      'mvbdu_dic,
      'blist,
      'rlist,
      'vlist,
      'mvbdu_leave_data,
      'int )
    handler ->
    'd ->
    'mvbdu ->
    Exception.exceptions_caught_and_uncaught
    * ( 'memo_tables,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        'mvbdu_leave_data,
        'int )
      handler;
  get:
    Remanent_parameters_sig.parameters ->
    Exception.exceptions_caught_and_uncaught ->
    ( 'memo_tables,
      'mvbdu_dic,
      'blist,
      'rlist,
      'vlist,
      'mvbdu_leave_data,
      'int )
    handler ->
    'd ->
    Exception.exceptions_caught_and_uncaught
    * (( 'memo_tables,
         'mvbdu_dic,
         'blist,
         'rlist,
         'vlist,
         'mvbdu_leave_data,
         'int )
       handler
      * 'mvbdu option);
}

and ('memo_tables,
      'mvbdu_dic,
      'blist,
      'rlist,
      'vlist,
      'mvbdu_leave_data,
      'int)
    handler = {
  data: 'memo_tables;
  mvbdu_dictionary: 'mvbdu_dic;
  association_list_dictionary: 'blist;
  variables_list_dictionary: 'vlist;
  range_list_dictionary: 'rlist;
  print_cell:
    Remanent_parameters_sig.parameters ->
    'mvbdu_leave_data Mvbdu_sig.cell ->
    unit;
  print_skel:
    Remanent_parameters_sig.parameters ->
    'mvbdu_leave_data Mvbdu_sig.skeleton ->
    unit;
  print_mvbdu:
    Remanent_parameters_sig.parameters ->
    'mvbdu_leave_data Mvbdu_sig.mvbdu ->
    unit;
}

type 'a pair = 'a * 'a

type ('mvbdu_leave_data,
       'mvbdu_dic,
       'blist,
       'rlist,
       'vlist,
       'c,
       'd,
       'int)
     unary_memoized_fun =
  ( 'mvbdu_leave_data,
    'mvbdu_dic,
    'blist,
    'rlist,
    'vlist,
    'mvbdu_leave_data ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'c),
    'mvbdu_leave_data Mvbdu_sig.mvbdu,
    'd,
    'int,
    'mvbdu_leave_data Mvbdu_sig.mvbdu )
  memoized_fun

type ('mvbdu_leave_data,
       'mvbdu_dic,
       'blist,
       'rlist,
       'vlist,
       'c,
       'd,
       'e,
       'f)
     unary_with_threshold_memoized_fun =
  ( 'mvbdu_leave_data,
    'mvbdu_dic,
    'blist,
    'rlist,
    'vlist,
    'mvbdu_leave_data ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'c),
    int * 'mvbdu_leave_data Mvbdu_sig.mvbdu,
    'd,
    'e,
    'f )
  memoized_fun

type ('mvbdu_leave_data,
       'mvbdu_dic,
       'blist,
       'rlist,
       'vlist,
       'c,
       'd,
       'int)
     binary_memoized_fun =
  ( 'mvbdu_leave_data,
    'mvbdu_dic,
    'blist,
    'rlist,
    'vlist,
    ('mvbdu_leave_data ->
    Exception.exceptions_caught_and_uncaught
    * ( 'mvbdu_leave_data,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        'c,
        'd,
        'int )
      unary_memoized_fun)
    pair,
    'mvbdu_leave_data Mvbdu_sig.mvbdu * 'mvbdu_leave_data Mvbdu_sig.mvbdu,
    'd,
    'int,
    'mvbdu_leave_data Mvbdu_sig.mvbdu )
  memoized_fun

type ('mvbdu_leave_data,
       'mvbdu_dic,
       'blist,
       'rlist,
       'vlist,
       'c,
       'd,
       'e)
     unary_other_memoized_fun =
  ( 'mvbdu_leave_data,
    'mvbdu_dic,
    'blist,
    'rlist,
    'vlist,
    'mvbdu_leave_data ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught
      * 'mvbdu_leave_data Mvbdu_sig.cell),
    'd * 'mvbdu_leave_data Mvbdu_sig.mvbdu,
    'c,
    'e,
    'mvbdu_leave_data Mvbdu_sig.mvbdu )
  memoized_fun

type ('mvbdu_leave_data,
       'mvbdu_dic,
       'blist,
       'rlist,
       'vlist,
       'c,
       'memo_tables,
       'int)
     reset = {
  empty_range_list:
    Exception.exceptions_caught_and_uncaught
    * ( 'mvbdu_leave_data,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        'c,
        'memo_tables,
        'int )
      unary_memoized_fun;
  empty_association_list:
    Exception.exceptions_caught_and_uncaught
    * ( 'mvbdu_leave_data,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        'c,
        'memo_tables,
        'int )
      unary_memoized_fun;
  empty_variables_list:
    Exception.exceptions_caught_and_uncaught
    * ( 'mvbdu_leave_data,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        'c,
        'memo_tables,
        'int )
      unary_memoized_fun;
  leaf:
    'mvbdu_leave_data ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught
      * 'mvbdu_leave_data Mvbdu_sig.cell);
  clean_head:
    Exception.exceptions_caught_and_uncaught
    * ( 'mvbdu_leave_data,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        'c,
        'memo_tables,
        'int )
      unary_memoized_fun;
  height:
    Exception.exceptions_caught_and_uncaught
    * ( bool,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        bool ->
        Exception_without_parameter.exceptions_caught_and_uncaught
        * (Exception_without_parameter.exceptions_caught_and_uncaught ->
          Exception_without_parameter.exceptions_caught_and_uncaught
          * ('mvbdu_leave_data, bool) Mvbdu_sig.premvbdu),
        bool Mvbdu_sig.mvbdu,
        'memo_tables,
        'c,
        int )
      memoized_fun;
  width:
    Exception.exceptions_caught_and_uncaught
    * ( bool,
        'mvbdu_dic,
        'blist,
        'rlist,
        'vlist,
        bool ->
        Exception_without_parameter.exceptions_caught_and_uncaught
        * (Exception_without_parameter.exceptions_caught_and_uncaught ->
          Exception_without_parameter.exceptions_caught_and_uncaught
          * ('mvbdu_leave_data, bool) Mvbdu_sig.premvbdu),
        bool Mvbdu_sig.mvbdu,
        'memo_tables,
        'c,
        int )
      memoized_fun;
  build_false:
    int ->
    int ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught
      * 'mvbdu_leave_data Mvbdu_sig.cell);
  build_true:
    int ->
    int ->
    'mvbdu_leave_data Mvbdu_sig.mvbdu ->
    'mvbdu_leave_data Mvbdu_sig.mvbdu ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught
      * 'mvbdu_leave_data Mvbdu_sig.cell);
}
