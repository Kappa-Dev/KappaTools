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

type ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun =
  ( 'bool,
    'b,
    'blist,
    'rlist,
    'vlist,
    'bool ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'c),
    'bool Mvbdu_sig.mvbdu,
    'd,
    'e,
    'bool Mvbdu_sig.mvbdu )
  memoized_fun

type ('bool,
       'b,
       'blist,
       'rlist,
       'vlist,
       'c,
       'd,
       'e,
       'f)
     unary_with_threshold_memoized_fun =
  ( 'bool,
    'b,
    'blist,
    'rlist,
    'vlist,
    'bool ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'c),
    int * 'bool Mvbdu_sig.mvbdu,
    'd,
    'e,
    'f )
  memoized_fun

type ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) binary_memoized_fun =
  ( 'bool,
    'b,
    'blist,
    'rlist,
    'vlist,
    ('bool ->
    Exception.exceptions_caught_and_uncaught
    * ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun)
    pair,
    'bool Mvbdu_sig.mvbdu * 'bool Mvbdu_sig.mvbdu,
    'd,
    'e,
    'bool Mvbdu_sig.mvbdu )
  memoized_fun

type ('bool, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_other_memoized_fun =
  ( 'bool,
    'b,
    'blist,
    'rlist,
    'vlist,
    'bool ->
    Exception.exceptions_caught_and_uncaught
    * (Exception.exceptions_caught_and_uncaught ->
      Exception.exceptions_caught_and_uncaught * 'bool Mvbdu_sig.cell),
    'd * 'bool Mvbdu_sig.mvbdu,
    'c,
    'e,
    'bool Mvbdu_sig.mvbdu )
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
