type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'f, 'g) memoized_fun = {
  f: Remanent_parameters_sig.parameters -> Exception.method_handler -> 'c;
  store:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('f, 'b, 'blist, 'rlist, 'vlist, 'a, 'g) handler ->
    'd ->
    'a Mvbdu_sig.mvbdu ->
    Exception.method_handler * ('f, 'b, 'blist, 'rlist, 'vlist, 'a, 'g) handler;
  get:
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    ('f, 'b, 'blist, 'rlist, 'vlist, 'a, 'g) handler ->
    'd ->
    Exception.method_handler
    * (('f, 'b, 'blist, 'rlist, 'vlist, 'a, 'g) handler
      * 'a Mvbdu_sig.mvbdu option);
}

and ('f, 'b, 'c, 'rlist, 'vlist, 'd, 'e) handler = {
  data: 'f;
  mvbdu_dictionary: 'b;
  association_list_dictionary: 'c;
  variables_list_dictionary: 'vlist;
  range_list_dictionary: 'rlist;
  print_cell: Remanent_parameters_sig.parameters -> 'd Mvbdu_sig.cell -> unit;
  print_skel:
    Remanent_parameters_sig.parameters -> 'd Mvbdu_sig.skeleton -> unit;
  print_mvbdu: Remanent_parameters_sig.parameters -> 'd Mvbdu_sig.mvbdu -> unit;
}

type 'a pair = 'a * 'a

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    'a ->
    Exception.method_handler
    * (Exception.method_handler -> Exception.method_handler * 'c),
    'a Mvbdu_sig.mvbdu,
    'd,
    'e )
  memoized_fun

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) binary_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    ('a ->
    Exception.method_handler
    * ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun)
    pair,
    'a Mvbdu_sig.mvbdu * 'a Mvbdu_sig.mvbdu,
    'd,
    'e )
  memoized_fun

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_other_memoized_fun =
  ( 'a,
    'b,
    'blist,
    'rlist,
    'vlist,
    'a ->
    Exception.method_handler
    * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell),
    'd * 'a Mvbdu_sig.mvbdu,
    'c,
    'e )
  memoized_fun

type ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) reset = {
  empty_range_list:
    Exception.method_handler
    * ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun;
  empty_association_list:
    Exception.method_handler
    * ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun;
  empty_variables_list:
    Exception.method_handler
    * ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun;
  leaf:
    'a ->
    Exception.method_handler
    * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell);
  clean_head:
    Exception.method_handler
    * ('a, 'b, 'blist, 'rlist, 'vlist, 'c, 'd, 'e) unary_memoized_fun;
  build_false:
    int ->
    int ->
    Exception.method_handler
    * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell);
  build_true:
    int ->
    int ->
    'a Mvbdu_sig.mvbdu ->
    'a Mvbdu_sig.mvbdu ->
    Exception.method_handler
    * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell);
}
