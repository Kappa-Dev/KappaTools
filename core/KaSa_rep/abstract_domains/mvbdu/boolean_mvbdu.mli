val sanity_check : bool
val test_workbench : bool
(*module type Sig_Mvbdu_Skeleton =
  sig
    type t = bool Mvbdu_sig.skeleton
    val compare:t->t -> int
    val print: 'a -> 'b -> unit
  end

    module Mvbdu_Skeleton: Sig_Mvbdu_Skeleton*)

module D_mvbdu_skeleton :
  Dictionary.Dictionary
    with type key = int
     and type value = bool Mvbdu_sig.skeleton

module D_Range_list_skeleton :
  Dictionary.Dictionary
    with type key = int
     and type value = (int option * int option) List_sig.skeleton

module D_Association_list_skeleton :
  Dictionary.Dictionary
    with type key = int
     and type value = int List_sig.skeleton

module D_Variables_list_skeleton :
  Dictionary.Dictionary
    with type key = int
     and type value = unit List_sig.skeleton

module Hash_key : Set.OrderedType
module Hash_1 : Int_storage.Storage
module Hash_2 : Int_storage.Storage

type memo_tables = {
  boolean_mvbdu_identity: bool Mvbdu_sig.mvbdu Hash_1.t;
  boolean_mvbdu_not: bool Mvbdu_sig.mvbdu Hash_1.t;
  boolean_mvbdu_and: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_or: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_xor: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_nand: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_equiv: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_is_implied: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_imply: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_nis_implied: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_nimply: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_nor: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_fst: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_nfst: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_snd: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_nsnd: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_clean_head: bool Mvbdu_sig.mvbdu Hash_1.t;
  boolean_mvbdu_keep_head_only: bool Mvbdu_sig.mvbdu Hash_1.t;
  boolean_mvbdu_redefine: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_redefine_range: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_monotonicaly_rename: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_project_keep_only: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_project_abstract_away: bool Mvbdu_sig.mvbdu Hash_2.t;
  boolean_mvbdu_length_variables_list: int Hash_1.t;
  boolean_mvbdu_merge_variables_lists: unit List_sig.list Hash_2.t;
  boolean_mvbdu_overwrite_association_list: int List_sig.list Hash_2.t;
  boolean_mvbdu_extensional_description_of_variables_list: int list Hash_1.t;
  boolean_mvbdu_extensional_description_of_association_list:
    (int * int) list Hash_1.t;
  boolean_mvbdu_extensional_description_of_range_list:
    (int * (int option * int option)) list Hash_1.t;
  boolean_mvbdu_variables_of_mvbdu: unit List_sig.list Hash_1.t;
  boolean_mvbdu_extensional_description_of_mvbdu: (int * int) list list Hash_1.t;
}

type unary_memoized_fun

type mvbdu_dic =
  (bool Mvbdu_sig.cell, bool Mvbdu_sig.mvbdu) D_mvbdu_skeleton.dictionary

type association_list_dic =
  (int List_sig.cell, int List_sig.list) D_Association_list_skeleton.dictionary

type range_list_dic =
  ( (int option * int option) List_sig.cell,
    (int option * int option) List_sig.list )
  D_Range_list_skeleton.dictionary

type variables_list_dic =
  (unit List_sig.cell, unit List_sig.list) D_Variables_list_skeleton.dictionary

type handler =
  ( memo_tables,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    bool,
    int )
  Memo_sig.handler

type memo_unary

val last_entry :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler * int

val print_memo :
  Exception_without_parameter.method_handler ->
  handler ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler

val extensional_description_of_mvbdu :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler * (handler * (int * int) list list)

val extensional_description_of_range_list :
  'g ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  (int option * int option) List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * (int * (int option * int option)) list option)

val extensional_description_of_association_list :
  'a ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  int List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * (int * int) list option)

val extensional_description_of_variables_list :
  'a ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  'h List_sig.list ->
  Exception_without_parameter.method_handler * (handler * int list option)

val overwrite_association_lists :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  handler ->
  int List_sig.list ->
  int List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * int List_sig.list option)

val length :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  handler ->
  'g List_sig.list ->
  Exception_without_parameter.method_handler * (handler * int)

val f :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler

val mvbdu_cartesian_decomposition_depth :
  ('a -> 'b -> 'c -> 'd -> 'e * 'f * 'g) ->
  ('a -> 'f -> 'e -> 'g -> 'c * 'b * int list) ->
  ('a -> 'b -> 'c -> int list -> 'h * 'i * 'j) ->
  ('a -> 'i -> 'h -> 'd -> 'j -> 'k * 'l * 'd) ->
  ('a -> 'l -> 'k -> 'd -> 'j -> 'm * 'n * 'd) ->
  ('a -> 'n -> 'm -> 'd -> 'd -> 'c * 'b * 'o) ->
  ('o -> 'd -> bool) ->
  'a ->
  'b ->
  'c ->
  'd ->
  int ->
  'c * 'b * ('d option * 'd list)

val variables_of_mvbdu :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  handler ->
  'c Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * unit List_sig.list option)

val project_abstract_away :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val project_keep_only :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  'b List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val memo_keep_head_only :
  ( bool,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    'a,
    memo_tables,
    'b )
  Memo_sig.unary_memoized_fun

val redefine :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  int List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val redefine_range :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  (int option * int option) List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val monotonicaly_rename :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  int List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val range_list_allocate :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  ((int option * int option) List_sig.cell ->
  (int option * int option) List_sig.cell ->
  int) ->
  D_Range_list_skeleton.value ->
  (int option * int option) List_sig.cell ->
  (int -> (int option * int option) List_sig.list) ->
  handler ->
  Exception_without_parameter.method_handler
  * (int
    * (int option * int option) List_sig.cell
    * (int option * int option) List_sig.list
    * handler)
    option

val boolean_mvbdu_nsnd :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_nfst :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_snd :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_fst :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_nand :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_and :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_or :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_nor :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_constant_bi_false :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_constant_bi_true :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_nis_implied :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_is_implied :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_nimply :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_imply :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_equiv :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_xor :
  Remanent_parameters_sig.parameters ->
  handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_constant_true :
  Remanent_parameters_sig.parameters ->
  ( memo_tables,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    bool,
    'a )
  Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (( memo_tables,
       mvbdu_dic,
       association_list_dic,
       range_list_dic,
       variables_list_dic,
       bool,
       'a )
     Memo_sig.handler
    * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_constant_false :
  Remanent_parameters_sig.parameters ->
  ( memo_tables,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    bool,
    'a )
  Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (( memo_tables,
       mvbdu_dic,
       association_list_dic,
       range_list_dic,
       variables_list_dic,
       bool,
       'a )
     Memo_sig.handler
    * bool Mvbdu_sig.mvbdu option)

val init_remanent :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  Exception_without_parameter.method_handler
  * ( memo_tables,
      ('a, 'b) D_mvbdu_skeleton.dictionary,
      ('c, 'd) D_Association_list_skeleton.dictionary,
      ('e, 'f) D_Range_list_skeleton.dictionary,
      ('g, 'h) D_Variables_list_skeleton.dictionary,
      bool,
      'i )
    Memo_sig.handler

val boolean_mvbdu_false :
  Remanent_parameters_sig.parameters ->
  ( 'a,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    'b,
    'c )
  Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler
  * (( 'a,
       mvbdu_dic,
       association_list_dic,
       range_list_dic,
       variables_list_dic,
       'b,
       'c )
     Memo_sig.handler
    * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_true :
  Remanent_parameters_sig.parameters ->
  ( 'a,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    'b,
    'c )
  Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler
  * (( 'a,
       mvbdu_dic,
       association_list_dic,
       range_list_dic,
       variables_list_dic,
       'b,
       'c )
     Memo_sig.handler
    * bool Mvbdu_sig.mvbdu option)

val boolean_mvbdu_not :
  Remanent_parameters_sig.parameters ->
  ( memo_tables,
    mvbdu_dic,
    association_list_dic,
    range_list_dic,
    variables_list_dic,
    bool,
    'a )
  Memo_sig.handler ->
  Exception_without_parameter.method_handler ->
  Remanent_parameters_sig.parameters ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (( memo_tables,
       mvbdu_dic,
       association_list_dic,
       range_list_dic,
       variables_list_dic,
       bool,
       'a )
     Memo_sig.handler
    * bool Mvbdu_sig.mvbdu option)

val association_list_allocate :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  (int List_sig.cell -> int List_sig.cell -> int) ->
  D_Association_list_skeleton.value ->
  int List_sig.cell ->
  (int -> int List_sig.list) ->
  handler ->
  Exception_without_parameter.method_handler
  * (int * int List_sig.cell * int List_sig.list * handler) option

val variables_list_allocate :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  (unit List_sig.cell -> unit List_sig.cell -> int) ->
  D_Variables_list_skeleton.value ->
  unit List_sig.cell ->
  (int -> unit List_sig.list) ->
  handler ->
  Exception_without_parameter.method_handler
  * (int * unit List_sig.cell * unit List_sig.list * handler) option

val print_mvbdu :
  Remanent_parameters_sig.parameters -> bool Mvbdu_sig.mvbdu -> unit

val clean_head :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val keep_head_only :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  handler ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
  * (handler * bool Mvbdu_sig.mvbdu option)

val merge_variables_lists :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  handler ->
  unit List_sig.list ->
  unit List_sig.list ->
  Exception_without_parameter.method_handler
  * (handler * unit List_sig.list option)

val print_boolean_mvbdu :
  Remanent_parameters_sig.parameters ->
  Exception_without_parameter.method_handler ->
  bool Mvbdu_sig.mvbdu ->
  Exception_without_parameter.method_handler
