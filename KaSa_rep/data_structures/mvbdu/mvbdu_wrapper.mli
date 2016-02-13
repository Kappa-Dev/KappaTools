(** API of the multi-valued binary decision diagrams library *)

module type Mvbdu =
  sig
    type handler = (Boolean_mvbdu.memo_tables,Boolean_mvbdu.mvbdu_dic,Boolean_mvbdu.association_list_dic,Boolean_mvbdu.variables_list_dic,bool,int) Memo_sig.handler
    type mvbdu
    type hconsed_association_list
    type hconsed_variables_list

    type 'output constant = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> Exception.method_handler * handler * 'output
    type ('input,'output) unary =  Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'output) binary = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input1 -> 'input2 -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'input3,'output) ternary = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input1 -> 'input2 -> 'input3 -> Exception.method_handler * handler * 'output

    val init: Remanent_parameters_sig.parameters -> Exception.method_handler -> Exception.method_handler * handler
    val is_init: unit -> bool
    val equal: mvbdu -> mvbdu -> bool
    val equal_with_logs: (mvbdu,mvbdu,bool) binary
    val mvbdu_false: mvbdu constant
    val mvbdu_true:  mvbdu constant
    val mvbdu_not: (mvbdu,mvbdu) unary
    val mvbdu_id:  (mvbdu,mvbdu) unary
    val mvbdu_unary_true: (mvbdu,mvbdu) unary
    val mvbdu_unary_false: (mvbdu,mvbdu) unary
    val mvbdu_and:  (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_or: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_xor: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_nand: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_nor: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_imply: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_rev_imply: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_equiv: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_nimply: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_nrev_imply: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_bi_true: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_bi_false: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_fst: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_snd: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_nfst: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_nsnd: (mvbdu,mvbdu,mvbdu) binary
    val mvbdu_redefine: (mvbdu,hconsed_association_list,mvbdu) binary
    val mvbdu_rename: (mvbdu,hconsed_association_list,mvbdu) binary
    val mvbdu_project_keep_only: (mvbdu,hconsed_variables_list,mvbdu) binary
    val mvbdu_project_abstract_away: (mvbdu,hconsed_variables_list,mvbdu) binary
    val mvbdu_cartesian_decomposition_depth: (mvbdu,int,mvbdu option * mvbdu list) binary
    val mvbdu_full_cartesian_decomposition: (mvbdu,mvbdu list) unary

    val mvbdu_cartesian_abstraction: (mvbdu,mvbdu list) unary

    val build_association_list: ((int * int) list,hconsed_association_list) unary
    val build_sorted_association_list: ((int * int) list,hconsed_association_list) unary
    val build_reverse_sorted_association_list: ((int * int) list,hconsed_association_list) unary
    val empty_association_list : hconsed_association_list constant
    val build_variables_list: (int list,hconsed_variables_list) unary
    val build_sorted_variables_list: (int list,hconsed_variables_list) unary
    val build_reverse_sorted_variables_list: (int list,hconsed_variables_list) unary
    val empty_variables_list: hconsed_variables_list constant

    val overwrite_association_lists: (hconsed_association_list,hconsed_association_list,hconsed_association_list) binary
    val merge_variables_lists: (hconsed_variables_list,hconsed_variables_list,hconsed_variables_list) binary

    val extensional_of_variables_list: (hconsed_variables_list,int list) unary
    val extensional_of_association_list: (hconsed_association_list,(int*int) list) unary
    val extensional_of_mvbdu: (mvbdu,(int * int) list list) unary

    val variables_list_of_mvbdu: (mvbdu,hconsed_variables_list) unary

    val print: Remanent_parameters_sig.parameters -> mvbdu -> unit
    val print_association_list: Remanent_parameters_sig.parameters -> hconsed_association_list -> unit
    val print_variables_list: Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit

    val store_by_variables_list:
      ( Remanent_parameters_sig.parameters ->
	Exception.method_handler ->
	'data -> 
	List_sig.hash_key ->
	'map ->
	Exception.method_handler * 'data) ->
      ( Remanent_parameters_sig.parameters ->
	Exception.method_handler ->
	List_sig.hash_key ->
	'data ->
	'map ->
	Exception.method_handler * 'map) ->
      'data ->
      ('data,'data,'data) binary ->
       (hconsed_variables_list,'data,'map,'map) ternary

    val store_by_mvbdu:
      ( Remanent_parameters_sig.parameters ->
	Exception.method_handler ->
	'data ->
	Mvbdu_sig.hash_key ->
	'map ->
	Exception.method_handler * 'data) ->
      ( Remanent_parameters_sig.parameters ->
	Exception.method_handler ->
	Mvbdu_sig.hash_key ->
	'data ->
	'map ->
	Exception.method_handler * 'map) ->
      'data ->
      ('data,'data,'data) binary ->
       (mvbdu,'data,'map,'map) ternary

    val last_entry: (unit,int) unary

  end

module type Internalized_mvbdu =
  sig
    type mvbdu
    type hconsed_association_list
    type hconsed_variables_list
    val init: Remanent_parameters_sig.parameters -> unit
    val is_init: unit -> bool
    val equal: mvbdu -> mvbdu -> bool
    val mvbdu_false: unit -> mvbdu
    val mvbdu_true:  unit -> mvbdu
    val mvbdu_not: mvbdu -> mvbdu
    val mvbdu_id:  mvbdu -> mvbdu
    val mvbdu_unary_true: mvbdu -> mvbdu
    val mvbdu_unary_false: mvbdu -> mvbdu
    val mvbdu_and: mvbdu -> mvbdu -> mvbdu
    val mvbdu_or:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_xor:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_nand:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_nor:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_imply:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_rev_imply:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_equiv:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_nimply:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_nrev_imply:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_bi_true:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_bi_false:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_fst:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_snd:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_nfst:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_nsnd:  mvbdu -> mvbdu -> mvbdu
    val mvbdu_redefine:  mvbdu -> hconsed_association_list -> mvbdu
    val mvbdu_rename: mvbdu -> hconsed_association_list -> mvbdu
    val mvbdu_project_abstract_away: mvbdu -> hconsed_variables_list -> mvbdu
    val mvbdu_project_keep_only: mvbdu -> hconsed_variables_list -> mvbdu
    val mvbdu_cartesian_abstraction: mvbdu -> mvbdu list
    val mvbdu_cartesian_decomposition_depth: mvbdu -> int -> mvbdu option * mvbdu list
    val mvbdu_full_cartesian_decomposition: mvbdu -> mvbdu list

    val build_association_list: (int * int) list ->  hconsed_association_list
    val build_sorted_association_list: (int * int) list -> hconsed_association_list
    val build_reverse_sorted_association_list: (int * int) list -> hconsed_association_list
    val empty_association_list : unit -> hconsed_association_list
    val build_variables_list: int list ->  hconsed_variables_list
    val build_sorted_variables_list: int list -> hconsed_variables_list
    val build_reverse_sorted_variables_list: int list -> hconsed_variables_list
    val empty_variables_list : unit -> hconsed_variables_list
    val overwrite_association_lists: hconsed_association_list -> hconsed_association_list -> hconsed_association_list
    val merge_variables_lists: hconsed_variables_list -> hconsed_variables_list -> hconsed_variables_list

    val extensional_of_variables_list: hconsed_variables_list -> int list
    val extensional_of_association_list: hconsed_association_list -> (int*int) list
    val extensional_of_mvbdu: mvbdu -> (int * int) list list

    val variables_list_of_mvbdu: mvbdu -> hconsed_variables_list

    val print: Remanent_parameters_sig.parameters -> mvbdu -> unit
    val print_association_list: Remanent_parameters_sig.parameters -> hconsed_association_list -> unit
    val print_variables_list: Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit
  end

module type Nul =
  sig
  end
module Make (M:Nul): Mvbdu
module Mvbdu:Mvbdu
module IntMvbdu:Internalized_mvbdu
module Optimized_Mvbdu:Mvbdu
module Optimized_IntMvbdu:Internalized_mvbdu


