(**
   * mvbdu_wrapper.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 08/03/2010
   * Last modification: Time-stamp: <Dec 09 2018>
   * *
   * This library provides test benchmarks for the library of sets of finite maps from integers to integers
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

module type Mvbdu = sig
  type key
  type value

  type handler =
    ( Boolean_mvbdu.memo_tables,
      Boolean_mvbdu.mvbdu_dic,
      Boolean_mvbdu.association_list_dic,
      Boolean_mvbdu.range_list_dic,
      Boolean_mvbdu.variables_list_dic,
      bool,
      int )
    Memo_sig.handler

  type mvbdu
  type hconsed_range_list
  type hconsed_association_list
  type hconsed_variables_list
  type hconsed_renaming_list

  type 'output constant =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    Exception.method_handler * handler * 'output

  type ('input, 'output) unary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input ->
    Exception.method_handler * handler * 'output

  type ('input1, 'input2, 'output) binary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input1 ->
    'input2 ->
    Exception.method_handler * handler * 'output

  type ('input1, 'input2, 'input3, 'output) ternary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input1 ->
    'input2 ->
    'input3 ->
    Exception.method_handler * handler * 'output

  val init :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Exception.method_handler * handler

  val is_init : unit -> bool

  val get_handler :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Exception.method_handler * handler

  val reset :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Exception.method_handler * handler

  val equal : mvbdu -> mvbdu -> bool
  val equal_with_logs : (mvbdu, mvbdu, bool) binary
  val mvbdu_false : mvbdu constant
  val mvbdu_true : mvbdu constant
  val mvbdu_not : (mvbdu, mvbdu) unary
  val mvbdu_id : (mvbdu, mvbdu) unary
  val mvbdu_unary_true : (mvbdu, mvbdu) unary
  val mvbdu_unary_false : (mvbdu, mvbdu) unary
  val mvbdu_and : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_or : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_xor : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nand : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nor : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_imply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_rev_imply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_equiv : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nimply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nrev_imply : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_bi_true : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_bi_false : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_fst : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_snd : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nfst : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_nsnd : (mvbdu, mvbdu, mvbdu) binary
  val mvbdu_redefine : (mvbdu, hconsed_association_list, mvbdu) binary
  val mvbdu_redefine_range : (mvbdu, hconsed_range_list, mvbdu) binary
  val mvbdu_subseteq : (mvbdu, mvbdu, bool) binary
  val mvbdu_of_hconsed_asso : (hconsed_association_list, mvbdu) unary
  val mvbdu_of_association_list : ((key * value) list, mvbdu) unary
  val mvbdu_of_sorted_association_list : ((key * value) list, mvbdu) unary

  val mvbdu_of_reverse_sorted_association_list :
    ((key * value) list, mvbdu) unary

  val mvbdu_of_hconsed_range : (hconsed_range_list, mvbdu) unary

  val mvbdu_of_range_list :
    ((key * (value option * value option)) list, mvbdu) unary

  val mvbdu_of_sorted_range_list :
    ((key * (value option * value option)) list, mvbdu) unary

  val mvbdu_of_reverse_sorted_range_list :
    ((key * (value option * value option)) list, mvbdu) unary

  val mvbdu_rename : (mvbdu, hconsed_renaming_list, mvbdu) binary
  val mvbdu_project_keep_only : (mvbdu, hconsed_variables_list, mvbdu) binary

  val mvbdu_project_abstract_away :
    (mvbdu, hconsed_variables_list, mvbdu) binary

  val mvbdu_cartesian_decomposition_depth :
    (mvbdu, int, mvbdu option * mvbdu list) binary

  val mvbdu_full_cartesian_decomposition : (mvbdu, mvbdu list) unary
  val mvbdu_cartesian_abstraction : (mvbdu, mvbdu list) unary

  val build_association_list :
    ((key * value) list, hconsed_association_list) unary

  val build_sorted_association_list :
    ((key * value) list, hconsed_association_list) unary

  val build_reverse_sorted_association_list :
    ((key * value) list, hconsed_association_list) unary

  val empty_association_list : hconsed_association_list constant

  val build_range_list :
    ((key * (value option * value option)) list, hconsed_range_list) unary

  val build_sorted_range_list :
    ((key * (value option * value option)) list, hconsed_range_list) unary

  val build_reverse_sorted_range_list :
    ((key * (value option * value option)) list, hconsed_range_list) unary

  val empty_range_list : hconsed_range_list constant
  val build_variables_list : (key list, hconsed_variables_list) unary
  val build_sorted_variables_list : (key list, hconsed_variables_list) unary

  val build_reverse_sorted_variables_list :
    (key list, hconsed_variables_list) unary

  val empty_variables_list : hconsed_variables_list constant
  val build_renaming_list : ((key * key) list, hconsed_renaming_list) unary

  val build_sorted_renaming_list :
    ((key * key) list, hconsed_renaming_list) unary

  val build_reverse_sorted_renaming_list :
    ((key * key) list, hconsed_renaming_list) unary

  val empty_renaming_list : hconsed_renaming_list constant

  val overwrite_association_lists :
    ( hconsed_association_list,
      hconsed_association_list,
      hconsed_association_list )
    binary

  val merge_variables_lists :
    ( hconsed_variables_list,
      hconsed_variables_list,
      hconsed_variables_list )
    binary

  val nbr_variables : (hconsed_variables_list, int) unary
  val extensional_of_variables_list : (hconsed_variables_list, key list) unary

  val extensional_of_association_list :
    (hconsed_association_list, (key * value) list) unary

  val extensional_of_range_list :
    (hconsed_range_list, (key * (value option * value option)) list) unary

  val extensional_of_mvbdu : (mvbdu, (key * value) list list) unary
  val variables_list_of_mvbdu : (mvbdu, hconsed_variables_list) unary
  val print : Remanent_parameters_sig.parameters -> mvbdu -> unit

  val print_association_list :
    Remanent_parameters_sig.parameters -> hconsed_association_list -> unit

  val print_variables_list :
    Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit

  val store_by_variables_list :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'data ->
    List_sig.hash_key ->
    'map ->
    Exception.method_handler * 'data) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    List_sig.hash_key ->
    'data ->
    'map ->
    Exception.method_handler * 'map) ->
    'data ->
    ('data, 'data, 'data) binary ->
    (hconsed_variables_list, 'data, 'map, 'map) ternary

  val store_by_mvbdu :
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    'data ->
    Mvbdu_sig.hash_key ->
    'map ->
    Exception.method_handler * 'data) ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    Mvbdu_sig.hash_key ->
    'data ->
    'map ->
    Exception.method_handler * 'map) ->
    'data ->
    ('data, 'data, 'data) binary ->
    (mvbdu, 'data, 'map, 'map) ternary

  val last_entry : (unit, int) unary
  val hash_of_range_list : hconsed_range_list -> int
  val hash_of_association_list : hconsed_association_list -> int
  val hash_of_variables_list : hconsed_variables_list -> int
end

module type Internalized_mvbdu = sig
  type key
  type value
  type mvbdu

  type handler =
    ( Boolean_mvbdu.memo_tables,
      Boolean_mvbdu.mvbdu_dic,
      Boolean_mvbdu.association_list_dic,
      Boolean_mvbdu.range_list_dic,
      Boolean_mvbdu.variables_list_dic,
      bool,
      int )
    Memo_sig.handler

  type hconsed_range_list
  type hconsed_association_list
  type hconsed_variables_list
  type hconsed_renaming_list

  val init : Remanent_parameters_sig.parameters -> unit
  val import_handler : handler -> unit

  val export_handler :
    Exception.method_handler -> Exception.method_handler * handler option

  val is_init : unit -> bool
  val equal : mvbdu -> mvbdu -> bool
  val mvbdu_false : unit -> mvbdu
  val mvbdu_true : unit -> mvbdu
  val mvbdu_not : mvbdu -> mvbdu
  val mvbdu_id : mvbdu -> mvbdu
  val mvbdu_unary_true : mvbdu -> mvbdu
  val mvbdu_unary_false : mvbdu -> mvbdu
  val mvbdu_and : mvbdu -> mvbdu -> mvbdu
  val mvbdu_or : mvbdu -> mvbdu -> mvbdu
  val mvbdu_xor : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nand : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nor : mvbdu -> mvbdu -> mvbdu
  val mvbdu_imply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_rev_imply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_equiv : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nimply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nrev_imply : mvbdu -> mvbdu -> mvbdu
  val mvbdu_bi_true : mvbdu -> mvbdu -> mvbdu
  val mvbdu_bi_false : mvbdu -> mvbdu -> mvbdu
  val mvbdu_fst : mvbdu -> mvbdu -> mvbdu
  val mvbdu_snd : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nfst : mvbdu -> mvbdu -> mvbdu
  val mvbdu_nsnd : mvbdu -> mvbdu -> mvbdu
  val mvbdu_redefine : mvbdu -> hconsed_association_list -> mvbdu
  val mvbdu_redefine_range : mvbdu -> hconsed_range_list -> mvbdu
  val mvbdu_subseteq : mvbdu -> mvbdu -> bool
  val mvbdu_of_hconsed_asso : hconsed_association_list -> mvbdu
  val mvbdu_of_association_list : (key * value) list -> mvbdu
  val mvbdu_of_sorted_association_list : (key * value) list -> mvbdu
  val mvbdu_of_reverse_sorted_association_list : (key * value) list -> mvbdu
  val mvbdu_of_hconsed_range : hconsed_range_list -> mvbdu
  val mvbdu_of_range_list : (key * (value option * value option)) list -> mvbdu

  val mvbdu_of_sorted_range_list :
    (key * (value option * value option)) list -> mvbdu

  val mvbdu_of_reverse_sorted_range_list :
    (key * (value option * value option)) list -> mvbdu

  val mvbdu_rename : mvbdu -> hconsed_renaming_list -> mvbdu
  val mvbdu_project_abstract_away : mvbdu -> hconsed_variables_list -> mvbdu
  val mvbdu_project_keep_only : mvbdu -> hconsed_variables_list -> mvbdu
  val mvbdu_cartesian_abstraction : mvbdu -> mvbdu list

  val mvbdu_cartesian_decomposition_depth :
    mvbdu -> int -> mvbdu option * mvbdu list

  val mvbdu_full_cartesian_decomposition : mvbdu -> mvbdu list
  val build_association_list : (key * value) list -> hconsed_association_list

  val build_sorted_association_list :
    (key * value) list -> hconsed_association_list

  val build_reverse_sorted_association_list :
    (key * value) list -> hconsed_association_list

  val empty_association_list : unit -> hconsed_association_list

  val build_range_list :
    (key * (value option * value option)) list -> hconsed_range_list

  val build_sorted_range_list :
    (key * (value option * value option)) list -> hconsed_range_list

  val build_reverse_sorted_range_list :
    (key * (value option * value option)) list -> hconsed_range_list

  val empty_range_list : unit -> hconsed_range_list
  val build_variables_list : key list -> hconsed_variables_list
  val build_sorted_variables_list : key list -> hconsed_variables_list
  val build_reverse_sorted_variables_list : key list -> hconsed_variables_list
  val empty_variables_list : unit -> hconsed_variables_list
  val build_renaming_list : (key * key) list -> hconsed_renaming_list
  val build_sorted_renaming_list : (key * key) list -> hconsed_renaming_list

  val build_reverse_sorted_renaming_list :
    (key * key) list -> hconsed_renaming_list

  val empty_renaming_list : unit -> hconsed_renaming_list

  val overwrite_association_lists :
    hconsed_association_list ->
    hconsed_association_list ->
    hconsed_association_list

  val merge_variables_lists :
    hconsed_variables_list -> hconsed_variables_list -> hconsed_variables_list

  val nbr_variables : hconsed_variables_list -> int
  val extensional_of_variables_list : hconsed_variables_list -> key list

  val extensional_of_association_list :
    hconsed_association_list -> (key * value) list

  val extensional_of_mvbdu : mvbdu -> (key * value) list list
  val variables_list_of_mvbdu : mvbdu -> hconsed_variables_list
  val print : Remanent_parameters_sig.parameters -> mvbdu -> unit

  val print_association_list :
    Remanent_parameters_sig.parameters -> hconsed_association_list -> unit

  val print_variables_list :
    Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit

  val hash_of_association_list : hconsed_association_list -> int
  val hash_of_variables_list : hconsed_variables_list -> int
end

module type Nul = sig end

module Make (_ : Nul) : Mvbdu with type key = int and type value = int = struct
  type key = int
  type value = int

  type handler =
    ( Boolean_mvbdu.memo_tables,
      Boolean_mvbdu.mvbdu_dic,
      Boolean_mvbdu.association_list_dic,
      Boolean_mvbdu.range_list_dic,
      Boolean_mvbdu.variables_list_dic,
      bool,
      int )
    Memo_sig.handler

  type mvbdu = bool Mvbdu_sig.mvbdu
  type hconsed_range_list = (value option * value option) List_sig.list
  type hconsed_association_list = value List_sig.list
  type hconsed_variables_list = unit List_sig.list
  type hconsed_renaming_list = key List_sig.list

  type 'output constant =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    Exception.method_handler * handler * 'output

  type ('input, 'output) unary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input ->
    Exception.method_handler * handler * 'output

  type ('input1, 'input2, 'output) binary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input1 ->
    'input2 ->
    Exception.method_handler * handler * 'output

  type ('input1, 'input2, 'input3, 'output) ternary =
    Remanent_parameters_sig.parameters ->
    handler ->
    Exception.method_handler ->
    'input1 ->
    'input2 ->
    'input3 ->
    Exception.method_handler * handler * 'output

  let lift0 pos f parameters handler error =
    match f parameters handler error parameters with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a =
        Exception.warn_with_exn parameters error pos Exit (fun _ ->
            failwith "Cannot recover from bugs in constant initilization")
      in
      error, handler, a

  let init, is_init, reset, get_handler =
    let used = ref None in
    let init parameter error =
      match !used with
      | Some a ->
        Exception.warn parameter error __POS__
          ~message:"MVBDU should be initialised once only" Exit a
      | None ->
        let error, handler = Boolean_mvbdu.init_remanent parameter error in
        let error, handler, _ =
          lift0 __POS__ Boolean_mvbdu.boolean_mvbdu_false parameter handler
            error
        in
        let error, handler, _ =
          lift0 __POS__ Boolean_mvbdu.boolean_mvbdu_true parameter handler error
        in

        let () = used := Some handler in
        error, handler
    in
    let is_init () = !used != None in
    let get_handler parameter error =
      match !used with
      | None ->
        let error, handler = init parameter error in
        Exception.warn parameter error __POS__ ~message:"Uninitialised handler"
          Exit handler
      | Some a -> error, a
    in
    let reset parameter error =
      match !used with
      | None ->
        let error, handler = init parameter error in
        Exception.warn parameter error __POS__ ~message:"Uninitialised handler"
          Exit handler
      | Some _ ->
        let error, handler = Boolean_mvbdu.init_remanent parameter error in
        let () = used := Some handler in
        error, handler
    in
    init, is_init, reset, get_handler

  let equal = Mvbdu_core.mvbdu_equal
  let equal_with_logs _p h e a b = e, h, equal a b

  let last_entry parameters handler error () =
    let error, int = Boolean_mvbdu.last_entry parameters handler error in
    error, handler, int

  let mvbdu_true = lift0 __POS__ Boolean_mvbdu.boolean_mvbdu_true
  let mvbdu_false = lift0 __POS__ Boolean_mvbdu.boolean_mvbdu_false

  let lift1 pos f parameters handler error a =
    match f parameters handler error parameters a with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let lift1bis _string f parameters handler error a =
    let a, (b, c) =
      f
        (Boolean_mvbdu.association_list_allocate parameters)
        error parameters handler a
    in
    a, b, c

  let lift1bisbis _string f parameters handler error
      (a : (int * (int option * int option)) list) =
    let a, (b, c) =
      f
        (Boolean_mvbdu.range_list_allocate parameters)
        error parameters handler a
    in
    a, b, c

  let lift1ter _string f parameters handler error a =
    let a, (b, c) =
      f
        (Boolean_mvbdu.association_list_allocate parameters)
        parameters error handler a
    in
    a, b, c

  let lift1terter _string f parameters handler error a =
    let a, (b, c) =
      f
        (Boolean_mvbdu.range_list_allocate parameters)
        parameters error handler a
    in
    a, b, c

  let liftvbis _string f parameters handler error a =
    let a, (b, c) =
      f
        (Boolean_mvbdu.variables_list_allocate parameters)
        error parameters handler
        (List.rev_map (fun x -> x, ()) a)
    in
    a, b, c

  let liftvter _string f parameters handler error a =
    let a, (b, c) =
      f
        (Boolean_mvbdu.variables_list_allocate parameters)
        parameters error handler
        (List.rev_map (fun x -> x, ()) a)
    in
    a, b, c

  let lift1_ pos f parameters handler error a =
    match f parameters handler error a with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let lift1__ _string f parameters handler error a =
    match f parameters handler error a with
    | error, (handler, a) -> error, handler, a

  let lift1four buildlist pos f parameters handler error a =
    match f parameters error handler a with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, handler, list = buildlist parameters handler error [] in
      let error, a = Exception.warn parameters error pos Exit list in
      error, handler, (a : unit List_sig.list)

  let lift1five pos f parameters handler error a =
    match f parameters error parameters handler a with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit [] in
      error, handler, a

  let lift1_seven _string f parameters handler error a =
    match f parameters error handler a with
    | error, (handler, int) -> error, handler, int

  let lift2 pos f parameters handler error a b =
    match f parameters handler error parameters a b with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let lift2bis pos f parameters handler error a b =
    match f parameters error parameters handler a b with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let lift2ter pos f parameters handler error a b =
    match f parameters error parameters handler a b with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let lift2four pos f parameters handler error a b =
    match f parameters error handler a b with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let lift2five pos f parameters handler error a b =
    match f parameters error handler a b with
    | error, (handler, Some a) -> error, handler, a
    | error, (handler, None) ->
      let error, a = Exception.warn parameters error pos Exit a in
      error, handler, a

  let (mvbdu_not : (mvbdu, mvbdu) unary) =
    lift1 __POS__ Boolean_mvbdu.boolean_mvbdu_not

  let mvbdu_id _parameters handler error a = error, handler, a

  let mvbdu_unary_true parameters handler error _ =
    mvbdu_true parameters handler error

  let mvbdu_unary_false parameters handler error _ =
    mvbdu_false parameters handler error

  let mvbdu_and = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_and
  let mvbdu_or = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_or
  let mvbdu_xor = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_xor
  let mvbdu_nand = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_nand
  let mvbdu_nor = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_nor
  let mvbdu_imply = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_imply
  let mvbdu_rev_imply = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_is_implied
  let mvbdu_equiv = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_equiv
  let mvbdu_nimply = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_nimply
  let mvbdu_nrev_imply = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_nis_implied
  let mvbdu_bi_true = lift2 __POS__ Boolean_mvbdu.boolean_constant_bi_true
  let mvbdu_bi_false = lift2 __POS__ Boolean_mvbdu.boolean_constant_bi_false
  let mvbdu_fst _parameters handler error a _b = error, handler, a
  let mvbdu_snd _parameters handler error _a b = error, handler, b
  let mvbdu_nfst = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_nfst
  let mvbdu_nsnd = lift2 __POS__ Boolean_mvbdu.boolean_mvbdu_nsnd
  let mvbdu_redefine = lift2bis __POS__ Boolean_mvbdu.redefine
  let mvbdu_redefine_range = lift2bis __POS__ Boolean_mvbdu.redefine_range
  let mvbdu_rename = lift2bis __POS__ Boolean_mvbdu.monotonicaly_rename
  let mvbdu_project_keep_only = lift2ter __POS__ Boolean_mvbdu.project_keep_only

  let mvbdu_project_abstract_away =
    lift2ter __POS__ Boolean_mvbdu.project_abstract_away

  let build_association_list = lift1bis __POS__ List_algebra.build_list

  let build_sorted_association_list =
    lift1ter __POS__ List_algebra.build_sorted_list

  let build_reverse_sorted_association_list =
    lift1ter __POS__ List_algebra.build_reversed_sorted_list

  let build_range_list = lift1bisbis __POS__ List_algebra.build_list

  let build_sorted_range_list =
    lift1terter __POS__ List_algebra.build_sorted_list

  let build_reverse_sorted_range_list =
    lift1terter __POS__ List_algebra.build_reversed_sorted_list

  let empty_range_list parameter handler error =
    build_range_list parameter handler error []

  let empty_association_list parameter handler error =
    build_association_list parameter handler error []

  let mvbdu_subseteq parameter handler error mvbdu1 mvbdu2 =
    let error, handler, union =
      mvbdu_or parameter handler error mvbdu1 mvbdu2
    in
    error, handler, equal mvbdu2 union

  let mvbdu_of_hconsed_asso parameter handler error asso =
    let error, handler, mvbdu_true = mvbdu_true parameter handler error in
    mvbdu_redefine parameter handler error mvbdu_true asso

  let mvbdu_of_hconsed_range parameter handler error asso =
    let error, handler, mvbdu_true = mvbdu_true parameter handler error in
    mvbdu_redefine_range parameter handler error mvbdu_true asso

  let mvbdu_of_asso_gen f parameter handler error asso =
    let error, handler, hconsed_list = f parameter handler error asso in
    mvbdu_of_hconsed_asso parameter handler error hconsed_list

  let mvbdu_of_range_gen f parameter handler error asso =
    let error, handler, hconsed_list = f parameter handler error asso in
    mvbdu_of_hconsed_range parameter handler error hconsed_list

  let mvbdu_of_hconsed_range parameter handler error asso =
    let error, handler, mvbdu_true = mvbdu_true parameter handler error in
    mvbdu_redefine_range parameter handler error mvbdu_true asso

  let mvbdu_of_association_list = mvbdu_of_asso_gen build_association_list
  let mvbdu_of_range_list = mvbdu_of_range_gen build_range_list

  let mvbdu_of_sorted_association_list =
    mvbdu_of_asso_gen build_sorted_association_list

  let mvbdu_of_sorted_range_list = mvbdu_of_range_gen build_sorted_range_list

  let mvbdu_of_reverse_sorted_association_list =
    mvbdu_of_asso_gen build_reverse_sorted_association_list

  let mvbdu_of_reverse_sorted_range_list =
    mvbdu_of_range_gen build_reverse_sorted_range_list

  let build_renaming_list = build_association_list
  let build_sorted_renaming_list = build_sorted_association_list
  let build_reverse_sorted_renaming_list = build_sorted_renaming_list
  let empty_renaming_list = empty_association_list

  let build_variables_list =
    liftvbis "line 257, build_list" List_algebra.build_list

  let build_sorted_variables_list =
    liftvter "line 259, build_list" List_algebra.build_reversed_sorted_list

  let build_reverse_sorted_variables_list =
    liftvter "line 261, build_list" List_algebra.build_sorted_list

  let empty_variables_list parameter handler error =
    build_variables_list parameter handler error []

  let variables_list_of_mvbdu parameter handler error mvbdu =
    lift1four build_sorted_variables_list __POS__
      Boolean_mvbdu.variables_of_mvbdu parameter handler error mvbdu

  let extensional_of_association_list parameters handler error l =
    lift1five __POS__ Boolean_mvbdu.extensional_description_of_association_list
      parameters handler error l

  let extensional_of_range_list parameters handler error l =
    lift1five __POS__ Boolean_mvbdu.extensional_description_of_range_list
      parameters handler error l

  let extensional_of_variables_list parameters handler error l =
    lift1five __POS__ Boolean_mvbdu.extensional_description_of_variables_list
      parameters handler error l

  let extensional_of_mvbdu parameters handler error mvbdu =
    lift1__ __POS__ Boolean_mvbdu.extensional_description_of_mvbdu parameters
      handler error mvbdu

  let print = Boolean_mvbdu.print_mvbdu
  let print_association_list = List_algebra.print_association_list
  let print_variables_list = List_algebra.print_variables_list
  let mvbdu_clean_head = lift1_ __POS__ Boolean_mvbdu.clean_head
  let mvbdu_keep_head_only = lift1_ __POS__ Boolean_mvbdu.keep_head_only

  let mvbdu_cartesian_abstraction parameters handler error bdu =
    let error, handler, bdd_true = mvbdu_true parameters handler error in
    (*let error = Exception.check_point
          Exception.warn parameters error error' __POS__ Exit
      in*)
    let error, handler, bdd_false = mvbdu_false parameters handler error in
    (*let error = Exception.check_point
          Exception.warn parameters error error'' __POS__ Exit
      in*)
    let rec aux error handler bdu list =
      if equal bdu bdd_true || equal bdu bdd_false then
        error, handler, List.rev list
      else (
        let error, handler, head =
          mvbdu_keep_head_only parameters error handler bdu
        in
        (*let error = Exception.check_point
              Exception.warn parameters error error' __POS__ Exit
          in*)
        let error, handler, tail =
          mvbdu_clean_head parameters error handler bdu
        in
        (*let error = Exception.check_point
              Exception.warn parameters error error'' __POS__ Exit
          in*)
        aux error handler tail (head :: list)
      )
    in
    aux error handler bdu []

  let mvbdu_cartesian_decomposition_depth parameters handler error bdu int =
    Boolean_mvbdu.mvbdu_cartesian_decomposition_depth variables_list_of_mvbdu
      extensional_of_variables_list build_sorted_variables_list
      mvbdu_project_keep_only mvbdu_project_abstract_away mvbdu_and equal
      parameters handler error bdu int

  let mvbdu_full_cartesian_decomposition parameters handler error bdu =
    let error, handler, l =
      variables_list_of_mvbdu parameters handler error bdu
    in
    (*let error = Exception.check_point
          Exception.warn parameters error error' __POS__ Exit
      in*)
    let error, handler, list =
      extensional_of_variables_list parameters handler error l
    in
    (*let error = Exception.check_point
          Exception.warn parameters error error'' __POS__ Exit
      in*)
    let size = List.length list in
    let error, handler, (bdu_opt, list) =
      mvbdu_cartesian_decomposition_depth parameters handler error bdu (size / 2)
    in
    (*let error = Exception.check_point
          Exception.warn parameters error error_3 __POS__ Exit
      in*)
    match bdu_opt with
    | None -> error, handler, list
    | Some bdu -> error, handler, bdu :: list

  let merge_variables_lists parameters handler error l1 l2 =
    lift2four __POS__ Boolean_mvbdu.merge_variables_lists parameters handler
      error l1 l2

  let overwrite_association_lists parameters handler error l1 l2 =
    lift2five __POS__ Boolean_mvbdu.overwrite_association_lists parameters
      handler error l1 l2

  let nbr_variables parameter handler error l =
    lift1_seven "line 539" Boolean_mvbdu.length parameter handler error l

  let store_by_gen get_id get set default join parameters handler error
      hash_consed_object data storage =
    let id = get_id hash_consed_object in
    let error, old_data = get parameters error default id storage in
    let error, handler, data = join parameters handler error old_data data in
    let error, storage = set parameters error id data storage in
    error, handler, storage

  let store_by_variables_list get set default join parameters handler error
      hash_consed_object data storage =
    store_by_gen List_core.id_of_list get set default join parameters handler
      error hash_consed_object data storage

  let store_by_mvbdu get set default join parameters handler error
      hash_consed_object data storage =
    store_by_gen Mvbdu_core.id_of_mvbdu get set default join parameters handler
      error hash_consed_object data storage

  let hash_of_association_list x = List_core.id_of_list x
  let hash_of_variables_list x = List_core.id_of_list x
  let hash_of_range_list x = List_core.id_of_list x
end

module Internalize (M : Mvbdu with type key = int and type value = int) :
  Internalized_mvbdu
    with type key = int
     and type value = int
     and type mvbdu = M.mvbdu = struct
  module Mvbdu = M
  include Mvbdu

  let handler = ref None

  let parameter =
    ref
      (Remanent_parameters.get_parameters
         ~called_from:Remanent_parameters_sig.Internalised ())

  let import_handler h = handler := Some h

  let export_handler error =
    match !handler with
    | None -> Exception.warn !parameter error __POS__ Exit None
    | Some _ -> error, !handler

  let check pos error error' handler' =
    let () = handler := Some handler' in
    if error' == error then
      ()
    else (
      let error', () = Exception.warn !parameter error pos Exit () in
      Exception.print !parameter error'
    )

  let init parameters =
    let error = Exception.empty_error_handler in
    let error', output = init parameters error in
    let () = parameter := parameters in
    check __POS__ error error' output

  let is_init () = None != !handler
  let equal = M.equal

  let get_handler pos error =
    match !handler with
    | None ->
      let () = init !parameter in
      let error', () =
        Exception.warn !parameter error pos ~message:" uninitialised mvbdu" Exit
          ()
      in
      (match !handler with
      | None -> failwith "unrecoverable errors in bdu get_handler"
      | Some h -> error', h)
    | Some h -> error, h

  let lift_const s f =
    let error = Exception.empty_error_handler in
    let error', handler = get_handler s error in
    let error', handler, mvbdu = f !parameter handler error' in
    let _ = check s error error' handler in
    mvbdu

  let mvbdu_true () = lift_const __POS__ M.mvbdu_true
  let mvbdu_false () = lift_const __POS__ M.mvbdu_false

  let lift_unary s f x =
    let error = Exception.empty_error_handler in
    let error', handler = get_handler s error in
    let error', handler, mvbdu = f !parameter handler error' x in
    let _ = check s error error' handler in
    mvbdu

  let mvbdu_id = lift_unary __POS__ M.mvbdu_id
  let mvbdu_not = lift_unary __POS__ M.mvbdu_not
  let mvbdu_unary_true _ = mvbdu_true ()
  let mvbdu_unary_false _ = mvbdu_false ()
  let mvbdu_bi_true _ _ = mvbdu_true ()
  let mvbdu_bi_false _ _ = mvbdu_false ()

  let lift_binary s f x y =
    let error = Exception.empty_error_handler in
    let error', handler = get_handler s error in
    let error', handler, mvbdu = f !parameter handler error' x y in
    let _ = check s error error' handler in
    mvbdu

  let lift_binary''' s f x y =
    let error = Exception.empty_error_handler in
    let error', handler = get_handler s error in
    let error', handler, mvbdu = f !parameter handler error' x y in
    let _ = check s error error' handler in
    mvbdu

  let lift_binary'''' s f x y =
    let error = Exception.empty_error_handler in
    let error', handler = get_handler s error in
    let error', handler, mvbdu = f !parameter handler error' x y in
    let _ = check s error error' handler in
    mvbdu

  let mvbdu_and = lift_binary __POS__ M.mvbdu_and
  let mvbdu_or = lift_binary __POS__ M.mvbdu_or
  let mvbdu_nand = lift_binary __POS__ M.mvbdu_nand
  let mvbdu_snd _ b = b
  let mvbdu_nsnd _ b = mvbdu_not b
  let mvbdu_fst a _ = a
  let mvbdu_nfst a _ = mvbdu_not a
  let mvbdu_xor = lift_binary __POS__ M.mvbdu_xor
  let mvbdu_nor = lift_binary __POS__ M.mvbdu_nor
  let mvbdu_imply = lift_binary __POS__ M.mvbdu_imply
  let mvbdu_nimply = lift_binary __POS__ M.mvbdu_nimply
  let mvbdu_rev_imply = lift_binary __POS__ M.mvbdu_rev_imply
  let mvbdu_nrev_imply = lift_binary __POS__ M.mvbdu_nrev_imply
  let mvbdu_equiv = lift_binary __POS__ M.mvbdu_equiv
  let mvbdu_redefine = lift_binary __POS__ M.mvbdu_redefine
  let mvbdu_redefine_range = lift_binary __POS__ M.mvbdu_redefine_range
  let mvbdu_rename = lift_binary __POS__ Mvbdu.mvbdu_rename
  let mvbdu_project_keep_only = lift_binary __POS__ M.mvbdu_project_keep_only

  let mvbdu_project_abstract_away =
    lift_binary __POS__ M.mvbdu_project_abstract_away

  let build_association_list = lift_unary __POS__ M.build_association_list

  let build_sorted_association_list =
    lift_unary __POS__ M.build_sorted_association_list

  let build_reverse_sorted_association_list =
    lift_unary __POS__ M.build_reverse_sorted_association_list

  let empty_association_list () = build_association_list []
  let build_renaming_list = lift_unary __POS__ M.build_renaming_list

  let build_sorted_renaming_list =
    lift_unary __POS__ M.build_sorted_renaming_list

  let build_reverse_sorted_renaming_list =
    lift_unary __POS__ M.build_reverse_sorted_renaming_list

  let empty_renaming_list () = build_renaming_list []
  let build_range_list = lift_unary __POS__ M.build_range_list
  let build_sorted_range_list = lift_unary __POS__ M.build_sorted_range_list

  let build_reverse_sorted_range_list =
    lift_unary __POS__ M.build_reverse_sorted_range_list

  let empty_range_list () = build_range_list []
  let mvbdu_subseteq mvbdu1 mvbdu2 = equal (mvbdu_or mvbdu1 mvbdu2) mvbdu2
  let mvbdu_of_asso_gen f asso = mvbdu_redefine (mvbdu_true ()) (f asso)
  let mvbdu_of_hconsed_asso = mvbdu_of_asso_gen (fun x -> x)
  let mvbdu_of_association_list = mvbdu_of_asso_gen build_association_list

  let mvbdu_of_sorted_association_list =
    mvbdu_of_asso_gen build_sorted_association_list

  let mvbdu_of_reverse_sorted_association_list =
    mvbdu_of_asso_gen build_reverse_sorted_association_list

  let mvbdu_of_range_gen f asso = mvbdu_redefine_range (mvbdu_true ()) (f asso)
  let mvbdu_of_hconsed_range = mvbdu_of_range_gen (fun x -> x)
  let mvbdu_of_range_list = mvbdu_of_range_gen build_range_list
  let mvbdu_of_sorted_range_list = mvbdu_of_range_gen build_sorted_range_list

  let mvbdu_of_reverse_sorted_range_list =
    mvbdu_of_range_gen build_reverse_sorted_range_list

  let build_variables_list = lift_unary __POS__ M.build_variables_list

  let build_sorted_variables_list =
    lift_unary __POS__ M.build_sorted_variables_list

  let build_reverse_sorted_variables_list =
    lift_unary __POS__ M.build_reverse_sorted_variables_list

  let empty_variables_list () = build_variables_list []

  let merge_variables_lists l1 l2 =
    lift_binary''' __POS__ M.merge_variables_lists l1 l2

  let nbr_variables l = lift_unary __POS__ M.nbr_variables l

  let overwrite_association_lists l1 l2 =
    lift_binary'''' __POS__ M.overwrite_association_lists l1 l2

  let variables_list_of_mvbdu l = lift_unary __POS__ M.variables_list_of_mvbdu l

  let mvbdu_cartesian_abstraction =
    lift_unary __POS__ M.mvbdu_cartesian_abstraction

  let extensional_of_association_list l =
    lift_unary __POS__ M.extensional_of_association_list l

  let extensional_of_variables_list l =
    lift_unary __POS__ M.extensional_of_variables_list l

  let extensional_of_mvbdu mvbdu =
    lift_unary __POS__ M.extensional_of_mvbdu mvbdu

  let mvbdu_full_cartesian_decomposition =
    lift_unary __POS__ M.mvbdu_full_cartesian_decomposition

  let mvbdu_cartesian_decomposition_depth =
    lift_binary __POS__ M.mvbdu_cartesian_decomposition_depth
end

module Optimize (M : Mvbdu with type key = int and type value = int) :
  Mvbdu with type mvbdu = M.mvbdu and type key = int and type value = int =
struct
  module Mvbdu = M
  include Mvbdu

  let mvbdu_not parameters handler error a =
    mvbdu_nand parameters handler error a a

  let mvbdu_unary_true parameters handler error a =
    let error, handler, nota = mvbdu_not parameters handler error a in
    mvbdu_nand parameters handler error a nota

  let mvbdu_unary_false parameters handler error a =
    let error, handler, mvtrue = mvbdu_unary_true parameters handler error a in
    mvbdu_not parameters handler error mvtrue

  let mvbdu_and parameters handler error a b =
    let error, handler, ab = mvbdu_nand parameters handler error a b in
    mvbdu_not parameters handler error ab

  let mvbdu_or parameters handler error a b =
    let error, handler, na = mvbdu_not parameters handler error a in
    let error, handler, nb = mvbdu_not parameters handler error b in
    mvbdu_nand parameters handler error na nb

  let mvbdu_imply parameters handler error a b =
    let error, handler, notb = mvbdu_not parameters handler error b in
    mvbdu_nand parameters handler error a notb

  let mvbdu_rev_imply parameters handler error a b =
    let error, handler, nota = mvbdu_not parameters handler error a in
    mvbdu_nand parameters handler error nota b

  let mvbdu_nor parameters handler error a b =
    let error, handler, bddor = mvbdu_or parameters handler error a b in
    mvbdu_not parameters handler error bddor

  let mvbdu_equiv parameters handler error a b =
    let error, handler, direct = mvbdu_imply parameters handler error a b in
    let error, handler, indirect = mvbdu_imply parameters handler error b a in
    mvbdu_and parameters handler error direct indirect

  let mvbdu_xor parameters handler error a b =
    let error, handler, equiv = mvbdu_equiv parameters handler error a b in
    mvbdu_not parameters handler error equiv

  let mvbdu_nimply parameters handler error a b =
    let error, handler, imply = mvbdu_imply parameters handler error a b in
    mvbdu_not parameters handler error imply

  let mvbdu_nrev_imply parameters handler error a b =
    mvbdu_nimply parameters handler error b a

  let mvbdu_bi_true parameters handler error a _ =
    M.mvbdu_unary_true parameters handler error a

  let mvbdu_bi_false parameters handler error a _ =
    M.mvbdu_unary_false parameters handler error a

  let mvbdu_nfst parameters handler error a _ =
    mvbdu_not parameters handler error a

  let mvbdu_nsnd parameters handler error _ a =
    mvbdu_not parameters handler error a

  let mvbdu_cartesian_decomposition_depth parameters handler error bdu int =
    Boolean_mvbdu.mvbdu_cartesian_decomposition_depth variables_list_of_mvbdu
      extensional_of_variables_list build_sorted_variables_list
      mvbdu_project_keep_only mvbdu_project_abstract_away mvbdu_and equal
      parameters handler error bdu int

  let mvbdu_full_cartesian_decomposition parameters handler error bdu =
    let error, handler, l =
      variables_list_of_mvbdu parameters handler error bdu
    in
    let error, handler, list =
      extensional_of_variables_list parameters handler error l
    in
    let size = List.length list in
    let error, handler, (bdu_opt, list) =
      mvbdu_cartesian_decomposition_depth parameters handler error bdu (size / 2)
    in
    match bdu_opt with
    | None -> error, handler, list
    | Some bdu -> error, handler, bdu :: list
end

module Optimize_internalized
    (M : Internalized_mvbdu with type key = int and type value = int) :
  Internalized_mvbdu
    with type mvbdu = M.mvbdu
     and type key = int
     and type value = int = struct
  module Mvbdu = M

  type key = Mvbdu.key
  type value = Mvbdu.value
  type mvbdu = Mvbdu.mvbdu
  type hconsed_association_list = Mvbdu.hconsed_association_list
  type hconsed_variables_list = Mvbdu.hconsed_variables_list
  type hconsed_renaming_list = Mvbdu.hconsed_renaming_list
  type hconsed_range_list = Mvbdu.hconsed_range_list
  type handler = Mvbdu.handler

  let import_handler = Mvbdu.import_handler
  let export_handler = Mvbdu.export_handler
  let init = Mvbdu.init
  let is_init = Mvbdu.is_init
  let equal = Mvbdu.equal
  let mvbdu_nand a = Mvbdu.mvbdu_nand a
  let mvbdu_not a = mvbdu_nand a a
  let mvbdu_id = Mvbdu.mvbdu_id
  let mvbdu_true = Mvbdu.mvbdu_true
  let mvbdu_false = Mvbdu.mvbdu_false
  let mvbdu_unary_true a = mvbdu_nand a (mvbdu_not a)
  let mvbdu_unary_false a = mvbdu_not (mvbdu_unary_true a)
  let mvbdu_and a b = mvbdu_not (mvbdu_nand a b)
  let mvbdu_or a b = mvbdu_nand (mvbdu_not a) (mvbdu_not b)
  let mvbdu_imply a b = mvbdu_nand a (mvbdu_not b)
  let mvbdu_rev_imply a b = mvbdu_imply b a
  let mvbdu_nor a b = mvbdu_not (mvbdu_or a b)
  let mvbdu_equiv a b = mvbdu_and (mvbdu_imply a b) (mvbdu_imply b a)
  let mvbdu_xor a b = mvbdu_not (mvbdu_equiv a b)
  let mvbdu_nimply a b = mvbdu_not (mvbdu_imply a b)
  let mvbdu_nrev_imply a b = mvbdu_nimply b a
  let mvbdu_bi_true _ _ = M.mvbdu_true ()
  let mvbdu_bi_false _ _ = M.mvbdu_false ()
  let mvbdu_fst a _ = a
  let mvbdu_snd _ b = b
  let mvbdu_nfst a _ = mvbdu_not a
  let mvbdu_nsnd _ a = mvbdu_not a
  let build_association_list = M.build_association_list
  let build_sorted_association_list = M.build_sorted_association_list

  let build_reverse_sorted_association_list =
    M.build_reverse_sorted_association_list

  let build_renaming_list = M.build_renaming_list
  let build_sorted_renaming_list = M.build_sorted_renaming_list
  let build_reverse_sorted_renaming_list = M.build_reverse_sorted_renaming_list
  let build_range_list = M.build_range_list
  let build_sorted_range_list = M.build_sorted_range_list
  let build_reverse_sorted_range_list = M.build_reverse_sorted_range_list
  let mvbdu_redefine = M.mvbdu_redefine
  let mvbdu_redefine_range = M.mvbdu_redefine_range
  let mvbdu_subseteq = M.mvbdu_subseteq
  let mvbdu_of_hconsed_asso = M.mvbdu_of_hconsed_asso
  let mvbdu_of_association_list = M.mvbdu_of_association_list
  let mvbdu_of_sorted_association_list = M.mvbdu_of_sorted_association_list

  let mvbdu_of_reverse_sorted_association_list =
    M.mvbdu_of_reverse_sorted_association_list

  let mvbdu_of_reverse_sorted_range_list = M.mvbdu_of_reverse_sorted_range_list
  let mvbdu_of_sorted_range_list = M.mvbdu_of_sorted_range_list
  let mvbdu_of_range_list = M.mvbdu_of_range_list
  let mvbdu_of_hconsed_range = M.mvbdu_of_hconsed_range
  let mvbdu_rename = M.mvbdu_rename
  let mvbdu_project_keep_only = M.mvbdu_project_keep_only
  let mvbdu_project_abstract_away = M.mvbdu_project_abstract_away
  let build_variables_list = M.build_variables_list
  let build_sorted_variables_list = M.build_sorted_variables_list

  let build_reverse_sorted_variables_list =
    M.build_reverse_sorted_variables_list

  let empty_renaming_list = M.empty_renaming_list
  let empty_variables_list = M.empty_variables_list
  let empty_association_list = M.empty_association_list
  let empty_range_list = M.empty_range_list
  let merge_variables_lists = M.merge_variables_lists
  let overwrite_association_lists = M.overwrite_association_lists
  let print = M.print
  let print_association_list = M.print_association_list
  let print_variables_list = M.print_variables_list
  let mvbdu_cartesian_abstraction = M.mvbdu_cartesian_abstraction
  let extensional_of_association_list = M.extensional_of_association_list
  let extensional_of_variables_list = M.extensional_of_variables_list
  let extensional_of_mvbdu = M.extensional_of_mvbdu
  let variables_list_of_mvbdu = M.variables_list_of_mvbdu

  let mvbdu_cartesian_decomposition_depth =
    M.mvbdu_cartesian_decomposition_depth

  let mvbdu_full_cartesian_decomposition = M.mvbdu_full_cartesian_decomposition
  let hash_of_association_list = M.hash_of_association_list
  let hash_of_variables_list = M.hash_of_variables_list
  let nbr_variables = M.nbr_variables
end

module Vd = struct end
module Mvbdu = Make (Vd)
module IntMvbdu = Internalize (Make (Vd))
module Optimized_Mvbdu = Optimize (Make (Vd))
module Optimized_IntMvbdu = Internalize (Optimize (Make (Vd)))
module Optimized_IntMvbdu_bis = Optimize_internalized (Internalize (Make (Vd)))
