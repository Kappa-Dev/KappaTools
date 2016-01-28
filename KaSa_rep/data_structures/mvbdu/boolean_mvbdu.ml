(**
   * boolean_mvbdu.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2010, the 11th of March
   * Last modification: 2011, the 23rd of March
   * *
   * This library provides primitives to deal set of finite maps from integers to integers
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let sanity_check = false
let test_workbench = false
let trace_mvbdu_allocation = false

let invalid_arg parameters mh message exn value =
  Exception.warn parameters mh (Some "Mvbdu_bool") message exn (fun () -> value)

module Mvbdu_Skeleton =
struct
  type t = bool Mvbdu_sig.skeleton
  let (compare:t->t -> int) = compare
end

module Association_List_Skeleton =
struct
  type t = int List_sig.skeleton
  let (compare:t->t->int) = compare
end

module Variables_List_Skeleton =
struct
  type t = unit List_sig.skeleton
  let (compare:t->t->int) = compare
end

module Hash_key =
struct
  type t = int
  let compare = compare
end

module D_mvbdu_skeleton =
  (Dictionary.Dictionary_of_Ord (Mvbdu_Skeleton):Dictionary.Dictionary
   with type value = bool Mvbdu_sig.skeleton)

module D_Association_list_skeleton =
  (Dictionary.Dictionary_of_Ord (Association_List_Skeleton):Dictionary.Dictionary
   with type value = int List_sig.skeleton)

module D_Variables_list_skeleton =
  (Dictionary.Dictionary_of_Ord (Variables_List_Skeleton):Dictionary.Dictionary
   with type value = unit List_sig.skeleton)

module Hash_1 = Int_storage.Nearly_inf_Imperatif
module Hash_2 = Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif

type memo_unary = bool Mvbdu_sig.mvbdu Hash_1.t

type memo_tables =
  {
    boolean_mvbdu_identity    : bool Mvbdu_sig.mvbdu Hash_1.t;
    boolean_mvbdu_not         : bool Mvbdu_sig.mvbdu Hash_1.t;
    boolean_mvbdu_and         : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_or          : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_xor         : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nand        : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_equiv       : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_is_implied  : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_imply       : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nis_implied : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nimply      : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nor         : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_fst         : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nfst        : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_snd         : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nsnd        : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_clean_head  : bool Mvbdu_sig.mvbdu Hash_1.t;
    boolean_mvbdu_keep_head_only: bool Mvbdu_sig.mvbdu Hash_1.t;
    boolean_mvbdu_redefine    : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_monotonicaly_rename: bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_project_keep_only: bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_project_abstract_away: bool Mvbdu_sig.mvbdu Hash_2.t;

    boolean_mvbdu_merge_variables_lists: unit List_sig.list Hash_2.t;
    boolean_mvbdu_overwrite_association_list: int List_sig.list Hash_2.t;

    boolean_mvbdu_extensional_description_of_variables_list: int list Hash_1.t;
    boolean_mvbdu_extensional_description_of_association_list: (int*int) list Hash_1.t;

    boolean_mvbdu_variables_of_mvbdu: unit List_sig.list Hash_1.t;

    boolean_mvbdu_extensional_description_of_mvbdu: (int *int ) list list Hash_1.t;
  }

type mvbdu_dic = (bool Mvbdu_sig.cell, bool Mvbdu_sig.mvbdu) D_mvbdu_skeleton.dictionary
type association_list_dic  = (int List_sig.cell, int List_sig.list) D_Association_list_skeleton.dictionary
type variables_list_dic = (unit List_sig.cell, unit List_sig.list) D_Variables_list_skeleton.dictionary
type handler   = (memo_tables, mvbdu_dic, association_list_dic, variables_list_dic, bool, int) Memo_sig.handler

type unary_memoized_fun =
  (bool,
   mvbdu_dic,
   association_list_dic,
   variables_list_dic,
   Exception.method_handler -> bool -> Exception.method_handler  *
   (bool Mvbdu_sig.mvbdu,bool) Mvbdu_sig.premvbdu, memo_tables,
     memo_tables, int)
    Memo_sig.memoized_fun

let split_memo error handler =
  let x = handler.Memo_sig.data in
  error,
  [  (* _ -> mvbdu *)
    "id:",         x.boolean_mvbdu_identity;
    "not:",        x.boolean_mvbdu_not;
    "clean_head:", x.boolean_mvbdu_clean_head;
    "keep_head_only:", x.boolean_mvbdu_keep_head_only;
  ],
  [ (* _ -> _ -> mvbdu *)
    "and:",     x.boolean_mvbdu_and;
    "or:",      x.boolean_mvbdu_or;
    "xor:",     x.boolean_mvbdu_xor;
    "nand:",    x.boolean_mvbdu_nand;
    "<=>:",     x.boolean_mvbdu_equiv;
    "<=:",      x.boolean_mvbdu_is_implied;
    "=>:",      x.boolean_mvbdu_imply;
    "not <=:",  x.boolean_mvbdu_nis_implied;
    "not =>:",  x.boolean_mvbdu_nimply;
    "not:",     x.boolean_mvbdu_nor;
    "fst:",     x.boolean_mvbdu_fst;
    "not fst:", x.boolean_mvbdu_nfst;
    "snd:",     x.boolean_mvbdu_snd;
    "not snd:", x.boolean_mvbdu_nsnd;
    "reset:",   x.boolean_mvbdu_redefine;
    "rename:",  x.boolean_mvbdu_monotonicaly_rename;
    "project_onto:", x.boolean_mvbdu_project_keep_only;
    "project_away:", x.boolean_mvbdu_project_abstract_away;],
  [ (* _ -> variables_list *)
    "variables_of:", x.boolean_mvbdu_variables_of_mvbdu;
  ],
  [ (* _ -> _ -> variables_list *)
    "merge:", x.boolean_mvbdu_merge_variables_lists;
  ],
  [ (* _ -> _ -> association_list *)
    "overwrite:", x.boolean_mvbdu_overwrite_association_list;
  ],
  [ (* _ -> int list *)
    "extensional_of_variables_list:", x.boolean_mvbdu_extensional_description_of_variables_list;
  ],
  [ (* _ -> (int * int) list *)
    "Boolean_mvbdu_extensional_description_of_association_list:", x.boolean_mvbdu_extensional_description_of_association_list;
  ],
  [ (* _ -> (int * int) list list *)
    "Boolean_mvbdu+extensional_description_of_mvbdu:",x.boolean_mvbdu_extensional_description_of_mvbdu;
  ]

let rec print_cell parameter cell =
  match cell with
  | Mvbdu_sig.Leaf x ->
    let s = "Leaf "^(if x then "True" else "False")^" \n" in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%s" (Remanent_parameters.get_prefix parameter) s in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    ()
  | Mvbdu_sig.Node x ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%sNode(site_type:%i<%i)"
      (Remanent_parameters.get_prefix parameter)
      x.Mvbdu_sig.variable
      (x.Mvbdu_sig.upper_bound + 1)
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let parameter = Remanent_parameters.update_prefix parameter " " in
    let _ = print_mvbdu parameter x.Mvbdu_sig.branch_true in
    let _ = print_mvbdu parameter x.Mvbdu_sig.branch_false in
    ()

and print_mvbdu parameter mvbdu =
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%sId=%i" (Remanent_parameters.get_prefix parameter) mvbdu.Mvbdu_sig.id in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let parameter = Remanent_parameters.update_prefix parameter " " in
  let _ = print_cell parameter mvbdu.Mvbdu_sig.value in
  ()

and print_skeleton parameter skel =
  match skel with
  | Mvbdu_sig.Leaf x ->
    let s = "Leaf "^(if x then "True" else "False")^" \n" in
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s%s" (Remanent_parameters.get_prefix parameter) s in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    ()
  | Mvbdu_sig.Node x ->
    let () = Loggers.fprintf (Remanent_parameters.get_logger parameter) "%sNode(site_type:%i<%i,branch_true:%i,branch_false:%i)"
      (Remanent_parameters.get_prefix parameter)
      x.Mvbdu_sig.variable
      (x.Mvbdu_sig.upper_bound + 1)
      x.Mvbdu_sig.branch_true
      x.Mvbdu_sig.branch_false
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    ()

let init_data parameters error =
  let error,id = Hash_1.create parameters error 0 in
  let error,not = Hash_1.create parameters error 0 in
  let error,mvbdu_clean_head = Hash_1.create parameters error 0 in
  let error,mvbdu_keep_head_only = Hash_1.create parameters error 0 in
  let error,mvbdu_and = Hash_2.create parameters error (0,0) in
  let error,mvbdu_or = Hash_2.create parameters error (0,0) in
  let error,mvbdu_xor = Hash_2.create parameters error (0,0) in
  let error,mvbdu_nand = Hash_2.create parameters error (0,0) in
  let error,mvbdu_eq = Hash_2.create parameters error (0,0) in
  let error,mvbdu_nor = Hash_2.create parameters error (0,0) in
  let error,mvbdu_fst = Hash_2.create parameters error (0,0) in
  let error,mvbdu_snd = Hash_2.create parameters error (0,0) in
  let error,mvbdu_nfst = Hash_2.create parameters error (0,0) in
  let error,mvbdu_nsnd = Hash_2.create parameters error (0,0) in
  let error,mvbdu_imply = Hash_2.create parameters error (0,0) in
  let error,mvbdu_is_implied = Hash_2.create parameters error (0,0) in
  let error,mvbdu_nimply = Hash_2.create parameters error (0,0) in
  let error,mvbdu_nis_implied = Hash_2.create parameters error (0,0) in
  let error,mvbdu_redefine = Hash_2.create parameters error (0,0) in
  let error,mvbdu_project_keep_only = Hash_2.create parameters error (0,0) in
  let error,mvbdu_project_abstract_away = Hash_2.create parameters error (0,0) in
  let error,mvbdu_merge = Hash_2.create parameters error (0,0) in
  let error,mvbdu_overwrite = Hash_2.create parameters error (0,0) in
  let error,mvbdu_extensional_variables_list = Hash_1.create parameters error 0 in
  let error,mvbdu_extensional_association_list = Hash_1.create parameters error 0 in
  let error,mvbdu_variables_of = Hash_1.create parameters error 0 in
  let error,mvbdu_extensional_description_of_mvbdu = Hash_1.create parameters error 0 in
  let error,mvbdu_rename = Hash_2.create parameters error (0,0) in
  error,
  {
    boolean_mvbdu_clean_head = mvbdu_clean_head ;
    boolean_mvbdu_keep_head_only = mvbdu_keep_head_only ;
    boolean_mvbdu_identity = id ;
    boolean_mvbdu_not = not;
    boolean_mvbdu_and = mvbdu_and ;
    boolean_mvbdu_or = mvbdu_or ;
    boolean_mvbdu_xor = mvbdu_xor ;
    boolean_mvbdu_nand = mvbdu_nand ;
    boolean_mvbdu_equiv = mvbdu_eq;
    boolean_mvbdu_nor = mvbdu_nor ;
    boolean_mvbdu_fst = mvbdu_fst ;
    boolean_mvbdu_snd  = mvbdu_snd ;
    boolean_mvbdu_nfst = mvbdu_nfst;
    boolean_mvbdu_nsnd = mvbdu_nsnd;
    boolean_mvbdu_is_implied = mvbdu_is_implied;
    boolean_mvbdu_imply = mvbdu_imply;
    boolean_mvbdu_nis_implied = mvbdu_nis_implied;
    boolean_mvbdu_nimply = mvbdu_nimply;
    boolean_mvbdu_redefine = mvbdu_redefine;
    boolean_mvbdu_monotonicaly_rename = mvbdu_rename;
    boolean_mvbdu_project_keep_only = mvbdu_project_keep_only;
    boolean_mvbdu_project_abstract_away = mvbdu_project_abstract_away;
    boolean_mvbdu_merge_variables_lists = mvbdu_merge;
    boolean_mvbdu_overwrite_association_list = mvbdu_overwrite;
    boolean_mvbdu_extensional_description_of_variables_list = mvbdu_extensional_variables_list;
    boolean_mvbdu_extensional_description_of_association_list = mvbdu_extensional_association_list;
    boolean_mvbdu_variables_of_mvbdu = mvbdu_variables_of;
    boolean_mvbdu_extensional_description_of_mvbdu = mvbdu_extensional_description_of_mvbdu;
  }

let init_remanent parameters error =
  let error,data = init_data parameters error in
  error,{
    Memo_sig.data = data;
    Memo_sig.mvbdu_dictionary = D_mvbdu_skeleton.init ();
    Memo_sig.association_list_dictionary = D_Association_list_skeleton.init ();
    Memo_sig.variables_list_dictionary = D_Variables_list_skeleton.init ();
    Memo_sig.print_skel = print_skeleton ;
    Memo_sig.print_cell = print_cell ;
    Memo_sig.print_mvbdu = print_mvbdu
  }

let mvbdu_allocate =
  (fun parameters error b c d e
    (old_handler:('a,mvbdu_dic,association_list_dic,variables_list_dic,'c,'d) Memo_sig.handler) ->
      let old_dictionary = old_handler.Memo_sig.mvbdu_dictionary in
      let error,output =
        D_mvbdu_skeleton.allocate
          parameters
          error
          b
          c
          d
          e
          old_dictionary
      in
      match output with
      | None -> error,None
      | Some ((i:int), a, b, new_dic) ->
	let error =
	  if Remanent_parameters.get_trace parameters
	    && trace_mvbdu_allocation
	  then
	    let error,int =
	      D_mvbdu_skeleton.last_entry
		parameters
		error
		new_dic
	    in
	    if i=int
	    then
	      let () =
		Loggers.fprintf (Remanent_parameters.get_logger parameters) "LAST ENTRY: %i" int in
	      let () =
		Loggers.print_newline (Remanent_parameters.get_logger parameters)
	      in
	      error
	    else error
	  else error
	in
	let new_handler =
          Mvbdu_core.update_dictionary
            old_handler
            new_dic
        in
        error, (Some (i, a, b, new_handler)))

let build_memoize_unary f get_handler update_handler =
  Mvbdu_algebra.recursive_memoize
    f
    get_handler
    update_handler
    (fun parameters error handler mvbdu x ->
      let a,b =
        Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) x
      in
      a,(handler,b))
    (fun parameters error handler mvbdu ->
      Hash_1.set
        parameters
        error
        (Mvbdu_core.id_of_mvbdu mvbdu))

let build_memoize_binary f get_handler update_handler =
  Mvbdu_algebra.recursive_memoize
    f
    get_handler
    update_handler
    (fun parameters error handler (mvbdu_a,mvbdu_b) x ->
      let a,b =
        Hash_2.unsafe_get parameters error
          (Mvbdu_core.id_of_mvbdu mvbdu_a, Mvbdu_core.id_of_mvbdu mvbdu_b) x
      in
      a, (handler,b))
    (fun parameters error handler (mvbdu_a,mvbdu_b) ->
      Hash_2.set parameters error
        (Mvbdu_core.id_of_mvbdu mvbdu_a, Mvbdu_core.id_of_mvbdu mvbdu_b))

let memo_identity =
  Mvbdu_algebra.not_recursive_not_memoize_unary
    (fun error x -> error, x.Mvbdu_sig.value, Some x)
    (fun parameters error ->
      (fun bool -> error,
        (fun error -> Exception.warn parameters error (Some "Boolean_mvbdu")
          (Some "line 109") Exit (fun () -> Mvbdu_sig.Leaf bool))))
    mvbdu_allocate

let memo_not =
  (build_memoize_unary
     (fun parameters error x -> error, (fun error -> error, Mvbdu_sig.Leaf (not x)))
     (fun x -> x.Memo_sig.data.boolean_mvbdu_not)
     (fun x h ->
       {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_not = x}}))

let boolean_mvbdu_not parameters =
  Mvbdu_algebra.generic_unary
    (mvbdu_allocate parameters)
    memo_not

let memo_constant_true  =
  Mvbdu_algebra.not_recursive_not_memoize_unary
    (fun error _ -> error, Mvbdu_sig.Leaf true,None)
    (fun parameters error ->
      (fun bool -> error,
        (fun error -> Exception.warn parameters error (Some "Boolean_mvbdu")
          (Some "line 109") Exit (fun () -> Mvbdu_sig.Leaf true))))
    mvbdu_allocate

let memo_constant_false =
  Mvbdu_algebra.not_recursive_not_memoize_unary
    (fun error _ -> error, Mvbdu_sig.Leaf false,None)
    (fun parameters error ->
      (fun bool -> error,
        (fun error -> Exception.warn parameters error (Some "Boolean_mvbdu")
          (Some "line 109") Exit (fun () -> Mvbdu_sig.Leaf false))))
    mvbdu_allocate

let boolean_mvbdu_true parameters handler =
  Mvbdu_algebra.generic_zeroary
    (mvbdu_allocate parameters)
    handler
    (fun error -> error, Mvbdu_sig.Leaf true)

let boolean_mvbdu_false parameters handler =
  Mvbdu_algebra.generic_zeroary
    (mvbdu_allocate parameters)
    handler
    (fun error -> error, Mvbdu_sig.Leaf false)

let boolean_mvbdu_constant_true parameters =
  Mvbdu_algebra.generic_unary
    (mvbdu_allocate parameters)
    memo_constant_true

let boolean_mvbdu_constant_false parameters =
  Mvbdu_algebra.generic_unary
    (mvbdu_allocate parameters)
    memo_constant_false

let boolean_mvbdu_and parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,
                    if x then memo_identity else memo_constant_false)
         in
         (g,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_and)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_and = x}})
    )

let memo_or =
  build_memoize_binary
    (fun parameters error ->
      let g x = (error,
                 if x then memo_constant_true else memo_identity)
      in
      (g,g))
    (fun x -> x.Memo_sig.data.boolean_mvbdu_or)
    (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_or = x}})

let boolean_mvbdu_or parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    memo_or

let boolean_mvbdu_xor parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,
                    if x then memo_not else memo_identity)
         in
         (g,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_xor)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_xor = x}}))

let boolean_mvbdu_nand parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,
                    if x then memo_not else memo_constant_true)
         in
         (g,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_nand)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nand = x}}))

let boolean_mvbdu_equiv parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters )
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,
                    if x then memo_identity else memo_not)
         in (g,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_equiv)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_equiv = x}}))

let boolean_mvbdu_nor parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,
                    if x then memo_constant_false else memo_not)
         in
         (g,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_nor)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nor = x}}))

let boolean_mvbdu_imply parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,if x then memo_identity else memo_constant_true) in
         let h x = (error,if x then memo_constant_true else memo_not) in
         (g,h))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_imply)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_imply = x}}))

let boolean_mvbdu_is_implied parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,if x then memo_identity else memo_constant_true) in
         let h x = (error,if x then memo_constant_true else memo_not) in
         (h,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_is_implied)
       (fun x h ->
         {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_is_implied = x}}))

let boolean_mvbdu_nimply parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,if x then memo_not else memo_constant_false) in
         let h x = (error,if x then memo_constant_false else memo_identity) in
         (g,h))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_nimply)
       (fun x h ->
         {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nimply = x}}))

let boolean_mvbdu_nis_implied parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,if x then memo_not else memo_constant_false) in
         let h x = (error,if x then memo_constant_false else memo_identity) in
         (h,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_nis_implied)
       (fun x h ->
         {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nis_implied = x}}))

let boolean_constant_bi_true parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters )
    (Mvbdu_algebra.not_recursive_binary
       (fun error x y -> error,Mvbdu_sig.Leaf true, None)
       (fun parameters error ->
         let g (x:bool) =
           Exception.warn parameters error (Some "Boolean_mvbdu")
             (Some "line 361") Exit (fun () -> memo_identity)
         in
         (g,g))
       (mvbdu_allocate parameters))

let boolean_constant_bi_false parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (Mvbdu_algebra.not_recursive_binary
       (fun error x y -> error,Mvbdu_sig.Leaf false, None)
       (fun parameters error ->
         let g (x:bool) =
           Exception.warn parameters error (Some "Boolean_mvbdu")
             (Some "line 373") Exit (fun () -> memo_identity)
         in
         (g,g))
       (mvbdu_allocate parameters))

let boolean_mvbdu_fst parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (Mvbdu_algebra.not_recursive_binary
       (fun error x y -> error,x.Mvbdu_sig.value,Some x)
       (fun parameters error ->
         let g (x:bool) =
           Exception.warn parameters error (Some "Boolean_mvbdu")
             (Some "line 385") Exit (fun () -> memo_identity)
         in
         (g,g))
       (mvbdu_allocate parameters))

let boolean_mvbdu_snd parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (Mvbdu_algebra.not_recursive_binary
       (fun error x y -> error,x.Mvbdu_sig.value,Some y)
       (fun parameters error ->
         let g (x:bool) =
           Exception.warn parameters error (Some "Boolean_mvbdu")
             (Some "line 397") Exit (fun () -> memo_identity)
         in
         (g,g))
       (mvbdu_allocate parameters))

let boolean_mvbdu_nfst parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,if x then memo_constant_false else memo_constant_true) in
         let h x = (error,memo_not) in
         (g,h))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_nfst)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nfst = x}}))

let boolean_mvbdu_nsnd parameters =
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
       (fun parameters error ->
         let g x = (error,if x then memo_constant_false else memo_constant_true) in
         let h x = (error,memo_not) in
         (h,g))
       (fun x -> x.Memo_sig.data.boolean_mvbdu_nsnd)
       (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nsnd = x}}))

let gen_list_allocate allocate get_dic update parameters error b c d e (old_handler:('a,mvbdu_dic,association_list_dic,variables_list_dic,'c,'d) Memo_sig.handler) =
  let old_dictionary = get_dic old_handler in
  let error,output =
    allocate
      parameters
      error
      b
      c
      d
      e
      old_dictionary
  in
  match output with
  | None -> error, None
  | Some (i,a,b,new_dic) ->
    let new_handler =
      update
        old_handler
        new_dic
    in
    error, (Some (i, a, b, new_handler))


let association_list_allocate parameters error b c d e old_handler =
  gen_list_allocate D_Association_list_skeleton.allocate (fun x -> x.Memo_sig.association_list_dictionary)
    List_core.update_association_dictionary
    parameters error b c d e old_handler

let variables_list_allocate parameters error b c d e old_handler =
  gen_list_allocate D_Variables_list_skeleton.allocate (fun x -> x.Memo_sig.variables_list_dictionary)
    List_core.update_variables_dictionary
    parameters error b c d e old_handler

let memo_clean_head =
  Mvbdu_algebra.memoize_no_fun
    (fun x -> x.Memo_sig.data.boolean_mvbdu_clean_head)
    (fun x h ->
      {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_clean_head = x}})
    (fun parameters error handler mvbdu d ->
      let a,b = Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) d
      in a,(handler,b)
    )
    (fun parameters error h mvbdu ->
      Hash_1.set
        parameters
        error
        (Mvbdu_core.id_of_mvbdu mvbdu))

let clean_head parameters error handler =
  Mvbdu_algebra.clean_head
    (mvbdu_allocate parameters)
    memo_clean_head
    boolean_mvbdu_or
    handler
    error
    parameters

let memo_clean_head =
  Mvbdu_algebra.memoize_no_fun
    (fun x -> x.Memo_sig.data.boolean_mvbdu_clean_head)
    (fun x h ->
      {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_clean_head = x}})
    (fun parameters error handler mvbdu d ->
      match Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) d with
      | error,None ->
        clean_head parameters error handler mvbdu
      | error,Some x -> error,(handler,Some x)
    )
    (fun parameters error h mvbdu ->
      Hash_1.set
        parameters
        error
        (Mvbdu_core.id_of_mvbdu mvbdu))

let memo_keep_head_only =
  Mvbdu_algebra.memoize_no_fun
    (fun x -> x.Memo_sig.data.boolean_mvbdu_keep_head_only)
    (fun x h ->
      {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_keep_head_only = x}})
    (fun parameters error handler mvbdu d ->
      let a,b = Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) d
      in a,(handler,b)
    )
    (fun parameters error h mvbdu ->
      Hash_1.set
        parameters
        error
        (Mvbdu_core.id_of_mvbdu mvbdu))

let keep_head_only parameters error handler =
  Mvbdu_algebra.keep_head_only
    (mvbdu_allocate parameters)
    memo_keep_head_only
    boolean_mvbdu_true
    handler
    error
    parameters

let memo_keep_head_only =
  Mvbdu_algebra.memoize_no_fun
    (fun x -> x.Memo_sig.data.boolean_mvbdu_keep_head_only)
    (fun x h ->
      {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_keep_head_only = x}})
    (fun parameters error handler mvbdu d ->
      match Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) d with
      | error,None ->
        keep_head_only parameters error handler mvbdu
      | error,Some x -> error,(handler,Some x)
    )
    (fun parameters error h mvbdu ->
      Hash_1.set
        parameters
        error
        (Mvbdu_core.id_of_mvbdu mvbdu))


let reset_handler error =
  {
    Memo_sig.empty_association_list = error,memo_identity;
    Memo_sig.empty_variables_list = error,memo_identity;
    Memo_sig.leaf = (fun bool -> error,(fun error -> error, Mvbdu_sig.Leaf bool));
    Memo_sig.clean_head = error,memo_clean_head;
    Memo_sig.build_false =
      (fun var bound -> error,(fun error -> error, Mvbdu_sig.Leaf false));
    Memo_sig.build_true=
      (fun var bound mvbdu_false mvbdu_true ->
        error,
        (fun error ->
          error,
          if Mvbdu_core.mvbdu_equal mvbdu_true mvbdu_false
          then
            mvbdu_true.Mvbdu_sig.value
          else
            (Mvbdu_sig.Node
               {
                 Mvbdu_sig.variable = var ;
                 Mvbdu_sig.upper_bound = bound ;
                 Mvbdu_sig.branch_true = mvbdu_true ;
                 Mvbdu_sig.branch_false = mvbdu_false})
        ))
  }

let gen_bin_mvbdu_list f get set parameters error handler mvbdu_input list_input =
  let memoized_fun = Mvbdu_algebra.recursive_memoize
    (fun parameters -> reset_handler)
    get
    set
    (fun parameters error handler (mvbdu,list) d ->
      let a,b =
        Hash_2.unsafe_get
          parameters
          error
          (Mvbdu_core.id_of_mvbdu mvbdu, List_core.id_of_list list)
          d
      in
      a, (handler, b))
    (fun parameters error handler (mvbdu,list) ->
      Hash_2.set
        parameters
        error
        (Mvbdu_core.id_of_mvbdu mvbdu, List_core.id_of_list list))
  in
  f
    (mvbdu_allocate parameters)
    memoized_fun
    error
    handler
    mvbdu_input
    list_input

let redefine parameters error handler mvbdu_input list_input =
  gen_bin_mvbdu_list
    Mvbdu_algebra.redefine
    (fun x -> x.Memo_sig.data.boolean_mvbdu_redefine)
    (fun x h ->
      {
        h with Memo_sig.data = 
          {
            h.Memo_sig.data with boolean_mvbdu_redefine = x
          }
      })
    parameters error handler mvbdu_input list_input

let monotonicaly_rename parameters error handler mvbdu_input list_input =
  gen_bin_mvbdu_list
    Mvbdu_algebra.monotonicaly_rename
    (fun x -> x.Memo_sig.data.boolean_mvbdu_monotonicaly_rename)
    (fun x h ->
      {
        h with Memo_sig.data =
          {
            h.Memo_sig.data with boolean_mvbdu_monotonicaly_rename = x
          }
      })
    parameters error handler mvbdu_input list_input

let project_keep_only parameters error handler mvbdu_input list_input =
  gen_bin_mvbdu_list
    (fun a b -> Mvbdu_algebra.project_keep_only a b boolean_mvbdu_true)
    (fun x -> x.Memo_sig.data.boolean_mvbdu_project_keep_only)
    (fun x h ->
      {
        h with Memo_sig.data =
          {
            h.Memo_sig.data with boolean_mvbdu_project_keep_only = x
          }
      })
    parameters error handler mvbdu_input list_input

let project_abstract_away parameters error handler mvbdu_input list_input =
  gen_bin_mvbdu_list
    Mvbdu_algebra.project_abstract_away
    (fun x -> x.Memo_sig.data.boolean_mvbdu_project_abstract_away)
    (fun x h ->
      {
        h with Memo_sig.data = 
          {
            h.Memo_sig.data with boolean_mvbdu_project_abstract_away = x
          }
      })
    parameters error handler mvbdu_input list_input

let merge_variables_lists parameters error handler list1 list2 =
  List_algebra.overwrite
    (variables_list_allocate parameters)
    (fun parameter error handler (x1,x2) ->
      let error, output = 
        Hash_2.unsafe_get parameter error 
          (x1.List_sig.id, x2.List_sig.id)
          handler.Memo_sig.data.boolean_mvbdu_merge_variables_lists
      in
      error, (handler, output))
    (fun parameter error handler (x1,x2) output ->
      let error, memo = 
        Hash_2.set parameter error
          (x1.List_sig.id, x2.List_sig.id) 
          output 
          handler.Memo_sig.data.boolean_mvbdu_merge_variables_lists 
      in
      error,
      {
        handler with Memo_sig.data =
	  {
            handler.Memo_sig.data with boolean_mvbdu_merge_variables_lists = memo
          }
      })
    error parameters handler
    list1
    list2

let overwrite_association_lists parameters error handler list1 list2 =
  List_algebra.overwrite
    (association_list_allocate parameters)
    (fun parameter error handler (x1,x2) ->
      let error, output = 
        Hash_2.unsafe_get parameter error
          (x1.List_sig.id, x2.List_sig.id)
          handler.Memo_sig.data.boolean_mvbdu_overwrite_association_list 
      in
      error, (handler, output))
    (fun parameter error handler (x1,x2) output ->
      let error, memo = 
        Hash_2.set parameter error
          (x1.List_sig.id, x2.List_sig.id)
          output 
          handler.Memo_sig.data.boolean_mvbdu_overwrite_association_list 
      in
      error,
      {
        handler with Memo_sig.data =
	  {
            handler.Memo_sig.data with 
              boolean_mvbdu_overwrite_association_list = memo
          }
      })
    error parameters handler
    list1
    list2

let extensional_description_of_variables_list parameters error handler list =
  List_algebra.extensional_without_asso
    (fun parameter error handler x ->
      let error, output = 
        Hash_1.unsafe_get parameter error
          x.List_sig.id 
          handler.Memo_sig.data.boolean_mvbdu_extensional_description_of_variables_list 
      in
      error, (handler, output))
    (fun parameter error handler x output ->
      let error, memo =
        Hash_1.set parameter error
          x.List_sig.id 
          output 
          handler.Memo_sig.data.boolean_mvbdu_extensional_description_of_variables_list 
      in
      error,
      {
        handler with Memo_sig.data =
	  {
            handler.Memo_sig.data with
              boolean_mvbdu_extensional_description_of_variables_list = memo
          }
      })
    error handler list

let extensional_description_of_association_list parameters error handler list =
  List_algebra.extensional_with_asso
    (fun parameter error handler x ->
      let error, output =
        Hash_1.unsafe_get parameter error 
          x.List_sig.id
          handler.Memo_sig.data.boolean_mvbdu_extensional_description_of_association_list
      in
      error, (handler, output))
    (fun parameter error handler x output ->
      let error, memo = 
        Hash_1.set parameter error
          x.List_sig.id
          output
          handler.Memo_sig.data.boolean_mvbdu_extensional_description_of_association_list 
      in
      error,
      {
        handler with Memo_sig.data =
	  {
            handler.Memo_sig.data with 
              boolean_mvbdu_extensional_description_of_association_list = memo
          }
      })
    error handler list

let rec variables_of_mvbdu parameters error handler mvbdu =
  match 
    Hash_1.unsafe_get parameters error 
      mvbdu.Mvbdu_sig.id
      handler.Memo_sig.data.boolean_mvbdu_variables_of_mvbdu
  with
  | error, Some output -> error, (handler, Some output)
  | error, None ->
    begin
      let error, (handler, output) =
	match mvbdu.Mvbdu_sig.value with
	| Mvbdu_sig.Leaf _ ->
	  let error, (handler, list) =
            List_algebra.build_reversed_sorted_list
              (variables_list_allocate parameters) parameters error handler 
              []
	  in
          error, (handler, Some list)
	| Mvbdu_sig.Node a ->
	  let error, (handler, list_false) =
            variables_of_mvbdu parameters error handler
              a.Mvbdu_sig.branch_false 
          in
	  let error, (handler, list_true) = 
            variables_of_mvbdu parameters error handler 
              a.Mvbdu_sig.branch_true
          in
	  let error, (handler, singleton) =
            List_algebra.build_reversed_sorted_list 
              (variables_list_allocate parameters) parameters error handler 
              [a.Mvbdu_sig.variable, ()] 
          in
	  begin
	    match list_false, list_true with
	    | Some list_f, Some list_t ->
	      begin
		let error, (handler, list_sibblings) =
                  merge_variables_lists parameters error handler 
                    list_f 
                    list_t 
                in
		let error, (handler,output) =
		  match list_sibblings with
		  | Some list_s -> 
                    merge_variables_lists parameters error handler 
                      singleton 
                      list_s
		  | None -> 
                    Exception.warn parameters error (Some "Boolean_mvbdu") 
                      (Some "line 854") Exit (fun () -> handler,None)
		in
		error, (handler, output)
	      end
	    | None,_ | _,None ->
	      Exception.warn parameters error (Some "Boolean_mvbdu")
		(Some "line 863") Exit (fun () -> handler,None)
	  end
      in
      match output with
      | Some output ->
	let error, memo = 
          Hash_1.set parameters error
            mvbdu.Mvbdu_sig.id
            output
            handler.Memo_sig.data.boolean_mvbdu_variables_of_mvbdu 
        in
	error,
	({
          handler with Memo_sig.data =
	    {
              handler.Memo_sig.data with boolean_mvbdu_variables_of_mvbdu = memo
            }
        }, Some output)
      | None ->
	Exception.warn parameters error (Some "Boolean_mvbdu") 
          (Some "line 874") Exit (fun () -> (handler, None))
    end

let mvbdu_cartesian_decomposition_depth 
    variables_list_of_mvbdu extensional_of_variables_list 
    build_sorted_variables_list
    mvbdu_project_keep_only
    mvbdu_project_abstract_away 
    mvbdu_and 
    equal
    parameters handler error bdu int =
  let rec aux_k k handler error bdu_to_decompose list =
    if k > int
    then
      error, handler, (Some bdu_to_decompose,list)
    else
      let error, handler, l =
        variables_list_of_mvbdu parameters handler error bdu_to_decompose 
      in
      let error, handler, list_var =
        extensional_of_variables_list parameters handler error l 
      in
      let n_var = List.length list_var in
      if k > n_var / 2
      then
	error, handler, (Some bdu_to_decompose,list)
      else
	let parts = Tools_kasa.sorted_parts_of_list k list_var in
	let rec aux n_var list_of_parts handler error
            bdu_to_decompose 
            list_of_decomposed_bdu 
            decomposed_var
            =
	  if k > n_var / 2
	  then
	    error, handler, None, bdu_to_decompose :: list_of_decomposed_bdu
	  else
	    match list_of_parts with
	    | [] -> error, handler, Some bdu_to_decompose, list_of_decomposed_bdu
	    | h :: t ->
	      if
		List.exists (fun x -> Mods.IntSet.mem x decomposed_var) h
	      then
		aux n_var t handler error
                  bdu_to_decompose 
                  list_of_decomposed_bdu 
                  decomposed_var
	      else
		let error, handler, list =
                  build_sorted_variables_list parameters handler error h
                in
		let error, handler, restriction =
                  mvbdu_project_keep_only parameters handler error 
                    bdu_to_decompose 
                    list 
                in
		let error, handler, abstract_away =
                  mvbdu_project_abstract_away parameters handler error 
                    bdu_to_decompose
                    list 
                in
		let error, handler, cartesian_abstraction =
                  mvbdu_and parameters handler error 
                    restriction
                    abstract_away 
                in
		if equal cartesian_abstraction bdu_to_decompose
		then
		  let decomposed_var =
                    List.fold_left (fun set a -> Mods.IntSet.add a set) decomposed_var h
                  in
		  aux (n_var - k) t handler error
                    abstract_away
                    (restriction :: list_of_decomposed_bdu) 
                    decomposed_var
		else
		  aux n_var t handler error
                    bdu_to_decompose
                    list_of_decomposed_bdu decomposed_var
	in
	let error, handler, bdu_opt, list = 
          aux
	    n_var
	    parts
	    handler error bdu_to_decompose
	    list
	    Mods.IntSet.empty 
        in
	match bdu_opt with
	| None -> error, handler, (None, list)
	| Some bdu -> aux_k (k+1) handler error bdu list
  in
  let error, handler, (bdu_opt, list) = 
    aux_k 1 handler error bdu [] 
  in
  error, handler, (bdu_opt, List.rev list)

let rec extensional_description_of_mvbdu parameters handler error mvbdu =
  match 
    Hash_1.unsafe_get parameters error
      mvbdu.Mvbdu_sig.id
      handler.Memo_sig.data.boolean_mvbdu_extensional_description_of_mvbdu
  with
  | error, Some output -> error, (handler, output)
  | error, None ->
    begin
      let rec aux mvbdu remanent handler error output =
	match mvbdu.Mvbdu_sig.value with
	| Mvbdu_sig.Leaf true -> error, (handler, [] :: output)
	| Mvbdu_sig.Leaf false -> error, (handler, output)
	| Mvbdu_sig.Node a ->
	  let error, (handler, branch_true) =
            extensional_description_of_mvbdu parameters handler error
              a.Mvbdu_sig.branch_true 
          in
	  let upper_bound = a.Mvbdu_sig.upper_bound in
	  let error, (handler, output) =
	    match remanent, branch_true with
	    | _, [] -> error, (handler, output)
	    | None, _ ->
	      Exception.warn parameters error (Some "Boolean_mvbdu")
		(Some "line 947") Exit (fun () -> handler,[])
	    | Some (var, lower_bound), list ->
	      let head_list =
		let rec aux k res =
		  if k <= lower_bound then res else aux (k-1) (k::res)
		in aux upper_bound []
	      in
	      let output =
		List.fold_left
		  (fun output head ->
		    List.fold_left
		      (fun output tail ->
			((var, head) :: tail) :: output)
		      output list)
		  output head_list
	      in
	      error, (handler, output)
	  in
	  aux
	    a.Mvbdu_sig.branch_false
	    (Some (a.Mvbdu_sig.variable,upper_bound))
	    handler error
	    output
      in
      let error,(handler,output) = aux mvbdu None handler error [] in
      let error, memo = 
        Hash_1.set parameters error
          mvbdu.Mvbdu_sig.id
          output
          handler.Memo_sig.data.boolean_mvbdu_extensional_description_of_mvbdu 
      in
      error,
      ({
        handler with Memo_sig.data =
	  { 
            handler.Memo_sig.data with 
              boolean_mvbdu_extensional_description_of_mvbdu = memo
          }
      }, output)
    end

let print_boolean_mvbdu (error:Exception.method_handler) =
  Mvbdu_core.print_mvbdu error
    (fun error parameters a ->
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)  "%s %s"
          parameters.Remanent_parameters_sig.marshalisable_parameters.Remanent_parameters_sig.prefix
          (if a then "true" else "false")
      in
      let _ =
	Loggers.print_newline (Remanent_parameters.get_logger parameters) in
      error)
    (fun i -> "x" ^ (string_of_int i))

let (f:Exception.method_handler ->
     Remanent_parameters_sig.parameters ->
     bool Mvbdu_sig.mvbdu -> Exception.method_handler) = print_boolean_mvbdu

let print_hash1 error log  =
  Hash_1.print error print_boolean_mvbdu log

let print_hash2 error log =
  Hash_2.print error print_boolean_mvbdu log

let lift f a b c =
  let () = f b c
  in a

let print_hash3 error log =
  Hash_1.print error (lift List_algebra.print_variables_list) log

let print_hash4 error log =
  Hash_2.print error (lift List_algebra.print_variables_list) log

let print_hash5 error log =
  Hash_2.print error
    (lift List_algebra.print_association_list)
    log

let print_hash6 error log =
  Hash_1.print error
    (fun a b c ->
      let log = Remanent_parameters.get_logger b in
      let prefix = b.Remanent_parameters_sig.marshalisable_parameters.Remanent_parameters_sig.prefix in
      let () = Loggers.fprintf log "%s" prefix in
      let () = List.iter (Loggers.fprintf log "%i;") c in
      let () = Loggers.print_newline log in a)
    log

let print_hash7 error log =
  Hash_1.print error (fun a b c ->
    let log = Remanent_parameters.get_logger b in
    let prefix = b.Remanent_parameters_sig.marshalisable_parameters.Remanent_parameters_sig.prefix in
    let () = Loggers.fprintf log "%s" prefix in
    let () = List.iter (fun (a,b) -> Loggers.fprintf log "%i,%i;" a b) c in
    let () = Loggers.print_newline log in a)
    log

let print_hash8 error log =
  Hash_1.print error
    (fun a b c ->
      let log = Remanent_parameters.get_logger b in
      let prefix = b.Remanent_parameters_sig.marshalisable_parameters.Remanent_parameters_sig.prefix in
      let () = Loggers.fprintf log "%s" prefix in
      let () =
	List.iter
	  (fun x ->
	    let () =
	      List.iter
		(fun (a,b) -> Loggers.fprintf log "%i,%i;" a b)
		x
	    in
	    Loggers.fprintf log "\n")
	  c
      in
      let () = Loggers.fprintf log "\n" in a)
    log

let print_gen log parameters error (title,print_hash,l) =
  let () = Printf.fprintf log "%s:\n" title in
  List.fold_left
    (fun error (pref,x) ->
      print_hash error (Remanent_parameters.update_prefix parameters pref) x)
    error l

let print_memo (error:Exception.method_handler) handler parameters =
  let error,l1,l2,l3,l4,l5,l6,l7,l8 = split_memo error handler in
  let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" parameters.Remanent_parameters_sig.marshalisable_parameters.Remanent_parameters_sig.prefix in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let error = print_gen stdout parameters error ("Print Hash_1",print_hash1,l1) in
  let error = print_gen stdout parameters error ("Print Hash_2",print_hash2,l2) in
  let error = print_gen stdout parameters error ("Print Hash_3",print_hash3,l3) in
  let error = print_gen stdout parameters error ("Print Hash_4",print_hash4,l4) in
  let error = print_gen stdout parameters error ("Print Hash_5",print_hash5,l5) in
  let error = print_gen stdout parameters error ("Print Hash_6",print_hash6,l6) in
  let error = print_gen stdout parameters error ("Print Hash_7",print_hash7,l7) in
  let error = print_gen stdout parameters error ("Print Hash_8",print_hash8,l8) in
  error

let last_entry parameter handler error =
  Mvbdu_core.last_entry parameter
    handler error
    D_mvbdu_skeleton.last_entry
