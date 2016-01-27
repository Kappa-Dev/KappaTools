

module type Mvbdu =
  sig
    type handler = (Boolean_mvbdu.memo_tables,Boolean_mvbdu.mvbdu_dic,Boolean_mvbdu.association_list_dic,Boolean_mvbdu.variables_list_dic,bool,int) Memo_sig.handler
    type mvbdu
    type hconsed_association_list
    type hconsed_variables_list
    type 'output constant = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> Exception.method_handler * handler * 'output
    type ('input,'output) unary =  Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'output) binary = Remanent_parameters_sig.parameters -> handler ->  Exception.method_handler -> 'input1 -> 'input2 -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'input3,'output) ternary = Remanent_parameters_sig.parameters -> handler -> Exception.method_handler -> 'input1 -> 'input2 -> 'input3 -> Exception.method_handler * handler * 'output

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
    val build_reverse_sorted_association_list: ((int * int) list, hconsed_association_list) unary
    val empty_association_list : hconsed_association_list constant
    val build_variables_list: (int list,hconsed_variables_list) unary
    val build_sorted_variables_list: (int list,hconsed_variables_list) unary
    val build_reverse_sorted_variables_list: (int list,hconsed_variables_list) unary
    val empty_variables_list: hconsed_variables_list constant

    val overwrite_association_lists: (hconsed_association_list,hconsed_association_list,hconsed_association_list) binary
    val merge_variables_lists: (hconsed_variables_list,hconsed_variables_list,hconsed_variables_list) binary

    val extensional_of_variables_list: (hconsed_variables_list,int list) unary
    val extensional_of_association_list: (hconsed_association_list,(int*int) list) unary
    val extensional_of_mvbdu: (mvbdu, (int * int) list list) unary


    val variables_list_of_mvbdu: (mvbdu,hconsed_variables_list) unary

    val print: Remanent_parameters_sig.parameters -> mvbdu -> unit
    val print_association_list: Remanent_parameters_sig.parameters -> hconsed_association_list -> unit
    val print_variables_list: Remanent_parameters_sig.parameters -> hconsed_variables_list -> unit

 (*get set default join parameters handler error hash_consed_object data storage =*)
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
    val mvbdu_redefine: mvbdu -> hconsed_association_list -> mvbdu
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
    val build_variables_list: int list -> hconsed_variables_list
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

module Make (M:Nul)  =
  (struct
    type handler = (Boolean_mvbdu.memo_tables,Boolean_mvbdu.mvbdu_dic,Boolean_mvbdu.association_list_dic,Boolean_mvbdu.variables_list_dic,bool,int) Memo_sig.handler
    type mvbdu = bool Mvbdu_sig.mvbdu
    type hconsed_association_list = int List_sig.list
    type hconsed_variables_list = unit List_sig.list
    type 'output constant = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> Exception.method_handler * handler * 'output
    type ('input,'output) unary =  Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'output) binary = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input1 -> 'input2 -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'input3,'output) ternary = Remanent_parameters_sig.parameters -> handler -> Exception.method_handler -> 'input1 -> 'input2 -> 'input3 -> Exception.method_handler * handler * 'output

    let init,is_init =
      let used = ref None in
      let init parameter error =
	match
	  !used
	with
	| Some a ->
	  Exception.warn parameter error (Some "Mvbdu_wrapper.ml") (Some "MVBDU should be initialised once only")  Exit (fun _ -> a)
    	| None ->
	  begin
	    let error,handler = Boolean_mvbdu.init_remanent parameter error in
	    let () = used := Some handler in
	    error,handler
	  end
      in
      let is_init () = !used != None
      in
      init,is_init

    let equal = Mvbdu_core.mvbdu_equal
    let equal_with_logs p h e a b = e,h,equal a b
    let lift0 string f parameters handler error =
      match
	 f parameters handler error parameters
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun _ -> failwith "Cannot recover from bugs in constant initilization") in
        error, handler, a

    let last_entry parameters handler error () =
      let error,int = Boolean_mvbdu.last_entry parameters handler error in
      error,handler,int

    let mvbdu_true = lift0 "line 55, bdd_true" Boolean_mvbdu.boolean_mvbdu_true
    let mvbdu_false = lift0 "line 56, bdd_false" Boolean_mvbdu.boolean_mvbdu_false

    let lift1 string f parameters handler error a =
      match
	 f parameters handler error parameters a
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift1four string f parameters handler error a =
       match
	 f parameters error handler a
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift1bis string f parameters handler error a =
      let a,(b,c) =
	 f (Boolean_mvbdu.association_list_allocate parameters) error parameters handler a
      in a,b,c

    let lift1ter string f parameters handler error a =
      let a,(b,c) =
	 f (Boolean_mvbdu.association_list_allocate parameters) parameters error handler a
      in a,b,c

    let liftvbis string f parameters handler error a =
      let a,(b,c) =
	 f (Boolean_mvbdu.variables_list_allocate parameters) error parameters handler (List.rev_map (fun x -> (x,())) a)
      in a,b,c

    let liftvter string f parameters handler error a =
      let a,(b,c) =
	 f (Boolean_mvbdu.variables_list_allocate parameters) parameters error handler (List.rev_map (fun x -> (x,())) a)
      in a,b,c

    let lift1_ string f parameters handler error a =
      match
	 f parameters handler error a
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift1__ string f parameters handler error a =
      match
	 f parameters handler error a
      with
      | error,(handler,a) -> error,handler,a


    let lift1four buildlist string f parameters handler error a =
      match
	f parameters error handler a
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
         let error,handler,list =
	   buildlist parameters handler error []
	 in
	 let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> list)
        in
	error, handler, (a:unit List_sig.list)

    let lift1five string f parameters handler error a =
      match
	f parameters error parameters handler a
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->

	 let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> [])
        in
	error, handler, a
    let lift2 string f parameters handler error a b =
      match
	f parameters handler error parameters a b
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift2bis string f parameters handler error a b =
      match
	f parameters error  parameters handler a b
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift2ter string f parameters handler error a b =
      match
	f parameters error  parameters handler a b
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift2four string f parameters handler error a b =
      match
	f parameters error handler a b
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let lift2five string f parameters handler error a b =
      match
	f parameters error handler a b
      with
      | error,(handler,Some a) -> error,handler,a
      | error,(handler,None) ->
        let error, a =
          Exception.warn parameters error (Some "Mvbdu_wrapper.ml") (Some string)  Exit (fun () -> a)
        in
	error, handler, a

    let (mvbdu_not: (mvbdu,mvbdu) unary) = lift1 "line 80, bdd_not" Boolean_mvbdu.boolean_mvbdu_not

    let mvbdu_id parameters handler error a = error, handler, a

    let mvbdu_unary_true parameters handler error _ =  mvbdu_true parameters handler error
    let mvbdu_unary_false parameters handler error _ = mvbdu_false parameters handler error

    let mvbdu_and = lift2 "line_86, bdd_and" Boolean_mvbdu.boolean_mvbdu_and
    let mvbdu_or = lift2 "line 87, bdd_or" Boolean_mvbdu.boolean_mvbdu_or
    let mvbdu_xor = lift2 "line 88, bdd_false" Boolean_mvbdu.boolean_mvbdu_xor
    let mvbdu_nand = lift2 "line 89, bdd_nand" Boolean_mvbdu.boolean_mvbdu_nand
    let mvbdu_nor =  lift2 "line 90, bdd_nor" Boolean_mvbdu.boolean_mvbdu_nor
    let mvbdu_imply =  lift2 "line 91, bdd_imply" Boolean_mvbdu.boolean_mvbdu_imply
    let mvbdu_rev_imply =  lift2 "line 92, bdd_imply" Boolean_mvbdu.boolean_mvbdu_is_implied
    let mvbdu_equiv =  lift2 "line 93, bdd_equiv" Boolean_mvbdu.boolean_mvbdu_equiv
    let mvbdu_nimply = lift2 "line 94, bdd_nimply" Boolean_mvbdu.boolean_mvbdu_nimply
    let mvbdu_nrev_imply = lift2 "line 95, bdd_nrev_imply" Boolean_mvbdu.boolean_mvbdu_nis_implied
    let mvbdu_bi_true = lift2 "line 96, bdd_bi_true" Boolean_mvbdu.boolean_constant_bi_true
    let mvbdu_bi_false = lift2 "line 97, bdd_bi_false" Boolean_mvbdu.boolean_constant_bi_false
    let mvbdu_fst parameters handler error a b = error,handler,a
    let mvbdu_snd parameters handler error a b = error,handler,b
    let mvbdu_nfst = lift2 "line 100, bdd_nfst" Boolean_mvbdu.boolean_mvbdu_nfst
    let mvbdu_nsnd = lift2 "line 101, bdd_nsnd" Boolean_mvbdu.boolean_mvbdu_nsnd
    let mvbdu_redefine = lift2bis "line 102, bdd_redefine" Boolean_mvbdu.redefine
    let mvbdu_rename = lift2bis "line 389, bdd rename" Boolean_mvbdu.monotonicaly_rename
    let mvbdu_project_keep_only = lift2ter "line 246, bdd_project_keep_only" Boolean_mvbdu.project_keep_only
    let mvbdu_project_abstract_away = lift2ter "line 247, bdd_project_abstract_away" Boolean_mvbdu.project_abstract_away

    let build_association_list = lift1bis "line 181, build_list" List_algebra.build_list

    let build_sorted_association_list = lift1ter "line 181, build_list" List_algebra.build_sorted_list

    let build_reverse_sorted_association_list = lift1ter "line 181, build_list" List_algebra.build_reversed_sorted_list

    let empty_association_list parameter handler error = build_association_list parameter handler error []

    let build_variables_list 	=
      liftvbis "line 257, build_list"
	List_algebra.build_list

    let build_sorted_variables_list = liftvter "line 259, build_list" List_algebra.build_reversed_sorted_list

    let build_reverse_sorted_variables_list = liftvter "line 261, build_list" List_algebra.build_sorted_list

    let empty_variables_list parameter handler error = build_variables_list parameter handler error []

    let variables_list_of_mvbdu parameter handler error mvbdu =
      lift1four
	build_sorted_variables_list
	"line 331, variables_list_of"
	Boolean_mvbdu.variables_of_mvbdu
	parameter handler error mvbdu

    let extensional_of_association_list parameters handler error l =
      lift1five "line 347"
		Boolean_mvbdu.extensional_description_of_association_list parameters handler error l
    let extensional_of_variables_list parameters handler error l =
      lift1five "line 361"
		Boolean_mvbdu.extensional_description_of_variables_list parameters handler error l

    let extensional_of_mvbdu parameters handler error mvbdu =
      lift1__ "line 383"
	Boolean_mvbdu.extensional_description_of_mvbdu parameters handler error mvbdu

    let print = Boolean_mvbdu.print_mvbdu
    let print_association_list = List_algebra.print_association_list
    let print_variables_list = List_algebra.print_variables_list

    let mvbdu_clean_head = lift1_ "line 216, bdd_clean_head" Boolean_mvbdu.clean_head
    let mvbdu_keep_head_only = lift1_ "line 217, bdd_keep_head_only" Boolean_mvbdu.keep_head_only

    let mvbdu_cartesian_abstraction parameters handler error bdu =
      let error,handler,bdd_true = mvbdu_true parameters handler error in
      let error,handler,bdd_false = mvbdu_false parameters handler error in
      let rec aux error handler bdu list =
	if equal bdu bdd_true || equal bdu bdd_false
	then error,handler,List.rev list
	else
	  let error,handler,head = mvbdu_keep_head_only parameters error handler  bdu in
	  let error,handler,tail = mvbdu_clean_head parameters error handler bdu in
	  aux error handler tail (head::list)
      in aux error handler bdu []

    let mvbdu_cartesian_decomposition_depth parameters handler error bdu int =
      Boolean_mvbdu.mvbdu_cartesian_decomposition_depth variables_list_of_mvbdu extensional_of_variables_list build_sorted_variables_list mvbdu_project_keep_only mvbdu_project_abstract_away mvbdu_and equal parameters handler error bdu int

    let mvbdu_full_cartesian_decomposition parameters handler error bdu =
      let error,handler,l = variables_list_of_mvbdu parameters handler error bdu in
      let error,handler,list = extensional_of_variables_list parameters handler error l in
      let size = List.length list in
      let error,handler,(bdu_opt,list) = mvbdu_cartesian_decomposition_depth parameters handler error bdu (size/2) in
      match
	bdu_opt
      with
      | None -> error,handler,list
      | Some bdu -> error,handler,bdu::list

    let merge_variables_lists parameters handler error l1 l2 =
      lift2four "line 332" Boolean_mvbdu.merge_variables_lists parameters handler error l1 l2

    let overwrite_association_lists parameters handler error l1 l2 =
      lift2five "line 335" Boolean_mvbdu.overwrite_association_lists parameters handler error l1 l2

    let store_by_gen get_id get set default join parameters handler error hash_consed_object data storage  =
      let id = get_id hash_consed_object in
      let error, old_data = get parameters error default id storage in
      let error, handler, data = join parameters handler error old_data data in
      let error, storage = set parameters error id data storage in
      error, handler, storage

    let store_by_variables_list get set default join parameters handler error hash_consed_object data storage =
      store_by_gen List_core.id_of_list
	get
	set
	default
	join
	parameters
	handler
	error
	hash_consed_object
	data
	storage

    let store_by_mvbdu get set default join parameters handler error hash_consed_object data storage =
      store_by_gen Mvbdu_core.id_of_mvbdu
	get
	set
	default
	join
	parameters
	handler
	error
	hash_consed_object
	data
	storage

  end: Mvbdu)

module Internalize(M:Mvbdu) =
  (struct
    module Mvbdu = M
    type mvbdu = Mvbdu.mvbdu
    type hconsed_association_list = Mvbdu.hconsed_association_list
    type hconsed_variables_list = Mvbdu.hconsed_variables_list
    type handler = Mvbdu.handler
    let handler = ref None
    let parameter = ref (Remanent_parameters.get_parameters ~called_from:Remanent_parameters_sig.Internalised ())


    let check s error error' handler' =
      let () = handler:= Some handler' in
      if error'== error
      then
	()
      else
	let error',() =
	 Exception.warn !parameter error (Some "Mvbdu_wrapper.ml") (Some s)  Exit (fun () -> ())
	in
	Exception.print !parameter error'

    let init parameters =
      let error = Exception.empty_error_handler in
      let error',output = M.init parameters error in
      let () = parameter := parameters in
      check "line 194, init" error error' output

    let is_init () = None != !handler
    let equal = M.equal
    let get_handler s error =
      match
	!handler
      with
      | None ->
	let () = init !parameter in
	let error',() = Exception.warn !parameter error (Some "Mvbdu_wrapper.ml") (Some (s^" uninitialised mvbdu"))  Exit (fun () -> ())  in
	begin
	  match !handler with
	  | None -> failwith "unrecoverable errors in bdu get_handler"
	  | Some h -> error',h
	end
      | Some h -> error,h
    let lift_const s f =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' in
      let _ = check s error error' handler in
      mvbdu
    let mvbdu_true ()  = lift_const "line 218, mvbdu_true" M.mvbdu_true
    let mvbdu_false () = lift_const "line 219, mvbdu_false" M.mvbdu_false

    let lift_unary s f x =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' x in
      let _ = check s error error' handler in
      mvbdu


	
    let mvbdu_id = lift_unary "line 228, mvbdu_id" M.mvbdu_id
    let mvbdu_not = lift_unary "line 229, mvbdu_not" M.mvbdu_not

    let mvbdu_unary_true _ = mvbdu_true ()
    let mvbdu_unary_false _ = mvbdu_false ()
    let mvbdu_bi_true _ _ = mvbdu_true ()
    let mvbdu_bi_false _ _ = mvbdu_false ()
				
    let lift_binary s f x y =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' x y in
      let _ = check s error error' handler in
      mvbdu
    let lift_binary' s f x y =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' x y in
      let _ = check s error error' handler in
      mvbdu
    let lift_binary'' s f x y =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' x y in
      let _ = check s error error' handler in
      mvbdu

    let lift_binary''' s f x y =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' x y in
      let _ = check s error error' handler in
      mvbdu

    let lift_binary'''' s f x y =
      let error = Exception.empty_error_handler in
      let error',handler = get_handler s error in
      let error',handler,mvbdu = f !parameter handler error' x y in
      let _ = check s error error' handler in
      mvbdu

    let mvbdu_and = lift_binary "line 243, mvbdu_and" M.mvbdu_and
    let mvbdu_or  = lift_binary "line 244, mvbdu_or" M.mvbdu_or
    let mvbdu_nand = lift_binary "line 245, mvbdu_nand" M.mvbdu_nand
    let mvbdu_nor = lift_binary "line 246, mvbdu_nor" M.mvbdu_nor
    let mvbdu_snd _ b = b			
    let mvbdu_nsnd _ b = mvbdu_not b				
    let mvbdu_fst a _ = a
    let mvbdu_nfst a _ = mvbdu_not a
    let mvbdu_xor = lift_binary "line 251, mvbdu_xor" M.mvbdu_xor
    let mvbdu_nor = lift_binary "line 252, mvbdu_nor" M.mvbdu_nor
    let mvbdu_imply = lift_binary "line 253, mvbdu_imply" M.mvbdu_imply
    let mvbdu_nimply = lift_binary "line 254, mvbdu_nimply" M.mvbdu_nimply
    let mvbdu_rev_imply = lift_binary "line 255, mvbdu_imply" M.mvbdu_rev_imply
    let mvbdu_nrev_imply = lift_binary "line 256, mvbdu_nrev_imply" M.mvbdu_nrev_imply
    let mvbdu_equiv = lift_binary "line 256, mvbdu_nrev_imply" M.mvbdu_equiv
    let mvbdu_redefine = lift_binary "line 258, mvbdu_redefine" M.mvbdu_redefine
    let mvbdu_rename = lift_binary "line 624, mvbdu_rename" Mvbdu.mvbdu_rename
    let mvbdu_project_keep_only = lift_binary "line 380, mvbdu_project_keep_only" M.mvbdu_project_keep_only
    let mvbdu_project_abstract_away = lift_binary "line 381, mvbdu_project_abstract_away" M.mvbdu_project_abstract_away

    let build_association_list = lift_unary "line 297" M.build_association_list
    let build_sorted_association_list = lift_unary "line 298" M.build_sorted_association_list
    let build_reverse_sorted_association_list = lift_unary "line 299" M.build_reverse_sorted_association_list
    let empty_association_list () = build_association_list []

    let build_variables_list = lift_unary "line 297" M.build_variables_list
    let build_sorted_variables_list = lift_unary "line 298" M.build_sorted_variables_list
    let build_reverse_sorted_variables_list = lift_unary "line 299" M.build_reverse_sorted_variables_list
    let empty_variables_list () = build_variables_list []

    let merge_variables_lists l1 l2 =
      lift_binary''' "line 472"
	M.merge_variables_lists
	l1 l2

    let overwrite_association_lists l1 l2 =
      lift_binary'''' "line 475" M.overwrite_association_lists l1 l2

    let variables_list_of_mvbdu l =
      lift_unary "line 541" M.variables_list_of_mvbdu l
		
    let mvbdu_cartesian_abstraction = lift_unary "line 349" M.mvbdu_cartesian_abstraction

    let extensional_of_association_list l =
      lift_unary "line 509" M.extensional_of_association_list l
    let extensional_of_variables_list l =
      lift_unary "line 511" M.extensional_of_variables_list l

    let extensional_of_mvbdu mvbdu =
      lift_unary "line 576" M.extensional_of_mvbdu mvbdu

    let mvbdu_full_cartesian_decomposition = lift_unary "line 569" M.mvbdu_full_cartesian_decomposition
    let mvbdu_cartesian_decomposition_depth = lift_binary "line 570" M.mvbdu_cartesian_decomposition_depth
		
    let print = M.print
    let print_association_list = M.print_association_list
    let print_variables_list = M.print_variables_list
   end:Internalized_mvbdu)

module Optimize(M:Mvbdu) =
	 (struct
	     module Mvbdu = M
	     type handler = Mvbdu.handler
	     type mvbdu = Mvbdu.mvbdu
	     type hconsed_association_list = Mvbdu.hconsed_association_list
	     type hconsed_variables_list = Mvbdu.hconsed_variables_list
	     type 'output constant = 'output Mvbdu.constant
	     type ('input,'output) unary =  ('input,'output) Mvbdu.unary
	     type ('input1,'input2,'output) binary = ('input1,'input2,'output) Mvbdu.binary
	     type ('input1,'input2,'input3,'output) ternary = ('input1,'input2,'input3,'output) Mvbdu.ternary


	     let last_entry = Mvbdu.last_entry
	     let init = Mvbdu.init
	     let is_init = Mvbdu.is_init
	     let equal = Mvbdu.equal
	     let equal_with_logs = Mvbdu.equal_with_logs
	     let mvbdu_nand  = Mvbdu.mvbdu_nand
	     let mvbdu_not parameters handler error a = mvbdu_nand parameters handler error a a

	     let mvbdu_id = Mvbdu.mvbdu_id
	     let mvbdu_true = Mvbdu.mvbdu_true
	     let mvbdu_false = Mvbdu.mvbdu_false
	     let mvbdu_unary_true parameters handler error a =
	       let error,handler,nota = mvbdu_not parameters handler error a in
	       mvbdu_nand parameters handler error a nota
	     let mvbdu_unary_false parameters handler error a =
	       let error,handler,mvtrue = mvbdu_unary_true parameters handler error a in
	       mvbdu_not parameters handler error mvtrue
	     let mvbdu_and parameters handler error a b =
	       let error,handler,ab = mvbdu_nand parameters handler error a b in
	       mvbdu_not parameters handler error ab
	     let mvbdu_or parameters handler error a b =
	       let error,handler,na = mvbdu_not parameters handler error a in
	       let error,handler,nb = mvbdu_not parameters handler error b in
	       mvbdu_nand parameters handler error na nb
	     let mvbdu_imply parameters handler error a b =
	       let error,handler,notb = mvbdu_not parameters handler error b in
	       mvbdu_nand parameters handler error a notb
	     let mvbdu_rev_imply parameters handler error a b =
	       let error,handler,nota = mvbdu_not parameters handler error a in
	       mvbdu_nand parameters handler error nota b
	     let mvbdu_nor parameters handler error a b =
	       let error,handler,bddor = mvbdu_or parameters handler error a b in
	       mvbdu_not parameters handler error bddor
	     let mvbdu_equiv parameters handler error a b =
	       let error,handler,direct = mvbdu_imply parameters handler error a b in
	       let error,handler,indirect = mvbdu_imply parameters handler error b a in
	       mvbdu_and parameters handler error direct indirect
	     let mvbdu_xor parameters handler error a b =
	       let error,handler,equiv = mvbdu_equiv parameters handler error a b in
	       mvbdu_not parameters handler error equiv
	     let mvbdu_nimply parameters handler error a b =
	       let error,handler,imply = mvbdu_imply parameters handler error a b in
	       mvbdu_not parameters handler error imply
	     let mvbdu_nrev_imply parameters handler error a b = mvbdu_nimply parameters handler error b a
	     let mvbdu_bi_true parameters handler error a _ = M.mvbdu_unary_true parameters handler error a
	     let mvbdu_bi_false parameters handler error a _ = M.mvbdu_unary_false parameters handler error a
	     let mvbdu_fst = M.mvbdu_fst
	     let mvbdu_snd = M.mvbdu_snd
	     let mvbdu_nfst parameters handler error a _ = mvbdu_not parameters handler error a
	     let mvbdu_nsnd parameters handler error _ a = mvbdu_not parameters handler error a

	     let mvbdu_cartesian_abstraction = M.mvbdu_cartesian_abstraction
	     let mvbdu_redefine = M.mvbdu_redefine
	     let mvbdu_rename = M.mvbdu_rename
	     let mvbdu_project_keep_only = M.mvbdu_project_keep_only
	     let mvbdu_project_abstract_away = M.mvbdu_project_abstract_away
	     let build_association_list = M.build_association_list
	     let build_sorted_association_list = M.build_sorted_association_list
	     let build_reverse_sorted_association_list = M.build_reverse_sorted_association_list
             let build_variables_list = M.build_variables_list
	     let build_sorted_variables_list = M.build_sorted_variables_list
	     let build_reverse_sorted_variables_list = M.build_reverse_sorted_variables_list

	     let empty_association_list = M.empty_association_list
	     let empty_variables_list = M.empty_variables_list

	     let merge_variables_lists = M.merge_variables_lists
	     let overwrite_association_lists = M.overwrite_association_lists

	     let extensional_of_association_list = M.extensional_of_association_list
	     let extensional_of_variables_list = M.extensional_of_variables_list
	     let extensional_of_mvbdu = M.extensional_of_mvbdu
	     let variables_list_of_mvbdu = M.variables_list_of_mvbdu

	     let mvbdu_cartesian_decomposition_depth parameters handler error bdu int =
	       Boolean_mvbdu.mvbdu_cartesian_decomposition_depth variables_list_of_mvbdu extensional_of_variables_list build_sorted_variables_list mvbdu_project_keep_only mvbdu_project_abstract_away mvbdu_and equal parameters handler error bdu int

	     let mvbdu_full_cartesian_decomposition parameters handler error bdu =
	       let error,handler,l = variables_list_of_mvbdu parameters handler error bdu in
	       let error,handler,list = extensional_of_variables_list parameters handler error l in
	       let size = List.length list in
	       let error,handler,(bdu_opt,list) = mvbdu_cartesian_decomposition_depth parameters handler error bdu (size/2) in
	       match
		 bdu_opt
	       with
	       | None -> error,handler,list
	       | Some bdu -> error,handler,bdu::list

	     let print = M.print
	     let print_association_list = M.print_association_list
	     let print_variables_list = M.print_variables_list

	     let store_by_variables_list = M.store_by_variables_list
	     let store_by_mvbdu = M.store_by_mvbdu
	   end:Mvbdu)

module Optimize'(M:Internalized_mvbdu) =
	 (struct
	     module Mvbdu = M
			
	     type mvbdu = Mvbdu.mvbdu
	     type hconsed_association_list = Mvbdu.hconsed_association_list
	     type hconsed_variables_list = Mvbdu.hconsed_variables_list

	     let init = Mvbdu.init
	     let is_init = Mvbdu.is_init
	     let equal = Mvbdu.equal
	     let mvbdu_nand a = Mvbdu.mvbdu_nand a
	     let mvbdu_not a = mvbdu_nand  a a
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
	     let build_reverse_sorted_association_list = M.build_reverse_sorted_association_list
	     let mvbdu_redefine = M.mvbdu_redefine
	     let mvbdu_rename = M.mvbdu_rename
	     let mvbdu_project_keep_only = M.mvbdu_project_keep_only
	     let mvbdu_project_abstract_away = M.mvbdu_project_abstract_away
	     let build_variables_list = M.build_variables_list
	     let build_sorted_variables_list = M.build_sorted_variables_list
	     let build_reverse_sorted_variables_list = M.build_reverse_sorted_variables_list
	     let empty_variables_list = M.empty_variables_list
             let empty_association_list = M.empty_association_list
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

	     let mvbdu_cartesian_decomposition_depth = M.mvbdu_cartesian_decomposition_depth
							 	
	     let mvbdu_full_cartesian_decomposition = M.mvbdu_full_cartesian_decomposition

	   end:Internalized_mvbdu)

module Vd = struct end
module Mvbdu = Make(Vd)
module IntMvbdu = Internalize(Make (Vd))
module Optimized_Mvbdu = Optimize(Make(Vd))
module Optimized_IntMvbdu = Internalize(Optimize(Make (Vd)))


