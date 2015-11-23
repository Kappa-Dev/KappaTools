module type Mvbdu =
  sig
    type handler 
    type mvbdu
    type list
     type 'output constant = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> Exception.method_handler * handler * 'output
    type ('input,'output) unary =  Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'output) binary = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input1 -> 'input2 -> Exception.method_handler * handler * 'output
  
    val init: Remanent_parameters_sig.parameters -> Exception.method_handler -> Exception.method_handler * handler 
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
    val mvbdu_redefine: (mvbdu,list,mvbdu) binary 
  end


module type Internalize_mvbdu =
  sig
    type handler 
    type mvbdu
    type list
    val equal: mvbdu -> mvbdu -> bool 
    val mvbdu_false: mvbdu
    val mvbdu_true:  mvbdu 
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
    val mvbdu_redefine:  mvbdu -> mvbdu -> mvbdu
  end

module Mvbdu = 
  (struct 
    type handler = (Boolean_mvbdu.memo_tables,Boolean_mvbdu.mvbdu_dic,Boolean_mvbdu.list_dic,bool,int) Memo_sig.handler  
    type mvbdu = bool Mvbdu_sig.mvbdu
    type list = int List_sig.list
    type 'output constant = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> Exception.method_handler * handler * 'output
    type ('input,'output) unary =  Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'output) binary = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input1 -> 'input2 -> Exception.method_handler * handler * 'output

    let init = 
      let used = ref None in 
      let closure parameter error = 
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
      in closure

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
    let (mvbdu_not: (mvbdu,mvbdu) unary) = lift1 "line 80, bdd_not" Boolean_mvbdu.boolean_mvbdu_not

    let mvbdu_id parameters handler error a = error, handler, a

    let mvbdu_unary_true parameters handler error _ = 
      mvbdu_true parameters handler error 
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
  end: Mvbdu)

module Internalize(M:Mvbdu) = 
  (struct 
    module Mvbdu = M 
    type mvbdu = bool Mvbdu_sig.mvbdu
    type list = int List_sig.list

    let handler = ref None 
    let parameter = ref (Remanent_parameters.get_parameters ())

  
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
	  | Some h -> error,h
	end
      | Some h -> error,h
    let lift_const s f = 
      let error = Exception.empty_error_handler in 
      let error',handler = get_handler s error in 
      let error',handler,mvbdu = f !parameter handler error' in 
      let _ = check s error error' handler in 
      mvbdu 
    let mvbdu_true = lift_const "line 217, mvbdu_true" M.mvbdu_true 
   end:Internalize_mvbdu)

let main () = () 
let _ = main () 
