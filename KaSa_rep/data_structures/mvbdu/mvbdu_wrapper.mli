module type Mvbdu =
  sig
    type handler 
    type mvbdu
    type list
    type 'output constant = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> Exception.method_handler * handler * 'output
    type ('input,'output) unary =  Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input -> Exception.method_handler * handler * 'output
    type ('input1,'input2,'output) binary = Remanent_parameters_sig.parameters -> handler ->   Exception.method_handler -> 'input1 -> 'input2 -> Exception.method_handler * handler * 'output

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
    val mvbdu_redefine: (mvbdu,list,mvbdu) binary 
  end
  
module type Internalized_mvbdu =
  sig
    type mvbdu
    type list
    val init: Remanent_parameters_sig.parameters -> unit 
    val is_init: unit -> bool 
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
    val mvbdu_redefine:  mvbdu -> list -> mvbdu
  end

module Mvbdu:Mvbdu
module IntMvbdu:Internalized_mvbdu
module Optimized_Mvbdu:Mvbdu
module Optimized_IntMvbdu:Internalized_mvbdu


