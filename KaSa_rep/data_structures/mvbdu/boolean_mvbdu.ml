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


let sanity_check = true
let test_workbench = false 
    
let invalid_arg parameters mh message exn value = 
     Exception.warn parameters mh (Some "Mvbdu_bool") message exn (fun () -> value)
  
module Mvbdu_Skeleton = 
struct
type t = bool Mvbdu_sig.skeleton
let (compare:t->t -> int) = compare 
end 
    
module List_Skeleton = 
struct 
type t = int List_sig.skeleton
let (compare:t->t->int) = compare 
end 
  
  
module Hash_key = 
struct 
type t = int 
let compare = compare 
end 
  
  
                                                               
module D_mvbdu_skeleton = (Dictionary.Dictionary_of_Ord(Mvbdu_Skeleton): Dictionary.Dictionary with type value = bool Mvbdu_sig.skeleton)
module D_list_skeleton = (Dictionary.Dictionary_of_Ord(List_Skeleton): Dictionary.Dictionary with type value = int List_sig.skeleton) 
                                                                        
module Hash_1 = Int_storage.Nearly_inf_Imperatif
module Hash_2 = Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif
  
type memo_unary = bool Mvbdu_sig.mvbdu Hash_1.t  

  
type memo_tables = 
  {
    boolean_mvbdu_identity : bool Mvbdu_sig.mvbdu Hash_1.t ;
    boolean_mvbdu_not      : bool Mvbdu_sig.mvbdu Hash_1.t ;     
    boolean_mvbdu_and      : bool Mvbdu_sig.mvbdu Hash_2.t ;
    boolean_mvbdu_or      : bool Mvbdu_sig.mvbdu Hash_2.t ;
    boolean_mvbdu_xor      : bool Mvbdu_sig.mvbdu Hash_2.t ;
    boolean_mvbdu_nand      : bool Mvbdu_sig.mvbdu Hash_2.t ; 
    boolean_mvbdu_equiv : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_is_implied : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_imply : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nis_implied : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nimply : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nor : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_fst : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nfst : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_snd : bool Mvbdu_sig.mvbdu Hash_2.t;
    boolean_mvbdu_nsnd : bool Mvbdu_sig.mvbdu Hash_2.t ;
    boolean_mvbdu_clean_head : bool Mvbdu_sig.mvbdu Hash_1.t;
    boolean_mvbdu_redefine: bool Mvbdu_sig.mvbdu Hash_2.t;  
  }

type mvbdu_dic = (bool Mvbdu_sig.cell,bool Mvbdu_sig.mvbdu) D_mvbdu_skeleton.dictionary
type list_dic = (int List_sig.cell,int List_sig.list) D_list_skeleton.dictionary
type handler =  (memo_tables,mvbdu_dic,list_dic,bool,int) Memo_sig.handler  
  
type unary_memoized_fun = 
  (bool,
   mvbdu_dic,
   list_dic,
   Exception.method_handler -> bool -> Exception.method_handler  * (bool Mvbdu_sig.mvbdu,bool) Mvbdu_sig.premvbdu,memo_tables,
   memo_tables,
   int) 
      Memo_sig.memoized_fun

let split_memo error handler =
  let x = handler.Memo_sig.data in 
  error,
  [ "id",x.boolean_mvbdu_identity;
    "not",x.boolean_mvbdu_not     ;
    "clean_head",x.boolean_mvbdu_clean_head ;
  ],
  [ "and",x.boolean_mvbdu_and ;
    "or",x.boolean_mvbdu_or ;
    "xor",x.boolean_mvbdu_xor ;
    "nand",x.boolean_mvbdu_nand ; 
    "<=>",x.boolean_mvbdu_equiv ;
    "<=",x.boolean_mvbdu_is_implied ;
    "=>",x.boolean_mvbdu_imply ;
    "not <=",x.boolean_mvbdu_nis_implied ;
    "not =>",x.boolean_mvbdu_nimply ;
    "not",x.boolean_mvbdu_nor ;
    "fst",x.boolean_mvbdu_fst ;
    "not fst",x.boolean_mvbdu_nfst ;
    "snd",x.boolean_mvbdu_snd ;
    "not snd",x.boolean_mvbdu_nsnd ; 
    "reset",x.boolean_mvbdu_redefine]
  
let rec print_cell log prefix cell = 
  match cell 
  with 
    | 
      Mvbdu_sig.Leaf x -> 
       let s = "Leaf "^(if x then "True" else "False")^" \n" in 
       let _ = Printf.fprintf log "%s%s\n" prefix s in 
       ()
    | Mvbdu_sig.Node x -> 
       let _ = Printf.fprintf log "%sNode(%i<%i)\n" prefix x.Mvbdu_sig.variable x.Mvbdu_sig.upper_bound in
       let prefix' = prefix^" " in 
       let _ = print_mvbdu log prefix' x.Mvbdu_sig.branch_true in 
       let _ = print_mvbdu log prefix' x.Mvbdu_sig.branch_false in
      () 
and print_mvbdu log prefix mvbdu = 
  let _ = Printf.fprintf log "%sId=%i\n" prefix mvbdu.Mvbdu_sig.id in 
  let _ = print_cell log (prefix^" ") mvbdu.Mvbdu_sig.value in 
  ()
and print_skeleton log prefix skel = 
  match skel
  with   
    | 
      Mvbdu_sig.Leaf x -> 
       let s = "Leaf "^(if x then "True" else "False")^" \n" in 
       let _ = Printf.fprintf log "%s%s\n" prefix s in 
       ()
    | Mvbdu_sig.Node x -> 
       let _ = Printf.fprintf log "%sNode(%i<%i,%i,%i)\n" prefix x.Mvbdu_sig.variable x.Mvbdu_sig.upper_bound x.Mvbdu_sig.branch_true x.Mvbdu_sig.branch_false in
       () 
           
let init_data parameters error =
  let error,id = Hash_1.create parameters error 0 in 
  let error,not = Hash_1.create parameters error 0 in 
  let error,mvbdu_clean_head = Hash_1.create parameters error 0 in 
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
  error,
    {
      boolean_mvbdu_clean_head = mvbdu_clean_head ;
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
    }
    
let init_remanent parameters error =
  let error,data = init_data parameters error in 
  error,{
    Memo_sig.data = data;
    Memo_sig.mvbdu_dictionary = D_mvbdu_skeleton.init ();
    Memo_sig.list_dictionary = D_list_skeleton.init ();
    Memo_sig.print_skel = print_skeleton ;
    Memo_sig.print_cell = print_cell ;
    Memo_sig.print_mvbdu = print_mvbdu
        } 

let mvbdu_allocate = 
   (fun parameters error b c d e (old_handler:('a,mvbdu_dic,list_dic,'c,'d) Memo_sig.handler) -> 
     let old_dictionary = old_handler.Memo_sig.mvbdu_dictionary in 
     let error,output = D_mvbdu_skeleton.allocate parameters error b c d e old_dictionary in  
     match output with 
    | None -> error,None 
    | Some ((i:int),a,b,new_dic) -> 
      let new_handler = Mvbdu_core.update_dictionary old_handler new_dic in 
                            error,(Some (i,a,b,new_handler)))
      
      
                        
  
let build_memoize_unary f get_handler update_handler = 
  Mvbdu_algebra.recursive_memoize 
    f 
    get_handler 
    update_handler 
    (fun parameters error handler mvbdu x -> let a,b = Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) x in a,(handler,b))
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
        let a,b = Hash_2.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu_a,Mvbdu_core.id_of_mvbdu mvbdu_b) x in 
        a,(handler,b))
    (fun parameters error handler (mvbdu_a,mvbdu_b) -> 
      Hash_2.set parameters error (Mvbdu_core.id_of_mvbdu mvbdu_a,Mvbdu_core.id_of_mvbdu mvbdu_b)) 
  
let memo_identity = 
  Mvbdu_algebra.not_recursive_not_memoize_unary 
    (fun error x -> error,x.Mvbdu_sig.value,Some x)
    (fun parameters error -> (fun bool -> error, (fun error -> Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 109") Exit (fun () -> Mvbdu_sig.Leaf bool))))
    mvbdu_allocate


let memo_not = 
  (build_memoize_unary 
        (fun parameters error x -> error,(fun error -> error,Mvbdu_sig.Leaf (not x))) 
        (fun x -> x.Memo_sig.data.boolean_mvbdu_not) 
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_not = x}}))
  
let boolean_mvbdu_not parameters = 
  Mvbdu_algebra.generic_unary 
    (mvbdu_allocate parameters) 
    memo_not  
  
let memo_constant_true  = 
  Mvbdu_algebra.not_recursive_not_memoize_unary 
      (fun error _ -> error,Mvbdu_sig.Leaf true,None)
      (fun parameters error -> (fun bool -> error, (fun error -> Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 109") Exit (fun () -> Mvbdu_sig.Leaf true))))
      mvbdu_allocate  
                                                                                                                       
    
let memo_constant_false = 
  Mvbdu_algebra.not_recursive_not_memoize_unary  
      (fun error _ -> error,Mvbdu_sig.Leaf false,None)
      (fun parameters error -> (fun bool -> error, (fun error -> Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 109") Exit (fun () -> Mvbdu_sig.Leaf false))))
      mvbdu_allocate 
  

let boolean_mvbdu_true parameters handler = 
  Mvbdu_algebra.generic_zeroary 
    (mvbdu_allocate parameters)
    handler 
    (fun error -> error,Mvbdu_sig.Leaf true) 
  
let boolean_mvbdu_false parameters handler = 
  Mvbdu_algebra.generic_zeroary
    (mvbdu_allocate parameters)
    handler 
    (fun error -> error,Mvbdu_sig.Leaf false) 
  
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
          let g x = (error,if x then memo_identity else memo_constant_false)
          in 
           (g,g))
        (fun x -> x.Memo_sig.data.boolean_mvbdu_and)
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_and = x}})
    )  
    
 let memo_or = 
   build_memoize_binary
        (fun parameters error -> 
          let g x = (error,if x then memo_constant_true else memo_identity)
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
          let g x = (error,if x then memo_not else memo_identity)
          in 
           (g,g))
        (fun x -> x.Memo_sig.data.boolean_mvbdu_xor)
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_xor = x}}))
  
let boolean_mvbdu_nand parameters = 
   Mvbdu_algebra.generic_binary 
     (mvbdu_allocate parameters) 
      (build_memoize_binary
        (fun parameters error ->
          let g x = (error,if x then memo_not else memo_constant_true)
          in 
           (g,g))
        (fun x -> x.Memo_sig.data.boolean_mvbdu_nand)
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nand = x}}))
  
let boolean_mvbdu_equiv parameters = 
  Mvbdu_algebra.generic_binary 
    (mvbdu_allocate parameters )
    (build_memoize_binary
        (fun parameters error ->
            let g x = (error,if x then memo_identity else memo_not)
            in (g,g))
        (fun x -> x.Memo_sig.data.boolean_mvbdu_equiv)
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_equiv = x}}))
  

let boolean_mvbdu_nor parameters = 
   Mvbdu_algebra.generic_binary 
     (mvbdu_allocate parameters)
      (build_memoize_binary
        (fun parameters error ->
          let g x = (error,if x then memo_constant_false else memo_not)
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
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_is_implied = x}}))  
  
  let boolean_mvbdu_nimply parameters = 
  Mvbdu_algebra.generic_binary
    (mvbdu_allocate parameters)
    (build_memoize_binary
        (fun parameters error ->
          let g x = (error,if x then memo_not else memo_constant_false) in 
          let h x = (error,if x then memo_constant_false else memo_identity) in 
           (g,h))
        (fun x -> x.Memo_sig.data.boolean_mvbdu_nimply)
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nimply = x}}))  

 
  
 let boolean_mvbdu_nis_implied parameters = 
   Mvbdu_algebra.generic_binary 
     (mvbdu_allocate parameters)
      (build_memoize_binary
        (fun parameters error ->
          let g x = (error,if x then memo_not else memo_constant_false) in 
          let h x = (error,if x then memo_constant_false else memo_identity) in 
           (h,g))
        (fun x -> x.Memo_sig.data.boolean_mvbdu_nis_implied)
        (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_nis_implied = x}}))  
  
  let boolean_constant_bi_true parameters = 
    Mvbdu_algebra.generic_binary 
      (mvbdu_allocate parameters )
      (Mvbdu_algebra.not_recursive_binary
          (fun error x y -> error,Mvbdu_sig.Leaf true, None)
          (fun parameters error -> 
          let g (x:bool) =  
             Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 361") Exit (fun () -> memo_identity) 
          in 
            (g,g))
        (mvbdu_allocate parameters))
    
  let boolean_constant_bi_false parameters = 
    Mvbdu_algebra.generic_binary 
      (mvbdu_allocate parameters)
      (Mvbdu_algebra.not_recursive_binary
          (fun error x y -> error,Mvbdu_sig.Leaf true, None)
          (fun parameters error -> 
          let g (x:bool) =  
             Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 373") Exit (fun () -> memo_identity) 
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
             Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 385") Exit (fun () -> memo_identity) 
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
             Exception.warn parameters error (Some "Boolean_mvbdu") (Some "line 397") Exit (fun () -> memo_identity) 
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
    
 let list_allocate parameters = 
   (fun error b c d e (old_handler:('a,mvbdu_dic,list_dic,'c,'d) Memo_sig.handler) -> 
     let old_dictionary = old_handler.Memo_sig.list_dictionary in 
     let error,output = D_list_skeleton.allocate parameters error b c d e old_dictionary in  
     match output with 
    | None -> error,None 
    | Some ((i:int),a,b,new_dic) -> 
      let new_handler = List_core.update_dictionary old_handler new_dic in 
                            error,(Some (i,a,b,new_handler)))
       
let memo_clean_head = 
  Mvbdu_algebra.memoize_no_fun 
          (fun x -> x.Memo_sig.data.boolean_mvbdu_clean_head)
          (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_clean_head = x}})  
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
          (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_clean_head = x}})  
          (fun parameters error handler mvbdu d -> 
              match Hash_1.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu) d
              with 
               | error,None -> 
                   clean_head parameters error handler mvbdu
               | error,Some x -> error,(handler,Some x) 
                         )
          (fun parameters error h mvbdu -> 
            Hash_1.set 
              parameters 
              error 
              (Mvbdu_core.id_of_mvbdu mvbdu))
  
let reset_handler error = 
   {
    Memo_sig.empty_list = error,memo_identity;
    Memo_sig.leaf= (fun  bool -> error,(fun error -> error,Mvbdu_sig.Leaf bool));
    Memo_sig.clean_head=  error,memo_clean_head;
    Memo_sig.build_false= (fun var bound -> error,(fun error -> error,Mvbdu_sig.Leaf false));
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

let redefine parameters error handler mvbdu_input list_input = 
    let memoized_fun =   Mvbdu_algebra.recursive_memoize
          (fun parameters -> reset_handler) 
          (fun x -> x.Memo_sig.data.boolean_mvbdu_redefine)
          (fun x h -> {h with Memo_sig.data = {h.Memo_sig.data with boolean_mvbdu_redefine = x}})  
          (fun parameters error handler (mvbdu,list) d -> 
            let a,b = Hash_2.unsafe_get parameters error (Mvbdu_core.id_of_mvbdu mvbdu,List_core.id_of_list list) d in 
            a,(handler,b))
          (fun parameters error handler (mvbdu,list) -> 
            Hash_2.set 
              parameters 
              error 
              (Mvbdu_core.id_of_mvbdu mvbdu,List_core.id_of_list list)) 
  in
  Mvbdu_algebra.redefine (mvbdu_allocate parameters) memoized_fun error handler mvbdu_input list_input 

let print_boolean_mvbdu (error:Exception.method_handler) = 
  Mvbdu_core.print_mvbdu error  
  (fun error parameters a -> 
    let _ = Printf.fprintf parameters.Remanent_parameters_sig.log "%s %s\n" parameters.Remanent_parameters_sig.prefix (if a then "true" else "false") in error) 
  (fun i -> "x"^(string_of_int i)) 
  
let print_hash1 error parameters  =  
  Hash_1.print error print_boolean_mvbdu parameters  

let print_hash2 error log = 
  Hash_2.print error print_boolean_mvbdu log 
  
let print_memo (error:Exception.method_handler) handler parameters = 
  let error,l1,l2 = split_memo error handler in 
  let _ = Printf.fprintf parameters.Remanent_parameters_sig.log "%s\n" parameters.Remanent_parameters_sig.prefix in 
  let error = 
      List.fold_left
        (fun error (pref,x) -> print_hash1 error (Remanent_parameters.update_prefix parameters pref) x)
        error 
        l1
  in 
  let error = 
    List.fold_left 
            (fun error (pref,x) -> print_hash2 error (Remanent_parameters.update_prefix parameters pref) x)
            error l2
  in error