(**
   * list_sanity.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * 
   * Creation: 08/03/2010
   * Last modification: 15/11/2010
   * * 
   * This library provides primitives to check consistency of accociation lists
   *  
   * Copyright 2010 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)


let invalid_arg parameters mh message exn value = 
  Exception.warn parameters mh (Some "List_sanity") message exn (fun () -> value)
    
let rec safety_equal_list list_x list_y  = 
  match list_x.List_sig.value, list_y.List_sig.value with 
    | List_sig.Empty, List_sig.Empty -> true 
    | List_sig.Cons x, List_sig.Cons y -> 
      x.List_sig.variable = y.List_sig.variable 
      && x.List_sig.association = y.List_sig.association
        && safety_equal_list x.List_sig.tail y.List_sig.tail 
    | _ -> false 


let rec safety_check_maximal_sharing (allocate_uniquely:('a,'b,'c,'d) Sanity_test_sig.g)
    error list handler = 
  let list_val = list.List_sig.value in  
  match list_val with 
    | List_sig.Empty -> error,true,handler
    | List_sig.Cons x -> 
      (** check that list is uniquely represented in memory *)
      let error,output = allocate_uniquely 
        error
        compare 
        (List_core.get_skeleton list_val) 
        list_val 
        (fun key -> {List_sig.id = key; List_sig.value = list_val})
        handler 
      in 
      match output with 
        | None -> error,false,handler 
        | Some (i,asso,asso_id,handler) -> 
          begin
            match list_val with 
              | List_sig.Empty -> error,true,handler 
              | List_sig.Cons x -> 
                safety_check_maximal_sharing
                  allocate_uniquely
                  error
                  x.List_sig.tail
                  handler               
          end              

let rec safety_check_increasing_nodes_aux error list var = 
  match list.List_sig.value with 
    | List_sig.Empty -> error,true
    | List_sig.Cons x ->
      let new_var = x.List_sig.variable in 
      begin
        match compare var new_var 
        with 
          | a when a<0 -> 
            safety_check_increasing_nodes_aux
              error
              x.List_sig.tail
              new_var 
          | a when a>0 -> error,false
          | _  -> error,false 
      end
        
let safety_check_increasing_nodes error list = 
  match list.List_sig.value with 
    | List_sig.Empty -> error,true
    | List_sig.Cons x ->
      safety_check_increasing_nodes_aux
        error
        x.List_sig.tail
        x.List_sig.variable  
      
let print_flag log bool = 
  if bool 
  then Printf.fprintf log "Yes" 
  else Printf.fprintf log "No"

let sanity_check (allocate_uniquely:('a,'b,'c,'d) Sanity_test_sig.g) error log handler mvbdu =
  let error,bool1 =
    safety_check_increasing_nodes
      error
      mvbdu
  in
  let error,bool2,dictionary =
    safety_check_maximal_sharing
      allocate_uniquely
      error
      mvbdu
      handler
  in 
  error, dictionary, (bool1,bool2) 
    
let add_string m1 m2 = 
  if m1 = "" then m2 
  else if m2 = "" then m1 
  else m1^" / "^m2 

let m = "Error during Hashed list sanity check!"    
let m1true_instead_of_false =
  "List_sig.Nodes were not decreasing, which was not detected"   
let m1false_instead_of_true =
  "List_sig.Nodes are detected to be not increasing, although they are"  
let m2true_instead_of_false =
  "Representation in memory is not unique, but it was not detected" 
let m2false_instead_of_true =
  "Representation in memory is detected to be non unique, although it is"  
  
let test handler (b1,b2) bdu = 
  let error, mvbdu_handler,(c1,c2) =
    sanity_check
      handler.Sanity_test_sig.allocate_uniquely_association_list
      handler.Sanity_test_sig.error
      handler.Sanity_test_sig.output
      handler.Sanity_test_sig.mvbdu_handler
      bdu
  in  
  let handler =
    {handler with
      Sanity_test_sig.error = error;
      Sanity_test_sig.mvbdu_handler=mvbdu_handler}
  in 
  if c1 = b1 && c2 = b2 
  then 
    handler,true,None
  else 
    handler,
    false, 
    Some 
      (add_string 
         begin
           if c1 
           then (if not b1 then m1true_instead_of_false else "")
           else (if b1 then m1false_instead_of_true else "")   
         end  
         begin
           if c2 
           then (if not b2 then m2true_instead_of_false else "")
           else (if b2 then m2false_instead_of_true else "") 
         end  
      )
