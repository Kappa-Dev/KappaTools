(**
    * mvbdu_algebra.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2010, the 8th or March
    * Last modification: 2011, the 23rd of March
    * * 
    * This library provides primitives to deal set of finite maps from integers to integers
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)


  
let invalid_arg parameters mh message exn value = 
     Exception.warn parameters mh (Some "Mvbdu_algebra") message exn (fun () -> value)


let downgrade parameters mh message value mvbdu = 
  match mvbdu with 
    | Some x -> mh,x
    | None -> invalid_arg parameters mh message Exit (value ()) 
    
let rec generic_zeroary allocate handler f error parameters  = 
   let error,cell = f error in 
   let error,output = Mvbdu_core.build_already_compressed_cell allocate error handler (Mvbdu_core.get_skeleton cell) cell  in 
   match output 
   with 
    | None -> invalid_arg parameters error (Some "line 28") Exit (handler,None)                   
    | Some (key,cell,mvbdu,handler) ->
        error,(handler,Some mvbdu)
          

  
let rec generic_unary allocate (memoized_fun:('a,'b,'c,'d,'e,'f) Memo_sig.unary_memoized_fun) handler   error parameters mvbdu_input = 
  match memoized_fun.Memo_sig.get parameters error handler mvbdu_input with 
    | error,(handler,Some output) -> 
          error,(handler,Some output)
    | error,(handler,None) -> 
       begin
         let error,(handler,output) = 
           match mvbdu_input.Mvbdu_sig.value with
            | Mvbdu_sig.Leaf a ->
               let error,depreciated_fun = memoized_fun.Memo_sig.f parameters error a in
               generic_zeroary allocate handler depreciated_fun error  parameters  
            | Mvbdu_sig.Node x -> 
               begin 
                 match generic_unary allocate memoized_fun handler error parameters x.Mvbdu_sig.branch_true 
                 with
                   | error,(handler,None) -> 
                       error,(handler,None)
                   | error,(handler,Some(mvbdu_true)) -> 
                       begin
                         match generic_unary allocate memoized_fun handler error parameters x.Mvbdu_sig.branch_false 
                         with 
                           | error,(handler,None) -> 
                               error,(handler,None)
                           | error,(handler,Some(mvbdu_false)) -> 
                              begin 
                                match Mvbdu_core.compress_node allocate error handler (Mvbdu_sig.Node {x with Mvbdu_sig.branch_true = mvbdu_true ; Mvbdu_sig.branch_false = mvbdu_false}) 
                                with 
                                 | error,None ->     
                                     error,(handler,None)
                                 | error,Some(id,cell,mvbdu,handler) -> 
                                     error,(handler,Some(mvbdu))
                              end 
                     end                                 
               end
in   
    match output with 
      | None -> error,(handler,None)
      | Some mvbdu_output -> 
        let error,handler = memoized_fun.Memo_sig.store parameters error handler mvbdu_input mvbdu_output  in 
        error,(handler,Some mvbdu_output)
end 

let less parameters error x y = 
  match compare x.Mvbdu_sig.variable y.Mvbdu_sig.variable
  with 
      | 0 -> error,compare x.Mvbdu_sig.upper_bound y.Mvbdu_sig.upper_bound  
      | 1 | -1 as x-> error,x  
          | _ ->  invalid_arg parameters error (Some "line 81") Exit 0    
                                
            
let rec generic_binary allocate (memoized_fun:('a,'b,'c,'d,'e,'f) Memo_sig.binary_memoized_fun) handler error parameters mvbdu_a mvbdu_b = 
     match memoized_fun.Memo_sig.get parameters error handler (mvbdu_a,mvbdu_b) with 
          | error,(handler,Some output) ->   
               error,(handler,Some output)
          | error,(handler,None) -> 
             begin
               let error,(handler,output) = 
                 match mvbdu_a.Mvbdu_sig.value,mvbdu_b.Mvbdu_sig.value with
                      | Mvbdu_sig.Leaf a, _ ->
                              let error,depreciated = fst (memoized_fun.Memo_sig.f parameters error) a in 
                             generic_unary allocate depreciated handler error parameters mvbdu_b 
                      | _ ,Mvbdu_sig.Leaf b ->
                                let error,depreciated = snd (memoized_fun.Memo_sig.f parameters error) b in  
                             generic_unary allocate depreciated handler error parameters mvbdu_a  
                      | Mvbdu_sig.Node x,Mvbdu_sig.Node y -> 
                                let error,(cell,x_true,x_false,y_true,y_false) = 
                          let error,cmp = less parameters error x y in 
                              error,
                              match cmp 
                              with 
                                | 0 ->
                                  x,x.Mvbdu_sig.branch_true,x.Mvbdu_sig.branch_false,y.Mvbdu_sig.branch_true,y.Mvbdu_sig.branch_false 
                                | -1 -> 
                                  x,x.Mvbdu_sig.branch_true,x.Mvbdu_sig.branch_false,mvbdu_b,mvbdu_b
                                | _ -> 
                                  y,mvbdu_a,mvbdu_a,y.Mvbdu_sig.branch_true,y.Mvbdu_sig.branch_false 
                          in     
                          begin
                            match generic_binary allocate memoized_fun handler error parameters x_true y_true
                            with
                              | error,(handler,None) ->
                                       error,(handler,None)
                              | error,(handler,Some(mvbdu_true)) -> 
                                 begin    
                                         match generic_binary allocate memoized_fun handler error parameters x_false y_false
                                   with 
                                     | error,(handler,None) -> 
                                         error,(handler,None)
                                     | error,(handler,Some(mvbdu_false)) -> 
                                        begin 
                                          match Mvbdu_core.compress_node allocate error handler (Mvbdu_sig.Node {cell with Mvbdu_sig.branch_true = mvbdu_true ; Mvbdu_sig.branch_false = mvbdu_false}) 
                                          with 
                                           | error,None ->     
                                               error,(handler,None)
                                           | error,Some(id,cell,mvbdu,handler) -> 
                                               error,(handler,Some(mvbdu))
                                        end 
                                  end    
                           end 
                     in   
          match output with 
            | None ->    
             error,(handler,None)
            | Some mvbdu_output -> 
                 let error,handler = memoized_fun.Memo_sig.store parameters error handler (mvbdu_a,mvbdu_b) mvbdu_output  in 
                    error,(handler,Some mvbdu_output)
end 
        

             
let rec generic_unary_other allocate memoized_fun handler error parameters other mvbdu_input = 
   match memoized_fun.Memo_sig.get parameters error handler (other,mvbdu_input) 
   with 
     | error,(handler,Some output) -> error,(handler,Some output)
     | error,(handler,None) -> 
       begin
         let error,(handler,output) = 
           match mvbdu_input.Mvbdu_sig.value with
            | Mvbdu_sig.Leaf a ->
               let error,depreciated = memoized_fun.Memo_sig.f parameters error a other  in 
               generic_zeroary allocate handler depreciated error parameters 
            | Mvbdu_sig.Node x -> 
              begin 
                match generic_unary_other allocate memoized_fun handler error parameters x.Mvbdu_sig.branch_true other   
                with
                  | error,(handler,None) -> 
                       error,(handler,None)
                  | error,(handler,Some(mvbdu_true)) -> 
                       begin
                         match generic_unary_other allocate memoized_fun handler error parameters x.Mvbdu_sig.branch_false other  
                         with 
                           | error,(handler,None) -> 
                               error,(handler,None)
                           | error,(handler,Some(mvbdu_false)) -> 
                              begin 
                                match Mvbdu_core.compress_node allocate error handler (Mvbdu_sig.Node {x with Mvbdu_sig.branch_true = mvbdu_true ; Mvbdu_sig.branch_false = mvbdu_false}) 
                                with 
                                 | error,None ->     
                                     error,(handler,None)
                                 | error,Some(id,cell,mvbdu,handler) -> 
                                     error,(handler,Some(mvbdu))
                              end 
                     end                                 
               end
         in   
          match output with 
            | None -> error,(handler,None)
            | Some mvbdu_output -> 
                let error,handler = memoized_fun.Memo_sig.store parameters error handler (other,mvbdu_input) mvbdu_output  in 
                  error,(handler,Some mvbdu_output)
end 

              
let rec clean_head allocate memoized_fun union handler error parameters (mvbdu_input:'mvbdu) = 
   match memoized_fun.Memo_sig.get parameters error handler mvbdu_input 
   with 
    | error,(handler,Some output) ->                  
        error,(handler,Some output)
    | error,(handler,None) -> 
       begin
         let error,(handler,mvbdu_output) = 
           match mvbdu_input.Mvbdu_sig.value 
           with 
            | Mvbdu_sig.Leaf a -> 
                error,(handler,Some mvbdu_input) 
            | Mvbdu_sig.Node x -> 
                let var_ref = x.Mvbdu_sig.variable in 
                let rec aux handler error mvbdu_input_list mvbdu_output = 
                  match mvbdu_input_list
                  with 
                    | [] -> 
                            error,(handler,mvbdu_output)
                    | t::q ->
                      begin                         
                        match t.Mvbdu_sig.value 
                        with 
                    | Mvbdu_sig.Node x when x.Mvbdu_sig.variable = var_ref ->
                              aux handler error (x.Mvbdu_sig.branch_true::x.Mvbdu_sig.branch_false::q) mvbdu_output
                    | Mvbdu_sig.Node _ 
                          | Mvbdu_sig.Leaf _ -> 
                            begin
                              match mvbdu_output 
                              with
                                | None -> 
                                    aux handler error q (Some t) 
                                | Some a -> 
                                    let error,(handler,output) = union parameters handler error parameters t a in   
                                    begin
                                      match output 
                                      with 
                                        |  None -> 
                                              invalid_arg parameters error (Some "line 227") Exit (handler,None)    
                                        | Some a -> 
                                              aux handler error q (Some (a:'mvbdu))  
                                    end
                            end  
                      end 
                in 
                aux handler error [mvbdu_input] None 
         in 
         begin
           match mvbdu_output with 
            | None -> 
                 error,(handler,None)
            | Some mvbdu_output ->                                 
                 let error,handler = memoized_fun.Memo_sig.store parameters error handler mvbdu_input mvbdu_output  in 
                  error,(handler,Some (mvbdu_output:'mvbdu))
         end  
     end    
              
              
          
let rec redefine allocate memoized_fun error parameters handler mvbdu_input list_input = 
   match memoized_fun.Memo_sig.get parameters error handler (mvbdu_input,list_input) 
   with 
     | error,(handler,Some output) -> error,(handler,Some output)
     | error,(handler,None) -> 
        begin
         let error,(handler,output) = 
           match list_input.List_sig.value 
           with 
             | List_sig.Empty ->  
                 let error,depreciated = (memoized_fun.Memo_sig.f parameters error).Memo_sig.empty_list in 
                  generic_unary allocate depreciated handler error parameters mvbdu_input
             | List_sig.Cons(list) -> 
                 begin
                   match mvbdu_input.Mvbdu_sig.value
                   with 
                     | Mvbdu_sig.Node mvbdu when compare list.List_sig.variable mvbdu.Mvbdu_sig.variable > 0
                       ->
                           let error,(handler,b_true) = redefine allocate memoized_fun error parameters handler mvbdu.Mvbdu_sig.branch_true list_input in 
                           let error,mvbdu_true = downgrade parameters error (Some "line 266") (fun () -> mvbdu.Mvbdu_sig.branch_true) b_true in    
                           let error,(handler,b_false) = redefine allocate memoized_fun error parameters handler mvbdu.Mvbdu_sig.branch_false list_input in 
                           let error,mvbdu_false = downgrade parameters error (Some "line 268") (fun () -> mvbdu.Mvbdu_sig.branch_false) b_false in   
                           begin 
                                match Mvbdu_core.compress_node allocate error handler (Mvbdu_sig.Node {mvbdu with Mvbdu_sig.branch_true = mvbdu_true ; Mvbdu_sig.branch_false = mvbdu_false}) 
                                with 
                                 | error,None ->     
                                     error,(handler,None)
                                 | error,Some(id,cell,mvbdu,handler) -> 
                                     error,(handler,Some(mvbdu))
                              end 
                     | _ -> 
                       begin 
                         let error,(handler,branch_true) = 
                              match mvbdu_input.Mvbdu_sig.value with 
                                 | Mvbdu_sig.Node x when compare list.List_sig.variable x.Mvbdu_sig.variable = 0 -> 
                                     let error,depreciated = (memoized_fun.Memo_sig.f parameters error).Memo_sig.clean_head  in 
                                     generic_unary allocate depreciated handler error parameters mvbdu_input 
                                 | _ -> error,(handler,Some mvbdu_input)
                        in 
                        let error,branch_true = downgrade parameters error (Some "line 286") (fun () -> mvbdu_input) branch_true in 
                        let error,depreciated = (memoized_fun.Memo_sig.f parameters error).Memo_sig.build_false list.List_sig.variable list.List_sig.association in 
                        let error,(handler,branch_false) = generic_zeroary allocate handler depreciated error parameters  in
                        let error,branch_false = downgrade parameters error (Some "line 289") (fun () -> mvbdu_input) branch_false in 
                        let error,depreciated = (memoized_fun.Memo_sig.f parameters error).Memo_sig.build_true  
                                                      list.List_sig.variable list.List_sig.association 
                                                      branch_false branch_true in 
                        let error,(handler,enriched_branch_true) = generic_zeroary allocate handler depreciated error parameters in 
                        let error,enriched_branch_true = 
                          downgrade parameters error (Some "line 295") (fun () -> mvbdu_input) enriched_branch_true in 
                         let error,depreciated = (memoized_fun.Memo_sig.f parameters error).Memo_sig.build_true  
                                                      list.List_sig.variable (list.List_sig.association-1) 
                                                      enriched_branch_true branch_false in 
                        let error,(handler,rep) = generic_zeroary allocate handler depreciated error parameters in 
                        let error,rep = downgrade parameters error (Some "line 300") (fun () -> mvbdu_input) rep in 
                                   redefine allocate memoized_fun error parameters handler rep list.List_sig.tail  
                      end 
               end
              in 
               match output with 
                 | None -> 
                     error,(handler,None)
                 | Some mvbdu_output ->                                 
                   let error,handler = memoized_fun.Memo_sig.store parameters error handler (mvbdu_input,list_input) mvbdu_output  in 
                  error,(handler,Some (mvbdu_output:'mvbdu))
        end  
     
let mvbdu_identity handler parameters error mvbdu = error,(handler,Some mvbdu)
let mvbdu_constant a handler parameters error _ = error,(handler,Some a)
  
let recursive_memoize f get_handler update_handler get_storage set_storage  = 
  {
    Memo_sig.f = f ;
    Memo_sig.store = 
      (fun parameters error handler key value -> 
          let storage = get_handler handler in 
          let error,storage' = set_storage parameters error handler key value storage in
          let handler' = 
             if storage' == storage 
             then handler 
             else update_handler storage' handler 
          in 
             error,handler');
    Memo_sig.get = 
      (fun parameters error handler key -> 
          let storage = get_handler handler in 
          let a,(handler,b) =   get_storage parameters error handler key storage 
          in a,(handler,b))
  }
  
let recursive_not_memoize f = 
  {
    Memo_sig.f = f; 
    Memo_sig.store = (fun parameters error handler _ _  -> error,handler);
    Memo_sig.get = (fun paramters error handler _ -> error,(handler,None))
  }   
  
let memoize_no_fun a b c d = 
  (recursive_memoize (fun _ -> raise Exit) a b c d:('a,'b,'c,'d,'e,'f) Memo_sig.unary_memoized_fun)  

let memoize_binary_no_fun a b c d = 
  (recursive_memoize (fun _ -> raise Exit) a b c d:('a,'b,'c,'d,'e,'f) Memo_sig.binary_memoized_fun)  
  
let not_recursive_not_memoize_unary  f g allocate = 
  { Memo_sig.f = g;
    Memo_sig.store = (fun parameters error handler _ _ -> error,handler);
    Memo_sig.get = (fun parameters error handler x -> 
              let error,output_wo_id,output = f error x in
              match output with 
                | None -> 
                    (match allocate 
                              parameters 
                              error 
                              compare 
                              (Mvbdu_core.get_skeleton output_wo_id) 
                              output_wo_id
                              (fun key -> {Mvbdu_sig.id=key;Mvbdu_sig.value =output_wo_id}) 
                              handler
                     with | error,None ->  (raise Exit)
                          | error,Some (i,a,b,handler) -> 
                                  error,(handler,Some b)) 
                | Some _ -> error,(handler,output))}
   
let not_recursive_memoize_unary
  f   
  g 
  (get_handler:'a -> 'b)  
  (update_handler:'b -> 'a -> 'a) 
  (get_storage:Exception.method_handler -> 'c -> 'b -> Exception.method_handler * ('d option)) 
  (set_storage:Exception.method_handler -> 'c -> 'd -> 'b -> Exception.method_handler * 'b)  
  allocate 
  = 
   let store = (fun parameters error handler key value -> 
          let storage = get_handler handler in 
          let error,storage' = set_storage error key value storage in
          let handler' = 
             if storage' == storage 
             then handler 
             else update_handler storage' handler 
          in 
          error,handler')
   in 
     {
    Memo_sig.f = g ;
    Memo_sig.store = store;
    Memo_sig.get = 
      (fun parameters error handler key -> 
          let storage = get_handler handler in 
          let a,b =   get_storage error key storage in 
          match b with 
                | Some x -> a,(handler,b) 
                | None ->
                  begin
                  ( 
                    let error,handler,output_wo_id,output = f error handler key in
                    match output with 
                      | None -> 
                          (match allocate 
                              error 
                              compare 
                              (Mvbdu_core.get_skeleton output_wo_id) 
                              output_wo_id
                              (fun key -> {Mvbdu_sig.id=key;Mvbdu_sig.value =output_wo_id}) 
                              handler
                           with 
                            | error,None ->  (raise Exit)
                            | error,Some (i,a,b,handler) -> 
                              let error,handler = storage error handler key b in   
                                error,(handler,Some b)) 
                            | Some _ -> error,(handler,output))
                                end )
                              }
                  
let a = (not_recursive_memoize_unary: (Exception.method_handler -> 'handler -> 'g) -> 
                                     (Remanent_parameters_sig.parameters -> Exception.method_handler -> 'e) -> ('handler -> 'dic) -> ('dic -> 'handler -> 'handler) -> (Exception.method_handler -> 'c -> 'dic -> Exception.method_handler * ('d option)) -> 
  ( Exception.method_handler -> 'c -> 'd -> 'dic -> Exception.method_handler * 'dic) -> 'h -> 'f  )
  
  let not_recursive_binary f g allocate = 
   { Memo_sig.f = g;
    Memo_sig.store = (fun parameters error handler _ _ -> error,handler);
    Memo_sig.get = (fun parameters error handler (x,y) -> 
              let error,output_wo_id,output = f error x y in
              match output with 
                | None -> 
                  (match allocate 
                          error 
                          compare 
                          (Mvbdu_core.get_skeleton output_wo_id)
                          output_wo_id 
                          (fun key -> {Mvbdu_sig.id = key ; Mvbdu_sig.value = output_wo_id})
                          handler 
                   with | error,None -> (raise Exit)
                        | error,Some (i,a,b,handler) -> error,(handler,Some b))
                | Some _ -> error,(handler,output))}
   
 let id_of_mvbdu x = x.Mvbdu_sig.id 
   