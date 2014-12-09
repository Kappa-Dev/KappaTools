(**
    * mvbdu.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 08/03/2010
    * Last modification: 23/11/2010
    * * 
    * This library provides primitives to deal set of finite maps from integers to integers
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

let invalid_arg parameters mh message exn value = 
     Exception.warn parameters mh (Some "List_algebra") message exn (fun () -> value)

let rec build_reversed_sorted_list_aux allocate parameters error handler list already = 
  List.fold_left 
    (fun (error,(handler,already)) (var,asso) -> 
       let error,output = 
          List_core.build_list 
            allocate   
            error 
            handler 
            (List_sig.Cons 
              {
                List_sig.variable = var;
                List_sig.association = asso;
                List_sig.tail = already.List_sig.id
                })
            (List_sig.Cons 
              {
                List_sig.variable = var;
                List_sig.association = asso;
                List_sig.tail = already
                     })
       in 
        match output with 
          | Some (key,cell,list,handler) -> error,(handler,list)
          | None -> invalid_arg parameters error (Some "Line 41") Exit (handler,already)
          ) 
  (error,(handler,already))
  list

let build_reversed_sorted_list allocate parameters error handler list = 
  let error,output = 
    List_core.build_list allocate error handler (List_sig.Empty) (List_sig.Empty) 
  in 
  match output with 
    | Some (key,cell,empty_list,handler) -> build_reversed_sorted_list_aux allocate parameters error handler list empty_list
    | None -> invalid_arg parameters error (Some "52") Exit (handler,{List_sig.id=0;List_sig.value = List_sig.Empty})

let build_sorted_list allocate parameters error handler list = 
  build_reversed_sorted_list allocate parameters error handler (List.rev list) 
  
let build_list allocate parameters error handler list = 
  let sort (i,_) (j,_) = - (compare i j) in  
   build_reversed_sorted_list allocate error parameters handler (List.sort sort list) 
  
  