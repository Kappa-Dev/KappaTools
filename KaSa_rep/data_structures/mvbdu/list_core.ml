(**
    * list_core.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 08/03/2010
    * Last modification: 23/11/2010
    * * 
    * This library provides primitives to deal associations list 
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)


let sanity_check = true
let test_workbench = false 
  

let invalid_arg parameters mh message exn value = 
     Exception.warn parameters mh (Some "Mvbdu") message exn (fun () -> value)

let get_hash_key list = list.List_sig.id

let list_equal a b = a==b
  
let get_skeleton prelist = 
  match prelist with
    | List_sig.Empty -> List_sig.Empty 
    | List_sig.Cons x -> 
        List_sig.Cons
        { x with 
          List_sig.tail = get_hash_key x.List_sig.tail}
                
let build_list (allocate:('a,'b,'c) Sanity_test_sig.g) error handler skeleton cell = 
      allocate error compare skeleton cell (fun key -> {List_sig.id=key;List_sig.value=cell}) handler  
  

let id_of_list x = x.List_sig.id 

let update_dictionary handler dictionary = 
  if handler.Memo_sig.list_dictionary == dictionary 
  then 
    handler 
  else  
    {handler with Memo_sig.list_dictionary = dictionary}