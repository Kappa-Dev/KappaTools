   (**
    * dictionary.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2010, the 18th of October
    * Last modification: 2011, the 21rd of March
    * * 
    * This library provides primitives to deal indexed set of values
    * During the construction membership, tranduction, and new key can be handled in O(log n)
    * After the construction key can be translated in O(1)
    * *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)


type 'a dictionary = 
  {
    key_of: 'a -> int;
    value_of_key: int -> 'a;
    is_stabilized: bool;
    stabilize: unit -> 'a dictionary
  }
  
module type Dictionary = 
sig
  type value 
  type ('a,'b) dictionary 
    
  val init: unit -> ('a,'b) dictionary     
  val member: Remanent_parameters_sig.parameters -> Exception.method_handler -> value -> ('a,'b) dictionary -> Exception.method_handler * bool
  val allocate: Remanent_parameters_sig.parameters -> Exception.method_handler -> ('a -> 'a -> int) -> value -> 'a -> (int -> 'b) -> ('a,'b) dictionary -> Exception.method_handler * (int * 'a * 'b * ('a,'b) dictionary) option
  val allocate_uniquely: Remanent_parameters_sig.parameters -> Exception.method_handler -> ('a -> 'a -> int) -> value -> 'a -> (int -> 'b) -> ('a,'b) dictionary -> Exception.method_handler * (int * 'a * 'b * ('a,'b) dictionary) option
  (*  val allocate_f_id: Exception.method_handler -> ('a -> 'a -> int) -> value -> (int -> 'a) -> 'a dictionary -> Exception.method_handler * (int * 'a * 'a dictionary) option*)
  val allocate_bool: Remanent_parameters_sig.parameters -> Exception.method_handler -> ('a -> 'a -> int) -> value -> 'a -> (int -> 'b) -> ('a,'b) dictionary -> Exception.method_handler * (bool * (int * 'a * 'b * ('a,'b) dictionary) option)
    
  val unsafe_allocate: Remanent_parameters_sig.parameters -> Exception.method_handler -> value -> 'a -> (int -> 'b) -> ('a,'b) dictionary -> Exception.method_handler * (int * 'a * 'b * ('a,'b)  dictionary)
  val translate: Remanent_parameters_sig.parameters -> Exception.method_handler -> int -> ('a,'b) dictionary -> Exception.method_handler * (value * 'a * 'b) option  
  val stabilize: ('a,'b) dictionary -> ('a,'b) dictionary
  val print: Remanent_parameters_sig.parameters -> Exception.method_handler  -> (Remanent_parameters_sig.parameters -> Exception.method_handler -> int -> value -> 'a -> 'b -> Exception.method_handler) -> ('a,'b) dictionary -> Exception.method_handler 
  val last_entry: Remanent_parameters_sig.parameters -> Exception.method_handler -> ('a,'b) dictionary -> Exception.method_handler * int
  val fold : (value -> 'a * 'b -> int -> 'c -> 'c) -> ('a, 'b) dictionary -> 'c -> 'c 
end

exception Association_is_existing_already_with_a_different_value 
exception Association_is_existing_with_the_same_value_in_a_different_location_memory
exception Association_is_not_defined 
  
module Dictionary = 
  (functor (Hash:Hash.Hash) -> 
    (struct 
      type value = Hash.key

      type ('a,'b) in_construction = 
	  {
	    hash_table:('a*'b) Hash.hash; 
	    fresh:int
	  }

      type ('a,'b) stabilized = (value * 'a * 'b) option array

      type ('a,'b) dictionary = 
	  {
	    is_stabilized: bool;
	    stabilized:('a,'b) stabilized;
	    in_construction:('a,'b) in_construction
	  }
      
      let invalid_arg parameters mh message exn  = 
	Exception.warn parameters mh (Some "Dictionary") message exn (fun () -> None)
   
      let invalid_arg_bool parameters mh message exn = 
	Exception.warn parameters mh (Some "Dictionary") message exn
	  (fun () -> false, None)
	  
      let preinit () = 
	{
          hash_table = Hash.create 1;
          fresh = 0
	}
      
      let premember parameters error value in_construction =
	let error , output = Hash.find_option_without_logs (* indeed, this function is used to test whether or not there is an association *)
			       parameters error value in_construction.hash_table in
	error,output != None 
	      
      let preallocate parameters error value asso in_construction =
	match
	  Hash.find_option_without_logs                   (* indeed, this function is used to check that either there is no association, or that this is the same association *)
	    parameters error value in_construction.hash_table
	with
        | error,None ->
           begin
             let fresh = in_construction.fresh in
             let error,hash_table =
	       Hash.add parameters error value asso fresh in_construction.hash_table in
             let hash = 
	       {
                 hash_table = hash_table; 
                  fresh = fresh + 1
		}
              in 
              error,Some (fresh,asso,hash) 
            end  
          | error,Some (i, asso') when asso'== asso -> 
	    error,Some (i, asso',in_construction)
          | error,Some _ -> invalid_arg parameters error
	    (Some "wrong association, line 95")
	    Association_is_existing_already_with_a_different_value
            
      let pretranslate parameters error key stabilized = 
	if key>=0 && key<Array.length stabilized
	then 
	  match stabilized.(key)
	  with 
          | Some a -> error, Some a
          | None -> invalid_arg parameters error
	    (Some "missing entry, line 101") Association_is_not_defined    
	else 
	  if key<0 then
	  invalid_arg parameters error
	    (Some "negative key are not allowed") Association_is_not_defined
	  else
	    invalid_arg parameters error
	      (Some "missing entry, line 126") Association_is_not_defined

      let prestabilize in_construction = 
	let array = Array.make in_construction.fresh None in 
	let _ = 
          Hash.iter 
            (fun a  (asso,asso') i -> array.(i)<-Some (a,asso,asso'))
            in_construction.hash_table
	in 
	array
	  
      let init a = 
	{
	  in_construction =  (preinit ());
	  stabilized = Array.make 0 None;
	  is_stabilized = true
	}
       
      let member parameters error value dictionary = 
	premember parameters error value dictionary.in_construction 
	  
      let stabilize dictionary = 
	if dictionary.is_stabilized 
	then 
          dictionary
	else 
          {
	    dictionary with is_stabilized = true;
              stabilized = prestabilize dictionary.in_construction
	  }
        
      let translate parameters error key dictionary = 
	let dictionary = stabilize dictionary in 
        pretranslate parameters error key dictionary.stabilized
          
      let allocate_uniquely_or_not uniquely parameters error compare (value:value) asso build dictionary = 
	let in_construction = dictionary.in_construction in 
	match
	  Hash.find_option_without_logs (* indeed, this function is used to check that either there is no association, or that this is the same association *)
	    parameters error value in_construction.hash_table 
	with   
          | error,None ->   
            begin
              let fresh = in_construction.fresh in 
              let asso_id = build fresh in 
              let error, hash_table =
		Hash.add parameters error value (asso,asso_id) fresh
		  in_construction.hash_table in 
              let hash = 
		{
                  hash_table = hash_table; 
                  fresh = fresh + 1
		}
              in 
              let dictionary = 
		if dictionary.is_stabilized
		then {dictionary with is_stabilized = false} 
		else dictionary 
              in 
              let dictionary = 
		{
		  dictionary with in_construction = hash
		} 
              in 
              error, (true, Some (fresh, asso, asso_id, dictionary))
            end  
          | error, Some (i, (asso', asso'_id)) when asso'== asso ->
	    error, (false, Some (i, asso', asso'_id, dictionary))
          | error, Some (i, (asso', asso'_id)) when compare asso asso' = 0 -> 
            if uniquely 
            then 
              invalid_arg_bool parameters error
		(Some "wrong association (the image is not uniquely described in memory), line 171")
		Association_is_existing_with_the_same_value_in_a_different_location_memory
            else
              error, (false, Some (i, asso', asso'_id, dictionary))
          | error, Some _  -> invalid_arg_bool parameters error (Some "wrong association (several images for the same key), line 174") Association_is_existing_with_the_same_value_in_a_different_location_memory
	    
      let allocate aa e x v a d f =
	let a,(b,c) = allocate_uniquely_or_not false aa e x v a d f in a,c  
      let allocate_uniquely aa e x v a d f =
	let a,(b,c) = allocate_uniquely_or_not true aa e x v a d f in a,c
      let allocate_bool aa e = allocate_uniquely_or_not false aa e 
        
      let unsafe_allocate parameters error value asso build dictionary = 
	let in_construction = dictionary.in_construction in 
	let fresh = in_construction.fresh in 
	let asso_id = build fresh in 
	let error,hash_table = Hash.add_or_overwrite parameters error value (asso,asso_id) fresh in_construction.hash_table in 
	let hash = 
	  {
            hash_table = hash_table; 
            fresh = fresh + 1
	  }
	in 
	let dictionary = 
	  {
	    dictionary with in_construction = hash
	  } 
	in 
	error,(fresh,asso,asso_id,dictionary)
	  
      let print parameters error print dic = 
	let dic = 
          if dic.is_stabilized
          then 
            dic 
          else 
            stabilize dic 
	in 
	let n = Array.length dic.stabilized in 
	let rec aux key error = 
          if key=n 
          then error 
          else 
            let error = 
              match dic.stabilized.(key) with  
		| None -> error
		| Some (value, a, b) -> print parameters error key value a b
            in aux (key+1) error  
	in aux 0 error 
        
      let fold f dictionary = 
        let in_construction = dictionary.in_construction in 
        Hash.fold f in_construction.hash_table

      let last_entry parameters error dic = error, dic.in_construction.fresh - 1

     end: Dictionary with type value = Hash.key))
    
module Dictionary_of_Ord =
  (functor (O:SetMap.OrderedType) -> Dictionary (Hash.Hash_of_Ord(O)))
  
