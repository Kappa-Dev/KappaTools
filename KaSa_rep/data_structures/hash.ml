(**
    * hash.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 18/10/2010
    * Last modification: 06/01/2011
    * * 
    * This library provides signature for hash tables and several implementations
    * *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

module type Hash = 
  sig
    type key
    type 'a hash 
      
    val create: int -> 'a hash
    val add: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a -> int -> 'a hash -> Exception.method_handler * 'a hash 
    val find: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a hash -> Exception.method_handler * (int * 'a) 
    val find_option: Remanent_parameters_sig.parameters -> Exception.method_handler -> key -> 'a hash -> Exception.method_handler * (int*'a)  option 
    
    val iter: (key -> 'a -> int -> unit) -> 'a hash -> unit 
    val fold: (key -> 'b -> int -> 'a -> 'a) -> 'b hash -> 'a -> 'a  
end 

module Hash = 
  (functor (Map:Set_and_map.Set_and_Map) -> 
    (struct
      type key = Map.key 
      type 'a hash = (int*'a) Map.map
        
      let create x = Map.empty_map 
      let add parameters error key asso int = Map.add_map parameters error key (int,asso)  
      let find = Map.find_map  
      let find_option = Map.find_map_option 
          
            
      let iter f = Map.iter_map (fun (a:key) (b,c) -> f a c b)   
      let fold f = Map.fold_map (fun a (b,c) d -> f a c b d)    

end:Hash with type key = Map.key))

module Hash_of_Ord = (functor (O:Set_and_map.OrderedType) -> Hash(Set_and_map.Make(O)))
