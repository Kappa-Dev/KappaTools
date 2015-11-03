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
    val add: key -> 'a -> int -> 'a hash -> 'a hash
    val find_option: key -> 'a hash -> (int*'a)  option
    
    val iter: (key -> 'a -> int -> unit) -> 'a hash -> unit 
    val fold: (key -> 'b -> int -> 'a -> 'a) -> 'b hash -> 'a -> 'a  
end 

module Hash = 
  (functor (Map:SetMap.S) -> 
    (struct
      type key = Map.elt
      type 'a hash = (int*'a) Map.Map.t
        
      let create _ = Map.Map.empty
      let add key asso int = Map.Map.add key (int,asso)
      let find_option = Map.Map.find_option
          
            
      let iter f = Map.Map.iter (fun (a:key) (b,c) -> f a c b)
      let fold f = Map.Map.fold (fun a (b,c) d -> f a c b d)

end:Hash with type key = Map.elt))

module Hash_of_Ord = (functor (O:SetMap.OrderedType) -> Hash(SetMap.Make(O)))
