(**
  * binomialtree.ml 
  *
  * Data-structures for imperative binomial trees 
  * Insertion is log time
  * Reading the max element in constant time
  * Deleting the max element is log time 
  * Memory consumption is constant (in place)
  * 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS 
  *  
  * Creation: 19/03/2012
  * Last modification: 19/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2012,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module type BinomialTree = 
    sig
      type key 
      type t 
        
      exception Full_tree 
      val create : int -> t
      val read_max : t -> key option
      val insert : key -> t -> t
      val remove_max : t -> t 
      val iteri: (int -> key -> unit) -> t -> unit
      val is_empty: t -> bool 
    end

module Make = 
  (functor (O:Map.OrderedType) -> 
    (struct 
      exception Full_tree 

      type key = O.t 
          
      type t = 
          {
            capacity:int;
            current_size:int;
            array:key option array
          }
            
      let create i = 
        let i = max i 1 in 
        {
          capacity=i;
          current_size=0;
          array= Array.make (i+1) None
        }

      let is_empty t = t.current_size = 0 

      let get_max t = 
        try 
          Array.get t.array 0 
        with 
          | _ -> None 

      let is_root i = i=1 
      let father i = i/2 
      let son_left i = 2*i 
      let son_right i = 2*i+1 

      let swap t i j = 
        let tmp = Array.get t.array i in 
        let _ = Array.set t.array i (Array.get t.array j) in 
        let _ = Array.set t.array j tmp 
        in ()

      let rec bubble_up t i = 
        if is_root i 
        then () 
        else 
          let father = father i in 
          let value = Array.get t.array i in 
          let value_father = Array.get t.array father in 
          match value,value_father 
          with Some value,Some value_father -> 
            if O.compare value_father value < 0 
            then 
              let _ = swap t i father in
              bubble_up t father 
            else 
              ()
            | _ -> () 

      let rec bubble_down t i = 
        let son_left = son_left i in 
        let son_right = son_right i in 
        let well_placed i son = 
          if son>t.current_size then true
          else 
            match Array.get t.array i,Array.get t.array son 
            with 
              | Some a,Some b -> compare a b >= 0 
              | _ -> true 
        in 
        match well_placed i son_left,well_placed i son_right,well_placed son_left son_right  
        with 
          | true,true,_ -> () 
          | true,false,_ | false,false,false -> 
            let _ = swap t i son_right in 
            bubble_down t son_right 
          | false,true,_ | false,false,true -> 
            let _ = swap t i son_left in 
            bubble_down t son_left 
              

      let read_max t = Array.get t.array 1
      
      let remove_max t = 
        let _ = Array.set t.array 1 None in 
        let _ = swap t 1 t.current_size in 
        let _ = bubble_down t 1 in 
        {t with current_size = t.current_size - 1}

      let insert i t = 
        let size = t.current_size + 1 in 
        if size > t.capacity 
        then raise Full_tree 
        else 
          let _ = Array.set t.array size (Some i) in 
          let _ = bubble_up t size in 
          {t with current_size = t.current_size + 1}

      let iteri f t = 
        let rec aux k = 
          if k>t.current_size 
          then ()
          else 
            let _ = 
              match 
                Array.get t.array k 
              with 
                | Some a -> f k a 
                | None -> ()
            in 
            aux (k+1)
        in aux 1 

end:BinomialTree with type key=O.t))

