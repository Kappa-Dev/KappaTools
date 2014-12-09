   (**
    * set_and_map.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2010, the 7th of July 
    * Last modification: 2013, 27th of September
    *  
    * This library provides primitives to deal with Set and Maps of of ordered elements, in the fashion of Ocaml's Map;
    * It provides efficient iterators
    *  
    * Copyright 2010,2011,2012,2013 Institut National de Recherche 
    * en Informatique et en Automatique.  
    * All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)


module type OrderedType = sig 
  type t 
  val compare : t -> t -> int 
end
  
module type Set_and_Map = sig
  type elt
  type set
  type key
  type 'a map 
    
    
  val empty_set:set
  val is_empty_set:set -> bool
  val mem_set: elt -> set -> bool 
  val add_set: Remanent_parameters_sig.parameters -> Exception.method_handler -> elt -> set -> Exception.method_handler * set
  val singleton: elt -> set
  val is_singleton: set -> bool 
  val remove: Remanent_parameters_sig.parameters ->Exception.method_handler -> elt -> set -> Exception.method_handler * set 
  val union: Remanent_parameters_sig.parameters ->Exception.method_handler -> set -> set -> Exception.method_handler * set 
  val inter: Remanent_parameters_sig.parameters ->Exception.method_handler -> set -> set -> Exception.method_handler * set
  val diff: Remanent_parameters_sig.parameters ->Exception.method_handler -> set -> set -> Exception.method_handler * set 
  val compare_set: set -> set -> int
  val equal_set: set -> set -> bool 
  val subset: set -> set -> bool 
  val iter_set: (elt -> unit) -> set -> unit
  val fold_set: (elt -> 'a -> 'a) -> set -> 'a -> 'a
  val exists: (elt -> bool) -> set -> bool 
  val for_all: (elt -> bool) -> set -> bool 
  val filter: Remanent_parameters_sig.parameters ->Exception.method_handler -> (elt -> bool) -> set -> Exception.method_handler * set
  val partition: Remanent_parameters_sig.parameters ->Exception.method_handler -> (elt -> bool) -> set -> Exception.method_handler * set * set
  val cardinal: set -> int
  val elements: set -> elt list
  val min_elt: set -> elt 
  val max_elt: set -> elt 
  val choose: set -> elt
  val split: Remanent_parameters_sig.parameters ->Exception.method_handler -> elt -> set -> Exception.method_handler * (set * bool * set)
  
  val empty_map: 'a map
  val is_empty_map: 'a map -> bool
  val add_map: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a -> 'a map -> Exception.method_handler * 'a map
  val find_map: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a map -> Exception.method_handler * 'a
  val find_map_option : Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a map -> Exception.method_handler * 'a option 
  val remove_map: Remanent_parameters_sig.parameters ->Exception.method_handler -> key -> 'a map -> Exception.method_handler * 'a map
  val mem_map:  key -> 'a map -> bool
  val iter_map: (key -> 'a -> unit) -> 'a map -> unit
  val map_map: ('a -> 'b) -> 'a map -> 'b map
  val mapi_map: (key -> 'a -> 'b) -> 'a map -> 'b map
  val fold_map: (key -> 'a -> 'b -> 'b) -> 'a map -> 'b -> 'b
  val equal_map: ('a -> 'a -> bool) -> 'a map -> 'a map -> bool
  val update_map: Remanent_parameters_sig.parameters ->Exception.method_handler -> 'a map -> 'a map -> Exception.method_handler * 'a map    
  val map2_map: Remanent_parameters_sig.parameters ->Exception.method_handler -> ('a -> 'a -> 'a) -> 'a map -> 'a map -> Exception.method_handler * 'a map 
  val fold2z_map: Remanent_parameters_sig.parameters -> Exception.method_handler -> (key -> 'a  -> 'b  -> (Exception.method_handler * 'c)  -> (Exception.method_handler * 'c)) -> 'a map -> 'b map -> 'c -> Exception.method_handler * 'c 
   val fold2_map: Remanent_parameters_sig.parameters -> Exception.method_handler -> (key -> 'a  -> 'b  -> (Exception.method_handler * 'c)  -> (Exception.method_handler * 'c)) -> (key -> 'a   -> (Exception.method_handler * 'c)  -> (Exception.method_handler * 'c)) -> (key -> 'b  -> (Exception.method_handler * 'c)  -> (Exception.method_handler * 'c)) ->  'a map -> 'b map -> 'c -> Exception.method_handler * 'c 
  val forall_map: (key -> 'a -> bool) -> 'a map -> bool 
  val min_elt_map: (key -> 'a -> bool) -> 'a map -> key option  
  val diff_map: Remanent_parameters_sig.parameters ->Exception.method_handler -> 'a map -> 'a map -> Exception.method_handler * 'a map * 'a map 
  val diff_map_pred: Remanent_parameters_sig.parameters ->Exception.method_handler -> ('a -> 'a -> bool) -> 'a map -> 'a map -> Exception.method_handler * 'a map * 'a map 
end

module Make(Ord:OrderedType) =  
  (struct
   type elt = Ord.t
   type set = Empty_set | Node_set of set * elt * set * int 
      
   let height_set t = 
       match t with 
         | Empty_set -> 0
         | Node_set(_,_,_,h) -> h 

  let empty_set = Empty_set         
   
  let is_empty_set set = set == empty_set  
  
  let is_singleton set = 
      match set with 
          Empty_set -> false
         | Node_set (set1,_,set2,_) -> is_empty_set set1 && is_empty_set set2 
           
  let invalid_arg_set parameters mh message exn  = 
     Exception.warn parameters mh (Some "Set_and_map") message exn (fun () -> empty_set)
    
  let node_set left value right =
    Node_set(left,value,right,(max (height_set left) (height_set right))+1)       

  let balance_set parameters mh left value right = 
      let height_left = height_set left in 
      let height_right = height_set right in 
      if height_left > height_right + 2 then begin
         match left with 
         |   Empty_set -> invalid_arg_set parameters mh (Some "balance_set,line 94") (invalid_arg "Set_and_map.balance_set")
         | Node_set(leftleft,leftvalue,leftright,_) -> 
             if height_set leftleft >= height_set leftright then 
                 mh,node_set leftleft leftvalue (node_set leftright value right)
             else begin
               match leftright with 
                 | Empty_set -> invalid_arg_set parameters mh (Some "balance_set,line 100") (invalid_arg "Set_and_Map.balance_set")
                 | Node_set(leftrightleft,leftrightvalue,leftrightright,_) ->
                      (mh,
                      node_set 
                        (node_set leftleft leftvalue leftrightleft) 
                        leftrightvalue
                        (node_set leftrightright value right))
                 end
       end else if height_right > height_left + 2 then begin 
         match right with 
        | Empty_set -> invalid_arg_set parameters mh (Some  "balance_set,line 110") (invalid_arg "Set_and_Map.balance_set")
        | Node_set(rightleft,rightvalue,rightright,_) -> 
            if height_set rightright >= height_set rightleft then 
              mh,node_set (node_set left value rightleft) rightvalue rightright
            else begin
              match rightleft with 
              | Empty_set -> invalid_arg_set parameters mh (Some "balance_set,line 116") (invalid_arg "Set_and_Map.balance_set")
              | Node_set(rightleftleft,rightleftvalue,rightleftright,_) -> 
                  mh,node_set 
                    (node_set left value rightleftleft)
                    rightleftvalue
                    (node_set rightleftright rightvalue rightright)
            end  
      end else
        mh,Node_set (left,value,right,1+(max height_left height_right))
        
  let rec add_set parameters mh new_value set = 
    match set with 
      | Empty_set -> mh,Node_set(empty_set,new_value,empty_set,1)
      | Node_set(left,value_set,right,_) -> 
          let c = Ord.compare new_value value_set in 
          if c = 0 then mh,set 
          else if c<0 then 
             let mh', left' = add_set parameters mh new_value left in  
             balance_set parameters mh' left' value_set right 
          else 
            let mh', right' = add_set parameters mh new_value right in  
              balance_set parameters mh' left value_set right'
            
  let rec join_set parameters mh left value right = 
    match left,right with 
      | Empty_set,_ -> add_set parameters mh value right 
      | _,Empty_set -> add_set parameters mh value left
      | Node_set(leftleft,leftvalue,leftright,leftheight),Node_set(rightleft,rightvalue,rightright,rightheight) -> 
          if leftheight > rightheight + 2 
          then 
            let mh', right' = join_set parameters mh leftright value right in  
            balance_set parameters mh' leftleft leftvalue right'
          else if rightheight > leftheight +2 then 
            let mh', left' = join_set parameters mh left value rightleft in 
            balance_set parameters mh' left'  rightvalue rightright 
          else 
            mh,node_set left value right 
           
  let rec min_elt set = 
      match set with 
      |   Empty_set -> raise Not_found 
      | Node_set(Empty_set,v,r,_) -> v 
      | Node_set(left,_,_,_) -> min_elt set 
        
    
  let rec max_elt set = 
      match set with 
      |   Empty_set -> raise Not_found 
      | Node_set(_,v,Empty_set,_) -> v 
      | Node_set(_,_,right,_) -> max_elt set 

  let rec remove_min_elt_set parameters mh set =
   match set with 
      | Empty_set -> invalid_arg_set parameters mh (Some "remove_min_elt_set,line 169") (invalid_arg "Set_and_Map.remove_min_elt_set")
      | Node_set(Empty_set,_,right,_) -> mh,right
      | Node_set(left,value,right,_) -> 
          let mh', left' = remove_min_elt_set parameters mh left in 
          balance_set parameters mh' left' value right
        
  let merge_set parameters mh set1 set2 = 
    match set1,set2 with 
      | Empty_set,_ -> mh,set2
      | _,Empty_set -> mh,set1 
      | _ -> 
          let mh',left2 = remove_min_elt_set parameters mh set2 in 
            balance_set parameters mh' set1 (min_elt set2)  left2 
        
  let concat_set parameters mh set1 set2 = 
      match set1,set2 with 
      |   Empty_set,_ -> mh,set2 
      | _,Empty_set -> mh,set1 
      | _ -> 
          let mh',left2 =remove_min_elt_set parameters mh set2 in  
            join_set parameters mh' set1 (min_elt set2) left2 
        
  let rec split parameters mh split_value set = 
      match set with 
      | Empty_set -> mh,(empty_set,false,empty_set)
      | Node_set(left,set_value,right,_) ->
          let c = Ord.compare split_value set_value in 
          if c=0 then mh,(left,true,right)
          else if c<0 then 
              let mh',(leftleft,bool,rightleft) = split parameters mh split_value left in  
              let mh'',rightright = join_set parameters mh' rightleft set_value right in  
               mh'',(leftleft,bool,rightright) 
          else 
            let mh',(leftright,bool,rightright) = split parameters mh split_value right in 
            let mh'',leftleft = join_set parameters mh' left set_value leftright in 
              mh'',(leftleft,bool,rightright)
             
  let rec mem_set searched_value set = 
      match set with 
      |   Empty_set -> false
      | Node_set(left,set_value,right,_) -> 
          let c = Ord.compare searched_value set_value in 
           c=0 || mem_set searched_value (if c < 0 then left else right)
            
  let singleton value = Node_set(empty_set,value,empty_set,1)
             
  let rec remove parameters mh value set = 
      match set with 
        | Empty_set -> mh,empty_set 
        | Node_set(left,value_set,right,_) ->
          let c = Ord.compare value value_set in 
          if c = 0 then merge_set parameters mh left right 
          else if c < 0 then 
            let mh', left' = remove parameters mh value left in 
            balance_set parameters mh' left' value_set right
          else 
            let mh', right' = remove parameters mh value right in  
              balance_set parameters mh' left value_set right'
            
  let rec union parameters mh set1 set2 = 
     match set1,set2 with 
        | Empty_set,_ -> mh,set2 
        | _,Empty_set -> mh,set1   
        | Node_set(left1,value1,right1,height1),Node_set(left2,value2,right2,height2) -> 
          if height1 > height2 then 
            if height2 = 1 then add_set parameters mh value2 set1 
            else begin 
              let mh',(left2,_,right2) = split parameters mh value1 set2 in
              let mh'',left' = union parameters mh' left1 left2 in 
              let mh''', right' = union parameters mh'' right1 right2 in 
              join_set parameters mh''' left' value1 right'
            end  
            else 
              if height1 = 1 then add_set parameters mh value1 set2 
              else begin 
                  let mh',(left1,_,right1) = split parameters mh value2 set1 in
                  let mh'',left' = union parameters mh' left1 left2 in 
                  let mh''',right' = union parameters mh'' right1 right2 in  
                  join_set parameters mh''' left' value2 right'
              end
                
  let suture_set parameters mh (left1,value1,right1) (left2,bool,right2) f = 
    let mh' ,left' = f parameters mh left1 left2 in 
    let mh'', right' = f parameters mh' right1 right2 in 
    if bool then 
        join_set parameters mh'' left' value1 right'
    else
        concat_set parameters mh'' left' right' 
  
  let suture_not_set parameters mh (left1,value1,right1) (left2,bool,right2) f = 
    let mh' ,left' = f parameters mh left1 left2 in 
    let mh'', right' = f parameters mh' right1 right2 in 
    if bool then 
       concat_set parameters mh'' left' right'
    else 
      join_set parameters mh'' left' value1 right'
    
  let rec inter parameters mh set1 set2 = 
    match set1,set2 with 
     | Empty_set,_ 
     | _,Empty_set -> mh,empty_set
     | Node_set(left1,value1,right1,_),_ ->
         let mh',triple2 = split parameters mh value1 set2 in  
         suture_set parameters mh' (left1,value1,right1) triple2 inter 
                    
  let rec diff parameters mh set1 set2 = 
      match set1,set2 with 
     | Empty_set,_ -> mh,empty_set       
     | _,Empty_set -> mh,set1 
     | Node_set(left1,value1,right1,_),_ -> 
         let mh',triple2 = split parameters mh value1 set2 in 
         suture_not_set parameters mh' (left1,value1,right1) triple2 diff  
           
  type enumeration = End | More of elt * set * enumeration
    
  let rec cons_enum set enum = 
      match set with 
        | Empty_set -> enum
        | Node_set(left,value,right,_) -> cons_enum left (More(value,right,enum))

  let rec compare_aux e1 e2= 
      match e1,e2 with 
        | End,End -> 0
        | End,_ -> -1
        | _ , End -> 1 
        | More(v1,r1,e1),More(v2,r2,e2) -> 
           let c = Ord.compare v1 v2 in 
          if c<>0 
          then c 
          else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
            
  let compare_set set1 set2 = 
    compare_aux (cons_enum set1 End) (cons_enum set2 End)
    
  let equal_set set1 set2 = 
      compare set1 set2 = 0 
          
  let rec subset set1 set2 = 
      match set1,set2 with 
        | Empty_set,_ -> true
        | _,Empty_set -> false
        | Node_set(left1,value1,right1,_),Node_set(left2,value2,right2,_) -> 
            let c = Ord.compare value1 value2 in   
            if c=0 then   
              subset left1 left2 && subset right1 right2
            else if c < 0 then 
              subset (Node_set(left1,value1,empty_set,0)) left2 && subset right1 set2
            else  
              subset (Node_set(empty_set,value1,right1,0)) right2 && subset left1 set2
              
  let rec iter_set f set = 
    match set with 
        | Empty_set -> () 
        | Node_set(left,value,right,_) ->
          let _ = iter_set f left in 
          let _ = f value in 
          let _ = iter_set f right in 
          ()
          
  let rec fold_set f set accu = 
    match set with 
        | Empty_set -> accu          
        | Node_set(left,value,right,_) -> fold_set f right (f value (fold_set f left accu))
          
  let rec for_all p set = 
      match set with 
        | Empty_set -> true
        | Node_set(left,value,right,_) -> p value && for_all p left && for_all p right
          
  let rec exists p set =
    match set with 
        | Empty_set -> false
        | Node_set(left,value,right,_) -> p value || exists p left || exists p right 
          
  let filter parameters rh p set = 
      let rec filt accu set = 
          match set with 
           | Empty_set -> accu
           | Node_set(left,value,right,_) ->
             let rh,list = accu in  
             filt (filt (if p value then add_set parameters rh value list else accu) left) right in 
        filt (rh,empty_set) set 
                                                                      
  let partition parameters rh p set =  
    let rec part (rh,t,f as accu) set = 
        match set with 
            | Empty_set -> accu 
            | Node_set(left,value,right,_) -> 
               
              part 
                (part 
                  begin
                    if p value 
                    then   
                      let a,b = add_set parameters rh value t
                      in a,b,f
                    else 
                      let a,c = add_set parameters rh value f in 
                      a,t,c
                  end  
                  left) 
              right
    in 
    part (rh,empty_set,empty_set) set
                
  let rec cardinal set = 
      match set with 
        | Empty_set -> 0 
        | Node_set(left,value,right,_) -> cardinal left + 1 + cardinal right

  let elements set = 
    let rec elements_aux accu set = 
        match set with 
        | Empty_set -> accu
        | Node_set(left,value,right,_) -> elements_aux (value::(elements_aux accu right)) left
  in elements_aux [] set
    
          
  let choose = min_elt          
          
  type key = Ord.t
  type 'data map =  
    Empty_map 
    | Node_map of 'data map * key * 'data * 'data map * int
     
  let empty_map = Empty_map 
  
  let invalid_arg_map parameters mh message exn  =    
     Exception.warn parameters mh (Some "Set_and_map") message exn (fun () -> empty_map)
  let invalid_arg_def parameters mh message exn def =    
     Exception.warn parameters mh (Some "Set_and_map") message exn (fun () -> def)
  let is_empty_map x = x==empty_map
    
  let height_map map = 
      match map with 
          | Empty_map -> 0 
          | Node_map(_,_,_,_,h) -> h
            
  let create_map left key0 data right  = 
    Node_map (left,key0,data,right, 1 + max (height_map left) (height_map right))
    
  let rec find_map parameters rh key map = 
    match map with  
          | Empty_map -> Exception.warn parameters rh (Some "Set_and_map.ml") (Some "find_map, line 405") Not_found (fun () -> raise Not_found)
          | Node_map (left,key_map,data,right,_) -> 
              let cmp = compare key key_map in 
            if cmp = 0 then rh,data
            else if cmp>0 then find_map parameters rh key right  
            else find_map parameters rh key left 
  
  let rec find_map_option parameters rh key map = 
    match map with  
          | Empty_map -> rh,None             
          | Node_map (left,key_map,data,right,_) -> 
              let cmp = compare key key_map in 
            if cmp = 0 then rh,Some data
            else if cmp>0 then find_map_option parameters rh key right  
            else find_map_option parameters rh key left
              
  let balance_map parameters rh left key data right = 
    let height_left = height_map left in
    let height_right = height_map right in 
    begin
      if height_left <= height_right + 2 
      then
        begin
          if height_right <= height_left + 2 
          then rh,create_map left key data right
          else 
            begin
            match right with 
              Empty_map -> invalid_arg_map parameters rh (Some "balance_map, line 424") (invalid_arg "Set_and_map.balance_map")
          | Node_map (left0,key0,data0,right0,_) -> 
            begin
              if height_map left0 <= height_map right0 
              then 
                rh,create_map (create_map left key data left0) key0 data0 right0
              else
                begin
                  match left0 with 
                    | Empty_map -> invalid_arg_map parameters rh (Some "Set_and_map.balance_map, line 433") (invalid_arg "Set_and_map.balance_map")
                    | Node_map (left1,key1,data1,right1,_) -> 
                      rh,  
                      create_map
                          (create_map left key data left1)
                          key1
                          data1 
                          (create_map right1 key0 data0 right0)
              end  
            end
          end
        end
      else
        begin
          match left with 
                    | Empty_map -> invalid_arg_map parameters rh (Some "balance_map,448") (invalid_arg "Set_and_map.balance_map") 
                    | Node_map (left0,key0,data0,right0,_) -> 
                      begin
                        if height_map right0 <= height_map left0 
                        then 
                          rh,create_map left0 key0 data0 (create_map right0 key data right)
                        else 
                          begin
                          match right0 with 
                              | Empty_map -> invalid_arg_map parameters rh (Some "balance_map,457") (invalid_arg "Set_and_map.balance_map")
                              | Node_map (left1,key1,data1,right1,_) -> 
                                   rh,
                                   create_map
                                      (create_map left0 key0 data0 left1)
                                      key1
                                      data1 
                                      (create_map right1 key data right)
                          end
                      end
        end  
    end
    
  let rec add_map parameters rh key data map = 
    match map with 
      | Empty_map -> rh,Node_map (empty_map,key,data,empty_map,1)
      | Node_map (left,key_map,data_map,right,height)->
        let cmp = compare key key_map in 
        if cmp = 0 then 
          rh,Node_map(left,key_map,data,right,height)
        else if cmp < 0 then 
            let rh', left' = add_map parameters rh key data left in 
            balance_map parameters rh' left' key_map data_map right 
        else 
          let rh', right' = add_map parameters rh key data right in  
          balance_map parameters rh' left key_map data_map right'
          
  let rec min_binding map key data = 
    match map with 
      Empty_map -> (key,data)
      | Node_map (left2,key2,data2,_,_) -> 
          min_binding left2 key2 data2 
 
  let rec remove_min_binding parameters rh map key data map' = 
      match map with 
      | Empty_map -> rh,map'
      | Node_map (left2,key2,data2,right2,_) ->
          let rh', left'  = remove_min_binding parameters rh left2 key2 data right2 in 
          balance_map parameters rh' left' key2 data2 right2
        
  let merge_map parameters rh map1 map2 = 
    match map1 with 
      | Empty_map -> rh,map2 
      | Node_map(left1,key1,data1,right1,_) ->
          begin
            match map2 with 
             | Empty_map -> rh,map1 
             | Node_map(left2,key2,data2,right2,_) -> 
                 let (key3,data3) = min_binding left2 key2 data2 in 
                 let rh', left' = remove_min_binding parameters rh left2 key2 data2 right2 in     
               balance_map parameters rh' map1 key3 data3 left' 
          end  
        
   let rec remove_map parameters rh key map = 
       match map with 
             | Empty_map -> rh,empty_map
             | Node_map (left,key_map,data,right,_) ->
               let cmp = compare key key_map in 
                 if cmp = 0 
                 then merge_map parameters rh left right
                 else if cmp < 0 
                 then 
                   let rh', left' = remove_map parameters rh key left in 
                   balance_map parameters rh' left'  key_map data right
                 else 
                   let rh', right' = remove_map parameters rh key right in 
                   balance_map parameters rh' left key_map data right'
               
  let rec map_map f map = 
      match map with
             | Empty_map -> empty_map
             | Node_map(left,key,data,right,height) -> Node_map(map_map f left,key,f data,map_map f right,height)

   let rec mapi_map f map = 
      match map with
             | Empty_map -> empty_map
             | Node_map(left,key,data,right,height) -> Node_map(mapi_map f left,key,f key data,mapi_map f right,height)
               
   let rec mem_map key map = 
    match map with  
          | Empty_map -> false
          | Node_map (left,key_map,data,right,_) -> 
              let cmp = compare key key_map in 
            if cmp = 0 then true
            else if cmp>0 then mem_map key right  
            else mem_map key left                 
               
  let rec iter_map f map = 
    match map with 
          | Empty_map -> ()
          | Node_map(left,key,data,right,_) -> 
              let _ = iter_map f left in 
              let _ = f key data in 
                iter_map f right
            
            
            
  let rec fold_map f map value = 
    match map with 
          | Empty_map -> value 
          | Node_map(left,key,data,right,_) -> 
              fold_map f right (f key data (fold_map f left value)) 
                   
            
  let rec cut_opt value map = 
      match map with 
          | Empty_map -> None 
          | Node_map (left1,key1,data1,right1,height1) -> 
              let cmp = Ord.compare value key1 in 
              if cmp = 0 then
                Some (left1,data1,right1)
              else if cmp < 0 then 
                match cut_opt value left1 with  
                  | None -> None 
                  |Some (left2,data2,right2) -> 
                    Some (left2,data2,Node_map(right2,key1,data1,right1,height1))
              else 
                match cut_opt value right1 with 
                  | None -> None
                  | Some (left2,data2,right2) -> 
                    Some (Node_map(left1,key1,data1,left2,height1),data2,right2)
   
  let rec join_map parameters rh left key value right =
    match balance_map parameters rh left key value right with 
            | rh',Empty_map -> invalid_arg_map parameters rh (Some "join_map, line 580") (invalid_arg "Set_and_map.join_map")
            | rh',(Node_map (left2,key2,data2,right2,_) as map2) -> 
              let h = height_map left2 - height_map right2 in 
              if h > 2 || h< -2 
              then join_map parameters rh' left2 key2 data2 right2 
              else rh',map2 
   
  let rec split_map parameters rh value map = 
      match map with 
          | Empty_map -> rh,(empty_map,None,empty_map) 
          | Node_map (left1,key1,data1,right1,_) -> 
              let cmp = Ord.compare value key1 in 
              if cmp = 0 then
                rh,(left1,Some data1,right1)
              else if cmp < 0 then 
                let rh',(left2,data2,right2) = split_map parameters rh value left1 in 
                let rh'',right2' = join_map parameters rh' right2 key1 data1 right1 in     
                rh'',(left2,data2,right2')
              else 
                let rh',(left2,data2,right2) = split_map parameters rh value right1 in
                let rh'',left2' = join_map parameters rh' left1 key1 data1 left2 in  
                    rh'',(left2',data2,right2)
                    
  let rec forall2iz p fail map1 map2 =
      if map1==map2 then true else 
        match map1 with 
          | Empty_map -> fail ()           
          | Node_map(left1,key1,data1,right1,_) -> 
              begin 
                  match cut_opt key1 map2 with 
                  | Some (left2,data2,right2) -> 
                       begin 
                           (if data1==data2 then true else p key1 data1 data2)
                            && forall2iz p fail left1 left2   
                            && forall2iz p fail right1 right2
                       end  
                  | None -> fail () 
                    end  
                    
   let rec forall_map p map =
        match map with 
          | Empty_map -> true           
          | Node_map(left,key,data,right,_) -> 
              begin 
                  (p key data)
                  && forall_map p right 
                  && forall_map p left    
              end                      
  
   let rec min_elt_map p map = 
       match map with 
         | Empty_map -> None 
         | Node_map(left,key,data,right,_) -> 
             begin 
               match min_elt_map p left with 
               | None -> if p key data then Some key else min_elt_map p right
               | some -> some 
             end
            
  let equal_map p = forall2iz (fun x -> p) (fun ()->false)
    
  let rec update_map parameters rh map1 map2 =
     if map1==map2 then rh,map2 
     else  
       match map1 with 
         | Empty_map -> rh,map2 
         | Node_map(left1,key1,data1,right1,_) -> 
             let rh',(left2,data2,right2) = split_map parameters rh key1 map2 in 
             let rh'', left' = update_map parameters rh' left1 left2 in 
             let rh''', right' = update_map parameters rh'' right1 right2 in 
             join_map parameters rh''' left' key1 
                 (match data2 with None -> data1 | Some d2 -> d2)
                 right' 
                   
     let rec map2_map parameters rh f map1 map2 =
       match map1 with 
         | Empty_map -> rh,map2 
         | Node_map(left1,key1,data1,right1,_) -> 
             let rh',(left2,data2,right2) = split_map parameters rh key1 map2 in 
             let rh'', left' = map2_map parameters rh' f left1 left2 in 
             let rh''', right' = map2_map parameters rh'' f right1 right2 in 
             join_map parameters rh''' left' key1 
                 (match data2 with None -> data1 | Some d2 -> f data1 d2)
                 right'       
         
     let rec fold2z_map parameters rh f map1 map2 res =
       match map1,map2 with 
         | Empty_map,Empty_map -> rh,res 
         | Empty_map , _ | _ , Empty_map -> invalid_arg_def parameters rh (Some "fold2_map,line 683") Exit res
         | Node_map(left1,key1,data1,right1,_),_ -> 
             let rh',(left2,data2,right2) = split_map parameters rh key1 map2 in 
             begin 
               match data2 with 
                 | None -> 
                   invalid_arg_def parameters rh (Some "fold2_map,line 690") Exit res
                 | Some data2 -> 
                   let rh'', res' = fold2z_map parameters rh' f left1 left2 res in 
                   let rh''',res'' = f key1 data1 data2 (rh'',res') in
                     fold2z_map parameters rh''' f right1 right2 res''
             end  
	       
     let rec fold2_map parameters rh f g h map1 map2 res =
       match map1,map2 with 
         | Empty_map,Empty_map -> rh,res 
         | Empty_map , _ -> fold_map h map2 (rh,res) 
	 | _ , Empty_map -> fold_map g map1 (rh,res) 
         | Node_map(left1,key1,data1,right1,_),_ -> 
             let rh',(left2,data2,right2) = split_map parameters rh key1 map2 in 
             begin 
               match data2 with 
                 | None -> 
		   let rh'', res' = fold2_map parameters rh' f g h left1 left2 res in 
                   let rh''',res'' = g key1 data1 (rh'',res') in
                     fold2_map parameters rh''' f g h right1 right2 res''
                 | Some data2 -> 
                   let rh'', res' = fold2_map parameters rh' f g h left1 left2 res in 
                   let rh''',res'' = f key1 data1 data2 (rh'',res') in
                     fold2_map parameters rh''' f g h right1 right2 res''
             end
                   
     let rec diff_map parameters rh map1 map2 =
       match map1 with 
       | Empty_map -> 
         rh,Empty_map,map2 
       | Node_map(left1,key1,data1,right1,_) -> 
         let error,(left2,data2,right2) = split_map parameters rh key1 map2 in 
         let error,oleft1,oleft2 = diff_map parameters error left1 left2 in
         let error,oright1,oright2 = diff_map parameters error right1 right2 in
         begin 
           match data2 with 
           | Some x when x = data1 ->  
             let error,o1 = merge_map parameters error oleft1 oright1 in 
             let error,o2 = merge_map parameters error oleft2 oright2 in
             error,o1,o2 
           | Some data2  ->  
             let error,o1 = join_map parameters error oleft1 key1 data1 oright1 in 
             let error,o2 = join_map parameters error oleft2 key1 data2 oright2 in
             error,o1,o2 
           | None -> 
             let error,o1 = join_map parameters error oleft1 key1 data1 oright1 in 
             let error,o2 = merge_map parameters error oleft2 oright2 in
             error,o1,o2 
         end 

     let rec diff_map_pred parameters rh pred map1 map2 =
       match map1 with 
       | Empty_map -> 
         rh,Empty_map,map2 
       | Node_map(left1,key1,data1,right1,_) -> 
         let error,(left2,data2,right2) = split_map parameters rh key1 map2 in 
         let error,oleft1,oleft2 = diff_map_pred parameters error pred left1 left2 in
         let error,oright1,oright2 = diff_map_pred parameters error pred right1 right2 in
         begin 
           match data2 with 
           | Some x when pred x data1 ->  
             let error,o1 = merge_map parameters error oleft1 oright1 in 
             let error,o2 = merge_map parameters error oleft2 oright2 in
             error,o1,o2 
               
           | Some data2  ->  
             let error,o1 = join_map parameters error oleft1 key1 data1 oright1 in 
             let error,o2 = join_map parameters error oleft2 key1 data2 oright2 in
             error,o1,o2 
           | None -> 
             let error,o1 = join_map parameters error oleft1 key1 data1 oright1 in 
             let error,o2 = merge_map parameters error oleft2 oright2 in
             error,o1,o2 
                   end 
                     
                     
end:Set_and_Map with type key = Ord.t and type elt = Ord.t)
      
    
  
  
