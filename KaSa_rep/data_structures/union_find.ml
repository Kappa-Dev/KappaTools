(**
    * union_find.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2015, the 11th of March
    * Last modification: 
    * * 
    * This library provides primitives to deal with union find algorithm with
    * path compression
    *  
    * Copyright 2010,2011 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)

module type Storage =
  sig
    type t = data ref
     and data = Array of int array
              | Node_Array of int * int * t

    val create : int -> int -> data ref
    val init : int -> (int -> int) -> data ref
    val rerootk : t -> (unit -> 'a) -> 'a
    val reroot : t -> unit
    val get : t -> int -> int
    val set : t -> int -> int -> t
    val print_array : t -> unit

  end

module UnionFind =
  (struct
      type t = data ref
       and data =
         | Array of int array 
         | Node_Array of int * int * t
                                       
      let create n v = ref (Array (Array.make n v))
      let init n f = ref (Array (Array.init n f))
                         
      (* reroot t ensures that t becomes an Array node *)
      let rec reroot t =
        match !t with
        | Array _ -> ()
        | Node_Array (i, v, t') -> 
           reroot t';
           begin match !t' with
                 | Array a as n ->
                    let v' = a.(i) in
                    a.(i) <- v;
                    t := n;
                    t' := Node_Array (i, v', t)
                 | Node_Array _ -> assert false
           end
             
      let rec rerootk t k =
        match !t with
        | Array _ -> k ()
        | Node_Array (i, v, t') -> 
           rerootk t' (fun () ->
                       begin
                         match !t' with
                         | Array a as n ->
                            let v' = a.(i) in
                            a.(i) <- v;
                            t := n;
                            t' := Node_Array (i, v', t)
                         | Node_Array _ -> assert false
                       end;
                       k()
                      )

      let reroot t = rerootk t (fun () -> ())

      let rec get t i =
        match !t with
        | Array a -> 
           a.(i)
        | Node_Array _ -> 
           reroot t; 
           begin
             match !t with
             | Array a -> a.(i)
             | Node_Array _ -> assert false
           end
             
      let set t i v = 
        reroot t;
        match !t with
        | Array a as n ->
           let old = a.(i) in
           if old == v then
             t
           else begin
               a.(i) <- v;
               let res = ref n in
               t := Node_Array (i, old, res);
               res
             end
        | Node_Array _ -> assert false

      let rec print_array t =
        match !t with
        | Array l -> Array.iter (fun i -> print_int i; print_string " ") l
        | Node_Array (_,_,t')-> print_array t'
                                            
    end)

(****************************************************************************)
(* union find *)

type ufind = { 
    mutable father: UnionFind.t; (* mutable to allow path compression *)
  }
      
let create n = 
  { father = UnionFind.init n (fun i -> i) }

let rec find_aux f i = 
  let fi = UnionFind.get f i in
  if fi == i then 
    f, i
  else 
    let f, r = find_aux f fi in 
    let f = UnionFind.set f i r in
    f, r
      
let find h x = 
  let f, rx = find_aux h.father x in
  h.father <- f;
  rx
  
let union h x y =
  let rx = find h x in
  let ry = find h y in
  if rx != ry
  then
    begin
      { father = UnionFind.set h.father ry rx }
    end
  else
    h

let print_union {father} =
  UnionFind.print_array father
