(**
  * fifo.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 27th of July
  * Last modification: 
  * 
  * FIFO list
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "FIFO") message exn (fun () -> default)  

let trace = false

module type QSig = sig
  type 'a queue 
  val empty : 'a queue 
  val push : 'a -> 'a queue -> 'a queue
  val pop : 'a queue -> 'a * 'a queue
  val iter : ('a -> unit) -> 'a queue -> unit
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b queue -> 'a
end

module Queue:QSig = struct 
(* Queues implemented as pairs of lists with
   list1 head = queue front
   list2 head = queue back
*)
  type 'a queue = 'a list * 'a list
  let empty =  ([], []) 

  let is_empty q = (q = [])

  let push x (l1, l2) =  (l1, x::l2) 

  let reverse l = List.rev_append l []

  let rec pop q = 
    match q with
	[], [] -> failwith "Queue is empty"
      | h::t, l -> (h, (t, l))
      | [], l -> pop (reverse l, [])

  let iter (f : 'a -> unit) (s1,s2) =
    List.iter f s2;
    List.iter f (reverse s1)

  let fold (f: 'a -> 'b -> 'a) x (s1, s2) =
    List.fold_left f x s1;
    List.fold_left f x s2

end

(*TESTING*)
(*open Printf

let print_list l =
  let rec aux acc =
    match acc with
      | [] -> ()
      | x :: tl ->
        Printf.fprintf stdout "%i " x;
        aux tl
  in
  aux l

(* sample uses of the module *) 
let store_queue = List.fold_left (fun a b ->
  Queue.push b a
)Queue.empty [2;3;4;5]

let print_queue =
  Queue.iter (fun a ->
    let _ =
      fprintf stdout "Store_queue: ";
      fprintf stdout "%i " a
    in
    ()
  ) store_queue

(*let (e', q3) = Queue.pop store_queue*)
let q3 = Queue.pop store_queue

let print_pop qu = 
  let (a, q) = qu in
  let _ =
    fprintf stdout "\na:%i " a
  in
  Queue.iter (fun a ->
    let _ =
      fprintf stdout "\nQ:";
      fprintf stdout "%i " a
    in
    ()
  ) q
  
let print_p = print_pop q3*)
