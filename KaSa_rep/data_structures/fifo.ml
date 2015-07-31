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

module type Worklist =
  sig
    val create : int -> int list * int list  * bool array
    val is_empty : int list * int list * bool array -> bool
    val push : int -> int list * int list * bool array -> int list * int list * bool array
    val pop : int list * int list * bool array -> int list * int list * bool array
  end

    
module Wlist: Worklist =
struct

  let init_array n = Array.make n false
    
  let create n = [], [], init_array n
    
  let is_empty w =
    match w with
      | [], [], [||] -> true
      | _ -> false
        
  let push x w =
    let (input, output, ar) = w in
    if ar.(x) then
      w
    else
      begin
      ar.(x) <- true;
      x :: input, output, ar
      end

  let rec pop w =
    let (input, output, ar) = w in
    match output with
      | [] -> 
        begin
          match input with
            | [] -> w
            | _ -> 
              pop ([], (List.rev input), ar)
        end
      | h :: tl ->
        begin
          ar.(h) <- false;
          input, tl, ar
        end
end


(*TEST*)
(*open Printf

let init_wl = Wlist.create 10

let print_list l =
  let rec aux acc =
    match acc with
      | [] -> fprintf stdout "\nEMPTY\n"
      | x :: tl -> Printf.fprintf stdout "%i " x;
        aux tl
  in aux l

let print_array ar =
  Array.iter (fun a -> let () = fprintf stdout "%b " a in ()) ar

let print_x_again =
  let (l, l2, w) = init_wl in
  print_array w

let push_x = Wlist.push 2 init_wl
let push_y = Wlist.push 3 push_x
let push_w = Wlist.push 3 push_y

let print_w =
  let (l1, l2, w) = push_w in
  fprintf stdout "\nTEST\n";
  print_array w;
  print_list l1;
  fprintf stdout "\nL2\n";
  print_list l2

let pop_x = Wlist.pop push_w
let pop_x' = Wlist.pop pop_x



let print_x =
  let (l1, l2, w) = pop_x in
  fprintf stdout "\nPOP_X\n";
  print_array w;
  fprintf stdout "\nL1: \n";
  print_list l1;
  fprintf stdout "\nL2: \n";
  print_list l2


let print_x' =
  let (l1, l2, w) = pop_x' in
  fprintf stdout "\nTEST\n";
  print_array w;
  fprintf stdout "\nL1: \n";
  print_list l1;
  fprintf stdout "\nL2: \n";
  print_list l2*)

(*let print_x'' =
  let (l1, l2, w) = pop_x' in
  fprintf stdout "\nPOP\n";
  print_array w;
  print_list l1;
  fprintf stdout "\nL2\n";
  print_list l2

let print_x''' =
  let (l1, l2, w) = pop_x'' in
  fprintf stdout "\nPOP\n";
  print_array w;
  print_list l1;
  fprintf stdout "\nL2\n";
  print_list l2*)

(*let pop_y' = Wlist.pop push_y

let print_popy' =
  fprintf stdout "POP:%i \n" pop_y'*)


(*type 'a t = 'a list ref

exception Empty

let create () = ref []

let add x w =
  try
    List.iter (fun x' -> if x = x' then raise Exit) !w;
    (* We did not find x, so add it. *)
    w := x :: !w
  with Exit -> ()

let push = add

let add_list l w =
  List.iter (fun e -> add e w) l

let take w = match !w with
  | h::tl -> w := tl; h
  | [] -> raise Empty

let filter f w =
  w := List.filter f !w

let pop = take

let peek w = match !w with
  | h::tl -> h
  | [] -> raise Empty

let top = peek

let all w = !w

let clear w = w := []

let is_empty w = !w = []

let length w = List.length !w

let iter f w = List.iter f !w

let fold f a w = List.fold_left f a !w*)
