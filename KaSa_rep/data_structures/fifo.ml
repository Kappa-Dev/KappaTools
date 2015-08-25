(**
  * fifo.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 2015, the 27th of July
  * Last modification: 
  * 
  * Work list - FIFO
  * 
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

open Set_and_map

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "FIFO") message exn (fun () -> default)

let trace = false

module type Work_list =
sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val push : Remanent_parameters_sig.parameters -> Exception.method_handler -> elt -> t -> Exception.method_handler * t
  val pop : Remanent_parameters_sig.parameters -> Exception.method_handler -> t -> Exception.method_handler * (elt option * t)
  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val print_wl : t -> unit
end
    
module WlMake (Ord: OrderedType) =
    (struct
        
      module WSet = Set_and_map.Make (Ord)

      type elt = Ord.t
      type t = elt list * elt list * WSet.set

      let empty = [], [], WSet.empty_set

      let is_empty x =
        let _, _, pool = x in
        WSet.is_empty_set pool

      let push parameter error e x =
        let in_list, out_list, pool = x in
        if WSet.mem_set e pool
        then
          error, x
        else
          let error, add_elt =
            WSet.add_set parameter error e pool
          in
          error, ((e :: in_list), out_list, add_elt)
            
      let rec pop parameter error x =
        let in_list, out_list, pool = x in
        if is_empty x
        then
          error, (None, x)
        else
          begin
            match out_list with
              | [] -> pop parameter error ([], (List.rev in_list), pool)
              | h :: tl ->
                let error, remove_elt =
                  WSet.remove parameter error h pool
                in
                error, ((Some h), (in_list, tl, remove_elt))
          end

      let fold_left f acc x =
        let in_list, out_list, _ = x in
        List.fold_left f (List.fold_left f acc out_list) (List.rev in_list)

      let print_wl wl = 
        let _ = fold_left 
          (fun  unit a -> Printf.fprintf stdout "%i " a)
          () wl
        in 
        print_newline () 

     end)

module Int = struct type t = int let compare = compare end
module IntWL = WlMake(Int)
