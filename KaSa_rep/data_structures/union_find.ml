(**
    * union_find.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2015, the 11th of March
    * Last modification: 
    * * 
    * This library provides primitives to deal with union find algorithm with
    * ranks and path compression
    *  
    * Copyright 2010,2011 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)

(*TODO*)
module Union_find_array =
  functor (Basic:Storage with type key = int and type dimension = int) ->
  (struct
      type key = int
      type dimension = int
      type 'a t =
        {
          mutable array: 'a array; (*for path-compression *)
          size : int
        }

      let dimension error a = error, a.size
      let key_list = Basic.key_list
      let get = Basic.get
      let unsafe_get = Basic.unsafe_get
      let init = Basic.init
      let set = Basic.set
      let print = Basic.print
      let print_var_f = Basic.print_var_f
      let print_site_f = Basic.print_site_f
      let iter = Basic.iter
      let fold = Basic.fold
      let fold2_common = Basic.fold2_common
                     
      let rec create parameters error size =
        if size < 0
        then
          let error, array = create parameters error 0 in
          invalid_arg parameters error (Some "create, line 60") Exit array
        else
        error,
          {array = Array.make (size+1);
           size = size
          }

      (*let find_parent parent i =
        let rec find j =
          if parent.(j) = j
          then j
          else find parent.(j)
        in
        find i*)

      (* path-compression *)

      let rec path_compression parent i =
        if parent.(i) = i
        then
          parent, i
        else
          let parent, j = path_compression parent j in
          let parent_j = parent.(i) <- j in
          parent_j, j

      (* find_parent *)

      let find_parent {parent; _} i =
        let a, rx = path_compression parent i in
        parent <- a;
        rx

      let union_rank {parent; _} i j =
        let parent_i = find_parent parent i in
        let parent_j = find_parent parent j in
        if parent_i < parent_j
        then
          begin
            parent.(parent_i) <- parent.(parent_j);
            parent_j = parent_j + parent_i
          end
        else
          begin
            parent.(parent_j) <- parent.(parent_i);
            parent_i = parent_i + parent_j
          end

      let is_connected_ranked {parent; _} i j =
        (find_parent parent i) = (find_parent parent j)
      
  end:Storage with type key = int and type dimension = int)
