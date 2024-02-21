(**
    * union_find.ml
    * openkappa
    * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 2015, the 11th of March
    * Last modification: Time-stamp: <Apr 18 2018>
    * *
    * This library provides primitives to deal with union find algorithm with
    * path compression
    *
    * Copyright 2010,2011 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    *  under the terms of the GNU Library General Public License *)

module type Union_find = sig
  type key
  type dimension
  type t

  val create :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    dimension ->
    Exception.method_handler * t

  val union_list :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    key list ->
    Exception.method_handler * t

  val iteri :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    (Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    key ->
    Exception.method_handler) ->
    t ->
    Exception.method_handler

  val get_representent :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    key ->
    t ->
    Exception.method_handler * t * key
end

module Make =
functor
  (Storage : Int_storage.Storage)
  ->
  (*    functor (Map: Map_wrapper.Map_with_logs) ->*)
    (
    struct
      type key = Storage.key
      type t = key Storage.t
      type dimension = Storage.dimension

      let create parameters error n =
        Storage.init parameters error n (fun _ e x -> e, x)

      (************************************************************************************)
      (* findSet(e): which return a pointer to the representative of the set
         containing e. Since the set are disjoint, e containted in one set
         only. Therefore, the returned representative can be uniquely determined.
      *)

      let findSet parameter error e t =
        let pointToRoot parameter error root l t =
          List.fold_left
            (fun (error, t) i -> Storage.set parameter error i root t)
            (error, t) l
        in
        let rec helper parameter error e l t =
          let error, parent = Storage.unsafe_get parameter error e t in
          match parent with
          | None -> error, (t, e)
          | Some p when p <> e -> helper parameter error p (e :: l) t
          | Some p ->
            (* base case: we hit the root node make all collected nodes on the
               path point to the root. And return the root afterwards *)
            let error, t = pointToRoot parameter error p l t in
            error, (t, p)
        in
        helper parameter error e [] t

      let get_representent parameters error e t =
        let error, (union, elt) = findSet parameters error e t in
        error, union, elt
      (*********************************************************************)
      (*UNION*)

      let union parameter error x y t =
        let error, (t, root_x) = findSet parameter error x t in
        let error, (t, root_y) = findSet parameter error y t in
        Storage.set parameter error root_x root_y t

      (*let eq_classes_map parameter error a =
        (*  let classes = Cckappa_sig.Site_map_and_set.Map.empty in*)
        =======
        let union x y a =
        let root_x = findSet x a in
        let root_y = findSet y a in
        let _ = a.(root_x) <- root_y in
        (*let _ = print_string "dump: "; dump a; print_string "\n" in*)
        a

        let eq_classes_map parameter error a =
        let classes = Ckappa_sig.Site_map_and_set.Map.empty in
        >>>>>>> remove doublicate module, todo: hidden type site_name
        let size = Array.length a in
        let rec aux k (classes,union_list) =
          if  k < 0
          then
            classes, union_list
          else
            (*find the parent of the union*)
            let rep = findSet k a in
            (*check if inside classes has already has this parent*)
        <<<<<<< HEAD
            let error',get_rep =
        (*        Cckappa_sig.Site_map_and_set.Map.find_default parameter error [] rep classes in*)
            let error = Exception.check warn parameter error error' (Some "line 73") Exit in
            (*store the result inside classes*)
        (*      let error,classes =
        Cckappa_sig.Site_map_and_set.Map.add_or_overwrite
        =======
            let error', get_rep =
              Ckappa_sig.Site_map_and_set.Map.find_default
                parameter
                error
                []
                rep
                classes
            in
            let error = Exception.check warn parameter error error' (Some "line 73") Exit in
            (*store the result inside classes*)
            let error,classes =
        Ckappa_sig.Site_map_and_set.Map.add_or_overwrite
        >>>>>>> remove doublicate module, todo: hidden type site_name
        parameter
        error
                rep
        (k :: get_rep)
        classes
            in*)
            aux (k - 1) (classes, union_list)
        in
        let classes, a = aux (size - 1) (classes, a) in
        classes, a*)

      (************************************************************************************)
      (* compute union-find in a list*)

      let union_list parameter error a (list : key list) =
        match list with
        | [] -> error, a
        | t :: q ->
          let rec aux parameter to_visit error a =
            match to_visit with
            | [] -> error, a
            | t' :: q' ->
              let error, union_array = union parameter error t t' a in
              aux parameter q' error union_array
          in
          aux parameter q error a

      let iteri = Storage.iter
    end :
      Union_find
        with type key = Storage.key
         and type t = Storage.key Storage.t
         and type dimension = Storage.dimension)
