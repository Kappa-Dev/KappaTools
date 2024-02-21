(**
  * fifo.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 27th of July
  * Last modification: Time-stamp: <Aug 06 2016>
  *
  * Work list - FIFO
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

open SetMap

let local_trace = false

module type Work_list = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool

  val push :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    elt ->
    t ->
    Exception.method_handler * t

  val pop :
    Remanent_parameters_sig.parameters ->
    Exception.method_handler ->
    t ->
    Exception.method_handler * (elt option * t)

  val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
  val print_wl : Remanent_parameters_sig.parameters -> t -> unit
end

module WlMake (Ord : OrderedType with type t = int) = struct
  module WSetMap = Map_wrapper.Make (SetMap.Make (Ord))
  module WSet = WSetMap.Set

  type elt = Ord.t
  type t = elt list * elt list * WSet.t

  let empty = [], [], WSet.empty

  let is_empty x =
    let _, _, pool = x in
    WSet.is_empty pool

  let push parameter error e x =
    let in_list, out_list, pool = x in
    if WSet.mem e pool then
      error, x
    else (
      let error', add_elt = WSet.add parameter error e pool in
      let error =
        Exception.check_point Exception.warn parameter error error' __POS__ Exit
      in
      error, (e :: in_list, out_list, add_elt)
    )

  let fold_left f acc x =
    let in_list, out_list, _ = x in
    List.fold_left f (List.fold_left f acc out_list) (List.rev in_list)

  let print_wl parameters wl =
    (*let _ = fold_left
      (fun  () a -> Printf.fprintf (Remanent_parameters.get_log parameters) "%i " a)
      () wl
      in
      (*print_newline()*)
      let _ = print_newline () in*)
    let _, _, set = wl in
    WSet.iter
      (fun i ->
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "%i " i)
      set;
    Loggers.fprintf (Remanent_parameters.get_logger parameters) "\n"

  let rec pop parameter error x =
    let in_list, out_list, pool = x in
    if is_empty x then
      error, (None, x)
    else (
      match out_list with
      | [] -> pop parameter error ([], List.rev in_list, pool)
      | h :: tl ->
        let error, remove_elt = WSet.remove parameter error h pool in
        error, (Some h, (in_list, tl, remove_elt))
    )

  (*for debug*)
  (* let rec pop parameter error x =
     let in_list, out_list, pool = x in
     if is_empty x
     then
     error, (None, x)
     else
     begin
     match out_list with
     | [] -> pop parameter error ([], (List.rev in_list), pool)
     | h :: tl ->
     let _ = Printf.fprintf  (Remanent_parameters.get_log parameter)
     "BEFORE REMOVE %i " h in
     let _ =  WSet.iter (fun i ->
     Printf.fprintf (Remanent_parameters.get_log parameter) "%i " i) pool in
     let error,remove_elt = WSet.remove parameter error h pool in
     let _ =  WSet.iter (fun i ->
     Printf.fprintf (Remanent_parameters.get_log parameter) "%i " i) remove_elt
     in
     error, ((Some h), (in_list, tl, remove_elt))
     end*)

  (*for debug*)
  (*let push p e f x =
    let _ = Printf.fprintf  (Remanent_parameters.get_log p) "BEFORE PUUSH %i\n " f in
    let _ = print_wl p x in
    let error,wl = push p e f x in
    let _ = Printf.fprintf (Remanent_parameters.get_log p) "OUTPUT\n" in
    let _ = print_wl p wl in
    let _ = Printf.fprintf (Remanent_parameters.get_log p) "\n" in
    error,wl *)
end

module IntWL = Mods.IntMap
