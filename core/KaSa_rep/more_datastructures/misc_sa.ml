(**
  * misc_sa.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 16/12/2010
  * Last modification: Time-stamp: <Jul 02 2016>
  * *
  * Various functions
  *
  * Copyright 2010 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let const_unit _ = ()

let array_of_list create set parameters error list =
  let n = List.length list in
  let a = create parameters error n in
  let rec aux l k a =
    match l with
    | [] -> a
    | t :: q -> aux q (k + 1) (set parameters (fst a) k t (snd a))
  in
  aux list 0 a

let unsome (error, x) f =
  match x with
  | None -> f error
  | Some x -> error, x

let rev_inter_list compare l1 l2 =
  let rec aux l1 l2 rep =
    match l1, l2 with
    | [], _ | _, [] -> List.rev rep
    | a :: b, c :: d ->
      if compare a c = 0 then
        aux b d (a :: rep)
      else if compare a c < 0 then
        aux b l2 rep
      else
        aux l1 d rep
  in
  aux l1 l2 []

let rev_inter_list_with_and and_fun compare parameters handler error l1 l2 =
  let rec aux error handler l1 l2 rep =
    match l1, l2 with
    | [], _ | _, [] -> error, handler, List.rev rep
    | (a, a') :: b, (c, c') :: d ->
      if compare a c = 0 then (
        let error, handler, answer = and_fun parameters handler error a' c' in
        aux error handler b d ((a, answer) :: rep)
      ) else if compare a c < 0 then
        aux error handler b l2 rep
      else
        aux error handler l1 d rep
  in
  aux error handler l1 l2 []

let trace parameters string =
  if
    parameters.Remanent_parameters_sig.marshalisable_parameters
      .Remanent_parameters_sig.trace
  then
    Loggers.fprintf
      (Remanent_parameters.get_logger parameters)
      "%s%s"
      parameters.Remanent_parameters_sig.marshalisable_parameters
        .Remanent_parameters_sig.prefix (string ())

let inter_list compare l1 l2 = List.rev (rev_inter_list compare l1 l2)

let inter_list_with_and fun_and compare parameters handler error l1 l2 =
  let error, handler, rep =
    rev_inter_list_with_and fun_and compare parameters handler error l1 l2
  in
  error, handler, List.rev rep

let list_0_n k =
  let rec aux k sol =
    if k < 0 then
      sol
    else
      aux (k - 1) (k :: sol)
  in
  aux k []

let list_minus l1 l2 =
  let rec aux l1 l2 rep =
    match l1, l2 with
    | t1 :: q1, t2 :: q2 when t1 = t2 -> aux q1 q2 rep
    | t1 :: q1, _ -> aux q1 l2 (t1 :: rep)
    | [], _ -> rep
  in
  List.rev (aux l1 l2 [])

let print_comma parameter bool comma =
  if bool then
    Loggers.fprintf (Remanent_parameters.get_logger parameter) "%s" comma

let fetch_array i array def =
  try
    match array.(i) with
    | None -> def
    | Some i -> i
  with _ -> def
