(**
   * mvbdu_sanity.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 08/03/2010
   * Last modification: Time-stamp: <Jan 01 2017>
   * *
   * This library provides primitives to check that set of finite maps are well-formed
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let rec safety_equal_mvbdu_working_list working_list mvbdu_x mvbdu_y =
  match mvbdu_x.Mvbdu_sig.value, mvbdu_y.Mvbdu_sig.value with
  | Mvbdu_sig.Leaf a, Mvbdu_sig.Leaf b when a = b ->
    (match working_list with
    | [] -> true
    | (a, b) :: tail -> safety_equal_mvbdu_working_list tail a b)
  | Mvbdu_sig.Node x, Mvbdu_sig.Node y ->
    x.Mvbdu_sig.variable = y.Mvbdu_sig.variable
    && x.Mvbdu_sig.upper_bound = y.Mvbdu_sig.upper_bound
    && safety_equal_mvbdu_working_list
         ((x.Mvbdu_sig.branch_false, y.Mvbdu_sig.branch_false) :: working_list)
         x.Mvbdu_sig.branch_true y.Mvbdu_sig.branch_true
  | Mvbdu_sig.Leaf _, _ | _, Mvbdu_sig.Leaf _ -> false

let safety_equal_mvbdu a b = safety_equal_mvbdu_working_list [] a b

let rec safety_equal_list list_x list_y =
  match list_x.List_sig.value, list_y.List_sig.value with
  | List_sig.Empty, List_sig.Empty -> true
  | List_sig.Cons x, List_sig.Cons y ->
    x.List_sig.variable = y.List_sig.variable
    && x.List_sig.association = y.List_sig.association
    && safety_equal_list x.List_sig.tail y.List_sig.tail
  | List_sig.Empty, _ | _, List_sig.Empty -> false

let rec safety_compare_nodes_working_list working_list =
  match working_list with
  | [] -> 0
  | (Mvbdu_sig.Leaf a, Mvbdu_sig.Leaf b) :: tail ->
    let cmp1 = compare a b in
    if cmp1 = 0 then
      safety_compare_nodes_working_list tail
    else
      cmp1
  | (Mvbdu_sig.Leaf _, _) :: _ -> 1
  | (_, Mvbdu_sig.Leaf _) :: _ -> -1
  | (Mvbdu_sig.Node x, Mvbdu_sig.Node y) :: _ ->
    let cmp2 = compare x.Mvbdu_sig.variable y.Mvbdu_sig.variable in
    if cmp2 = 0 then (
      let cmp3 = compare x.Mvbdu_sig.upper_bound y.Mvbdu_sig.upper_bound in
      if cmp3 = 0 then
        safety_compare_nodes_working_list
          (( x.Mvbdu_sig.branch_true.Mvbdu_sig.value,
             y.Mvbdu_sig.branch_false.Mvbdu_sig.value )
          :: ( x.Mvbdu_sig.branch_false.Mvbdu_sig.value,
               y.Mvbdu_sig.branch_false.Mvbdu_sig.value )
          :: working_list)
      else
        cmp3
    ) else
      cmp2

let safety_compare_nodes a b =
  safety_compare_nodes_working_list [ a.Mvbdu_sig.value, b.Mvbdu_sig.value ]

let rec safety_check_maximal_sharing_working_list
    (allocate_uniquely : ('a, 'b, 'c, 'd, 'e) Sanity_test_sig.f) error
    working_list handler =
  match working_list with
  | [] -> error, true, handler
  | mvbdu :: tail ->
    (* check that mvbdu is uniquely represented in memory *)
    let error, output =
      try
        allocate_uniquely error compare
          (Mvbdu_core.get_skeleton mvbdu.Mvbdu_sig.value)
          mvbdu.Mvbdu_sig.value
          (fun key ->
            { Mvbdu_sig.id = key; Mvbdu_sig.value = mvbdu.Mvbdu_sig.value })
          handler
      with _ -> error, None
    in
    (match output with
    | None -> error, false, handler
    | Some (_i, _asso, _asso_id, handler) ->
      (match mvbdu.Mvbdu_sig.value with
      | Mvbdu_sig.Leaf _ ->
        safety_check_maximal_sharing_working_list allocate_uniquely error tail
          handler
      | Mvbdu_sig.Node x ->
        safety_check_maximal_sharing_working_list allocate_uniquely error
          (x.Mvbdu_sig.branch_true :: x.Mvbdu_sig.branch_false :: tail)
          handler))

let safety_check_maximal_sharing
    (allocate_uniquely : ('a, 'b, 'c, 'd, 'e) Sanity_test_sig.f) error mvbdu =
  safety_check_maximal_sharing_working_list allocate_uniquely error [ mvbdu ]

let rec safety_check_maximaly_compressed_working_list error working_list =
  match working_list with
  | [] -> error, true
  | head :: tail ->
    (match head.Mvbdu_sig.value with
    | Mvbdu_sig.Leaf _ ->
      safety_check_maximaly_compressed_working_list error tail
    | Mvbdu_sig.Node x ->
      (* check that mvbdu is maximally compressed *)
      if
        x.Mvbdu_sig.branch_true == x.Mvbdu_sig.branch_false
        (*sibbling should be different*)
      then
        error, false
      else (
        match x.Mvbdu_sig.branch_false.Mvbdu_sig.value with
        | Mvbdu_sig.Leaf _ ->
          safety_check_maximaly_compressed_working_list error tail
        | Mvbdu_sig.Node y ->
          if
            x.Mvbdu_sig.branch_true == y.Mvbdu_sig.branch_true
            (*successive true_sibbling should be different*)
          then
            error, false
          else
            safety_check_maximaly_compressed_working_list error
              (x.Mvbdu_sig.branch_false :: x.Mvbdu_sig.branch_true :: tail)
      ))

let safety_check_maximaly_compressed error mvbdu =
  safety_check_maximaly_compressed_working_list error [ mvbdu ]

let rec safety_check_increasing_nodes_working_list error working_list =
  match working_list with
  | [] -> error, true
  | (mvbdu, bool, var, bound) :: tail ->
    (match mvbdu.Mvbdu_sig.value with
    | Mvbdu_sig.Leaf _ -> safety_check_increasing_nodes_working_list error tail
    | Mvbdu_sig.Node x ->
      let new_var = x.Mvbdu_sig.variable in
      (match compare var new_var with
      | a when a < 0 ->
        safety_check_increasing_nodes_working_list error
          ((x.Mvbdu_sig.branch_false, false, new_var, x.Mvbdu_sig.upper_bound)
          :: (x.Mvbdu_sig.branch_true, true, new_var, x.Mvbdu_sig.upper_bound)
          :: tail)
      | a when a > 0 -> error, false
      | _ ->
        if bool then
          error, false
        else (
          let new_bound = x.Mvbdu_sig.upper_bound in
          if compare bound new_bound >= 0 then
            error, false
          else
            safety_check_increasing_nodes_working_list error
              ((x.Mvbdu_sig.branch_false, false, new_var, new_bound)
              :: (x.Mvbdu_sig.branch_true, true, new_var, new_bound)
              :: tail)
        )))

let safety_check_increasing_nodes error mvbdu =
  match mvbdu.Mvbdu_sig.value with
  | Mvbdu_sig.Leaf _ -> error, true
  | Mvbdu_sig.Node x ->
    let new_var = x.Mvbdu_sig.variable in
    let new_bound = x.Mvbdu_sig.upper_bound in
    safety_check_increasing_nodes_working_list error
      [
        x.Mvbdu_sig.branch_false, false, new_var, new_bound;
        x.Mvbdu_sig.branch_true, true, new_var, new_bound;
      ]

let print_flag log bool =
  if bool then
    Printf.fprintf log "Yes"
  else
    Printf.fprintf log "No"

let sanity_check (allocate_uniquely : ('a, 'b, 'c, 'd, 'e) Sanity_test_sig.f)
    error _log handler mvbdu =
  let error, bool1 = safety_check_increasing_nodes error mvbdu in
  let error, bool2, dictionary =
    safety_check_maximal_sharing allocate_uniquely error mvbdu handler
  in
  let error, bool3 = safety_check_maximaly_compressed error mvbdu in
  error, dictionary, (bool1, bool2, bool3)

let add_string m1 m2 =
  if m1 = "" then
    m2
  else if m2 = "" then
    m1
  else
    m1 ^ " / " ^ m2

let m = "Error during MVBDU sanity check!"

let m1true_instead_of_false =
  "Mvbdu_sig.Nodes/bounds were not decreasing, which was not detected"

let m1false_instead_of_true =
  "Mvbdu_sig.Nodes/bounds are detected to be not increasing, although they are"

let m2true_instead_of_false =
  "Representation in memory is not unique, but it was not detected"

let m2false_instead_of_true =
  "Representation in memory is detected to be non unique, although it is"

let m3true_instead_of_false =
  "MVBDU is not maximally compressed, which was not detected"

let m3false_instead_of_true =
  "MVBDU is maximally compressed, which was not detected"

let test handler (b1, b2, b3) bdu =
  let error, mvbdu_handler, (c1, c2, c3) =
    sanity_check handler.Sanity_test_sig.allocate_uniquely_mvbdu
      handler.Sanity_test_sig.error handler.Sanity_test_sig.output
      handler.Sanity_test_sig.mvbdu_handler bdu
  in
  let handler =
    { handler with Sanity_test_sig.error; Sanity_test_sig.mvbdu_handler }
  in
  if c1 = b1 && c2 = b2 && c3 = b3 then
    handler, true, None
  else
    ( handler,
      false,
      Some
        (add_string
           (if c1 then
              if not b1 then
                m1true_instead_of_false
              else
                ""
            else if b1 then
              m1false_instead_of_true
            else
              "")
           (add_string
              (if c2 then
                 if not b2 then
                   m2true_instead_of_false
                 else
                   ""
               else if b2 then
                 m2false_instead_of_true
               else
                 "")
              (if c3 then
                 if not b3 then
                   m3true_instead_of_false
                 else
                   ""
               else if b3 then
                 m3false_instead_of_true
               else
                 ""))) )
