(**
   * mvbdu_core.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2010, the 8th of March
   * Last modification: Time-stamp: <Dec 20 2018>
   * *
   * This library provides primitives to deal set of finite maps from integers to integers
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let sanity_check = true
let test_workbench = false
let get_hash_key mvbdu = mvbdu.Mvbdu_sig.id
let mvbdu_equal a b = a == b

type ('a, 'b) reduction =
  | Fail
  | Remove of 'a
  | Guard_ub of 'b
  | Guard_lb of 'b

let get_skeleton cell =
  match cell with
  | Mvbdu_sig.Leaf x -> Mvbdu_sig.Leaf x
  | Mvbdu_sig.Node x ->
    Mvbdu_sig.Node
      {
        x with
        Mvbdu_sig.branch_true = get_hash_key x.Mvbdu_sig.branch_true;
        Mvbdu_sig.branch_false = get_hash_key x.Mvbdu_sig.branch_false;
      }

let print_flag parameters bool =
  if bool then
    Loggers.fprintf (Remanent_parameters.get_logger parameters) "Yes"
  else
    Loggers.fprintf (Remanent_parameters.get_logger parameters) "No"

let build_already_compressed_cell allocate error handler skeleton cell =
  allocate error compare skeleton cell
    (fun key -> { Mvbdu_sig.id = key; Mvbdu_sig.value = cell })
    handler

let boolean_pattern ~boolean_setting cell =
  match boolean_setting, cell with
  | _, Mvbdu_sig.Leaf _ -> Fail
  | (leave, boolean_predicate, false), Mvbdu_sig.Node x ->
    let var = x.Mvbdu_sig.variable in
    let bound = x.Mvbdu_sig.upper_bound in
    let branch_false = x.Mvbdu_sig.branch_false in
    let branch_true = x.Mvbdu_sig.branch_true in
    if boolean_predicate var then (
      match branch_false.Mvbdu_sig.value, branch_true.Mvbdu_sig.value with
      | Mvbdu_sig.Node x', _ ->
        let var' = x'.Mvbdu_sig.variable in
        let bound' = x'.Mvbdu_sig.upper_bound in
        let branch_true' = x'.Mvbdu_sig.branch_true in
        if var = var' && bound' >= 1 && bound <= -1 then
          Remove (var, branch_true', leave)
        (* if non b then A else if b then A else false ==> A *)
        else
          Fail
      | Mvbdu_sig.Leaf _, _ -> Fail
    ) else
      Fail
  | (leave, boolean_predicate, true), Mvbdu_sig.Node x ->
    let var = x.Mvbdu_sig.variable in
    let bound = x.Mvbdu_sig.upper_bound in
    let branch_false = x.Mvbdu_sig.branch_false in
    let branch_true = x.Mvbdu_sig.branch_true in
    if boolean_predicate var then (
      match branch_false.Mvbdu_sig.value, branch_true.Mvbdu_sig.value with
      | Mvbdu_sig.Leaf a, _ when not (a = leave) ->
        Guard_ub (var, bound, branch_true, branch_false, leave)
      | _, Mvbdu_sig.Leaf a when not (a = leave) ->
        Guard_lb (var, bound, branch_true, branch_false, leave)
      | Mvbdu_sig.Leaf _, _ -> Fail
      | Mvbdu_sig.Node x', _ ->
        let var' = x'.Mvbdu_sig.variable in
        let bound' = x'.Mvbdu_sig.upper_bound in
        let branch_true' = x'.Mvbdu_sig.branch_true in
        if var = var' && bound' >= 1 && bound <= -1 then
          Remove (var, branch_true', leave)
        (* if non b then A else if b then A else false ==> A *)
        else
          Fail
    ) else
      Fail

let deactivate (a, b, _) = a, b, false

let boolean_var ~boolean_setting var =
  let _, b, _ = boolean_setting in
  b var

let rec compress_node ~boolean_setting allocate error handler cell =
  match cell with
  | Mvbdu_sig.Leaf _a as x ->
    build_already_compressed_cell allocate error handler x x
  | Mvbdu_sig.Node x ->
    let variable = x.Mvbdu_sig.variable in
    let bound = x.Mvbdu_sig.upper_bound in
    let branch_true = x.Mvbdu_sig.branch_true in
    let branch_false = x.Mvbdu_sig.branch_false in
    if mvbdu_equal branch_true branch_false then
      ( error,
        Some
          ( get_hash_key branch_true,
            branch_true.Mvbdu_sig.value,
            branch_true,
            handler ) )
    else (
      match branch_false.Mvbdu_sig.value with
      | Mvbdu_sig.Node x'
        when boolean_var ~boolean_setting x'.Mvbdu_sig.variable
             && x'.Mvbdu_sig.variable = variable
             && x'.Mvbdu_sig.upper_bound = bound ->
        (*overwrite*)
        ( error,
          Some
            ( get_hash_key branch_false,
              branch_false.Mvbdu_sig.value,
              branch_false,
              handler ) )
      | Mvbdu_sig.Node x'
        when boolean_var ~boolean_setting x'.Mvbdu_sig.variable
             && x'.Mvbdu_sig.variable = variable
             && x'.Mvbdu_sig.upper_bound < bound ->
        (match
           compress_node
             ~boolean_setting:(deactivate boolean_setting)
             allocate error handler
             (Mvbdu_sig.Node
                {
                  Mvbdu_sig.variable;
                  Mvbdu_sig.upper_bound = x.Mvbdu_sig.upper_bound;
                  Mvbdu_sig.branch_true = x.Mvbdu_sig.branch_true;
                  Mvbdu_sig.branch_false = x'.Mvbdu_sig.branch_false;
                })
         with
        | error, None -> error, None
        | error, Some (_, _, y, handler) ->
          compress_node
            ~boolean_setting:(deactivate boolean_setting)
            allocate error handler
            (Mvbdu_sig.Node
               {
                 Mvbdu_sig.variable;
                 Mvbdu_sig.upper_bound = x'.Mvbdu_sig.upper_bound;
                 Mvbdu_sig.branch_true = x'.Mvbdu_sig.branch_true;
                 Mvbdu_sig.branch_false = y;
               }))
      | Mvbdu_sig.Node x when mvbdu_equal x.Mvbdu_sig.branch_true branch_true ->
        ( error,
          Some
            ( get_hash_key branch_false,
              branch_false.Mvbdu_sig.value,
              branch_false,
              handler ) )
      | Mvbdu_sig.Node _ | Mvbdu_sig.Leaf _ ->
        (match boolean_pattern ~boolean_setting cell with
        | Fail ->
          build_already_compressed_cell allocate error handler
            (Mvbdu_sig.Node
               {
                 Mvbdu_sig.variable;
                 Mvbdu_sig.upper_bound = bound;
                 Mvbdu_sig.branch_true = branch_true.Mvbdu_sig.id;
                 Mvbdu_sig.branch_false = branch_false.Mvbdu_sig.id;
               })
            (Mvbdu_sig.Node
               {
                 Mvbdu_sig.variable;
                 Mvbdu_sig.upper_bound = bound;
                 Mvbdu_sig.branch_true;
                 Mvbdu_sig.branch_false;
               })
        | Remove (_var, p, _leave) ->
          error, Some (get_hash_key p, p.Mvbdu_sig.value, p, handler)
        | Guard_ub (var, bound, branch_true, branch_false, leave) ->
          (match
             build_already_compressed_cell allocate error handler
               (Mvbdu_sig.Leaf leave) (Mvbdu_sig.Leaf leave)
           with
          | error, None -> error, None
          | error, Some (_, _, leave, handler) ->
            if bound >= 1 then
              compress_node
                ~boolean_setting:(deactivate boolean_setting)
                allocate error handler
                (Mvbdu_sig.Node
                   {
                     Mvbdu_sig.variable = var;
                     Mvbdu_sig.upper_bound = 1;
                     Mvbdu_sig.branch_true;
                     Mvbdu_sig.branch_false = leave;
                   })
            else (
              match
                compress_node
                  ~boolean_setting:(deactivate boolean_setting)
                  allocate error handler
                  (Mvbdu_sig.Node
                     {
                       Mvbdu_sig.variable = var;
                       Mvbdu_sig.upper_bound = 1;
                       Mvbdu_sig.branch_true = branch_false;
                       Mvbdu_sig.branch_false = leave;
                     })
              with
              | error, None -> error, None
              | error, Some (_, _, bdu, handler) ->
                compress_node
                  ~boolean_setting:(deactivate boolean_setting)
                  allocate error handler
                  (Mvbdu_sig.Node
                     {
                       Mvbdu_sig.variable = var;
                       Mvbdu_sig.upper_bound = bound;
                       Mvbdu_sig.branch_true;
                       Mvbdu_sig.branch_false = bdu;
                     })
            ))
        | Guard_lb (var, bound, branch_true, branch_false, leave) ->
          (match
             build_already_compressed_cell allocate error handler
               (Mvbdu_sig.Leaf leave) (Mvbdu_sig.Leaf leave)
           with
          | error, None -> error, None
          | error, Some (_, _, leave, handler) ->
            if bound = -1 then
              compress_node
                ~boolean_setting:(deactivate boolean_setting)
                allocate error handler
                (Mvbdu_sig.Node
                   {
                     Mvbdu_sig.variable = var;
                     Mvbdu_sig.upper_bound = -1;
                     Mvbdu_sig.branch_true = leave;
                     Mvbdu_sig.branch_false;
                   })
            else (
              match
                compress_node
                  ~boolean_setting:(deactivate boolean_setting)
                  allocate error handler
                  (Mvbdu_sig.Node
                     {
                       Mvbdu_sig.variable = var;
                       Mvbdu_sig.upper_bound = bound;
                       Mvbdu_sig.branch_true;
                       Mvbdu_sig.branch_false;
                     })
              with
              | error, None -> error, None
              | error, Some (_, _, bdu, handler) ->
                compress_node
                  ~boolean_setting:(deactivate boolean_setting)
                  allocate error handler
                  (Mvbdu_sig.Node
                     {
                       Mvbdu_sig.variable = var;
                       Mvbdu_sig.upper_bound = -1;
                       Mvbdu_sig.branch_true = leave;
                       Mvbdu_sig.branch_false = bdu;
                     })
            )))
    )

let rec print_mvbdu error print_leaf string_of_var parameters mvbdu =
  match mvbdu.Mvbdu_sig.value with
  | Mvbdu_sig.Leaf a -> print_leaf error parameters a
  | Mvbdu_sig.Node x ->
    let parameters' = Remanent_parameters.update_prefix parameters " " in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s if(mvbdu_id:%d) %s < %d then "
        parameters.Remanent_parameters_sig.marshalisable_parameters
          .Remanent_parameters_sig.prefix mvbdu.Mvbdu_sig.id
        (string_of_var x.Mvbdu_sig.variable)
        (x.Mvbdu_sig.upper_bound + 1)
    in
    let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
    let error =
      print_mvbdu error print_leaf string_of_var parameters'
        x.Mvbdu_sig.branch_true
    in
    let _ =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters)
        "%s else "
        parameters.Remanent_parameters_sig.marshalisable_parameters
          .Remanent_parameters_sig.prefix
    in
    let _ = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
    let error =
      print_mvbdu error print_leaf string_of_var parameters'
        x.Mvbdu_sig.branch_false
    in
    error

let id_of_mvbdu x = x.Mvbdu_sig.id

let update_dictionary handler dictionary =
  if handler.Memo_sig.mvbdu_dictionary == dictionary then
    handler
  else
    { handler with Memo_sig.mvbdu_dictionary = dictionary }

let last_entry parameter handler error last_entry =
  let dic = handler.Memo_sig.mvbdu_dictionary in
  last_entry parameter error dic
