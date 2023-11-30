(**
   * mvbdu.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 08/03/2010
   * Last modification: Time-stamp: <Feb 21 2018>
   * *
   * This library provides primitives to deal set of finite maps from integers to integers
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let build_reversed_sorted_list_aux allocate parameters error handler list
    already =
  List.fold_left
    (fun (error, (handler, already)) (var, asso) ->
      let error, output =
        List_core.build_list allocate error handler
          (List_sig.Cons
             {
               List_sig.variable = var;
               List_sig.association = asso;
               List_sig.tail = already.List_sig.id;
             })
          (List_sig.Cons
             {
               List_sig.variable = var;
               List_sig.association = asso;
               List_sig.tail = already;
             })
      in
      match output with
      | Some (_key, _cell, list, handler) -> error, (handler, list)
      | None -> Exception.warn parameters error __POS__ Exit (handler, already))
    (error, (handler, already))
    list

let build_reversed_sorted_list allocate parameters error handler list =
  let error, output =
    List_core.build_list allocate error handler List_sig.Empty List_sig.Empty
  in
  match output with
  | Some (_key, _cell, empty_list, handler) ->
    build_reversed_sorted_list_aux allocate parameters error handler list
      empty_list
  | None ->
    Exception.warn parameters error __POS__ Exit
      (handler, { List_sig.id = 0; List_sig.value = List_sig.Empty })

let build_sorted_list allocate parameters error handler list =
  build_reversed_sorted_list allocate parameters error handler (List.rev list)

let build_list allocate parameters error handler list =
  let sort (i, _) (j, _) = -compare i j in
  build_reversed_sorted_list allocate error parameters handler
    (List.sort sort list)

let rec print_cell parameter cell =
  match cell with
  | List_sig.Empty ->
    let s = "[]" in
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s"
        (Remanent_parameters.get_prefix parameter)
        s
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    ()
  | List_sig.Cons x ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s(site_type:%i = %i)"
        (Remanent_parameters.get_prefix parameter)
        x.List_sig.variable x.List_sig.association
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let parameter = Remanent_parameters.update_prefix parameter " " in
    let _ = print_association_list parameter x.List_sig.tail in
    ()

and print_association_list parameter list =
  let _ =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%sId=%i"
      (Remanent_parameters.get_prefix parameter)
      list.List_sig.id
  in
  let _ = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let _ =
    print_cell
      (Remanent_parameters.update_prefix parameter " ")
      list.List_sig.value
  in
  ()

let rec print_cell parameter cell =
  match cell with
  | List_sig.Empty ->
    let s = "[]" in
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s%s"
        (Remanent_parameters.get_prefix parameter)
        s
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    ()
  | List_sig.Cons x ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameter)
        "%s(site_type:%i)"
        (Remanent_parameters.get_prefix parameter)
        x.List_sig.variable
    in
    let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
    let _ =
      print_variables_list
        (Remanent_parameters.update_prefix parameter " ")
        x.List_sig.tail
    in
    ()

and print_variables_list parameter list =
  let () =
    Loggers.fprintf
      (Remanent_parameters.get_logger parameter)
      "%sId=%i"
      (Remanent_parameters.get_prefix parameter)
      list.List_sig.id
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameter) in
  let () =
    print_cell
      (Remanent_parameters.update_prefix parameter " ")
      list.List_sig.value
  in
  ()

let rec extensional_gen f get set error parameters handler list =
  match get parameters error handler list with
  | error, (handler, Some output) -> error, (handler, Some output)
  | error, (handler, None) ->
    (match list.List_sig.value with
    | List_sig.Empty -> error, (handler, Some [])
    | List_sig.Cons a1 ->
      let error, (handler, tail) =
        extensional_gen f get set error parameters handler a1.List_sig.tail
      in
      (match tail with
      | Some tail ->
        let output = f a1 :: tail in
        let error, handler = set parameters error handler list output in
        error, (handler, Some output)
      | None -> Exception.warn parameters error __POS__ Exit (handler, Some [])))

let extensional_with_asso get set error parameters handler list =
  extensional_gen
    (fun a1 -> a1.List_sig.variable, a1.List_sig.association)
    get set error parameters handler list

let extensional_without_asso get set error parameters handler list =
  extensional_gen
    (fun a1 -> a1.List_sig.variable)
    get set error parameters handler list

let rec length allocate get set error parameters handler list =
  match get parameters error handler list with
  | error, (handler, Some output) -> error, (handler, output)
  | error, (handler, None) ->
    let error, (handler, output) =
      match list.List_sig.value with
      | List_sig.Empty -> error, (handler, 0)
      | List_sig.Cons a ->
        let error, (handler, tail_size) =
          length allocate get set error parameters handler a.List_sig.tail
        in
        error, (handler, tail_size + 1)
    in
    let error, handler = set parameters error handler list output in
    error, (handler, output)

let rec overwrite allocate get set error parameters handler list1 list2 =
  match get parameters error handler (list1, list2) with
  | error, (handler, Some output) -> error, (handler, Some output)
  | error, (handler, None) ->
    let error, (handler, output) =
      match list1.List_sig.value, list2.List_sig.value with
      | List_sig.Empty, _ -> error, (handler, list2)
      | _, List_sig.Empty -> error, (handler, list1)
      | List_sig.Cons a1, List_sig.Cons a2 ->
        let var1 = a1.List_sig.variable in
        let var2 = a2.List_sig.variable in
        let cmp = compare var1 var2 in
        let var, asso, tail1, tail2 =
          if cmp < 0 then
            var1, a1.List_sig.association, a1.List_sig.tail, list2
          else if cmp = 0 then
            var1, a2.List_sig.association, a1.List_sig.tail, a2.List_sig.tail
          else
            var2, a2.List_sig.association, list1, a2.List_sig.tail
        in
        let error, (handler, tail) =
          overwrite allocate get set error parameters handler tail1 tail2
        in
        (match tail with
        | Some tail ->
          let error, output =
            List_core.build_list allocate error handler
              (List_sig.Cons
                 {
                   List_sig.variable = var;
                   List_sig.association = asso;
                   List_sig.tail = tail.List_sig.id;
                 })
              (List_sig.Cons
                 {
                   List_sig.variable = var;
                   List_sig.association = asso;
                   List_sig.tail;
                 })
          in
          (match output with
          | Some (_key, _cell, list, handler) -> error, (handler, list)
          | None ->
            Exception.warn parameters error __POS__ Exit
              (handler, { List_sig.id = 0; List_sig.value = List_sig.Empty }))
        | None ->
          Exception.warn parameters error __POS__ Exit
            (handler, { List_sig.id = 0; List_sig.value = List_sig.Empty }))
    in
    let error, handler = set parameters error handler (list1, list2) output in
    error, (handler, Some output)
