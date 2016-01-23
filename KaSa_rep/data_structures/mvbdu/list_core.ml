(**
    * list_core.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 08/03/2010
    * Last modification: 23/11/2010
    * *
    * This library provides primitives to deal associations list
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let sanity_check = true
let test_workbench = false

let invalid_arg parameters mh message exn value =
  Exception.warn parameters mh (Some "Mvbdu") message exn (fun () -> value)

let get_hash_key list = list.List_sig.id

let list_equal a b = a==b

let get_skeleton prelist =
  match prelist with
    | List_sig.Empty -> List_sig.Empty
    | List_sig.Cons x ->
      List_sig.Cons
        { x with
          List_sig.tail = get_hash_key x.List_sig.tail}

let build_list allocate error handler skeleton cell =
  allocate
    error
    compare
    skeleton
    cell
    (fun key -> {List_sig.id = key; List_sig.value = cell})
    handler

let id_of_list x = x.List_sig.id

let update_association_dictionary handler dictionary =
  if handler.Memo_sig.association_list_dictionary == dictionary
  then
    handler
  else
    {handler with Memo_sig.association_list_dictionary = dictionary}

let update_variables_dictionary handler dictionary =
  if handler.Memo_sig.variables_list_dictionary == dictionary
  then
    handler
  else
    {handler with Memo_sig.variables_list_dictionary = dictionary}

let rec print_list error print_empty string_of_var string_of_value (parameters:Remanent_parameters_sig.parameters) list =
  match list.List_sig.value with
    | List_sig.Empty -> print_empty error parameters
    | List_sig.Cons x ->
       let parameters' = Remanent_parameters.update_prefix parameters " " in
      let _ =
        Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "%s (%d )%s=%s;"
          parameters.Remanent_parameters_sig.marshalisable_parameters.Remanent_parameters_sig.prefix
          (id_of_list list)
	  (string_of_var x.List_sig.variable)
          (string_of_value x.List_sig.association)
      in
      let _ =
	Loggers.print_newline (Remanent_parameters.get_logger parameters)
      in
      let error =
        print_list
          error
	  print_empty
	  string_of_var
	  string_of_value
	  parameters'
	  x.List_sig.tail
      in
      error
