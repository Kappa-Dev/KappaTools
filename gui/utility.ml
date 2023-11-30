(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let print_string s list = Html.txt s :: list
let print_newline list = print_string "\n" list

let print_single_binding_state a list =
  print_string Public_data.binding_state_opening_backend_symbol
    (print_string a
       (print_string Public_data.binding_state_closing_backend_symbol list))

let print_single_internal_state a list =
  print_string Public_data.internal_state_opening_backend_symbol
    (print_string a
       (print_string Public_data.internal_state_closing_backend_symbol list))

let print_counter_interval (a, b) list =
  let open_range, inf =
    match a with
    | None ->
      ( Public_data.open_interval_exclusive_symbol,
        Public_data.minus_infinity_symbol )
    | Some a -> Public_data.open_interval_inclusive_symbol, string_of_int a
  in
  let close_range, sup =
    match b with
    | None ->
      ( Public_data.close_interval_exclusive_symbol,
        Public_data.plus_infinity_symbol )
    | Some b -> Public_data.close_interval_inclusive_symbol, string_of_int b
  in
  print_string open_range
    (print_string inf
       (print_string Public_data.counter_state_range_backend_symbol
          (print_string sup (print_string close_range list))))

let print_counter_state (a, b) list =
  print_string Public_data.counter_state_opening_backend_symbol
    (print_counter_interval (a, b)
       (print_string Public_data.counter_state_closing_backend_symbol list))

let print_site site list =
  let site_name, prop_opt, binding_opt, counter_opt = site in
  let list =
    match binding_opt with
    | None -> print_string Public_data.missing_binding_site_backend_symbol list
    | Some Public_data.Free ->
      print_single_binding_state Public_data.free_backend_symbol list
    | Some Public_data.Wildcard ->
      print_single_binding_state Public_data.wildcard_backend_symbol list
    | Some Public_data.Bound_to_unknown ->
      print_single_binding_state Public_data.bound_to_unknown_backend_symbol
        list
    | Some (Public_data.Binding_type (agent_name, site_name)) ->
      let binding_type_symbol = Public_data.binding_type_backend_symbol in
      print_single_binding_state
        (Public_data.string_of_binding_type ~binding_type_symbol ~agent_name
           ~site_name ())
        list
    | Some (Public_data.Bound_to i) ->
      print_single_binding_state (string_of_int i) list
  in
  let list =
    match counter_opt with
    | None -> list
    | Some a -> print_counter_state a list
  in
  let list =
    match prop_opt with
    | None -> list
    | Some a -> print_single_internal_state a list
  in
  print_string site_name list

let print_agent agent list =
  let agent_name, interface = agent in
  let list = print_string ")" list in
  let list =
    snd
      (List.fold_left
         (fun (b, list) site ->
           let list =
             if b then
               print_string "," list
             else
               list
           in
           let list = print_site site list in
           true, list)
         (false, list) interface)
  in
  let list = Html.txt "(" :: list in
  let list = Html.txt agent_name :: list in
  list

let print_site_graph agent_list list =
  snd
    (List.fold_left
       (fun (b, list) agent ->
         let list =
           if b then
             print_string "," list
           else
             list
         in
         true, print_agent agent list)
       (false, list) (List.rev agent_list))

let print_method_handler mh =
  let uncaught = Exception_without_parameter.get_uncaught_exception_list mh in
  let caught = Exception_without_parameter.get_caught_exception_list mh in
  List.fold_right
    (fun x l ->
      Html.p
        [
          Html.txt
            (Format.asprintf "%a" Exception_without_parameter.pp_caught x);
        ]
      :: l)
    caught
    (List.map
       (fun x ->
         Html.p
           [
             Html.txt
               (Format.asprintf "%a" Exception_without_parameter.pp_uncaught x);
           ])
       uncaught)
