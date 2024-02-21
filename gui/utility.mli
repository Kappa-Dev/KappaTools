(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

val print_newline : ([> `PCDATA ] as 'a) Html.elt list -> 'a Html.elt list

val print_string :
  string Html.wrap -> ([> `PCDATA ] as 'a) Html.elt list -> 'a Html.elt list

val print_site_graph :
  (string Html.wrap
  * (string Html.wrap
    * string Html.wrap option
    * Public_data.binding_state option
    * (int option * int option) option)
    list)
  list ->
  ([> `PCDATA ] as 'a) Html.elt list ->
  'a Html.elt list

val print_method_handler :
  Exception_without_parameter.method_handler -> [> Html_types.p ] Html.elt list
