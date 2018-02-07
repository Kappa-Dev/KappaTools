(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

val split : string -> char -> string * string option
val print_newline: ([> `PCDATA ] as 'a) Html.elt list -> 'a Html.elt list
val print_string:
  string Html.wrap ->
  ([> `PCDATA ] as 'a) Html.elt list -> 'a Html.elt list
val print_site_graph:
  (string Html.wrap *
   (string Html.wrap * string Html.wrap option *
    Public_data.binding_state option)
     list)
    list -> ([> `PCDATA ] as 'a) Html.elt list -> 'a Html.elt list
