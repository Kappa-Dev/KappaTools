(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

val toggle_element :
  (Api_types_j.simulation_info option -> bool) ->
  [< Html_types.div_content_fun ] Html.elt list ->
  [> Html_types.div ] Html.elt

val badge :
  (Api_types_j.simulation_info option -> int) ->
  [> `PCDATA | `Span ] Html.elt Tyxml_js.Wrap.tlist

val label_news :
  bool Tyxml_js.Wrap.t ->
  (Api_types_j.simulation_info option -> int) ->
  [> `PCDATA | `Span ] Html.elt Tyxml_js.Wrap.tlist
