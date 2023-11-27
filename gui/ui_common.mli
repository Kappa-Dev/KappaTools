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

val export_controls :
  export_select_id:string ->
  export_filename_id:string ->
  export_button_id:string ->
  export_data_label:string ->
  [> Html_types.div ] Html.elt

val badge :
  (Api_types_j.simulation_info option -> int) ->
  [> `PCDATA | `Span ] Html.elt Tyxml_js.Wrap.tlist

val arguments : string -> string list
val version : ?test:'a option -> prod:'a -> dev:'a -> 'a

module type Menu = sig
  val content :
    unit ->
    [> `Button | `Div | `Ul | `A of [> `PCDATA | `Span ] ] Tyxml_js.Html5.elt
    list

  val onload : unit -> unit
end

module type Div = sig
  val id : string
  val content : unit -> Html_types.div_content_fun Tyxml_js.Html.elt list
  val onload : unit -> unit
end

module type Tab = sig
  val navli :
    unit ->
    Html_types.flow5_without_interactive Tyxml_js.Html5.elt ReactiveData.RList.t

  val content : unit -> Html_types.div_content_fun Tyxml_js.Html5.elt list
  val onload : unit -> unit
  val onresize : unit -> unit
end

module type SubTab = sig
  include Tab

  val parent_hide : unit -> unit
  val parent_shown : unit -> unit
end

module type Panel = sig
  val content : unit -> Html_types.div Tyxml_js.Html5.elt
  val onload : unit -> unit
  val onresize : unit -> unit
end

val id_dom : string -> 'a Js.t
val document : Dom_html.document Js.t

val navtabs :
  string ->
  (string
  * Html_types.nmtokens Tyxml_js.Wrap.t option
  * Html_types.flow5_without_interactive Html.elt Tyxml_js.Wrap.tlist)
  list ->
  [> Html_types.ul ] Html.elt

val level :
  ?debug:'a ->
  ?info:'a ->
  ?notice:'a ->
  ?warning:'a ->
  ?error:'a ->
  ?fatal:'a ->
  unit ->
  'a list

val create_modal :
  id:string ->
  title_label:string ->
  body:[< Html_types.div_content_fun ] Html.elt list ->
  submit_label:string ->
  submit:('a Js.t, Dom_html.submitEvent Js.t) Dom_html.event_listener ->
  [> Html_types.div ] Html.elt

val navcontent :
  ?id:string ->
  string list ->
  (string * string list * [< Html_types.div_content_fun ] Html.elt list) list ->
  [> Html_types.div ] Html.elt

val option_label : string -> string

val label_news :
  bool Tyxml_js.Wrap.t ->
  (Api_types_j.simulation_info option -> int) ->
  [> `PCDATA | `Span ] Html.elt Tyxml_js.Wrap.tlist

val features : ?default:'a list -> (string * 'a) list -> 'a list
val input_change : Dom_html.inputElement Js.t -> (string -> unit) -> unit
