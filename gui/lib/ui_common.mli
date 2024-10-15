(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

val export_controls :
  export_select_id:string ->
  export_filename_id:string ->
  export_button_id:string ->
  export_data_label:string ->
  [> Html_types.div ] Html.elt

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

val document : Dom_html.document Js.t
val id_dom : string -> 'a Js.t

val switch_class : string -> string list -> string list -> unit
(** [switch_class elt_id add_list remove_list] adds and removes classes 
    to DOM element with id `elt_id` *)

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
  ?log:'a ->
  ?warning:'a ->
  ?error:'a ->
  unit ->
  'a list

val create_modal_text_input :
  id:string ->
  title_label:string ->
  body:[< Html_types.div_content_fun ] Html.elt list ->
  submit_label:string ->
  submit:('a Js.t, Dom_html.submitEvent Js.t) Dom_html.event_listener ->
  [> Html_types.div ] Html.elt

val create_modal_error :
  id:string ->
  is_critical:bool ->
  error_content:string ->
  [> Html_types.div ] Html.elt

val open_modal_error : is_critical:bool -> error_content:string -> unit

val navcontent :
  ?id:string ->
  string list ->
  (string * string list * [< Html_types.div_content_fun ] Html.elt list) list ->
  [> Html_types.div ] Html.elt

val option_label : ?max_size:int -> string -> string
val features : ?default:'a list -> (string * 'a) list -> 'a list
val input_change : Dom_html.inputElement Js.t -> (string -> unit) -> unit
