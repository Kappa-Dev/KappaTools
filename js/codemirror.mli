type t

val create : Dom_html.element Js.t -> t Js.t
val get_editor_value : t Js.t -> Js.js_string Js.t
val set_editor_value : t Js.t -> Js.js_string Js.t -> unit
val starter : t Js.t -> bool ref -> unit
