val show_warnings : Dom_html.divElement Js.t -> unit
val show_error :
  (Format.formatter -> 'a -> unit) -> Dom_html.divElement Js.t ->
  'a Location.annot  -> unit
val show_info :
  (Format.formatter -> unit) -> Dom_html.divElement Js.t -> unit
