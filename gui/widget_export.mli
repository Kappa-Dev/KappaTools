module Html = Tyxml_js.Html5

type handler = { suffix: string; label: string; export: string -> unit }

type configuration = {
  id: string;
  handlers: handler list;
  show: bool React.signal;
}

val content :
  configuration -> [< Html_types.div_content_fun > `Form `Table ] Html.elt

val export_png : ?svg_style_id:string -> svg_div_id:string -> unit -> handler
val export_json : serialize_json:(unit -> string) -> handler
val export_svg : ?svg_style_id:string -> svg_div_id:string -> unit -> handler
val export_data_label : configuration -> string
val onload : configuration -> unit

val inline_content :
  configuration -> [> `Button | `Div | `PCDATA ] Html.elt list
