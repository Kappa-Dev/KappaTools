type direction = Direct | Reverse | None | Both
type options =
  | Label of string
  | Width of string
  | Height of string
  | Direction of direction
  | DotStyle of string

let print_graph_preamble logger title =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    Loggers.fprintf logger "digraph G{\n"
  | Loggers.HTML ->
    begin
      let () = Loggers.fprintf logger
        "<!doctype html>\n\n
<html>\n\n
<head>\n\n
\t<meta charset=\"utf-8\">\n
\t<title>%s</title>\n
\t<script src=\"http://d3js.org/d3.v3.min.js\" charset=\"utf-8\"></script>\n
\t<script src=\"http://cpettitt.github.io/project/dagre-d3/latest/dagre-d3.min.js\" charset=\"utf-8\"></script>\n
\t<style>\n
\t\tdt {float: left; clear: left; width: 20em;}\n
\t\tdd {font-weight: bold; margin: 0 0 0 21em;}\n
\t\t.node rect {stroke: #333; fill: #fff;}\n
\t\t.edgePath path {stroke: #333; fill: #333; stroke-width: 1.5px;}\n
\t</style>\n
</head>\n
<body>\n
\t<div class=\"container\">\n
\t<h1>%s</h1>\n
<svg width=%s height=%s><g/></svg>\n" title title "0" "0"
      in ()
    end
  | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_graph_foot logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.DOT -> Loggers.fprintf logger "}\n"
  | Loggers.HTML ->
    Loggers.fprintf
      logger
      "\t\tvar svg = d3.select(\"svg\"),inner = svg.select(\"g\");\n
\t\t// Set up zoom support\n
\t\tvar zoom = d3.behavior.zoom().on(\"zoom\", function() {\n
\t\t\tinner.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" +\n
\t\t\t\"scale(\" + d3.event.scale + \")\");\n
\t\t});\n
\t\tsvg.call(zoom);// Create the renderer\n
\t\tvar render = new dagreD3.render();\n
\t\t// Run the renderer. This is what draws the final graph.\n
\t\trender(inner, g);\n
\t\t// Center the graph\n
\t\tvar initialScale = 0.75;\n
\t\tzoom\n
\t\t.translate([(svg.attr(\"width\") - g.graph().width * initialScale) / 2, 20])\n
\t\t.scale(initialScale)\n
\t\t.event(svg);\n
\t\tsvg.attr('height', g.graph().height * initialScale + 40);\n
\t</script>\n
\t</div>\n
\t</body>\n
\t</html>\n"
  | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_comment logger string =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "/*%s*/" string
  | Loggers.HTML -> Loggers.fprintf logger "//%s" string
  | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let open_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML -> Loggers.fprintf logger "\t<p><dl>\n"
  | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
let close_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML -> Loggers.fprintf logger "\t\t</dl></p>\n"
  | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_asso logger string1 string2 =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "/*%s %s*/" string1 string2
  | Loggers.HTML -> Loggers.fprintf logger "\t\t\t<dt>%s</dt><dd>%s</dd>" string1 string2
  | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_node logger id ?directives =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let () = Loggers.fprintf logger "%s" id in
      let directives =
        match directives with
        | Some l -> l
        | None -> []
      in
      let direction,directives =
        List.partition
          (fun a ->
             match a with
             | Direction _ -> true
             | _ -> false)
          directives
      in
      let () =
        match directives with
        | [] -> ()
        | _ ->
          let () = Loggers.fprintf logger " [" in
          let _ =
            List.fold_left
              (fun bool option ->
                 let () = if bool then Loggers.fprintf logger " " in
                 let () =
                   match option with
                 | Label string -> Loggers.fprintf logger "label=\"%s\"" string
                 | Width string -> Loggers.fprintf logger "width=\"%s\"" string
                 | Height string -> Loggers.fprintf logger "height=\"%s\"" string
                 | Direction _ -> ()
                 | DotStyle string -> Loggers.fprintf logger "style=\"%s\"" string
                 in
                 true
              )
              false directives
          in
          let () = Loggers.fprintf logger "];\n" in
          ()
      in ()
    end
  | Loggers.HTML -> ()
  | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_edge logger id1 id2 ?directives =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let () = Loggers.fprintf logger "%s" id1 in
      let directives =
        match directives with
        | Some l -> l
        | None -> []
      in
      let direction,directives =
        List.partition
          (fun a ->
             match a with
             | Direction _ -> true
             | _ -> false)
          directives
      in
      let direction = List.rev direction in
      let direction =
        match direction with
        | (Direction Direct)::_ | [] -> "->"
        | (Direction Reverse)::_ -> "<-"
        | (Direction Both)::_ -> "<->"
        | (Direction None)::_ -> "--"
      in
      let () = Loggers.fprintf logger "%s %s %s" id1 direction id2 in
      let () =
        match directives with
        | [] -> ()
        | _ ->
          let () = Loggers.fprintf logger " [" in
          let _ =
            List.fold_left
              (fun bool option ->
                 let () = if bool then Loggers.fprintf logger " " in
                 let () =
                   match option with
                   | Label string -> Loggers.fprintf logger "label=\"%s\"" string
                   | Width _
                   | Height _
                   | Direction _ -> ()
                   | DotStyle string -> Loggers.fprintf logger "style=\"%s\"" string
                 in
                 true
              )
              false directives
          in
          let () = Loggers.fprintf logger "];\n" in
          ()
      in ()
    end
  | Loggers.HTML -> ()
  | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
