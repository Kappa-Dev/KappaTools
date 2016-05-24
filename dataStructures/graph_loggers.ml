type direction = Direct | Reverse | Undirected | Both
type options =
  | Label of string
  | Width of string
  | Height of string
  | Direction of direction
  | DotStyle of string

let html_deps =
  ["http://d3js.org/d3.v3.min.js";
   "http://cpettitt.github.io/project/dagre-d3/latest/dagre-d3.min.js"]

let print_graph_preamble logger title =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "digraph G{\n"
  | Loggers.HTML_Graph ->
    begin
      let f_opt = Loggers.formatter_of_logger logger in
      match
        f_opt
      with
      | None -> ()
      | Some f ->
        let dependency f t =
          Format.fprintf f "<script src=\"%s\" charset=\"utf-8\"></script>" t
        in
        let () = Format.fprintf f "@[<v><!doctype html>@,@,<html>@," in
        let () = Format.fprintf f "@[<v 2><head>@,<meta charset=\"utf-8\">@," in
        let () = Format.fprintf f "<title>%s</title>@," title in
        let () = Pp.list ~trailing:Pp.space Pp.space dependency f html_deps in
        let () = Format.fprintf f "%t@]@,</head>@,"
            (fun f ->
               let () = Format.fprintf f "@[<v 2><style>@," in
               let () =
                 Format.fprintf f "dt {float: left; clear: left; width: 20em;}@," in
               let () =
                 Format.fprintf f "dd {font-weight: bold; margin: 0 0 0 21em;}@," in
               let () = Format.fprintf f ".node rect {stroke: #333; fill: #fff;}@," in
               let () =
                 Format.fprintf
                   f ".edgePath path {stroke: #333; fill: #333; stroke-width: 1.5px;}" in
               Format.fprintf f "@]@,</style>")
        in
        let () = Format.fprintf f "@[<v 2><body>@,<div class=\"container\">@," in
        let () = Format.fprintf
            f "<h1>@[%s@]</h1>@," title
        in
        let () = Format.fprintf f "<svg width=960 height=600><g/></svg>@," in
        let () = Format.fprintf f "<script>@," in
        let () = Format.fprintf f "// Create a new directed graph@," in
        let () =
          Format.fprintf f "var g = new dagreD3.graphlib.Graph().setGraph({});@," in
        ()
    end
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_graph_foot logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.DOT -> Loggers.fprintf logger "}\n"
  | Loggers.HTML_Graph ->
    begin
      let f_opt = Loggers.formatter_of_logger logger in
      match
        f_opt
      with
      | None -> ()
      | Some f ->
        let () = Format.fprintf
            f "var svg = d3.select(\"svg\"),inner = svg.select(\"g\");@,"
        in
        let () = Format.fprintf f "// Set up zoom support@," in
        let () = Format.fprintf f "var zoom = d3.behavior.zoom().on(\"zoom\", function() {@," in
        let () = Format.fprintf f "inner.attr(\"transform\", \"translate(\" + d3.event.translate + \")\" +@," in
        let () = Format.fprintf f "\"scale(\" + d3.event.scale + \")\");@,});@,svg.call(zoom);" in
        let () = Format.fprintf f "// Create the renderer@, var render = new dagreD3.render();@," in
        let () = Format.fprintf f "// Run the renderer. This is what draws the final graph.@," in
        let () = Format.fprintf f "render(inner, g);@," in
        let () = Format.fprintf f "// Center the graph@,var initialScale = 0.75;@," in
        let () = Format.fprintf f "zoom@," in
        let () = Format.fprintf
            f ".translate([(svg.attr(\"width\") - g.graph().width * initialScale) / 2, 20])@," in
        let () = Format.fprintf f ".scale(initialScale)@,.event(svg);@," in
        let () = Format.fprintf f "svg.attr('height', g.graph().height * initialScale + 40);" in
        let () = Format.fprintf f "@,</script>" in
        let () = Format.fprintf f "@,</div>@]@,</body>@,</html>@]@." in
        ()
    end
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_comment logger string =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "/*%s*/" string
  | Loggers.HTML_Graph -> Loggers.fprintf logger "//%s" string
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let open_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML_Graph -> Loggers.fprintf logger "\t<p><dl>\n"
  | Loggers.HTML | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
let close_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML_Graph -> Loggers.fprintf logger "\t\t</dl></p>\n"
  | Loggers.HTML | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_asso logger string1 string2 =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "/*%s %s*/" string1 string2
  | Loggers.HTML_Graph -> Loggers.fprintf logger "\t\t\t<dt>%s</dt><dd>%s</dd>" string1 string2
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let ignore_in_dot option =
  match option with
  | Label _ | Width _ | Height _ | DotStyle _ -> false
  | Direction _  -> true

let ignore_in_html option =
  match option with
  | Label _ -> false
  | Width _ | Height _
  | Direction _ | DotStyle _ -> true

let print_node logger ?directives:(directives=[]) id =
  let directives =
    match Loggers.get_encoding_format logger
    with
    | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> []
    | Loggers.DOT | Loggers.HTML_Graph ->
      let _,directives =
        List.partition
          (fun a ->
             match a with
             | Direction _ -> true
             | Label _ | Width _ | Height _ | DotStyle _ -> false)
          directives
      in
      directives
  in
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let () = Loggers.fprintf logger "%s" id in
      let _,directives =
        List.partition
          (fun a ->
             match a with
             | Direction _ -> true
             | Label _ | Width _ | Height _ | DotStyle _ -> false)
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
                 if ignore_in_dot option
                 then bool
                 else
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
  | Loggers.HTML_Graph ->
    let id_int = Loggers.int_of_string_id logger id in
    let () = Loggers.fprintf logger "g.setNode(%i, { " id_int in
    let bool =
      List.fold_left
        (fun bool option ->
           if ignore_in_html option
           then bool
           else
             let () = if bool then Loggers.fprintf logger ", " in
             let () =
               match option with
               | Label string -> Loggers.fprintf logger "label: \"%s\"" string
               | Width _ -> ()
               | Height _ -> ()
               | Direction _ -> ()
               | DotStyle _ -> ()
             in
             true
        )
        false directives
    in
    let () = if bool then Loggers.fprintf logger ", " in
    let () = Loggers.fprintf logger "style: \"fill: #f77\" });\n" in
    ()
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_edge logger ?directives:(directives=[]) id1 id2 =
  let direction,directives =
    List.partition
      (fun a ->
         match a with
         | Direction _ -> true
         | Label _ | Width _ | Height _ | DotStyle _ -> false)
      directives
  in
  let direction = List.rev direction in
  let direction =
    match direction with
    | (Direction Direct)::_ | [] -> "->"
    | (Direction Reverse)::_ -> "<-"
    | (Direction Both)::_ -> "<->"
    | (Direction Undirected)::_ -> "--"
    | (Label _ )::_ | (Height _)::_ | (Width _ )::_ | (DotStyle _)::_ -> "->"
  in
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let () = Loggers.fprintf logger "%s %s %s" id1 direction id2 in
      let () =
        match directives with
        | [] -> ()
        | _ ->
          let () = Loggers.fprintf logger " [" in
          let _ =
            List.fold_left
              (fun bool option ->
                 if ignore_in_dot option
                 then bool
                 else
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
  | Loggers.HTML_Graph ->
    let id1_int = Loggers.int_of_string_id logger id1 in
    let id2_int = Loggers.int_of_string_id logger id2 in
    let () = Loggers.fprintf logger "g.setEdge(%i,%i,{" id1_int id2_int in
    let _ =
      List.fold_left
        (fun bool option ->
           if ignore_in_dot option
           then bool
           else
             let () = if bool then Loggers.fprintf logger ", " in
             let () =
               match option with
               | Label string -> Loggers.fprintf logger "label: \"%s\"" string
               | Width _
               | Height _
               | Direction _ -> ()
               | DotStyle string -> Loggers.fprintf logger "style=\"%s\"" string
             in
             true
        )
        false directives
    in
    let () = Loggers.fprintf logger "})\n " in
    ()
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
