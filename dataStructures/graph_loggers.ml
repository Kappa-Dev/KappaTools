(**
  * graph_loggers.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 23/05/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let dot_color_encoding x =
  match
    x
  with
  | Graph_loggers_options.Red -> "red"
  | Graph_loggers_options.Green -> "green"
  | Graph_loggers_options.White -> "white"
  | Graph_loggers_options.Blue -> "blue"
  | Graph_loggers_options.Black -> "black"
  | Graph_loggers_options.LightSkyBlue -> "#87ceeb"
  | Graph_loggers_options.PaleGreen -> "#98fb98"

let svg_color_encoding x =
  match
    x
  with
  | Graph_loggers_options.Red -> "#f00"
  | Graph_loggers_options.Green -> "#0f0"
  | Graph_loggers_options.White -> "#fff"
  | Graph_loggers_options.Blue -> "#00f"
  | Graph_loggers_options.Black -> "#000"
  | Graph_loggers_options.LightSkyBlue -> "#8ce"
  | Graph_loggers_options.PaleGreen -> "#9f9"

type node_attribute =
  {
    node_color: Graph_loggers_options.color option;
    node_fillcolor: Graph_loggers_options.color option;
    node_label: string option ;
    node_width: int option ;
    node_height: int option ;
    node_shape: Graph_loggers_options.shape option ;
  }

type edge_attribute =
  {
    edge_color: Graph_loggers_options.color option;
    edge_label: string option ;
    edge_style: Graph_loggers_options.linestyle ;
    edge_direction: Graph_loggers_options.direction ;
    edge_arrowhead: Graph_loggers_options.headkind ;
    edge_arrowtail: Graph_loggers_options.headkind
  }

let dummy_node =
  {
    node_color = None ;
    node_fillcolor = None ;
    node_label = None ;
    node_width = None ;
    node_height = None ;
    node_shape = None ;
  }

let dummy_edge =
  {
    edge_color = None ;
    edge_label = None ;
    edge_style = Graph_loggers_options.Plain ;
    edge_direction = Graph_loggers_options.Direct ;
    edge_arrowhead = Graph_loggers_options.Normal ;
    edge_arrowtail = Graph_loggers_options.Normal ;
  }

let is_no_node_attributes node_attribute = node_attribute = dummy_node
let is_no_edge_attributes edge_attribute =
  dummy_edge =
  {
    edge_attribute
    with edge_direction = Graph_loggers_options.Direct ;
         edge_arrowhead = Graph_loggers_options.Normal ;
         edge_arrowtail = Graph_loggers_options.Normal  }

let between_attributes_in_dot logger bool =
  if bool then
    Loggers.fprintf logger " "
  else
    ()

let between_attributes_in_html logger bool =
  if bool then
    Loggers.fprintf logger ", "
  else
    ()

let html_deps =
  ["http://d3js.org/d3.v3.min.js";
   "http://cpettitt.github.io/project/dagre-d3/latest/dagre-d3.min.js"]

let shall_I_do_it format filter_in filter_out =
  let b1 =
    match
      filter_in
    with
    | None -> true
    | Some l -> List.mem format l
  in
  b1 && (not (List.mem format filter_out))

let print_graph_preamble
    logger
    ?filter_in:(filter_in=None) ?filter_out:(filter_out=[]) ?header:(header=[])
    title
  =
  let format = Loggers.get_encoding_format logger in
  if shall_I_do_it format filter_in filter_out
  then
    match
      format
    with
    | Loggers.DOT ->
      let () =
        List.iter
          (fun x ->
             let () = Loggers.fprintf logger "#%s" x in
             let () = Loggers.print_newline logger in
             ())
          header
      in
      let () = Loggers.fprintf logger "digraph G{" in
      let () = Loggers.print_newline logger in
      ()
    | Loggers.HTML_Graph ->
      begin
        let f_opt = Loggers.formatter_of_logger logger in
        match
          f_opt
        with
        | None -> ()
        | Some f ->
          let () = Loggers.fprintf logger "<!--@," in
          let () =
            List.iter
              (fun x ->
                 let () = Loggers.fprintf logger "%s" x in
                 let () = Loggers.print_newline logger in
                 ())
              header
          in
          let () = Loggers.fprintf logger "-->@," in
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
      | Loggers.Json | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_graph_foot logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.DOT ->
    let () = Loggers.fprintf logger "}" in
    Loggers.print_newline logger
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
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
  | Loggers.Json ->
    let graph = Loggers.graph_of_logger logger in
    ()

let print_comment
    logger
    ?filter_in:(filter_in=None) ?filter_out:(filter_out=[])
    string
  =
  let format = Loggers.get_encoding_format logger in
  if shall_I_do_it format filter_in filter_out
  then
    match
      format
    with
    | Loggers.DOT -> Loggers.fprintf logger "#%s" string
    | Loggers.HTML_Graph -> Loggers.fprintf logger "//%s" string
    | Loggers.Maple | Loggers.Matlab | Loggers.Octave  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let open_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML_Graph -> Loggers.fprintf logger "\t<p><dl>\n"
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
let close_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML_Graph -> Loggers.fprintf logger "\t\t</dl></p>\n"
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_asso logger string1 string2 =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "/*%s %s*/" string1 string2
  | Loggers.HTML_Graph -> Loggers.fprintf logger "\t\t\t<dt>%s</dt><dd>%s</dd>" string1 string2
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let shape_in_dot shape =
  match
    shape
  with
  | Graph_loggers_options.Invisible -> "style=\"invis\""
  | Graph_loggers_options.House -> "shape=\"house\""
  | Graph_loggers_options.Rect -> "shape=\"box\""
  | Graph_loggers_options.Ellipse -> "shape=\"ellipse\""
  | Graph_loggers_options.Circle -> "shape=\"circle\""

let shape_in_html shape =
  match
    shape
  with
  | Graph_loggers_options.Invisible -> Some "style: \"visibility:hidden\""
  | Graph_loggers_options.House -> Some "shape: \"house\""
  | Graph_loggers_options.Rect -> Some "shape: \"rect\""
  | Graph_loggers_options.Ellipse -> Some "shape: \"ellipse\""
  | Graph_loggers_options.Circle -> Some "shape: \"ellipse\""

let string_one_of_linestyle_in_dot _ = "-"
let string_two_of_linestyle_in_dot _ = "--"

let string_of_arrow_head_in_dot style =
  match
    style
  with
  | Graph_loggers_options.Normal -> ">"
  | Graph_loggers_options.Vee -> "|>"
  | Graph_loggers_options.Tee -> "|"
  | Graph_loggers_options.No_head -> ""

let string_of_arrow_tail_in_dot style =
  match
    style
  with
  | Graph_loggers_options.Normal -> "<"
  | Graph_loggers_options.Vee -> "<|"
  | Graph_loggers_options.Tee -> "|"
  | Graph_loggers_options.No_head -> ""

let string_of_arrow_in_html logger bool title style =
  match style
  with
  | Graph_loggers_options.Tee | Graph_loggers_options.Normal -> bool
  (*| Tee ->
        let () = between_attributes_in_html logger bool in
        let () =
          Loggers.fprintf logger "%s: \"tee\"" title
        in
        true*)
  | Graph_loggers_options.Vee ->
    let () = between_attributes_in_html logger bool in
    let () =
      Loggers.fprintf logger "%s: \"vee\"" title
    in
    true
  | Graph_loggers_options.No_head ->
    (*  let () = between_attributes_in_html logger bool in
        let () =
        Loggers.fprintf logger "%s: \"none\"" title
        in
        true*) bool

let print_node logger ?directives:(directives=[]) id =
  let attributes = dummy_node in
  let attributes =
    match Loggers.get_encoding_format logger with
    | Loggers.DOT | Loggers.HTML_Graph | Loggers.TXT ->
      List.fold_left
        (fun attributes option ->
           match
             option
           with
           | Graph_loggers_options.Label s -> {attributes with node_label = Some s }
           | Graph_loggers_options.Color s -> {attributes with node_color = Some s }
           | Graph_loggers_options.FillColor s -> {attributes with node_fillcolor = Some s}
           | Graph_loggers_options.Width i -> {attributes with node_width = Some i}
           | Graph_loggers_options.Height i -> {attributes with node_height = Some i}
           | Graph_loggers_options.Shape s -> {attributes with node_shape = Some s}
           | Graph_loggers_options.LineStyle _
           | Graph_loggers_options.Direction _
           | Graph_loggers_options.ArrowTail _
           | Graph_loggers_options.ArrowHead _ -> attributes
        )
        attributes
        directives
    | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.TXT_Tabular | Loggers.XLS | Loggers.HTML_Tabular | Loggers.HTML
      -> attributes
  in
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let () = Loggers.fprintf logger "\"%s\"" id in
      let () =
        if is_no_node_attributes attributes
        then ()
        else
          begin
            let () = Loggers.fprintf logger " [" in
            let bool = false in
            let bool =
              match attributes.node_label
              with
              | None -> bool
              | Some string ->
                let () = Loggers.fprintf logger "label=\"%s\"" string in
                true
            in
            let bool =
              match attributes.node_shape
              with
              | None -> bool
              | Some shape ->
                let () = between_attributes_in_dot logger bool in
                let () =
                  Loggers.fprintf logger "%s" (shape_in_dot shape)
                in
                true
            in
            let bool =
              match attributes.node_width
              with
              | None -> bool
              | Some i ->
                let () = between_attributes_in_dot logger bool in
                let () =
                  Loggers.fprintf logger "width=\"%ipx\"" i
                in
                true
            in
            let bool =
              match attributes.node_height
              with
              | None -> bool
              | Some i ->
                let () = between_attributes_in_dot logger bool in
                let () =
                  Loggers.fprintf logger "height=\"%ipx\"" i
                in
                true
            in
            let bool =
              match attributes.node_color
              with
              | None -> bool
              | Some s ->
                let () = between_attributes_in_dot logger bool in
                let () =
                  Loggers.fprintf
                    logger
                    "color=\"%s\""
                    (dot_color_encoding s)
                in
                true
            in
            let bool =
              match attributes.node_fillcolor
              with
              | None -> bool
              | Some s ->
                let () = between_attributes_in_dot logger bool in
                let () =
                  Loggers.fprintf
                    logger
                    "fillcolor=\"%s\" style=filled"
                    (dot_color_encoding s)
                in
                true
            in
            let () = if bool then () in
            let () = Loggers.fprintf logger "];" in
            let () = Loggers.print_newline logger in
            ()
          end
      in ()
    end
  | Loggers.HTML_Graph ->
    let id_int = Loggers.int_of_string_id logger id in
    let () = Loggers.fprintf logger "g.setNode(%i, { " id_int in
    let () =
      if is_no_node_attributes attributes
      then ()
      else
        begin
          let string =
            match attributes.node_label
            with
            | None -> id
            | Some string -> string
          in
          let string = String.escaped string in
          let () =
            Loggers.fprintf logger "label: \"%s\"" string
          in
          let () =
            match attributes.node_shape
            with
            | None -> ()
            | Some shape ->
              begin
                match shape_in_html shape
                with
                | None -> ()
                | Some shape ->
                  let () = between_attributes_in_html logger true in
                  let () =
                    Loggers.fprintf logger "%s" shape
                  in
                  ()
              end
          in
          let () =
            match attributes.node_width
            with
            | None -> ()
            | Some i ->
              let () = between_attributes_in_html logger true in
              let () =
                Loggers.fprintf logger "width: \"%i\"" i
              in
              ()
          in
          let () =
            match attributes.node_height
            with
            | None -> ()
            | Some i ->
              let () = between_attributes_in_html logger true in
              let () =
                Loggers.fprintf logger "height: \"%i\"" i
              in
              ()
          in
          let () =
            match attributes.node_color
            with
            | None -> ()
            | Some s ->
              let () = between_attributes_in_html logger true in
              let () =
                Loggers.fprintf
                  logger
                  "color: \"%s\""
                  (svg_color_encoding s)
              in
              ()
          in
          let () =
            match attributes.node_fillcolor
            with
            | None -> ()
            | Some s ->
              let () = between_attributes_in_html logger true in
              let () =
                Loggers.fprintf
                  logger
                  "style: \"fill: %s\" "
                  (svg_color_encoding s)
              in
              ()
          in
          let () = Loggers.fprintf logger " });@," in
          ()
        end
    in
    ()
  | Loggers.TXT ->
    begin
      match attributes.node_label
      with
      | None ->
        let () = Loggers.fprintf logger "Node: %s" id in
        let () = Loggers.print_newline logger in
        ()
      | Some label ->
        let () = Loggers.fprintf logger "Node:%s, Label:%s" id label in
        let () = Loggers.print_newline logger in
        ()
    end
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_edge logger ?directives:(directives=[]) ?prefix:(prefix="") id1 id2 =
  let attributes = dummy_edge in
  let attributes =
    match Loggers.get_encoding_format logger with
    | Loggers.DOT | Loggers.HTML_Graph | Loggers.TXT | Loggers.HTML ->
      List.fold_left
        (fun attributes option ->
           match
             option
           with
           | Graph_loggers_options.Label s -> {attributes with edge_label = Some s }
           | Graph_loggers_options.Color s -> {attributes with edge_color = Some s }
           | Graph_loggers_options.LineStyle s -> {attributes with edge_style = s}
           | Graph_loggers_options.Direction s -> {attributes with edge_direction = s}
           | Graph_loggers_options.ArrowTail s -> {attributes with edge_arrowtail = s}
           | Graph_loggers_options.ArrowHead s -> {attributes with edge_arrowhead = s}
           | Graph_loggers_options.Shape _
           | Graph_loggers_options.Width _
           | Graph_loggers_options.Height _
           | Graph_loggers_options.FillColor _ -> attributes
        )
        attributes
        directives
    | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.TXT_Tabular | Loggers.XLS | Loggers.HTML_Tabular
      -> attributes
  in
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let direction =
        match attributes.edge_direction
        with
        | Graph_loggers_options.Direct ->
          (string_one_of_linestyle_in_dot attributes.edge_style)^(string_of_arrow_head_in_dot Graph_loggers_options.Normal)
        | Graph_loggers_options.Undirected -> (string_two_of_linestyle_in_dot attributes.edge_style)
        | Graph_loggers_options.Both -> (string_of_arrow_tail_in_dot  Graph_loggers_options.Normal)^(string_one_of_linestyle_in_dot                                      attributes.edge_style)^(string_of_arrow_head_in_dot Graph_loggers_options.Normal)
        | Graph_loggers_options.Reverse -> (string_of_arrow_tail_in_dot
                        Graph_loggers_options.Normal)^(string_one_of_linestyle_in_dot
                                   attributes.edge_style)
      in
      let () = Loggers.fprintf logger "\"%s\" %s \"%s\"" id1 direction id2 in
      let () =
        if is_no_edge_attributes attributes
        then
          ()
        else
          let () = Loggers.fprintf logger " [" in
          let bool = false in
          let bool =
            match attributes.edge_label
            with
            | None -> bool
            | Some string ->
              let () = Loggers.fprintf logger "label=\"%s\"" string in
              true
          in
          let bool =
            match attributes.edge_style
            with
            | Graph_loggers_options.Plain -> bool
            | Graph_loggers_options.Dotted ->
              let () = Loggers.fprintf logger "style=\"dotted\"" in
              true
            | Graph_loggers_options.Dashed ->
              let () = Loggers.fprintf logger "style=\"dashed\"" in
              true
          in
          let bool =
            match attributes.edge_color
            with
            | None -> bool
            | Some s ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "color=\"%s\""
                  (dot_color_encoding s)
              in
              true
          in
          let bool =
            match attributes.edge_arrowhead
            with
            | Graph_loggers_options.Normal -> bool
            | Graph_loggers_options.Tee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowhead=\"tee\""
              in
              true
            | Graph_loggers_options.Vee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowhead=\"vee\""
              in
              true
            | Graph_loggers_options.No_head ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowhead=\"none\""
              in
              true
          in
          let bool =
            match attributes.edge_arrowtail
            with
            | Graph_loggers_options.Normal -> bool
            | Graph_loggers_options.Tee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowtail=\"tee\""
              in
              true
            | Graph_loggers_options.Vee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowtail=\"vee\""
              in
              true
            | Graph_loggers_options.No_head ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowtail=\"none\""
              in
              true
          in
          let () = if bool then () in
          let () = Loggers.fprintf logger "];" in
          let () = Loggers.print_newline logger in
          ()
      in ()
    end
  | Loggers.HTML_Graph ->
    let id1_int = Loggers.int_of_string_id logger id1 in
    let id2_int = Loggers.int_of_string_id logger id2 in
    let () = Loggers.fprintf logger "g.setEdge(%i,%i,{ " id1_int id2_int in
    let attributes =
      match attributes.edge_direction
      with
      | Graph_loggers_options.Undirected -> {attributes with edge_arrowhead=Graph_loggers_options.No_head ; edge_arrowtail=Graph_loggers_options.No_head}
      | Graph_loggers_options.Direct -> {attributes with edge_arrowtail=Graph_loggers_options.No_head}
      | Graph_loggers_options.Reverse -> {attributes with edge_arrowhead=Graph_loggers_options.No_head}
      | Graph_loggers_options.Both -> attributes
    in
    let bool = false in
    let bool =
      match attributes.edge_label
      with
      | None -> bool
      | Some string ->
        let () = Loggers.fprintf logger "label: \"%s\"" string in
        true
    in
    let bool =
      match attributes.edge_color
      with
      | None -> bool
      | Some s ->
        let () = between_attributes_in_html logger bool in
        let color = svg_color_encoding s in
        let () =
          Loggers.fprintf logger
            "style: \"stroke: %s; fill: white\", arrowheadStyle: \"fill: %s; stroke: %s\""
            color color color
        in
        true
    in
    let bool = string_of_arrow_in_html logger bool "arrowhead" attributes.edge_arrowhead in
    let bool = string_of_arrow_in_html logger bool "arrowtail" attributes.edge_arrowtail in
    let () = if bool then () else () in
    let () = Loggers.fprintf logger " });@," in
    ()
  | Loggers.TXT   | Loggers.HTML ->
    let label =
      match
        attributes.edge_label
      with
      | None -> ""
      | Some x -> x
    in
    let arrow =
      match
        attributes.edge_arrowhead
      with
      | Graph_loggers_options.No_head -> "--"
      | Graph_loggers_options.Normal | Graph_loggers_options.Vee  -> "->"
      | Graph_loggers_options.Tee -> "-|"
    in
    let () = Loggers.fprintf logger "%s%s %s %s%s" prefix id1 arrow id2 label in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML_Tabular | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_one_to_n_relation
    logger ?directives:(directives=[])
    ?style_one:(style_one=Graph_loggers_options.Plain)
    ?style_n:(style_n=Graph_loggers_options.Plain) id idlist
  =
  let fictitious = "Fictitious_"^id in
  let directives_fict =
    match
      Loggers.get_encoding_format logger
    with
    | Loggers.HTML_Graph ->
      List.rev ((Graph_loggers_options.Label "")::(Graph_loggers_options.Shape Graph_loggers_options.Circle)::(Graph_loggers_options.Width 0)::(Graph_loggers_options.Height 0)::(Graph_loggers_options.FillColor Graph_loggers_options.Black)::(List.rev directives))
    | Loggers.Json | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.HTML | Loggers.TXT | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT_Tabular | Loggers.XLS ->
      List.rev ((Graph_loggers_options.Label "")::(Graph_loggers_options.Shape Graph_loggers_options.Invisible)::(Graph_loggers_options.Width 0)::(Graph_loggers_options.Height 0)::(List.rev directives))
  in
  let directives_one =
    if style_one = Graph_loggers_options.Plain
    then
      directives
    else
      List.rev ((Graph_loggers_options.LineStyle style_one)::(List.rev directives))
  in
  let directives_n =
    if style_n = Graph_loggers_options.Plain
    then
      directives
    else
      List.rev ((Graph_loggers_options.LineStyle style_n)::(List.rev directives))
  in
  let _ = print_node logger fictitious ~directives:directives_fict in
  let _ = print_edge logger ~directives:directives_one fictitious id in
  let _ =
    List.iter
      (fun id' ->
         print_edge logger ~directives:directives_n fictitious id')
      idlist
  in
  ()
