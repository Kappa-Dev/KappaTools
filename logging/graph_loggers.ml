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
  | Graph_loggers_sig.Red -> "red"
  | Graph_loggers_sig.Green -> "green"
  | Graph_loggers_sig.White -> "white"
  | Graph_loggers_sig.Blue -> "blue"
  | Graph_loggers_sig.Black -> "black"
  | Graph_loggers_sig.LightSkyBlue -> "#87ceeb"
  | Graph_loggers_sig.PaleGreen -> "#98fb98"
  | Graph_loggers_sig.Brown -> "brown"

let svg_color_encoding x =
  match
    x
  with
  | Graph_loggers_sig.Red -> "#f00"
  | Graph_loggers_sig.Green -> "#0f0"
  | Graph_loggers_sig.White -> "#fff"
  | Graph_loggers_sig.Blue -> "#00f"
  | Graph_loggers_sig.Black -> "#000"
  | Graph_loggers_sig.LightSkyBlue -> "#8ce"
  | Graph_loggers_sig.PaleGreen -> "#9f9"
  | Graph_loggers_sig.Brown -> "#fc9"

type node_attribute =
  {
    node_color: Graph_loggers_sig.color option;
    node_fillcolor: Graph_loggers_sig.color option;
    node_label: string option ;
    node_width: int option ;
    node_height: int option ;
    node_shape: Graph_loggers_sig.shape option ;
    node_positions: Locality.t list ;
    node_contextual_help: string option;
    node_on_click: Yojson.Basic.json option;
  }

type edge_attribute =
  {
    edge_color: Graph_loggers_sig.color option;
    edge_label: string list option ;
    edge_style: Graph_loggers_sig.linestyle ;
    edge_direction: Graph_loggers_sig.direction ;
    edge_arrowhead: Graph_loggers_sig.headkind ;
    edge_arrowtail: Graph_loggers_sig.headkind ;
    edge_positions: Locality.t list ;
    edge_contextual_help: string option;
    edge_on_click: Yojson.Basic.json option;
  }

let dummy_node =
  {
    node_color = None ;
    node_fillcolor = None ;
    node_label = None ;
    node_width = None ;
    node_height = None ;
    node_shape = None ;
    node_positions = [] ;
    node_on_click = None ;
    node_contextual_help = None ;
  }

let dummy_edge =
  {
    edge_color = None ;
    edge_label = None ;
    edge_style = Graph_loggers_sig.Plain ;
    edge_direction = Graph_loggers_sig.Direct ;
    edge_arrowhead = Graph_loggers_sig.Normal ;
    edge_arrowtail = Graph_loggers_sig.Normal ;
    edge_positions = [] ;
    edge_on_click = None ;
    edge_contextual_help = None ;
  }

let is_no_node_attributes node_attribute = node_attribute = dummy_node
let is_no_edge_attributes edge_attribute =
  dummy_edge =
  {
    edge_attribute
    with edge_direction = Graph_loggers_sig.Direct ;
         edge_arrowhead = Graph_loggers_sig.Normal ;
         edge_arrowtail = Graph_loggers_sig.Normal  }

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

let print_preamble_shared_html_js f title =
  let () = Format.fprintf f "<div class=\"container\">@," in
  let () = Format.fprintf
      f "<h1>@[%s@]</h1>@," title
  in
  let () = Format.fprintf f "<svg width=960 height=600><g/></svg>@," in
  let () = Format.fprintf f "<script>@," in
  let () = Format.fprintf f "// Create a new directed graph@," in
  let () =
    Format.fprintf f "var g = new dagreD3.graphlib.Graph().setGraph({});@," in

  ()

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
    | Loggers.Js_Graph -> (* IN PROGESS *)
      begin
        let f_opt = Loggers.formatter_of_logger logger in
        match
          f_opt
        with
        | None -> ()
        | Some f ->
          print_preamble_shared_html_js f title
      end
    | Loggers.HTML_Graph  ->
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
          let () = Format.fprintf f "@[<v 2><body>@," in
          let () = print_preamble_shared_html_js f title in
          ()
      end

    | Loggers.Matrix | Loggers.Json
    | Loggers.Mathematica | Loggers.Maple | Loggers.Matlab
    | Loggers.DOTNET | Loggers.Octave | Loggers.SBML
    | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT
    | Loggers.TXT_Tabular | Loggers.XLS -> ()

let string_of_arrow_in_html logger bool title style =
  match style
  with
  | Graph_loggers_sig.Tee | Graph_loggers_sig.Normal -> bool
  (*| Tee ->
        let () = between_attributes_in_html logger bool in
        let () =
          Loggers.fprintf logger "%s: \"tee\"" title
        in
        true*)
  | Graph_loggers_sig.Vee ->
    let () = between_attributes_in_html logger bool in
    let () =
      Loggers.fprintf logger "%s: \"vee\"" title
    in
    true
  | Graph_loggers_sig.No_head ->
    (*  let () = between_attributes_in_html logger bool in
        let () =
        Loggers.fprintf logger "%s: \"none\"" title
        in
        true*) bool

let merge s s' =
  if s = Graph_loggers_sig.No_head then s' else s

let matrix_string_of_options l =
  let i =
    List.fold_left
      (
         List.fold_left
           (fun i l ->
              match l with
                Graph_loggers_sig.Color x ->
                begin
                  match x with
                  | Graph_loggers_sig.Brown -> i*7
                  | Graph_loggers_sig.Black -> i
                  | Graph_loggers_sig.Green -> i*3
                  | Graph_loggers_sig.Red -> i*(-1)
                  | Graph_loggers_sig.Blue -> i*5
                  | Graph_loggers_sig.White -> i
                  | Graph_loggers_sig.LightSkyBlue -> i*11
                  | Graph_loggers_sig.PaleGreen -> i*13
                end
              | Graph_loggers_sig.Position _
              | Graph_loggers_sig.Contextual_help _
              | Graph_loggers_sig.OnClick _
              | Graph_loggers_sig.ArrowHead _
              | Graph_loggers_sig.ArrowTail _
              | Graph_loggers_sig.FillColor _
              | Graph_loggers_sig.Label _
              | Graph_loggers_sig.Width _
              | Graph_loggers_sig.Height _
              | Graph_loggers_sig.Direction _
              | Graph_loggers_sig.Shape _
              | Graph_loggers_sig.LineStyle _
                -> i
           )) 1 l in
  string_of_int i

let print_foot_shared_html_js logger =
  let () =
    Mods.String2Map.iter
      (fun (id1,id2) list ->
         let list = List.rev list in
         let id1_int = Loggers.int_of_string_id logger id1 in
         let id2_int = Loggers.int_of_string_id logger id2 in
         let attributes = dummy_edge in
         let attributes =
           List.fold_left
             (fun attributes option_list ->
                List.fold_left
                  (fun attributes option ->
                     match
                       option
                     with
                     | Graph_loggers_sig.Label s ->
                       begin
                         match attributes.edge_label
                         with
                         | None ->
                           {attributes with
                            edge_label = Some [s] }
                         | Some s' ->
                           {attributes with
                            edge_label = Some (s::","::s') }
                       end
                     | Graph_loggers_sig.Color s ->
                       begin
                         match attributes.edge_color with
                         | None ->
                           {attributes with edge_color = Some s }
                         | Some s' when s=s' -> attributes
                         | Some _ ->
                           {attributes with edge_color = Some Graph_loggers_sig.Brown}
                       end
                     | Graph_loggers_sig.LineStyle s -> {attributes with
                                                         edge_style = s}
                     | Graph_loggers_sig.Direction s -> {attributes with
                                                         edge_direction = s}
                     | Graph_loggers_sig.ArrowTail s -> {attributes with
                                                         edge_arrowtail = merge s attributes.edge_arrowtail }
                     | Graph_loggers_sig.ArrowHead s -> {attributes with
                                                         edge_arrowhead = merge s
                                                             attributes.edge_arrowhead}
                     | Graph_loggers_sig.Position p ->
                       {attributes with
                        edge_positions =
                          p@attributes.edge_positions
                       }
                     | Graph_loggers_sig.Contextual_help s ->
                       {attributes with
                        edge_contextual_help =
                          match attributes.edge_contextual_help with
                          | None -> Some s
                          | Some s' -> Some (s'^s)}
                     | Graph_loggers_sig.OnClick json ->
                       {attributes with
                          edge_on_click = Some json}
                     | Graph_loggers_sig.Shape _
                     | Graph_loggers_sig.Width _
                     | Graph_loggers_sig.Height _
                     | Graph_loggers_sig.FillColor _ -> attributes
                  )
                  attributes option_list)
             attributes
             list
         in
         let () =
           Loggers.fprintf logger "g.setEdge(%i,%i,{ " id1_int
             id2_int in
         let attributes =
           match attributes.edge_direction
           with
           | Graph_loggers_sig.Undirected ->
             {attributes with
              edge_arrowhead=Graph_loggers_sig.No_head ;
              edge_arrowtail=Graph_loggers_sig.No_head}
           | Graph_loggers_sig.Direct ->
             {attributes with
              edge_arrowtail=Graph_loggers_sig.No_head}
           | Graph_loggers_sig.Reverse ->
             {attributes with edge_arrowhead=Graph_loggers_sig.No_head}
           | Graph_loggers_sig.Both -> attributes
         in
         let bool = false in
         let bool, s_opt=
           match attributes.edge_label
           with
           | None -> bool,None
           | Some string_list ->
             let () = Loggers.fprintf logger "label: \"" in
             let s =
               Format.asprintf
                 "%a" (fun fmt -> List.iter (Format.fprintf fmt "%s")) (List.rev string_list)
             in
             let s_opt,s' =
               if
                 String.length s > 100
               then
                 Some s, (String.sub s 0 100)^"..."
               else
                 None, s
             in
             let () = Loggers.fprintf logger "%s" s' in
             let () = Loggers.fprintf logger "\"" in
             true, s_opt
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
         let () =
           match s_opt
           with None -> ()
              | Some s ->
                Loggers.fprintf logger "<!--%s-->\n" s
         in
         ())
      (Loggers.get_edge_map logger)
  in
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
    let () = Format.fprintf f "@,</div>" in
    ()

let print_graph_foot logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.DOT ->
    let () = Loggers.fprintf logger "}" in
    Loggers.print_newline logger
  | Loggers.Matrix ->
    let nodes = Loggers.get_nodes logger in
    let edges = Loggers.get_edge_map logger in
    let () = Loggers.fprintf logger "\"rules\" :" in
    let () = Loggers.print_newline logger in
    let () = Loggers.open_row logger in
    let _ =
      List.fold_left
        (fun sep (s,_) ->
           let () = Loggers.fprintf logger "%s\"%s\"" sep s in
           ", "
        )
        "" nodes
    in
    let () = Loggers.close_row logger in
    let () = Loggers.fprintf logger "," in
    let () = Loggers.print_newline logger in
    let () = Loggers.fprintf logger "\"hits\" :" in
    let () = Loggers.print_newline logger in
    let () = Loggers.open_row logger in
    let _ =
      List.fold_left
        (fun sep _ ->
           let () = Loggers.fprintf logger "%s1" sep in
           ", "
        )
        "" nodes
    in
    let () = Loggers.close_row logger in
    let () = Loggers.fprintf logger "," in
    let () = Loggers.print_newline logger in
    let () = Loggers.fprintf logger "\"fluxs\" :" in
    let () = Loggers.print_newline logger in
    let () = Loggers.open_row logger in
    let _ =
      List.fold_left
        (fun b (s1,_) ->
           let () =
             if b then
               let () = Loggers.fprintf logger "," in
               let () = Loggers.print_newline logger in
               ()
           in
           let () = Loggers.open_row logger in
           let _ =
             List.fold_left
               (fun sep (s2,_) ->
                  let color_value =
                    match
                      Mods.String2Map.find_option
                        (s1,s2)
                        edges
                    with
                    | None -> "0"
                    | Some options -> matrix_string_of_options options
                  in
                  let () =
                    Loggers.fprintf logger "%s%s" sep color_value in
                  ", "
               )
               "" nodes
           in
           let () = Loggers.close_row logger in
           true
        )  false nodes
    in
    let () = Loggers.close_row logger in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Js_Graph ->
    begin
      print_foot_shared_html_js logger
    end
  | Loggers.HTML_Graph ->
    begin
      let f_opt = Loggers.formatter_of_logger logger in
      match
        f_opt
      with
      | None -> ()
      | Some f ->
        let () = print_foot_shared_html_js logger in
        let () = Format.fprintf f "@,</body>@]@,</html>@]@." in
        ()
    end
  | Loggers.Json
  | Loggers.Mathematica | Loggers.Maple | Loggers.Matlab | Loggers.Octave
  | Loggers.DOTNET | Loggers.SBML | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

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
    | Loggers.HTML_Graph | Loggers.Js_Graph -> Loggers.fprintf logger "%s" string
    | Loggers.Json
    | Loggers.Matrix
    | Loggers.SBML | Loggers.Maple | Loggers.Matlab | Loggers.Mathematica
    | Loggers.DOTNET | Loggers.Octave
    | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT
    | Loggers.TXT_Tabular | Loggers.XLS -> ()

let open_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML_Graph | Loggers.Js_Graph -> Loggers.fprintf logger "\t<p><dl>\n"
  | Loggers.Json
  | Loggers.Mathematica
  |  Loggers.SBML | Loggers.Maple | Loggers.Matlab
  | Loggers.DOTNET | Loggers.Octave
  | Loggers.Matrix | Loggers.HTML | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS -> ()
let close_asso logger =
  match Loggers.get_encoding_format logger with
  | Loggers.HTML_Graph | Loggers.Js_Graph -> Loggers.fprintf logger "\t\t</dl></p>\n"
  | Loggers.Json
  | Loggers.DOTNET | Loggers.Mathematica | Loggers.Maple
  | Loggers.Matlab | Loggers.Octave | Loggers.SBML
  | Loggers.Matrix | Loggers.HTML | Loggers.DOT | Loggers.HTML_Tabular | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_asso logger string1 string2 =
  match Loggers.get_encoding_format logger with
  | Loggers.DOT -> Loggers.fprintf logger "/*%s %s*/" string1 string2
  | Loggers.HTML_Graph | Loggers.Js_Graph -> Loggers.fprintf logger "\t\t\t<dt>%s</dt><dd>%s</dd>" string1 string2
  | Loggers.Json
  | Loggers.DOTNET | Loggers.Matrix | Loggers.SBML
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.Mathematica
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let shape_in_dot shape =
  match
    shape
  with
  | Graph_loggers_sig.Invisible -> "style=\"invis\""
  | Graph_loggers_sig.House -> "shape=\"house\""
  | Graph_loggers_sig.Rect -> "shape=\"box\""
  | Graph_loggers_sig.Ellipse -> "shape=\"ellipse\""
  | Graph_loggers_sig.Circle -> "shape=\"circle\""

let shape_in_html shape =
  match
    shape
  with
  | Graph_loggers_sig.Invisible -> Some "style: \"visibility:hidden\""
  | Graph_loggers_sig.House -> Some "shape: \"house\""
  | Graph_loggers_sig.Rect -> Some "shape: \"rect\""
  | Graph_loggers_sig.Ellipse -> Some "shape: \"ellipse\""
  | Graph_loggers_sig.Circle -> Some "shape: \"ellipse\""

let string_one_of_linestyle_in_dot _ = "-"
let string_two_of_linestyle_in_dot _ = "--"

let string_of_arrow_head_in_dot style =
  match
    style
  with
  | Graph_loggers_sig.Normal -> ">"
  | Graph_loggers_sig.Vee -> "|>"
  | Graph_loggers_sig.Tee -> "|"
  | Graph_loggers_sig.No_head -> ""

let string_of_arrow_tail_in_dot style =
  match
    style
  with
  | Graph_loggers_sig.Normal -> "<"
  | Graph_loggers_sig.Vee -> "<|"
  | Graph_loggers_sig.Tee -> "|"
  | Graph_loggers_sig.No_head -> ""


let print_node logger ?directives:(directives=[]) id =
  let attributes = dummy_node in
  let attributes =
    match Loggers.get_encoding_format logger with
    | Loggers.DOT | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.TXT ->
      List.fold_left
        (fun attributes option ->
           match
             option
           with
           | Graph_loggers_sig.Label s -> {attributes with node_label = Some s }
           | Graph_loggers_sig.Color s -> {attributes with node_color = Some s }
           | Graph_loggers_sig.FillColor s -> {attributes with node_fillcolor = Some s}
           | Graph_loggers_sig.Width i -> {attributes with node_width = Some i}
           | Graph_loggers_sig.Height i -> {attributes with node_height = Some i}
           | Graph_loggers_sig.Shape s -> {attributes with node_shape = Some s}
           | Graph_loggers_sig.Position p ->
             {attributes with node_positions = p@attributes.node_positions}
           | Graph_loggers_sig.OnClick json ->
             {attributes with node_on_click = Some json}
           | Graph_loggers_sig.Contextual_help s ->
             {attributes with node_contextual_help =
                                match attributes.node_contextual_help with
                                | None -> Some s
                                | Some s' -> Some (s'^s)
             }
           | Graph_loggers_sig.LineStyle _
           | Graph_loggers_sig.Direction _
           | Graph_loggers_sig.ArrowTail _
           | Graph_loggers_sig.ArrowHead _ -> attributes
        )
        attributes
        directives
    | Loggers.DOTNET | Loggers.Matrix | Loggers.Json
    | Loggers.Mathematica| Loggers.Maple | Loggers.Matlab | Loggers.Octave
    | Loggers.SBML
    | Loggers.TXT_Tabular | Loggers.XLS | Loggers.HTML_Tabular | Loggers.HTML
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
  | Loggers.HTML_Graph  ->
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
          ()
        end
    in
    let () = Loggers.fprintf logger " });@," in
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
  | Loggers.Matrix
  | Loggers.Js_Graph
  | Loggers.Json -> Loggers.add_node logger id directives
  | Loggers.DOTNET | Loggers.Mathematica
  | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.SBML
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_edge logger ?directives:(directives=[]) ?prefix:(prefix="") id1 id2 =
  let attributes = dummy_edge in
  let attributes =
    match Loggers.get_encoding_format logger with
    | Loggers.Matrix | Loggers.DOT | Loggers.HTML_Graph | Loggers.Js_Graph
    | Loggers.Json | Loggers.TXT | Loggers.HTML ->
      List.fold_left
        (fun attributes option ->
           match
             option
           with
           | Graph_loggers_sig.Label s -> {attributes with edge_label = Some [s] }
           | Graph_loggers_sig.Color s -> {attributes with edge_color = Some s }
           | Graph_loggers_sig.LineStyle s -> {attributes with edge_style = s}
           | Graph_loggers_sig.Direction s -> {attributes with edge_direction = s}
           | Graph_loggers_sig.ArrowTail s -> {attributes with edge_arrowtail = s}
           | Graph_loggers_sig.ArrowHead s -> {attributes with edge_arrowhead = s}
           | Graph_loggers_sig.Position p ->
             {attributes with edge_positions = p@attributes.edge_positions}
           | Graph_loggers_sig.Contextual_help s ->
             {attributes with edge_contextual_help =
                                match attributes.edge_contextual_help with
                                | None -> Some s
                                | Some s' -> Some (s'^s)}
           | Graph_loggers_sig.OnClick json ->
             {attributes with edge_on_click = Some json}
           | Graph_loggers_sig.Shape _
           | Graph_loggers_sig.Width _
           | Graph_loggers_sig.Height _
           | Graph_loggers_sig.FillColor _ -> attributes
        )
        attributes
        directives
    | Loggers.DOTNET | Loggers.Mathematica
    | Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.SBML
    | Loggers.TXT_Tabular | Loggers.XLS | Loggers.HTML_Tabular -> attributes
  in
  match Loggers.get_encoding_format logger with
  | Loggers.DOT ->
    begin
      let direction =
        match attributes.edge_direction
        with
        | Graph_loggers_sig.Direct ->
          (string_one_of_linestyle_in_dot attributes.edge_style)^(string_of_arrow_head_in_dot Graph_loggers_sig.Normal)
        | Graph_loggers_sig.Undirected -> (string_two_of_linestyle_in_dot attributes.edge_style)
        | Graph_loggers_sig.Both -> (string_of_arrow_tail_in_dot  Graph_loggers_sig.Normal)^(string_one_of_linestyle_in_dot                                      attributes.edge_style)^(string_of_arrow_head_in_dot Graph_loggers_sig.Normal)
        | Graph_loggers_sig.Reverse -> (string_of_arrow_tail_in_dot
                        Graph_loggers_sig.Normal)^(string_one_of_linestyle_in_dot
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
            | Some string_list ->
              let () = Loggers.fprintf logger "label=\"" in
              let () =
                List.iter
                  (Loggers.fprintf logger "%s")
                  (List.rev string_list)
              in
              let () = Loggers.fprintf logger "\"" in

              true
          in
          let bool =
            match attributes.edge_style
            with
            | Graph_loggers_sig.Plain -> bool
            | Graph_loggers_sig.Dotted ->
              let () = Loggers.fprintf logger "style=\"dotted\"" in
              true
            | Graph_loggers_sig.Dashed ->
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
            | Graph_loggers_sig.Normal -> bool
            | Graph_loggers_sig.Tee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowhead=\"tee\""
              in
              true
            | Graph_loggers_sig.Vee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowhead=\"vee\""
              in
              true
            | Graph_loggers_sig.No_head ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowhead=\"none\""
              in
              true
          in
          let bool =
            match attributes.edge_arrowtail
            with
            | Graph_loggers_sig.Normal -> bool
            | Graph_loggers_sig.Tee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowtail=\"tee\""
              in
              true
            | Graph_loggers_sig.Vee ->
              let () = between_attributes_in_dot logger bool in
              let () =
                Loggers.fprintf logger "arrowtail=\"vee\""
              in
              true
            | Graph_loggers_sig.No_head ->
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
| Loggers.TXT   | Loggers.HTML ->
    let label =
      match
        attributes.edge_label
      with
      | None -> [""]
      | Some x -> x
    in
    let arrow =
      match
        attributes.edge_arrowhead
      with
      | Graph_loggers_sig.No_head -> "--"
      | Graph_loggers_sig.Normal | Graph_loggers_sig.Vee  -> "->"
      | Graph_loggers_sig.Tee -> "-|"
    in
    let () = Loggers.fprintf logger "%s%s %s %s" prefix id1 arrow id2 in
    let () = List.iter (Loggers.fprintf logger "%s") (List.rev label) in
    let () = Loggers.print_newline logger in
   ()
| Loggers.Matrix | Loggers.Json | Loggers.HTML_Graph | Loggers.Js_Graph ->
  Loggers.add_edge logger id1 id2 directives
| Loggers.DOTNET | Loggers.Mathematica
| Loggers.Maple | Loggers.Matlab | Loggers.Octave | Loggers.SBML
| Loggers.HTML_Tabular | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_one_to_n_relation
    logger ?directives:(directives=[])
    ?style_one:(style_one=Graph_loggers_sig.Plain)
    ?style_n:(style_n=Graph_loggers_sig.Plain) id idlist
  =
  let fictitious = "Fictitious_"^id in
  let directives_fict =
    match
      Loggers.get_encoding_format logger
    with
    | Loggers.HTML_Graph  ->
      List.rev ((Graph_loggers_sig.Label "")::(Graph_loggers_sig.Shape Graph_loggers_sig.Circle)::(Graph_loggers_sig.Width 0)::(Graph_loggers_sig.Height 0)::(Graph_loggers_sig.FillColor Graph_loggers_sig.Black)::(List.rev directives))
    | Loggers.Js_Graph
    | Loggers.Json
    | Loggers.Matrix
    | Loggers.Mathematica | Loggers.Maple | Loggers.Matlab
    | Loggers.Octave | Loggers.SBML | Loggers.DOTNET
    | Loggers.HTML | Loggers.TXT | Loggers.DOT | Loggers.HTML_Tabular
    | Loggers.TXT_Tabular | Loggers.XLS ->
      List.rev ((Graph_loggers_sig.Label "")::(Graph_loggers_sig.Shape Graph_loggers_sig.Invisible)::(Graph_loggers_sig.Width 0)::(Graph_loggers_sig.Height 0)::(List.rev directives))
  in
  let directives_one =
    if style_one = Graph_loggers_sig.Plain
    then
      directives
    else
      List.rev ((Graph_loggers_sig.LineStyle style_one)::(List.rev directives))
  in
  let directives_n =
    if style_n = Graph_loggers_sig.Plain
    then
      directives
    else
      List.rev ((Graph_loggers_sig.LineStyle style_n)::(List.rev directives))
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
