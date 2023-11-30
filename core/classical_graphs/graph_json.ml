(**
  * graph_js.ml
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

let direction_to_json direction =
  match direction with
  | Graph_loggers_sig.Direct -> `String "Direct"
  | Graph_loggers_sig.Reverse -> `String "Reverse"
  | Graph_loggers_sig.Undirected -> `String "Undirected"
  | Graph_loggers_sig.Both -> `String "Both"

let shape_to_json shape =
  match shape with
  | Graph_loggers_sig.Invisible -> `String "Invisible"
  | Graph_loggers_sig.House -> `String "House"
  | Graph_loggers_sig.Invhouse -> `String "Invhouse"
  | Graph_loggers_sig.Rect -> `String "rect"
  | Graph_loggers_sig.Ellipse -> `String "ellipse"
  | Graph_loggers_sig.Circle -> `String "circle"

let headkind_to_json headkind =
  match headkind with
  | Graph_loggers_sig.Vee -> `String "vee"
  | Graph_loggers_sig.Tee -> `String "Tee"
  | Graph_loggers_sig.No_head -> `String "undirected"
  | Graph_loggers_sig.Normal -> `String "normal"

let linestyle_to_json linestyle =
  match linestyle with
  | Graph_loggers_sig.Plain -> `String "Plain"
  | Graph_loggers_sig.Dotted -> `String "Dotted"
  | Graph_loggers_sig.Dashed -> `String "Dashed"

let color_to_json color =
  match color with
  | Graph_loggers_sig.Red -> `String "red"
  | Graph_loggers_sig.Green -> `String "green"
  | Graph_loggers_sig.White -> `String "white"
  | Graph_loggers_sig.Blue -> `String "blue"
  | Graph_loggers_sig.Black -> `String "black"
  | Graph_loggers_sig.LightSkyBlue -> `String "lightskyblue"
  | Graph_loggers_sig.PaleGreen -> `String "palegreen"
  | Graph_loggers_sig.Brown -> `String "brown"
  | Graph_loggers_sig.Yellow -> `String "yellow"
  | Graph_loggers_sig.Grey -> `String "grey"

let directive_to_json option =
  match option with
  | Graph_loggers_sig.Color color -> "color", color_to_json color
  | Graph_loggers_sig.FillColor color -> "fillcolor", color_to_json color
  | Graph_loggers_sig.Label string -> "label", `String string
  | Graph_loggers_sig.Width int -> "width", `Int int
  | Graph_loggers_sig.Height int -> "height", `Int int
  | Graph_loggers_sig.Direction direction ->
    "direction", direction_to_json direction
  | Graph_loggers_sig.Shape shape -> "shape", shape_to_json shape
  | Graph_loggers_sig.ArrowHead headkind ->
    "arrowhead", headkind_to_json headkind
  | Graph_loggers_sig.ArrowTail headkind ->
    "arrowtail", headkind_to_json headkind
  | Graph_loggers_sig.LineStyle linestyle ->
    "linestyle", linestyle_to_json linestyle
  | Graph_loggers_sig.Position p ->
    ( "position",
      JsonUtil.of_list
        (fun json -> Loc.yojson_of_annoted JsonUtil.of_unit ((), json))
        p )
  | Graph_loggers_sig.Contextual_help s ->
    "contextual help", JsonUtil.of_string s
  | Graph_loggers_sig.OnClick json -> "on_click", json

let directives_to_json = JsonUtil.of_assoc directive_to_json

let node_to_json (id, directives) =
  `Assoc [ "id", `String id; "directives", directives_to_json directives ]

let edge_to_json (id1, id2, directives) =
  `Assoc
    [
      "source", `String id1;
      "target", `String id2;
      "directives", directives_to_json directives;
    ]

let nodes_to_json = JsonUtil.of_list node_to_json
let edges_to_json = JsonUtil.of_list edge_to_json

let to_json graph : Yojson.Basic.t =
  `Assoc
    [ "nodes", nodes_to_json (fst graph); "edges", edges_to_json (snd graph) ]

let linestyle_of_json = function
  | `String "Plain" -> Graph_loggers_sig.Plain
  | `String "Dotted" -> Graph_loggers_sig.Dotted
  | `String "Dashed" -> Graph_loggers_sig.Dashed
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct headkind", x))

let headkind_of_json = function
  | `String "vee" -> Graph_loggers_sig.Vee
  | `String "Tee" -> Graph_loggers_sig.Tee
  | `String "undirected" -> Graph_loggers_sig.No_head
  | `String "normal" -> Graph_loggers_sig.Normal
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct headkind", x))

let shape_of_json = function
  | `String "Invisible" -> Graph_loggers_sig.Invisible
  | `String "House" -> Graph_loggers_sig.House
  | `String "Invhouse" -> Graph_loggers_sig.Invhouse
  | `String "rect" -> Graph_loggers_sig.Rect
  | `String "ellipse" -> Graph_loggers_sig.Ellipse
  | `String "circle" -> Graph_loggers_sig.Circle
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct shape", x))

let direction_of_json = function
  | `String "Direct" -> Graph_loggers_sig.Direct
  | `String "Reverse" -> Graph_loggers_sig.Reverse
  | `String "Undirected" -> Graph_loggers_sig.Undirected
  | `String "Both" -> Graph_loggers_sig.Both
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct direction", x))

let color_of_json = function
  | `String "grey" -> Graph_loggers_sig.Grey
  | `String "yellow" -> Graph_loggers_sig.Yellow
  | `String "red" -> Graph_loggers_sig.Red
  | `String "green" -> Graph_loggers_sig.Green
  | `String "white" -> Graph_loggers_sig.White
  | `String "blue" -> Graph_loggers_sig.Blue
  | `String "black" -> Graph_loggers_sig.Black
  | `String "lightskyblue" -> Graph_loggers_sig.LightSkyBlue
  | `String "palegreen" -> Graph_loggers_sig.PaleGreen
  | `String "brown" -> Graph_loggers_sig.Brown
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct color", x))

let directive_of_json = function
  | "color", color -> Graph_loggers_sig.Color (color_of_json color)
  | "fillcolor", color -> Graph_loggers_sig.FillColor (color_of_json color)
  | "label", `String string -> Graph_loggers_sig.Label string
  | "width", `Int int -> Graph_loggers_sig.Width int
  | "height", `Int int -> Graph_loggers_sig.Height int
  | "direction", direction ->
    Graph_loggers_sig.Direction (direction_of_json direction)
  | "shape", shape -> Graph_loggers_sig.Shape (shape_of_json shape)
  | "arrowhead", headkind ->
    Graph_loggers_sig.ArrowHead (headkind_of_json headkind)
  | "arrowtail", headkind ->
    Graph_loggers_sig.ArrowTail (headkind_of_json headkind)
  | "linestyle", linestyle ->
    Graph_loggers_sig.LineStyle (linestyle_of_json linestyle)
  | "position", pos_list ->
    Graph_loggers_sig.Position
      (JsonUtil.to_list
         (fun json ->
           snd
             (Loc.annoted_of_yojson
                (JsonUtil.to_unit
                   ?error_msg:(Some (JsonUtil.build_msg "position")))
                json))
         pos_list)
  | "contextual help", contextual_help ->
    Graph_loggers_sig.Contextual_help (JsonUtil.to_string contextual_help)
  | "on_click", json -> Graph_loggers_sig.OnClick json
  | _, x -> raise (Yojson.Basic.Util.Type_error ("Not a correct directive", x))

let directives_of_json directives =
  JsonUtil.to_assoc
    ~error_msg:(JsonUtil.build_msg "list of directives")
    directive_of_json directives

let id_of_json = function
  | `String string -> string
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct id", x))

let node_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    (try
       ( id_of_json (List.assoc "id" l),
         directives_of_json (List.assoc "directives" l) )
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Not a correct node", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct node", x))

let edge_of_json = function
  | `Assoc l as x when List.length l = 3 ->
    (try
       ( id_of_json (List.assoc "source" l),
         id_of_json (List.assoc "target" l),
         directives_of_json (List.assoc "directives" l) )
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Not a correct edge", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct edge", x))

let nodes_of_json =
  JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "node list") node_of_json

let edges_of_json =
  JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "edge list") edge_of_json

let of_json = function
  | `Assoc l as x when List.length l = 2 ->
    (try
       ( nodes_of_json (List.assoc "nodes" l),
         edges_of_json (List.assoc "edges" l) )
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Not a correct environment", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct environment", x))
