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

let list_to_json to_json l =
  `List
    (List.rev_map to_json (List.rev l))

let json_to_list title of_json json =
  match json
  with
  | `List l as x ->
    begin
      try
        List.rev_map of_json (List.rev l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (("Not a correct "^title),x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error (("Not a correct "^title),x))

let assoc_to_json to_json l =
  `Assoc
    (List.rev_map to_json (List.rev l))

let json_to_assoc title of_json json =
  match json
  with
  | `Assoc l as x ->
    begin
      try
        List.rev_map of_json (List.rev l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (("Not a correct "^title),x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error (("Not a correct "^title),x))

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
  | Graph_loggers_sig.Rect -> `String "Rect"
  | Graph_loggers_sig.Ellipse -> `String "Ellipse"
  | Graph_loggers_sig.Circle-> `String "Circle"

let headkind_to_json headkind =
  match headkind with
  | Graph_loggers_sig.Vee -> `String "Vee"
  | Graph_loggers_sig.Tee -> `String "Tee"
  | Graph_loggers_sig.No_head -> `String "No_head"
  | Graph_loggers_sig.Normal -> `String "Normal"

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

let directive_to_json option =
  match option
  with
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

let directives_to_json = assoc_to_json directive_to_json

let node_to_json (id, directives) =
  `Assoc [
    "id", `String id;
    "directives", directives_to_json directives
  ]
let edge_to_json (id1, id2, directives) =
  `Assoc [
    "from", `String id1 ;
    "to", `String id2;
    "directives", directives_to_json directives
  ]


let nodes_to_json = list_to_json node_to_json
let edges_to_json = list_to_json edge_to_json

let edges_to_json edges =
  `List
    (List.rev_map edge_to_json (List.rev edges))

let to_json graph =
  (`Assoc [
    "nodes", nodes_to_json (fst graph);
    "edges", edges_to_json (snd graph)
    ]: Yojson.Basic.json)

let linestyle_of_json = function
  | `String "Plain" -> Graph_loggers_sig.Plain
  | `String "Dotted" -> Graph_loggers_sig.Dotted
  | `String "Dashed" -> Graph_loggers_sig.Dashed
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct headkind",x))
let headkind_of_json = function
  | `String "Vee" -> Graph_loggers_sig.Vee
  | `String "Tee" -> Graph_loggers_sig.Tee
  | `String "No_head" -> Graph_loggers_sig.No_head
  | `String "Normal" -> Graph_loggers_sig.Normal
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct headkind",x))
let shape_of_json = function
  | `String "Invisible" -> Graph_loggers_sig.Invisible
  | `String "House" -> Graph_loggers_sig.House
  | `String "Rect" -> Graph_loggers_sig.Rect
  | `String "Ellipse" -> Graph_loggers_sig.Ellipse
  | `String "Circle" -> Graph_loggers_sig.Circle
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct shape",x))

let direction_of_json = function
  | `String "Direct" -> Graph_loggers_sig.Direct
  | `String "Reverse" -> Graph_loggers_sig.Reverse
  | `String "Undirected" -> Graph_loggers_sig.Undirected
  | `String "Both" -> Graph_loggers_sig.Both
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct direction",x))

let color_of_json = function
  | `String "red" -> Graph_loggers_sig.Red
  | `String "green" -> Graph_loggers_sig.Green
  | `String "white" -> Graph_loggers_sig.White
  | `String "blue" -> Graph_loggers_sig.Blue
  | `String "black" -> Graph_loggers_sig.Black
  | `String "lightskyblue" -> Graph_loggers_sig.LightSkyBlue
  | `String "palegreen" -> Graph_loggers_sig.PaleGreen
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct color",x))

let directive_of_json =
  function
  | "color", color -> Graph_loggers_sig.Color (color_of_json color)
  | "fillcolor", color -> Graph_loggers_sig.FillColor (color_of_json color)
  | "label", `String string  -> Graph_loggers_sig.Label string
  | "width", `Int int -> Graph_loggers_sig.Width int
  | "height", `Int int -> Graph_loggers_sig.Height int
  | "direction", direction -> Graph_loggers_sig.Direction (direction_of_json direction)
  | "shape", shape -> Graph_loggers_sig.Shape (shape_of_json shape)
  | "arrowhead", headkind ->
    Graph_loggers_sig.ArrowHead (headkind_of_json headkind)
  | "arrowtail", headkind ->
    Graph_loggers_sig.ArrowTail (headkind_of_json headkind)
  | "linestyle", linestyle ->
    Graph_loggers_sig.LineStyle (linestyle_of_json linestyle)
  | (_,x) -> raise (Yojson.Basic.Util.Type_error ("Not a correct directive",x))

let directives_of_json = function
  | `Assoc l  ->
    List.rev_map directive_of_json (List.rev l)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct directive list",x))

let id_of_json = function
  | `String string -> string
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct id",x))

let node_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        id_of_json (List.assoc "id" l),
        directives_of_json (List.assoc "directives" l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct node",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct node",x))

let edge_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        id_of_json (List.assoc "from" l),
        id_of_json (List.assoc "to" l),
        directives_of_json (List.assoc "directives" l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct edge",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct edge",x))

let nodes_of_json = json_to_list "node list" node_of_json
let edges_of_json = json_to_list "edge list" edge_of_json
let of_json = function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        nodes_of_json (List.assoc "nodes" l),
        edges_of_json (List.assoc "edges" l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))

(*tokens = NamedDecls.of_json (fun _ -> ()) (List.assoc "tokens" l);
          algs = NamedDecls.of_json
              (fun x -> Location.dummy_annot (Alg_expr.of_json x))
              (List.assoc "algs" l);
          observables = (match List.assoc "observables" l with
              | `List o ->
                Tools.array_map_of_list
                  (fun x -> Location.dummy_annot (Alg_expr.of_json x)) o
              | _ -> raise Not_found);
          ast_rules = (match List.assoc "ast_rules" l with
              | `List o ->
                Tools.array_map_of_list
                  (function
                    | `List [`Null;r]->
                      (None, Location.dummy_annot (LKappa.rule_of_json r))
                    | `List [`String n;r]->
                      (Some (Location.dummy_annot n),
                       Location.dummy_annot (LKappa.rule_of_json r))
                    | _ -> raise Not_found) o
              | _ -> raise Not_found);
          rules = [||];
          cc_of_unaries = Connected_component.Set.empty;
          perturbations = [||];
          dependencies_in_time = Operator.DepSet.empty;
          dependencies_in_event = Operator.DepSet.empty;
          need_update_each_loop = Operator.DepSet.empty;
          algs_reverse_dependencies = [||];
          tokens_reverse_dependencies = [||];
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct environment",x))*)
