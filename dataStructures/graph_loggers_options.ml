(**
  * graph_loggers_options.ml
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

type direction = Direct | Reverse | Undirected | Both
type shape = Invisible | House | Rect | Ellipse | Circle
type headkind = Normal | Vee | Tee | No_head
type linestyle = Plain | Dotted | Dashed

type color = Red | Green | White | Blue | Black | LightSkyBlue | PaleGreen

type options =
  | Color of color
  | FillColor of color
  | Label of string
  | Width of int (*pixel*)
  | Height of int (*pixel*)
  | Direction of direction
  | Shape of shape
  | ArrowHead of headkind
  | ArrowTail of headkind
  | LineStyle of linestyle


type graph =
  (string * options list) list
  *  (string * string * options list) list

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
  | Direct -> `String "Direct"
  | Reverse -> `String "Reverse"
  | Undirected -> `String "Undirected"
  | Both -> `String "Both"

let shape_to_json shape =
  match shape with
  | Invisible -> `String "Invisible"
  | House -> `String "House"
  | Rect -> `String "Rect"
  | Ellipse -> `String "Ellipse"
  | Circle-> `String "Circle"

let headkind_to_json headkind =
  match headkind with
  | Vee -> `String "Vee"
  | Tee -> `String "Tee"
  | No_head -> `String "No_head"
  | Normal -> `String "Normal"

let linestyle_to_json linestyle =
  match linestyle with
  | Plain -> `String "Plain"
  | Dotted -> `String "Dotted"
  | Dashed -> `String "Dashed"

let color_to_json color =
  match color with
  | Red -> `String "red"
  | Green -> `String "green"
  | White -> `String "white"
  | Blue -> `String "blue"
  | Black -> `String "black"
  | LightSkyBlue -> `String "lightskyblue"
  | PaleGreen -> `String "palegreen"

let directive_to_json option =
  match option
  with
  | Color color -> "color", color_to_json color
  | FillColor color -> "fillcolor", color_to_json color
  | Label string -> "label", `String string
  | Width int -> "width", `Int int
  | Height int -> "height", `Int int
  | Direction direction ->
    "direction", direction_to_json direction
  | Shape shape -> "shape", shape_to_json shape
  | ArrowHead headkind ->
    "arrowhead", headkind_to_json headkind
  | ArrowTail headkind ->
    "arrowtail", headkind_to_json headkind
  | LineStyle linestyle ->
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


let nodes_to_json = assoc_to_json node_to_json
let edges_to_json = assoc_to_json edge_to_json

let edges_to_json edges =
  `List
    (List.rev_map edge_to_json (List.rev edges))

let to_json graph =
   `Assoc [
    "nodes", nodes_to_json (fst graph);
    "edges", edges_to_json (snd graph)
      ]

(*    NamedDecls.to_json (fun () -> `Null) env.tokens;
      "algs", NamedDecls.to_json (fun (x,_) -> Alg_expr.to_json x) env.algs;
      "observables",
      `List (Array.fold_right
             (fun (x,_) l -> Alg_expr.to_json x :: l) env.observables []);
      "ast_rules",
      `List
      (Array.fold_right (fun (n,(r,_)) l ->
           `List [(match n with None -> `Null | Some (n,_) -> `String n);
              LKappa.rule_to_json r]::l) env.ast_rules []);*)
(* rules : Primitives.elementary_rule array;
   cc_of_unaries : Connected_component.Set.t;
   perturbations : Primitives.perturbation array;
   dependencies_in_time : Operator.DepSet.t;
   dependencies_in_event : Operator.DepSet.t;
   need_update_each_loop : Operator.DepSet.t; (*union of 2 above for perf*)
   algs_reverse_dependencies : Operator.DepSet.t array;
   tokens_reverse_dependencies : Operator.DepSet.t array;*)

let linestyle_of_json = function
  | `String "Plain" -> Plain
  | `String "Dotted" -> Dotted
  | `String "Dashed" -> Dashed
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct headkind",x))
let headkind_of_json = function
  | `String "Vee" -> Vee
  | `String "Tee" -> Tee
  | `String "No_head" -> No_head
  | `String "Normal" -> Normal
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct headkind",x))
let shape_of_json = function
  | `String "Invisible" -> Invisible
  | `String "House" -> House
  | `String "Rect" -> Rect
  | `String "Ellipse" -> Ellipse
  | `String "Circle" -> Circle
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct shape",x))

let direction_of_json = function
  | `String "Direct" -> Direct
  | `String "Reverse" -> Reverse
  | `String "Undirected" -> Undirected
  | `String "Both" -> Both
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct direction",x))

let color_of_json = function
  | `String "red" -> Red
  | `String "green" -> Green
  | `String "white" -> White
  | `String "blue" -> Blue
  | `String "black" -> Black
  | `String "lightskyblue" -> LightSkyBlue
  | `String "palegreen" -> PaleGreen
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct color",x))

let directive_of_json =
  function
  | "color", color -> Color (color_of_json color)
  | "fillcolor", color -> FillColor (color_of_json color)
  | "label", `String string  -> Label string
  | "width", `Int int -> Width int
  | "height", `Int int -> Height int
  | "direction", direction -> Direction (direction_of_json direction)
  | "shape", shape -> Shape (shape_of_json shape)
  | "arrowhead", headkind ->
    ArrowHead (headkind_of_json headkind)
  | "arrowtail", headkind ->
    ArrowTail (headkind_of_json headkind)
  | "linestyle", linestyle ->
    LineStyle (linestyle_of_json linestyle)
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
