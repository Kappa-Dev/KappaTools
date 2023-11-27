(**
  * graph_loggers_sig.ml
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
type shape = Invisible | House | Rect | Ellipse | Circle | Invhouse
type headkind = Normal | Vee | Tee | No_head
type linestyle = Plain | Dotted | Dashed

type color =
  | Red
  | Green
  | White
  | Blue
  | Black
  | LightSkyBlue
  | PaleGreen
  | Brown
  | Yellow
  | Grey

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
  | OnClick of Yojson.Basic.t
  | Contextual_help of string
  | Position of Loc.t list

type graph =
  (string * options list) list * (string * string * options list) list

type t = {
  logger: Loggers.t;
  id_map: int Mods.StringMap.t ref;
  fresh_id: int ref;
  nodes: (string * options list) list ref;
  edges: (string * string * options list) list ref;
  edges_map: options list list Mods.String2Map.t ref;
}

let lift t = t.logger

let extend_logger logger =
  {
    logger;
    id_map = ref Mods.StringMap.empty;
    fresh_id = ref 1;
    nodes = ref [];
    edges = ref [];
    edges_map = ref Mods.String2Map.empty;
  }

let refresh_id t =
  let () = t.id_map := Mods.StringMap.empty in
  let () = t.nodes := [] in
  let () = t.edges := [] in
  let () = t.fresh_id := 1 in
  ()

let add_node t s d = t.nodes := (s, d) :: !(t.nodes)

let add_edge t s1 s2 d =
  let () = t.edges := (s1, s2, d) :: !(t.edges) in
  let map = !(t.edges_map) in
  let old_list =
    match Mods.String2Map.find_option (s1, s2) map with
    | Some l -> l
    | None -> []
  in
  let () = t.edges_map := Mods.String2Map.add (s1, s2) (d :: old_list) map in
  ()

let graph_of_logger logger = List.rev !(logger.nodes), List.rev !(logger.edges)
let get_edge_map t = !(t.edges_map)
let get_nodes t = !(t.nodes)
let fresh_id logger = Tools.get_ref logger.fresh_id

let int_of_string_id logger string =
  match Mods.StringMap.find_option string !(logger.id_map) with
  | Some a -> a
  | None ->
    let i = fresh_id logger in
    let () = logger.id_map := Mods.StringMap.add string i !(logger.id_map) in
    i
