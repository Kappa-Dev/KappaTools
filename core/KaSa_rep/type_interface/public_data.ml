(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(**************)
(* JSon labels*)
(**************)

let agent = "agent name"
let contactmap = "contact map"
let accuracy_string = "accuracy"
let dead_rules = "dead rules"
let dead_agents = "dead agents"
let map = "map"
let interface = "interface"
let site = "site name"
let stateslist = "states list"
let sitename = "site_name"
let sitetype = "site_type"
let sitelinks = "port_links"
let sitestates = "port_states"
let sitenodename = "node_type"
let sitenodeid = "node_id"
let sitenodesites = "node_sites"
let hyp = "site graph"
let refinement = "site graph list"
let domain_name = "domain name"
let refinements_list = "refinements list"
let refinement_lemmas = "refinement lemmas"
let rule_id = "id"
let agent_id = "id"
let label = "label"
let ast = "ast"
let position = "location"
let position_list = "location list"
let variable = "variable"
let rule = "rule"
let direct = "direct"
let side_effect = "side effect"
let source = "source"
let target_map = "target map"
let target = "target"
let location_pair_list = "location pair list"
let rhs = "RHS"
let lhs = "LHS"
let influencemap = "influence map"
let nodesofinfluencemap = "nodes of influence map"
let wakeup = "wake-up map"
let inhibition = "inhibition map"
let nodes = "nodes"
let total_string = "total"
let fwd_string = "fwd"
let bwd_string = "bwd"
let origin = "origin"
let direction = "direction"
let rule_hidden = "hidden"
let scc = "scc"
let accuracy_cm = "accuracy_cm"
let accuracy_scc = "accuracy_scc"
let contactmapscc = "contact map scc"
let counter = "counter"
let inf = "min"
let sup = "max"
let key = "key"
let locality = "locality"

(*******************)
(* Accuracy levels *)
(*******************)

type accuracy_level = Low | Medium | High | Full

let accuracy_levels = [ Low; Medium; High; Full ]
let contact_map_accuracy_levels = [ Low; High ]
let influence_map_accuracy_levels = [ Low; Medium; High ]
let reduction_accuracy_levels = [ Low; High ]

let accuracy_to_string = function
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"
  | Full -> "full"

let accuracy_to_json x = JsonUtil.of_string (accuracy_to_string x)

let accuracy_of_string = function
  | "low" -> Some Low
  | "medium" -> Some Medium
  | "high" -> Some High
  | "full" -> Some Full
  | _ -> None

let accuracy_of_json json =
  match accuracy_of_string (JsonUtil.to_string json) with
  | Some x -> x
  | None ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "accuracy level", json))

(******************************************************************)

module AccuracySetMap = SetMap.Make (struct
  type t = accuracy_level

  let compare a b =
    match a, b with
    | Low, Low -> 0
    | Low, _ -> -1
    | _, Low -> 1
    | Medium, Medium -> 0
    | Medium, _ -> -1
    | _, Medium -> 1
    | High, High -> 0
    | High, _ -> -1
    | _, High -> 1
    | Full, Full -> 0

  let print f = function
    | Full -> Format.fprintf f "Full"
    | High -> Format.fprintf f "High"
    | Medium -> Format.fprintf f "Medium"
    | Low -> Format.fprintf f "Low"
end)

module AccuracyMap = AccuracySetMap.Map

(******************************************************************)

(***************)
(* Contact map *)
(***************)

type contact_map = User_graph.connected_component

let site_type_to_json = function
  | User_graph.Counter i -> `List [ `String "counter"; `Int i ]
  | User_graph.Port p ->
    `List
      [
        `String "port";
        `Assoc
          [
            ( sitelinks,
              match p.User_graph.port_links with
              | User_graph.LINKS l ->
                JsonUtil.of_list
                  (fun ((xl, xr), y) ->
                    `List [ `List [ `Int xl; `Int xr ]; `Int y ])
                  l
              | User_graph.WHATEVER -> `Null
              | User_graph.SOME -> `Bool true
              | User_graph.TYPE (si, ty) ->
                `Assoc [ "site_name", `String si; "port_name", `String ty ] );
            ( sitestates,
              JsonUtil.of_option
                (JsonUtil.of_list JsonUtil.of_string)
                p.User_graph.port_states );
          ];
      ]

let site_to_json site =
  `Assoc
    [
      sitename, JsonUtil.of_string site.User_graph.site_name;
      sitetype, site_type_to_json site.User_graph.site_type;
    ]

let site_type_of_json = function
  | `List [ `String "counter"; `Int i ] -> User_graph.Counter i
  | `List [ `String "port"; `Assoc l ] as x ->
    (try
       let port_links =
         let json = List.assoc sitelinks l in
         User_graph.links_of_yojson json
       in
       let port_states =
         let json = List.assoc sitestates l in
         JsonUtil.to_option
           (JsonUtil.to_list ~error_msg:"state list"
              (JsonUtil.to_string ~error_msg:"state"))
           json
       in
       User_graph.Port { User_graph.port_links; User_graph.port_states }
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node type", x)))
  | x ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node type", x))

let site_of_json = function
  | `Assoc l as x ->
    (try
       let site_name =
         let json = List.assoc sitename l in
         JsonUtil.to_string json
       in
       let site_type =
         let json = List.assoc sitetype l in
         site_type_of_json json
       in
       { User_graph.site_name; User_graph.site_type }
     with _ ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node", x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node", x))

let site_node_sites_to_json list = JsonUtil.of_array site_to_json list

let site_node_sites_of_json =
  JsonUtil.to_array ~error_msg:"site node sites" site_of_json

let site_node_to_json = function
  | None -> `Null
  | Some node ->
    `Assoc
      [
        sitenodename, JsonUtil.of_string node.User_graph.node_type;
        sitenodesites, site_node_sites_to_json node.User_graph.node_sites;
      ]

let site_node_of_json = function
  | `Assoc l as x ->
    (try
       let node_id =
         let json = List.assoc_opt sitenodeid l in
         Option_util.map (JsonUtil.to_int ?error_msg:None) json
       in
       let node_type =
         let json = List.assoc sitenodename l in
         JsonUtil.to_string json
       in
       let node_sites =
         let json = List.assoc sitenodesites l in
         site_node_sites_of_json json
       in
       Some { User_graph.node_type; User_graph.node_id; User_graph.node_sites }
     with _ ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node", x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node", x))

let contact_map_to_json contact_map =
  `Assoc
    [
      ( contactmap,
        JsonUtil.of_pair ~lab1:accuracy_string ~lab2:map accuracy_to_json
          (JsonUtil.of_array (JsonUtil.of_array site_node_to_json))
          contact_map );
    ]

let contact_map_of_json = function
  | `Assoc l as x ->
    (try
       let json = List.assoc contactmap l in
       JsonUtil.to_pair ~lab1:accuracy_string ~lab2:map
         ~error_msg:(JsonUtil.build_msg "contact map")
         accuracy_of_json
         (JsonUtil.to_array
            ~error_msg:(JsonUtil.build_msg "site nodes list")
            (JsonUtil.to_array
               ~error_msg:(JsonUtil.build_msg "site nodes list")
               site_node_of_json))
         json
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "contact map", x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "contact map", x))

(**********************************************************)
(*strongly connected component*)

type scc = ((string * string) * (string * string)) list list

let string_pair_to_json (a, b) =
  JsonUtil.of_pair ~lab1:agent ~lab2:sitename JsonUtil.of_string
    JsonUtil.of_string (a, b)

let string_pair_of_json (json : Yojson.Basic.t) : string * string =
  JsonUtil.to_pair ~lab1:agent ~lab2:sitename ~error_msg:"site"
    (JsonUtil.to_string ~error_msg:"agent name")
    (JsonUtil.to_string ~error_msg:"site_name")
    json

let string_pair_pair_to_json (a, b) =
  JsonUtil.of_pair string_pair_to_json string_pair_to_json (a, b)

let string_pair_pair_of_json json : (string * string) * (string * string) =
  JsonUtil.to_pair ~error_msg:"bond" string_pair_of_json string_pair_of_json
    json

let string_pair_pair_list_to_json l =
  JsonUtil.of_list string_pair_pair_to_json l

let string_pair_pair_list_of_json json =
  JsonUtil.to_list ~error_msg:"list_of_bonds" string_pair_pair_of_json json

let string_pair_pair_list_list_to_json l =
  JsonUtil.of_list string_pair_pair_list_to_json l

let string_pair_pair_list_list_of_json json =
  JsonUtil.to_list ~error_msg:"list_of_lists_of_bonds"
    string_pair_pair_list_of_json json

let scc_to_json (cm_acc, scc_acc, scc) =
  `Assoc
    [
      ( contactmapscc,
        JsonUtil.of_triple ~lab1:accuracy_cm ~lab2:accuracy_scc ~lab3:map
          accuracy_to_json accuracy_to_json string_pair_pair_list_list_to_json
          (cm_acc, scc_acc, scc) );
    ]

let scc_of_json = function
  | `Assoc l as x ->
    (try
       let json = List.assoc contactmapscc l in
       JsonUtil.to_triple ~lab1:accuracy_cm ~lab2:accuracy_scc ~lab3:map
         ~error_msg:"scc decomposition" accuracy_of_json accuracy_of_json
         string_pair_pair_list_list_of_json json
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error
            (JsonUtil.build_msg "scc decomposition", x)))
  | x ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "scc decomposition", x))

(******************************************************************************)

(**************)
(* dead rules *)
(**************)

type rule_direction =
  | Direct_rule
  | Reverse_rule
  | Both_directions
  | Dummy_rule_direction
  | Variable

type rule = {
  rule_id: int;
  rule_label: string;
  rule_ast: string;
  rule_position: Loc.t;
  rule_direction: rule_direction;
  rule_hidden: bool;
}

let direction_to_json d =
  match d with
  | Direct_rule -> `String "direct"
  | Reverse_rule -> `String "reverse"
  | Both_directions -> `String "both"
  | Dummy_rule_direction -> `String "dummy"
  | Variable -> `String "variable"

let json_to_direction s =
  match s with
  | `String "direct" -> Direct_rule
  | `String "reverse" -> Reverse_rule
  | `String "both" -> Both_directions
  | `String "dummy" -> Dummy_rule_direction
  | `String "variable" -> Variable
  | x -> raise (Yojson.Basic.Util.Type_error ("rule direction", x))

let rule_to_json rule =
  `Assoc
    [
      rule_id, JsonUtil.of_int rule.rule_id;
      label, JsonUtil.of_string rule.rule_label;
      ast, JsonUtil.of_string rule.rule_ast;
      position, Loc.yojson_of_annoted JsonUtil.of_unit ((), rule.rule_position);
      direction, direction_to_json rule.rule_direction;
      rule_hidden, JsonUtil.of_bool rule.rule_hidden;
    ]

let json_to_rule = function
  | `Assoc l as x when List.length l = 6 ->
    (try
       {
         rule_id = JsonUtil.to_int (List.assoc rule_id l);
         rule_label = JsonUtil.to_string (List.assoc label l);
         rule_ast = JsonUtil.to_string (List.assoc ast l);
         rule_position =
           snd
             (Loc.annoted_of_yojson
                (JsonUtil.to_unit ~error_msg:(JsonUtil.build_msg "locality"))
                (List.assoc position l));
         rule_direction = json_to_direction (List.assoc direction l);
         rule_hidden = JsonUtil.to_bool (List.assoc rule_hidden l);
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg " rule", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("rule", x))

type var = {
  var_id: int;
  var_label: string;
  var_ast: string;
  var_position: Loc.t;
}

let var_to_json var =
  `Assoc
    [
      rule_id, JsonUtil.of_int var.var_id;
      label, JsonUtil.of_string var.var_label;
      ast, JsonUtil.of_string var.var_ast;
      position, Loc.yojson_of_annoted JsonUtil.of_unit ((), var.var_position);
    ]

let json_to_var = function
  | `Assoc l as x when List.length l = 4 ->
    (try
       {
         var_id = JsonUtil.to_int (List.assoc rule_id l);
         var_label = JsonUtil.to_string (List.assoc label l);
         var_ast = JsonUtil.to_string (List.assoc ast l);
         var_position =
           snd
             (Loc.annoted_of_yojson
                (JsonUtil.to_unit ~error_msg:(JsonUtil.build_msg "locality"))
                (List.assoc position l));
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg " var", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("var", x))

type ('rule, 'var) influence_node = Rule of 'rule | Var of 'var
type pos_of_rules_and_vars = ((int, int) influence_node * Loc.t) list

let influence_node_to_json rule_to_json var_to_json a =
  match a with
  | Var i -> `Assoc [ variable, var_to_json i ]
  | Rule i -> `Assoc [ rule, rule_to_json i ]

let influence_node_of_json json_to_rule json_to_var = function
  | `Assoc [ (s, json) ] when s = variable -> Var (json_to_var json)
  | `Assoc [ (s, json) ] when s = rule -> Rule (json_to_rule json)
  | x ->
    let error_msg = "Not a correct influence node" in
    raise (Yojson.Basic.Util.Type_error (error_msg, x))

let short_influence_node_to_json =
  influence_node_to_json JsonUtil.of_int JsonUtil.of_int

let short_influence_node_of_json =
  influence_node_of_json
    (JsonUtil.to_int ~error_msg:(JsonUtil.build_msg "rule id"))
    (JsonUtil.to_int ~error_msg:(JsonUtil.build_msg "var id"))

let pos_of_rules_and_vars_to_json =
  JsonUtil.of_list
    (JsonUtil.of_pair ~lab1:key ~lab2:locality short_influence_node_to_json
       (fun loc -> Loc.yojson_of_annoted JsonUtil.of_unit ((), loc)))

let pos_of_rules_and_vars_of_json =
  JsonUtil.to_list
    (JsonUtil.to_pair ~lab1:key ~lab2:locality short_influence_node_of_json
       (fun x ->
         snd
           (Loc.annoted_of_yojson
              (JsonUtil.to_unit ~error_msg:(JsonUtil.build_msg "locality"))
              x)))

let refined_influence_node_to_json =
  influence_node_to_json rule_to_json var_to_json

let refined_influence_node_of_json =
  influence_node_of_json json_to_rule json_to_var

let short_node_of_refined_node = function
  | Rule rule -> Rule rule.rule_id
  | Var var -> Var var.var_id

let position_of_refined_influence_node = function
  | Rule r -> r.rule_position
  | Var v -> v.var_position

module InfluenceNodeSetMap = SetMap.Make (struct
  type t = (int, int) influence_node

  let compare = compare

  let print f = function
    | Rule r -> Format.fprintf f "Rule %i" r
    | Var r -> Format.fprintf f "Var %i" r
end)

module InfluenceNodeMap = InfluenceNodeSetMap.Map

(* Relations *)

type 'a pair = 'a * 'a
type location = Direct of int | Side_effect of int

let dump_location fmt = function
  | Direct int -> Format.fprintf fmt "%i" int
  | Side_effect int -> Format.fprintf fmt "%i*" int

let dump_location_pair fmt (a, b) =
  Format.fprintf fmt "(%a,%a)" dump_location a dump_location b

let dump_location_pair_list fmt l =
  Format.fprintf fmt "[%a]"
    (Pp.list (fun fmt -> Format.pp_print_string fmt ";") dump_location_pair)
    l

let string_of_label_list l = Format.asprintf "%a" dump_location_pair_list l

type half_influence_map =
  location pair list InfluenceNodeMap.t InfluenceNodeMap.t

type influence_map = {
  nodes: (rule, var) influence_node list;
  positive: half_influence_map;
  negative: half_influence_map;
}

(* Location labels *)
let location_to_json a =
  match a with
  | Direct i -> `Assoc [ direct, JsonUtil.of_int i ]
  | Side_effect i -> `Assoc [ side_effect, JsonUtil.of_int i ]

let location_of_json ?(error_msg = "Not a correct location") = function
  | `Assoc [ (s, json) ] when s = direct -> Direct (JsonUtil.to_int json)
  | `Assoc [ (s, json) ] when s = side_effect ->
    Side_effect (JsonUtil.to_int json)
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg, x))

let half_influence_map_to_json =
  InfluenceNodeMap.to_json ~lab_key:source ~lab_value:target_map
    short_influence_node_to_json
    (InfluenceNodeMap.to_json ~lab_key:target ~lab_value:location_pair_list
       short_influence_node_to_json
       (JsonUtil.of_list
          (JsonUtil.of_pair ~lab1:rhs ~lab2:lhs location_to_json
             location_to_json)))

let half_influence_map_of_json =
  InfluenceNodeMap.of_json
    ~error_msg:(JsonUtil.build_msg "activation or inhibition map")
    ~lab_key:source ~lab_value:target_map short_influence_node_of_json
    (InfluenceNodeMap.of_json ~lab_key:target ~lab_value:location_pair_list
       ~error_msg:"map of lists of pairs of locations"
       short_influence_node_of_json
       (JsonUtil.to_list ~error_msg:"list of pair of locations"
          (JsonUtil.to_pair ~error_msg:"" ~lab1:rhs ~lab2:lhs
             (location_of_json ~error_msg:(JsonUtil.build_msg "location"))
             (location_of_json ~error_msg:(JsonUtil.build_msg "location")))))

(* Influence map *)

let nodes_list_to_json = JsonUtil.of_list refined_influence_node_to_json
let nodes_list_of_json = JsonUtil.to_list refined_influence_node_of_json

let influence_map_to_json influence_map =
  `Assoc
    [
      ( influencemap,
        JsonUtil.of_pair ~lab1:accuracy_string ~lab2:map accuracy_to_json
          (fun influence_map ->
            `Assoc
              [
                nodes, nodes_list_to_json influence_map.nodes;
                wakeup, half_influence_map_to_json influence_map.positive;
                inhibition, half_influence_map_to_json influence_map.negative;
              ])
          influence_map );
    ]

let nodes_of_influence_map_to_json nodes_list =
  `Assoc
    [
      ( nodesofinfluencemap,
        JsonUtil.of_pair ~lab1:accuracy_string ~lab2:map accuracy_to_json
          (fun nodes_list -> `Assoc [ nodes, nodes_list_to_json nodes_list ])
          nodes_list );
    ]

let nodes_of_influence_map_of_json = function
  | `Assoc l as x ->
    (try
       let json = List.assoc nodesofinfluencemap l in
       JsonUtil.to_pair ~lab1:accuracy_string ~lab2:map
         ~error_msg:(JsonUtil.build_msg "nodes of influence map1")
         accuracy_of_json
         (function
           | `Assoc l as x when List.length l = 1 ->
             (try nodes_list_of_json (List.assoc nodes l)
              with Not_found ->
                raise
                  (Yojson.Basic.Util.Type_error
                     (JsonUtil.build_msg "nodes of influence map", x)))
           | x ->
             raise
               (Yojson.Basic.Util.Type_error
                  (JsonUtil.build_msg "nodes of influence map", x)))
         json
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error
            (JsonUtil.build_msg "nodes of influence map", x)))
  | x ->
    raise
      (Yojson.Basic.Util.Type_error
         (JsonUtil.build_msg "nodes of influence map", x))

let local_influence_map_to_json influence_map =
  let accuracy, total, bwd, fwd, origin_opt, influence_map = influence_map in
  `Assoc
    [
      ( influencemap,
        `Assoc
          [
            accuracy_string, accuracy_to_json accuracy;
            total_string, JsonUtil.of_int total;
            fwd_string, JsonUtil.of_option JsonUtil.of_int fwd;
            bwd_string, JsonUtil.of_option JsonUtil.of_int bwd;
            origin, JsonUtil.of_option refined_influence_node_to_json origin_opt;
            ( map,
              (fun influence_map ->
                `Assoc
                  [
                    nodes, nodes_list_to_json influence_map.nodes;
                    wakeup, half_influence_map_to_json influence_map.positive;
                    ( inhibition,
                      half_influence_map_to_json influence_map.negative );
                  ])
                influence_map );
          ] );
    ]

let influence_map_of_json = function
  | `Assoc l as x ->
    (try
       let json = List.assoc influencemap l in
       JsonUtil.to_pair ~lab1:accuracy_string ~lab2:map
         ~error_msg:(JsonUtil.build_msg "influence map1")
         accuracy_of_json
         (function
           | `Assoc l as x when List.length l = 3 ->
             (try
                {
                  nodes = nodes_list_of_json (List.assoc nodes l);
                  positive = half_influence_map_of_json (List.assoc wakeup l);
                  negative =
                    half_influence_map_of_json (List.assoc inhibition l);
                }
              with Not_found ->
                raise
                  (Yojson.Basic.Util.Type_error
                     (JsonUtil.build_msg "influence map", x)))
           | x ->
             raise
               (Yojson.Basic.Util.Type_error
                  (JsonUtil.build_msg "influence map", x)))
         json
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "influence map", x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "influence map", x))

let local_influence_map_of_json = function
  | `Assoc l as x ->
    (try
       let json = List.assoc influencemap l in
       match json with
       | `Assoc l' ->
         let accuracy = accuracy_of_json (List.assoc accuracy_string l') in
         let total = JsonUtil.to_int (List.assoc total_string l') in
         let error_msg = JsonUtil.build_msg "fwd radius" in
         let fwd =
           JsonUtil.to_option
             (JsonUtil.to_int ~error_msg)
             (List.assoc fwd_string l')
         in
         let error_msg = JsonUtil.build_msg "bwd radius" in
         let bwd =
           JsonUtil.to_option
             (JsonUtil.to_int ~error_msg)
             (List.assoc bwd_string l')
         in
         let origin =
           JsonUtil.to_option refined_influence_node_of_json
             (List.assoc origin l')
         in
         let influence_map =
           (function
             | `Assoc l as x when List.length l = 3 ->
               (try
                  {
                    nodes = nodes_list_of_json (List.assoc nodes l);
                    positive = half_influence_map_of_json (List.assoc wakeup l);
                    negative =
                      half_influence_map_of_json (List.assoc inhibition l);
                  }
                with Not_found ->
                  raise
                    (Yojson.Basic.Util.Type_error
                       (JsonUtil.build_msg "local influence map", x)))
             | x ->
               raise
                 (Yojson.Basic.Util.Type_error
                    (JsonUtil.build_msg "local influence map", x)))
             (List.assoc map l')
         in
         accuracy, total, fwd, bwd, origin, influence_map
       | _ ->
         raise
           (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "influence map", x))
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "influence map", x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "influence map", x))

(***************)
(* dead rules  *)
(***************)

type dead_rules = rule list

let dead_rules_to_json json =
  `Assoc [ dead_rules, JsonUtil.of_list rule_to_json json ]

let dead_rules_of_json = function
  | `Assoc [ (s, json) ] as x when s = dead_rules ->
    (try JsonUtil.to_list json_to_rule json
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg dead_rules, x)))
  | x -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg dead_rules, x))

(***************)
(* dead agents *)
(***************)

type agent_kind = {
  agent_id: int;
  agent_ast: string;
  agent_position: Loc.t list;
}

let json_to_agent_kind = function
  | `Assoc l as x when List.length l = 3 ->
    (try
       {
         agent_id = JsonUtil.to_int (List.assoc agent_id l);
         agent_ast = JsonUtil.to_string (List.assoc ast l);
         agent_position =
           JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "locality list")
             (fun json ->
               snd
                 (Loc.annoted_of_yojson
                    (JsonUtil.to_unit
                       ~error_msg:(JsonUtil.build_msg "locality"))
                    json))
             (List.assoc position_list l);
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "agent kind", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("agent kind", x))

let agent_kind_to_json agent_kind =
  `Assoc
    [
      agent_id, JsonUtil.of_int agent_kind.agent_id;
      ast, JsonUtil.of_string agent_kind.agent_ast;
      ( position_list,
        JsonUtil.of_list
          (fun a -> Loc.yojson_of_annoted JsonUtil.of_unit ((), a))
          agent_kind.agent_position );
    ]

type dead_agents = agent_kind list

let json_of_dead_agents json =
  `Assoc [ dead_agents, JsonUtil.of_list agent_kind_to_json json ]

let json_to_dead_agents = function
  | `Assoc [ (s, json) ] as x when s = dead_agents ->
    (try JsonUtil.to_list json_to_agent_kind json
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg dead_agents, x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg dead_agents, x))

(*************************************)
(* non weakly reversible transitions *)
(*************************************)

type separating_transitions = (rule * (string * string) list) list

let separating_transitions_to_json =
  JsonUtil.of_list
    (JsonUtil.of_pair ~lab1:"rule" ~lab2:"potential-contexts" rule_to_json
       (JsonUtil.of_list
          (JsonUtil.of_pair ~lab1:"s1" ~lab2:"s2" JsonUtil.of_string
             JsonUtil.of_string)))

let separating_transitions_of_json =
  JsonUtil.to_list ~error_msg:"separating transitions list"
    (JsonUtil.to_pair ~error_msg:"separating transition" ~lab1:"rule"
       ~lab2:"potential-contexts" json_to_rule
       (JsonUtil.to_list ~error_msg:"separating transitions"
          (JsonUtil.to_pair ~error_msg:"transition" ~lab1:"s1" ~lab2:"s2"
             (JsonUtil.to_string ?error_msg:None)
             (JsonUtil.to_string ?error_msg:None))))

(***************)
(* Constraints *)
(***************)

type binding_state =
  | Free
  | Wildcard
  | Bound_to_unknown
  | Bound_to of int
  | Binding_type of string * string

type agent =
  string
  * (string
    * string option
    * binding_state option
    * (int option * int option) option)
    list

type 'site_graph lemma = { hyp: 'site_graph; refinement: 'site_graph list }
type 'site_graph poly_constraints_list = (string * 'site_graph lemma list) list

let lemma_to_json site_graph_to_json json =
  JsonUtil.of_pair ~lab1:hyp ~lab2:refinement site_graph_to_json
    (JsonUtil.of_list site_graph_to_json)
    (json.hyp, json.refinement)

let lemma_of_json site_graph_of_json json =
  let a, b =
    JsonUtil.to_pair ~lab1:hyp ~lab2:refinement ~error_msg:"lemma"
      site_graph_of_json
      (JsonUtil.to_list ~error_msg:"refinements list" site_graph_of_json)
      json
  in
  { hyp = a; refinement = b }

let get_hyp h = h.hyp
let get_refinement r = r.refinement
let free = ""
let wildcard = "?"
let bound = "!_"
let bond_id = "bond id"
let bound_to = "bound to"
let binding_type = "binding type"
let prop = "property state"
let bind = "binding state"
let binding_type_backend_symbol = "."
let free_backend_symbol = "."
let missing_binding_site_backend_symbol = ""
let wildcard_backend_symbol = "#"
let bound_to_unknown_backend_symbol = "_"
let internal_state_introduction_backend_symbol = "~"
let internal_state_delimiter_backend_symbol = ","
let binding_state_delimiter_backend_symbol = ","
let binding_state_opening_backend_symbol = "["
let binding_state_closing_backend_symbol = "]"
let internal_state_opening_backend_symbol = "{"
let internal_state_closing_backend_symbol = "}"
let counter_state_opening_backend_symbol = "{"
let counter_state_closing_backend_symbol = "}"
let counter_state_range_backend_symbol = " .. "
let open_interval_inclusive_symbol = "["
let close_interval_inclusive_symbol = "]"
let open_interval_exclusive_symbol = "]"
let close_interval_exclusive_symbol = "["
let plus_infinity_symbol = "+oo"
let minus_infinity_symbol = "-oo"

let string_of_binding_type ?(binding_type_symbol = ".") ~agent_name ~site_name
    () =
  Format.sprintf "%s%s%s" site_name binding_type_symbol agent_name

let binding_state_light_of_json = function
  | `Assoc [ (s, `Null) ] when s = free -> Free
  | `Assoc [ (s, `Null) ] when s = wildcard -> Wildcard
  | `Assoc [ (s, `Null) ] when s = bound_to -> Bound_to_unknown
  | `Assoc [ (s, j) ] when s = bond_id ->
    let i = JsonUtil.to_int ~error_msg:"wrong binding id" j in
    let bond_index = i in
    Bound_to bond_index
  | `Assoc [ (s, j) ] when s = binding_type ->
    let agent_name, site_name =
      JsonUtil.to_pair ~lab1:"agent" ~lab2:"site" ~error_msg:"binding type"
        (JsonUtil.to_string ~error_msg:"agent name")
        (JsonUtil.to_string ~error_msg:"site name")
        j
    in
    Binding_type (agent_name, site_name)
  | x -> raise (Yojson.Basic.Util.Type_error ("wrong binding state", x))

let binding_state_light_to_json = function
  | Free -> `Assoc [ free, `Null ]
  | Wildcard -> `Assoc [ wildcard, `Null ]
  | Bound_to_unknown -> `Assoc [ bound_to, `Null ]
  | Bound_to bond_index -> `Assoc [ bond_id, JsonUtil.of_int bond_index ]
  | Binding_type (agent_name, site_name) ->
    let j =
      JsonUtil.of_pair ~lab1:"agent" ~lab2:"site" JsonUtil.of_string
        JsonUtil.of_string (agent_name, site_name)
    in
    `Assoc [ binding_type, j ]

let counter_state_light_of_json =
  JsonUtil.to_pair ~lab1:inf ~lab2:sup ~error_msg:"wrong counter state"
    (JsonUtil.to_option (JsonUtil.to_int ~error_msg:counter))
    (JsonUtil.to_option (JsonUtil.to_int ~error_msg:counter))

let counter_state_light_to_json =
  JsonUtil.of_pair ~lab1:inf ~lab2:sup
    (JsonUtil.of_option JsonUtil.of_int)
    (JsonUtil.of_option JsonUtil.of_int)

let interface_light_to_json intf =
  JsonUtil.of_map ~lab_key:site ~lab_value:stateslist
    ~fold:(fun f a x ->
      List.fold_left (fun list (k, a, b, c) -> f k (a, b, c) list) x a)
    (*json -> elt*)
      (fun site -> JsonUtil.of_string site)
    (*json -> 'value*)
    (JsonUtil.of_triple ~lab1:prop ~lab2:bind ~lab3:counter
       (JsonUtil.of_option JsonUtil.of_string)
       (JsonUtil.of_option binding_state_light_to_json)
       (JsonUtil.of_option counter_state_light_to_json))
    intf

let interface_light_of_json json =
  JsonUtil.to_map ~lab_key:site ~lab_value:stateslist ~error_msg:interface
    ~empty:[]
    ~add:(fun k (a, b, c) list -> (k, a, b, c) :: list)
    (*json -> elt*)
      (fun json -> JsonUtil.to_string ~error_msg:site json)
    (*json -> 'value*)
    (JsonUtil.to_triple ~lab1:prop ~lab2:bind ~lab3:counter
       ~error_msg:"wrong binding state"
       (JsonUtil.to_option (JsonUtil.to_string ~error_msg:prop))
       (JsonUtil.to_option binding_state_light_of_json)
       (JsonUtil.to_option counter_state_light_of_json))
    json

let agent_gen_of_json interface_of_json =
  JsonUtil.to_pair ~lab1:agent ~lab2:interface ~error_msg:"agent"
    (JsonUtil.to_string ~error_msg:"agent name")
    interface_of_json

let poly_constraints_list_of_json site_graph_of_json =
  JsonUtil.to_list
    (JsonUtil.to_pair ~error_msg:"constraints list" ~lab1:domain_name
       ~lab2:refinements_list
       (JsonUtil.to_string ~error_msg:"abstract domain")
       (JsonUtil.to_list (lemma_of_json site_graph_of_json)))

let lemmas_list_of_json_gen interface_of_json = function
  | `Assoc l as x ->
    (try
       let json = List.assoc refinement_lemmas l in
       poly_constraints_list_of_json
         (JsonUtil.to_list ~error_msg:"site graph"
            (agent_gen_of_json interface_of_json))
         json
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error
            (JsonUtil.build_msg "refinement lemmas list", x)))
  | x ->
    raise
      (Yojson.Basic.Util.Type_error
         (JsonUtil.build_msg "refinement lemmas list", x))

let lemmas_list_of_json json =
  lemmas_list_of_json_gen interface_light_of_json json

let agent_gen_to_json interface_to_json =
  JsonUtil.of_pair ~lab1:agent ~lab2:interface JsonUtil.of_string
    interface_to_json

let poly_constraints_list_to_json site_graph_to_json constraints =
  JsonUtil.of_list
    (JsonUtil.of_pair ~lab1:domain_name ~lab2:refinements_list
       JsonUtil.of_string
       (JsonUtil.of_list (lemma_to_json site_graph_to_json)))
    constraints

let lemmas_list_to_json_gen interface_to_json constraints =
  `Assoc
    [
      ( refinement_lemmas,
        poly_constraints_list_to_json
          (JsonUtil.of_list (agent_gen_to_json interface_to_json))
          constraints );
    ]

let lemmas_list_to_json constraints =
  lemmas_list_to_json_gen interface_light_to_json constraints
