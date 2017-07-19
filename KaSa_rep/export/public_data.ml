(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(**************)
(* JSon labels*)
(**************)

let agent="agent name"
let contactmap="contact map"
let accuracy_string = "accuracy"
let dead_rules = "dead rules"
let map = "map"
let interface="interface"
let site="site name"
let stateslist="states list"
let prop="property states"
let bind="binding states"
let sitename = site
let sitelinks = bind
let sitestates = prop
let sitenodename = agent
let sitenodesites = "interface"
let hyp = "site graph"
let refinement = "site graph list"
let domain_name = "domain name"
let refinements_list = "refinements list"
let refinement_lemmas="refinement lemmas"
(*******************)
(* Accuracy levels *)
(*******************)

type accuracy_level = Low | Medium | High | Full

let accuracy_to_json accuracy =
  JsonUtil.of_string
    (
      match
        accuracy
      with
      | Low -> "low"
      | Medium -> "medium"
      | High -> "high"
      | Full -> "full"
    )

let accuracy_of_json json =
  match
    JsonUtil.to_string json
  with
  | "low" -> Low
  | "medium" -> Medium
  | "high" -> High
  | "full" -> Full
  | _ ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "accuracy level",json))

(******************************************************************)

module AccuracySetMap =
  SetMap.Make
    (struct
      type t = accuracy_level
      let compare a b =
        match a,b with
        | Low,Low -> 0
        | Low,_ -> -1
        | _,Low -> 1
        | Medium,Medium -> 0
        | Medium,_ -> -1
        | _,Medium -> 1
        | High, High -> 0
        | High,_ -> -1
        | _,High -> 1
        | Full,Full -> 0

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

type site = {
     site_name: string;
     site_links: (int * int) list;
     site_states: string list;
}

type site_node = {
     site_node_name: string;
     site_node_sites: site list (*ocaml repr="array">*);
}

type site_graph = site_node list (*ocaml repr="array"*)
type contact_map = site_graph

let site_to_json site =
  `Assoc
    [
      sitename,
      JsonUtil.of_string site.site_name;

      sitelinks,
      JsonUtil.of_list (JsonUtil.of_pair JsonUtil.of_int JsonUtil.of_int) site.site_links;

      sitestates,
      JsonUtil.of_list JsonUtil.of_string site.site_states
    ]

let site_of_json =
  function
    | `Assoc l as x ->
      begin
        try
          let site_name =
            let json = List.assoc sitename l in
            JsonUtil.to_string json
          in
          let site_links =
            let json = List.assoc sitelinks l in
            JsonUtil.to_list ~error_msg:"link list"
              (JsonUtil.to_pair ~error_msg:"link"
                 (JsonUtil.to_int ~error_msg:"agent id")
                 (JsonUtil.to_int ~error_msg:"site id"))
              json
          in
          let site_states =
            let json = List.assoc sitestates l in
            JsonUtil.to_list ~error_msg:"state list"
              (JsonUtil.to_string ~error_msg:"state")
              json
          in
          {
            site_name;
            site_links;
            site_states;
          }
        with
        | _ ->
          raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node",x))
      end
    | x -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node",x))


let site_node_sites_to_json list =
  JsonUtil.of_list site_to_json list

let site_node_sites_of_json =
  JsonUtil.to_list
    ~error_msg:"site node sites"
    site_of_json



let site_node_to_json node =
  `Assoc
    [ sitenodename,
      JsonUtil.of_string node.site_node_name;

      sitenodesites,
      site_node_sites_to_json node.site_node_sites]


let site_node_of_json =
function
| `Assoc l as x ->
  begin
    try
      let site_node_name =
        let json = List.assoc sitenodename l in
        JsonUtil.to_string json
      in
      let site_node_sites =
        let json = List.assoc sitenodesites l in
        site_node_sites_of_json json
      in
      {
        site_node_name;
        site_node_sites
      }
    with
    | _ ->
      raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node",x))
  end
| x -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "site node",x))

let contact_map_to_json contact_map=
  `Assoc
    [contactmap,
     JsonUtil.of_pair
       ~lab1:accuracy_string ~lab2:map
       accuracy_to_json
       (JsonUtil.of_list
          site_node_to_json)
       contact_map
    ]


let contact_map_of_json =
  function
  | `Assoc l as x ->
    begin
      try
        let json = List.assoc contactmap l in
        JsonUtil.to_pair
          ~lab1:accuracy_string ~lab2:map
          ~error_msg:(JsonUtil.build_msg "contact map")
          accuracy_of_json
          (JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "site nodes list")
             site_node_of_json
          )
          json


(*
             Mods.StringSetMap.Map.of_json
             ~lab_key:agent ~lab_value:interface
             (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "agent name"))
             (Mods.StringSetMap.Map.of_json
                ~error_msg:(JsonUtil.build_msg "interface")
                ~lab_key:site ~lab_value:stateslist
                (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "site name"))
                  (JsonUtil.to_pair
                     ~error_msg:(JsonUtil.build_msg "pair of lists of sites")
                     ~lab1:props ~lab2:binds
                     (JsonUtil.to_list
                        ~error_msg:(JsonUtil.build_msg "list of internal states")
                        (JsonUtil.to_string
                           ~error_msg:(JsonUtil.build_msg "internal state")))
                     (JsonUtil.to_list
                        ~error_msg:(JsonUtil.build_msg "list of binding states")
                        (JsonUtil.to_pair
                           ~error_msg:(JsonUtil.build_msg "binding type")
                           ~lab1:agent ~lab2:site
                           (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg
                                                             "agent name"))
                           (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg
                                                             "site")))))))
          json*)
      with
      | _ -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "contact map",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "contact map",x))

(******************************************************************************)

(**************)
(* dead rules *)
(**************)

type dead_rules = int list

let dead_rules_of_json =
  function
  | `Assoc l as x ->
    begin
      try
        let json = List.assoc dead_rules l in
        JsonUtil.to_list
          ~error_msg:"list of dead rules"
          (JsonUtil.to_int ~error_msg:"dead rule") json
      with
      | _ ->
        raise
          (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "dead  rules",x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "dead rules",x))

(***************)
(* dead agents *)
(***************)

type separating_transitions = (string * int (*rule_id*) * string) list

let separating_transitions_of_json =
  JsonUtil.to_list
    ~error_msg:"separating transitions"
    (
      JsonUtil.to_triple
        ~error_msg:"transition"
        ~lab1:"s1" ~lab2:"label" ~lab3:"s2"
        (JsonUtil.to_string ?error_msg:None)
        (JsonUtil.to_int ?error_msg:None)
        (JsonUtil.to_string ?error_msg:None))

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
  string * (*agent name*)
  (string * string option *  binding_state option) list

type 'site_graph lemma =
  {
    hyp : 'site_graph ;
    refinement : 'site_graph list
  }

type 'site_graph poly_constraints_list =
  (string * 'site_graph lemma list) list

let lemma_to_json site_graph_to_json json =
  JsonUtil.of_pair
    ~lab1:hyp ~lab2:refinement
    site_graph_to_json
    (JsonUtil.of_list site_graph_to_json)
    (json.hyp,json.refinement)

let lemma_of_json site_graph_of_json json =
  let a,b =
    JsonUtil.to_pair
      ~lab1:hyp ~lab2:refinement ~error_msg:"lemma"
      site_graph_of_json
      (JsonUtil.to_list  ~error_msg:"refinements list" site_graph_of_json)
      json
  in
  {
    hyp =  a;
    refinement =  b
  }

let get_hyp h = h.hyp

let get_refinement r = r.refinement


let free = ""
let wildcard = "?"
let bound = "!_"
let bond_id = "bond id"
let bound_to = "bound to"
let binding_type = "binding type"
let prop="property state"
let bind="binding state"

let binding_state_light_of_json =
  function
  | `Assoc [s, `Null] when s = free -> Free
  | `Assoc [s, `Null] when s = wildcard -> Wildcard
  | `Assoc [s, `Null] when s = bound_to -> Bound_to_unknown
  | `Assoc [s, j] when s = bond_id ->
    let i =
      JsonUtil.to_int ~error_msg:"wrong binding id" j
    in
    let bond_index = i in
    Bound_to bond_index
  | `Assoc [s, j] when s = binding_type ->
    let (agent_name, site_name) =
      JsonUtil.to_pair
        ~lab1:"agent" ~lab2:"site" ~error_msg:"binding type"
        (JsonUtil.to_string ~error_msg:"agent name") (JsonUtil.to_string
                                                        ~error_msg:"site name")
        j
    in
    Binding_type (agent_name, site_name)
  | x -> raise
           (Yojson.Basic.Util.Type_error ("wrong binding state",x))


let interface_light_of_json json
  =
  JsonUtil.to_map
    ~lab_key:site ~lab_value:stateslist ~error_msg:interface
    ~empty:[]
    ~add:(fun k (a,b) list -> (k,a,b)::list)
    (*json -> elt*)
    (fun json -> JsonUtil.to_string ~error_msg:site json)
    (*json -> 'value*)
    (JsonUtil.to_pair
       ~lab1:prop ~lab2:bind ~error_msg:"wrong binding state"
       (JsonUtil.to_option
          (JsonUtil.to_string ~error_msg:prop)

       )
       (JsonUtil.to_option
          binding_state_light_of_json)
    )
    json

let agent_gen_of_json interface_of_json =
  JsonUtil.to_pair
    ~lab1:agent ~lab2:interface ~error_msg:"agent"
    (JsonUtil.to_string ~error_msg:"agent name")
    interface_of_json

let agent_of_json json = agent_gen_of_json interface_light_of_json json

let poly_constraints_list_of_json site_graph_of_json =
  JsonUtil.to_list
    (JsonUtil.to_pair ~error_msg:"constraints list"
       ~lab1:domain_name ~lab2:refinements_list
       (JsonUtil.to_string ~error_msg:"abstract domain")
       (JsonUtil.to_list (lemma_of_json site_graph_of_json)))


let lemmas_list_of_json_gen agent_of_json =
  function
  | `Assoc l as x ->
    begin
      try
        let json =
          List.assoc refinement_lemmas l
        in
        poly_constraints_list_of_json
          (JsonUtil.to_list ~error_msg:"site graph" agent_of_json)
          json
      with
      | _ ->
        raise
          (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "refinement lemmas list",x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "refinement lemmas list",x))

let lemmas_list_of_json json =
  lemmas_list_of_json_gen agent_of_json json
