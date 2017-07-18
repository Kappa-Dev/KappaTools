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
let props="property states"
let binds="binding states"
let sitename = site
let sitelinks = binds
let sitestates = props
let sitenodename = agent
let sitenodesites = "interface"
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
