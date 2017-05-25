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

type contact_map =
  ((string list) * (string*string) list)
    Mods.StringSetMap.Map.t Mods.StringSetMap.Map.t

let contact_map_to_json contact_map=
  `Assoc
    [contactmap,
     JsonUtil.of_pair
       ~lab1:accuracy_string ~lab2:map
       accuracy_to_json
       (Mods.StringSetMap.Map.to_json
       ~lab_key:agent ~lab_value:interface
       JsonUtil.of_string
       (Mods.StringSetMap.Map.to_json
          ~lab_key:site ~lab_value:stateslist
          JsonUtil.of_string
          (JsonUtil.of_pair
             ~lab1:props ~lab2:binds
             (JsonUtil.of_list JsonUtil.of_string)
             (JsonUtil.of_list
                (JsonUtil.of_pair
                   ~lab1:agent ~lab2:site
                   JsonUtil.of_string
                   JsonUtil.of_string)
             )))) contact_map]

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
          (Mods.StringSetMap.Map.of_json
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
          json
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

