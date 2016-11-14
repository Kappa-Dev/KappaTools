(**
  * remanent_state.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June, the 25th of 2016
  * Last modification: Time-stamp: <Nov 14 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type init =
    Compil of ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil
  | Files of string list

type accuracy_level = Low | Medium | High | Full

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

type compilation =
  ((string Location.annot) * Ast.port list, Ast.mixture, string, Ast.rule)
    Ast.compil

type refined_compilation =
  (Ckappa_sig.agent, Ckappa_sig.mixture, string,
   Ckappa_sig.direction * Ckappa_sig.mixture Ckappa_sig.rule) Ast.compil

type quark_map = Quark_type.quarks

type rule_id = int
type var_id =  int

type influence_node =
  | Rule of rule_id
  | Var of var_id

(******************************************************************)

let influence_node_to_json a =
  match a with
  | Var i ->
    `Assoc ["variable",JsonUtil.of_int i]
  | Rule i  ->
    `Assoc ["rule",JsonUtil.of_int i]

let influence_node_of_json
    ?error_msg:(error_msg="Not a correct influence node")
  =
  function
  | `Assoc ["variable",json] ->
    Var (JsonUtil.to_int json)
  | `Assoc ["rule",json] ->
    Rule (JsonUtil.to_int json)
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

(******************************************************************)

module InfluenceNodeSetMap =
  SetMap.Make
    (struct
      type t = influence_node
      let compare = compare
      let print f = function
        | Rule r -> Format.fprintf f "Rule %i" r
        | Var r -> Format.fprintf f "Var %i" r
    end)

module InfluenceNodeMap = InfluenceNodeSetMap.Map

(******************************************************************)

type internal_influence_map =
  Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t
  * Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t

type location =
  | Direct of int
  | Side_effect of int

(******************************************************************)

let location_to_json a =
  match a with
  | Direct i -> `Assoc ["direct",JsonUtil.of_int i]
  | Side_effect i  -> `Assoc ["side_effects",JsonUtil.of_int i]

let location_of_json
    ?error_msg:(error_msg="Not a correct location")
  =
  function
  | `Assoc ["direct",json] -> Direct (JsonUtil.to_int json)
  | `Assoc ["side_effect",json] -> Side_effect (JsonUtil.to_int json)
  | x ->
    raise (Yojson.Basic.Util.Type_error (error_msg,x))

(******************************************************************)

type 'a pair = 'a * 'a

type influence_map =
  {
    positive: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
    negative: location pair list InfluenceNodeMap.t InfluenceNodeMap.t ;
  }

let wakeup = "wake-up map"
let inhibition = "inhibition map"
let source = "source"
let target_map = "target map"
let target = "target"
let location_pair_list = "location pair list"
let rhs = "RHS"
let lhs = "LHS"

let half_influence_map_to_json =
  InfluenceNodeMap.to_json
    ~lab_key:source ~lab_value:target_map
    influence_node_to_json
    (InfluenceNodeMap.to_json
       ~lab_key:target ~lab_value:location_pair_list
       influence_node_to_json
       (JsonUtil.of_list
          (JsonUtil.of_pair
             ~lab1:rhs ~lab2:lhs
             location_to_json
             location_to_json
          )
       )
    )

let half_influence_map_of_json =
  InfluenceNodeMap.of_json
    ~error_msg:(JsonUtil.build_msg "activation or inhibition map")
    ~lab_key:source ~lab_value:target_map
    (influence_node_of_json ~error_msg:(JsonUtil.build_msg "influence node"))
    (InfluenceNodeMap.of_json
       ~lab_key:target ~lab_value:location_pair_list
       ~error_msg:"map of lists of pairs of locations"
       (influence_node_of_json ~error_msg:(JsonUtil.build_msg "influence node"))
       (JsonUtil.to_list ~error_msg:"list of pair of locations"
          (JsonUtil.to_pair
             ~error_msg:""
             ~lab1:rhs ~lab2:lhs
             (location_of_json ~error_msg:(JsonUtil.build_msg "location"))
             (location_of_json ~error_msg:(JsonUtil.build_msg "location")))))

let influence_map_to_json influence_map =
  `Assoc
    [
      wakeup,half_influence_map_to_json influence_map.positive;
      inhibition,half_influence_map_to_json
        influence_map.negative;]

let influence_map_of_json ?error_msg:(error_msg=JsonUtil.build_msg "influence map") =
  function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        {positive =
           half_influence_map_of_json (List.assoc wakeup l);
         negative =
           half_influence_map_of_json (List.assoc inhibition l)}
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let agent="agent name"
let interface="interface"
let site="site name"
let stateslist="states list"
let prop="property states"
let bind="binding states"

(******************************************************************)

let contact_map_to_json =
  Mods.StringMap.to_json
    ~lab_key:agent ~lab_value:interface
    JsonUtil.of_string
    (Mods.StringMap.to_json
       ~lab_key:site ~lab_value:stateslist
       JsonUtil.of_string
       (JsonUtil.of_pair
          ~lab1:prop ~lab2:bind
          (JsonUtil.of_list JsonUtil.of_string)
          (JsonUtil.of_list
             (JsonUtil.of_pair
                ~lab1:agent ~lab2:site
                JsonUtil.of_string
                JsonUtil.of_string)
          )))

let contact_map_of_json =
  Mods.StringMap.of_json
    ~lab_key:agent ~lab_value:interface
    (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "agent name"))
    (Mods.StringMap.of_json
       ~error_msg:(JsonUtil.build_msg "interface")
       ~lab_key:site ~lab_value:stateslist
       (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "site name"))
       (JsonUtil.to_pair
          ~error_msg:(JsonUtil.build_msg "pair of lists of sites")
          ~lab1:prop ~lab2:bind
          (JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "list of internal states")
             (JsonUtil.to_string
                ~error_msg:(JsonUtil.build_msg "internal state")))
          (JsonUtil.to_list
             ~error_msg:(JsonUtil.build_msg "list of binding states")
             (JsonUtil.to_pair
                ~error_msg:(JsonUtil.build_msg "binding type")
                ~lab1:agent ~lab2:site
                (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "agent name"))
                (JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "site"))))))

type contact_map =
  ((string list) * (string*string) list) Mods.StringMap.t Mods.StringMap.t

type internal_contact_map =
  (Ckappa_sig.c_state list *
   (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name) list)
    Ckappa_sig.Site_map_and_set.Map.t Ckappa_sig.Agent_map_and_set.Map.t

(******************************************************************)

type ('static, 'dynamic) reachability_result = 'static * 'dynamic

type subviews_info = unit

type dead_rules = Ckappa_sig.c_rule_id list

type dead_agents = Ckappa_sig.c_agent_name list

type flow =
  Ckappa_sig.Site_union_find.t
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.t

(*******************************************************************)
(*TODO*)

type 'site_graph lemma =
  {
    hyp : 'site_graph ;
    refinement : 'site_graph list
  }

type 'site_graph poly_constraints_list =
  (string (*domain name*) * 'site_graph lemma list) list

type internal_constraints_list =
  Ckappa_backend.Ckappa_backend.t poly_constraints_list

type site_map =
  (string * (*site_string*)
   (string option *  Ckappa_backend.Ckappa_backend.binding_state option)
     Wrapped_modules.LoggedStringMap.t) list

type constraints_list =
  site_map poly_constraints_list

(*
string *
  (string *
    (string option * Ckappa_backend.Ckappa_backend.binding_state option)
      Wrapped_modules.LoggedStringMap.t)
        list Remanent_state.lemma list
*)
(*******************************************************************)
(*internal_/constraints_list -> json*)

let constraints_list_to_json_aux site_map =
  Wrapped_modules.LoggedStringMap.to_json
    (fun site_string -> JsonUtil.of_string site_string)
    (fun (internal_opt, binding_opt) ->
       JsonUtil.of_pair ~lab1:prop ~lab2:bind
         (fun internal_opt ->
            JsonUtil.of_option (fun internal_state ->
                JsonUtil.of_string internal_state
              ) internal_opt
         )
         (fun binding_opt ->
            match binding_opt with
            | None
            | Some Ckappa_backend.Ckappa_backend.Free ->
              JsonUtil.of_string ""
            | Some Ckappa_backend.Ckappa_backend.Wildcard ->
              JsonUtil.of_string "?"
            | Some Ckappa_backend.Ckappa_backend.Bound_to_unknown ->
              JsonUtil.of_string "!_"
            | Some (Ckappa_backend.Ckappa_backend.Bound_to b_int) ->
              JsonUtil.of_int
                (Ckappa_backend.Ckappa_backend.int_of_bond_index b_int)
            | Some (Ckappa_backend.Ckappa_backend.Binding_type
                      (agent_name, site_name)) ->
              JsonUtil.of_pair
                (fun agent_name ->
                   JsonUtil.of_string agent_name
                )
                (fun site_name ->
                   JsonUtil.of_string site_name
                )
                (agent_name, site_name)
         )
         (internal_opt, binding_opt)
    )
    site_map

let constraints_list_hyp_to_json hyp =
  let json =
    JsonUtil.of_assoc (fun (agent_string, site_map) ->
        agent_string, constraints_list_to_json_aux site_map
      ) hyp
in
json

let constraints_list_refinment_to_json refinement =
let json =
  JsonUtil.of_list (fun t ->
      constraints_list_hyp_to_json t
    ) refinement
in
json

let hyp = "hyp"
let refinement = "refinement"

let constraints_list_lemma_to_json lemma =
  `Assoc [
    hyp, constraints_list_hyp_to_json lemma.hyp;
    refinement, constraints_list_refinment_to_json lemma.refinement
  ]

let constraints_list_to_json constraints_list =
  JsonUtil.of_assoc (fun (agent_string, lemma_list) ->
      let json =
        JsonUtil.of_list (fun lemma ->
            constraints_list_lemma_to_json lemma
          ) lemma_list
      in
      agent_string, json
    ) constraints_list

(*******************************************************************)

let internal_constraints_list_hyp_to_json hyp =
  let json =
    Ckappa_sig.Agent_id_map_and_set.Map.to_json
      (fun agent_id -> JsonUtil.of_int (Ckappa_sig.int_of_agent_id agent_id)
      )
      (fun (agent_string, site_map) ->
         JsonUtil.of_pair
           (fun agent_string ->
              JsonUtil.of_string agent_string
           )
           (fun site_map ->
              constraints_list_to_json_aux site_map
           )
           (agent_string, site_map)
      )
      hyp
  in
  json

(*******************************************************************)
(*json -> contrainst_list/internal_constraints_list*)

(*let binding_opt_of_json ?error_msg:(error_msg="Not a correct binding state") =
  function
  | x ->  raise (Yojson.Basic.Util.Type_error (error_msg, x)) (*FIXME*)
  | `Assoc ["", json] -> Ckappa_backend.Ckappa_backend.Free
  | `Assoc ["?", json] -> Ckappa_backend.Ckappa_backend.Wildcard
  | `Assoc ["!_", json] -> Ckappa_backend.Ckappa_backend.Bound_to_unknown
  | `Assoc ["bound_to", json] ->
    let int = JsonUtil.to_int
        ~error_msg:(JsonUtil.build_msg "bound to") json
    in
    let bond_index = Ckappa_backend.Ckappa_backend.bond_index_of_int int in
    Ckappa_backend.Ckappa_backend.Bound_to bond_index
  | `Assoc ["binding_type", json] ->
    let agent_name =
      (fun json ->
         JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "agent name")
           json)
    in
    let site_name =
      (fun json ->
         JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "site name") json)
    in
    let (agent_name, site_name) =
      (JsonUtil.to_pair
         ~lab1:agent ~lab2:site ~error_msg:""
         (fun json -> agent_name json)
         (fun json -> site_name json)
         json)
    in
    Ckappa_backend.Ckappa_backend.Binding_type (agent_name, site_name)*)

let binding_opt_of_json json  =
  match json with
  (* x ->  raise (Yojson.Basic.Util.Type_error (error_msg, x)) (*FIXME*)*)
  | `Bool _ | `Float _
  | `Int _ | `List _ | `Null | `String _
  | `Assoc [] -> assert false (*FIXME*)
  | `Assoc ["", json] -> Ckappa_backend.Ckappa_backend.Free
  | `Assoc ["?", json] -> Ckappa_backend.Ckappa_backend.Wildcard
  | `Assoc ["!_", json] -> Ckappa_backend.Ckappa_backend.Bound_to_unknown
  | `Assoc ["bound_to", json] ->
    let int = JsonUtil.to_int
        ~error_msg:(JsonUtil.build_msg "bound to") json
    in
    let bond_index = Ckappa_backend.Ckappa_backend.bond_index_of_int int in
    Ckappa_backend.Ckappa_backend.Bound_to bond_index
  | `Assoc ["binding_type", json] ->
    let agent_name =
      (fun json ->
         JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "agent name")
           json)
    in
    let site_name =
      (fun json ->
         JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "site name") json)
    in
    let (agent_name, site_name) =
      (JsonUtil.to_pair
         ~lab1:agent ~lab2:site ~error_msg:""
         (fun json -> agent_name json)
         (fun json -> site_name json)
         json)
    in
    Ckappa_backend.Ckappa_backend.Binding_type (agent_name, site_name)
  | `Assoc (_::_) ->
    assert false (* FIXME *)

let pair_state = "pair state"
let site_map = "site map"
let internal = "internal state"
let binding = "binding state"

let constraints_list_of_json_aux json =
  Wrapped_modules.LoggedStringMap.of_json ~lab_key:site
    ~lab_value:pair_state ~error_msg:"site_map"
    (fun json -> (*elt:site_string*)
       JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "site name") json
    )
    (fun json -> (*internal_opt, binding_opt*)
       JsonUtil.to_pair ~lab1:internal ~lab2:binding ~error_msg:""
         (fun json ->
            JsonUtil.to_option
              (fun json ->
                 JsonUtil.to_string ~error_msg:(JsonUtil.build_msg "internal")
                   json)
              json)
         (fun json ->
            JsonUtil.to_option
              (fun json ->
                 binding_opt_of_json json)
              json)
         json)
    json

let constraints_list_hyp_of_json json =
  JsonUtil.to_list (fun json ->
      JsonUtil.to_pair
        (fun json ->
           JsonUtil.to_string
             ~error_msg:(JsonUtil.build_msg "agent name") json
        )
        (fun json ->
           constraints_list_of_json_aux json
        )
        json
    ) json

(*list *)
let constraints_list_refinment_of_json json =
  JsonUtil.to_list ~error_msg:"refinement"
    (fun json -> constraints_list_hyp_of_json json)
    json

(*json -> lemma *)
let constraints_list_lemma_of_json json =
  {
    hyp = constraints_list_hyp_of_json json;
    refinement = constraints_list_refinment_of_json json;
  }

let constraints_list_of_json json =
  JsonUtil.to_list (fun json ->
      JsonUtil.to_pair
        (fun json ->
           JsonUtil.to_string
             ~error_msg:(JsonUtil.build_msg "agent name") json)
        (fun json ->(*site graph lemma list*)
           JsonUtil.to_list (fun json ->
               constraints_list_lemma_of_json json
             ) json
        ) json) json

(*******************************************************************)

let internal_constraints_list_hyp_of_json json =
  Ckappa_sig.Agent_id_map_and_set.Map.of_json
    (fun json -> (*elt*)
       let int =
         JsonUtil.to_int ~error_msg:"agent_id" json
       in
       Ckappa_sig.agent_id_of_int int
    )
    (fun json -> (*val:agent_string, site_map*)
       JsonUtil.to_pair
         (fun json ->
           JsonUtil.to_string
             ~error_msg:(JsonUtil.build_msg "agent name") json
         )
         (fun json -> (*string_option * binding_option*)
            constraints_list_of_json_aux json
         ) json
    )
    json

(***************************************************************************)

let print_internal_constraints_list ?logger parameters error kappa_handler
    internal_constraints_list =
  let logger =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let (domain_name, lemma_list) = internal_constraints_list in
  let () =
  Loggers.fprintf logger
    "------------------------------------------------------------\n";
  Loggers.fprintf logger "* Export %s to JSon (internal constraints_list):\n"
    domain_name;
  Loggers.fprintf logger
    "------------------------------------------------------------\n";
  in
  List.fold_left (fun (error, bool) lemma ->
      let error =
        Ckappa_backend.Ckappa_backend.print logger parameters error
          kappa_handler
          lemma.hyp
      in
      let () = Loggers.fprintf logger "=> [" in
      let error, b =
        match lemma.refinement with
        | [] -> error, false
        | [hyp] ->
          Ckappa_backend.Ckappa_backend.print logger parameters error
            kappa_handler
            hyp, false
        | _::_ as l ->
          List.fold_left (fun (error, bool) hyp ->
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  (if bool  then "\t\tv " else "\t\t  ")
              in
              let error =
                Ckappa_backend.Ckappa_backend.print logger parameters error
                  kappa_handler
                  hyp
              in
              error, true
            ) (error, false) (List.rev l)
      in
      let () = Loggers.fprintf logger "]" in
      let () = Loggers.print_newline logger in
      error, b
    ) (error, false) lemma_list

(*print the information as the output of non relational properties*)
let print_internal_constraints_list_list ?logger parameters error kappa_handler
    list =
    let logger' =
      match
        logger
      with
      | None -> Remanent_parameters.get_logger parameters
      | Some a -> a
    in
    let error =
      List.fold_left (fun error pattern ->
          let error, _ =
            print_internal_constraints_list
              ?logger parameters error
              kappa_handler
              pattern
          in
          let () = Loggers.print_newline logger' in
          error
        ) error list
    in
    error

(***************************************************************************)

let print_for_list logger parameter error kappa_handler t =
  let _bool =
    List.fold_left (fun bool (agent_string, site_map) ->
        let _ =
          Ckappa_backend.Ckappa_backend.print_aux logger parameter error
            kappa_handler
            agent_string site_map bool
        in
        let () = Loggers.fprintf logger ")" in
        true
      ) false t
  in
  let () = Loggers.fprintf logger " " in
  error

let print_constraints_list ?logger parameters error kappa_handler constraints_list
  =
  let logger =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let (domain_name, lemma_list) = constraints_list in
  let () =
  Loggers.fprintf logger
    "------------------------------------------------------------\n";
  Loggers.fprintf logger "* Export %s to JSon (constraints_list):\n" domain_name;
  Loggers.fprintf logger
    "------------------------------------------------------------\n";
  in
  List.fold_left (fun (error, bool) lemma ->
      let error =
        print_for_list logger parameters error
          kappa_handler
          lemma.hyp
      in
      let () = Loggers.fprintf logger " => [" in
      (*refinement*)
      let error, b =(*TODO*)
        (*match lemma.refinement with
        | [] -> error, false
        | [hyp] ->
          print_for_list logger parameters error
          kappa_handler
            hyp, false
        | _:: _ as l ->*)
          List.fold_left (fun (error, bool) hyp ->
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  (if bool  then "\t\tv " else "\t\t  ")
              in
              let error =
                print_for_list logger parameters error kappa_handler hyp
              in
              error, true
            ) (error, false) (List.rev lemma.refinement)
      in
      let () = Loggers.fprintf logger "]" in
      let () = Loggers.print_newline logger in
      error, b
    ) (error, false) lemma_list

let print_constraints_list_list ?logger parameters error kappa_handler list =
  let logger' =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let error =
    List.fold_left (fun error pattern ->
        let error, _ =
          print_constraints_list ?logger parameters error kappa_handler pattern
        in
        let () = Loggers.print_newline logger' in
        error
      ) error list
  in
  error

(*******************************************************************)

let convert_site_graph error string_version =
  let error, current_list =
    Ckappa_sig.Agent_id_map_and_set.Map.fold
      (fun _ (agent_string, site_map) (error, current_list) ->
         (*-----------------------------------------*)
         let site_graph =
           (agent_string, site_map) :: current_list
         in
         error, site_graph
      ) string_version (error, [])
  in
  error, List.rev current_list

let convert_refinement error list =
  List.fold_left (fun (error, current_list) t ->
      let string_version = Ckappa_backend.Ckappa_backend.get_string_version t in
      let error, site_graph = convert_site_graph error string_version in
      error, site_graph :: current_list
    ) (error, []) list

let convert_refinement_pair_list parameters error kappa_handler pattern
    agent_id1 site_type1' agent_id2 site_type2'
    pair_list =
  List.fold_left (fun (error, current_list) l ->
      match l with
      | [siteone, state1; sitetwo, state2] when
          siteone == Ckappa_sig.fst_site
          && sitetwo == Ckappa_sig.snd_site ->
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id1
            site_type1'
            state1
            pattern
        in
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id2
            site_type2'
            state2
            pattern
        in
        let string_version =
          Ckappa_backend.Ckappa_backend.get_string_version
            pattern
        in
        let error, site_graph = convert_site_graph error string_version in
        error, site_graph :: current_list
      | _ -> Exception.warn parameters error __POS__ Exit []
    ) (error, []) pair_list

let convert_refinement_internal_pair_list parameters error kappa_handler pattern
    agent_id1 site_type1' agent_id2 site_type2' pair_list =
  List.fold_left (fun (error, current_list) l ->
      match l with
      | [siteone, state1; sitetwo, state2] when
          siteone == Ckappa_sig.fst_site
          && sitetwo == Ckappa_sig.snd_site ->
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id1
            site_type1'
            state1
            pattern
        in
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id2
            site_type2'
            state2
            pattern
        in
        error, pattern :: current_list
      | _ -> Exception.warn parameters error __POS__ Exit []
    ) (error, []) pair_list

(*******************************************************************)

let convert_refinement_internal error list =
  List.fold_left (fun (error, current_list) hyp ->
      error, hyp :: current_list
    ) (error, []) list

(*******************************************************************)
(*views domain*)

let convert_refinement_views_constraints_list parameters error
    handler_kappa agent_id site_type t state_list =
  List.fold_left (fun (error, current_list) state ->
      let error, t =
        Ckappa_backend.Ckappa_backend.add_state parameters
          error handler_kappa agent_id site_type state t
      in
      let string_version =
      Ckappa_backend.Ckappa_backend.get_string_version
        t
      in
      let error, site_graph = convert_site_graph error string_version in
      error, site_graph :: current_list
    ) (error, []) state_list

(*******************************************************************)

type ('static,'dynamic) state =
  {
    parameters    : Remanent_parameters_sig.parameters ;
    log_info : StoryProfiling.StoryStats.log_info ;
    prehandler: Cckappa_sig.kappa_handler option ;
    handler       : Cckappa_sig.kappa_handler option ;
    init : init ;
    compilation   : compilation option ;
    refined_compilation : refined_compilation option ;
    c_compil : Cckappa_sig.compil option ;
    quark_map: quark_map option ;
    internal_influence_map:
      (Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t *
       Quark_type.Labels.label_set_couple Ckappa_sig.PairRule_setmap.Map.t)
        AccuracyMap.t ;
    influence_map : influence_map AccuracyMap.t ;
    internal_contact_map: internal_contact_map AccuracyMap.t;
    contact_map   : contact_map AccuracyMap.t ;
    signature     : Signature.s option;
    bdu_handler: Mvbdu_wrapper.Mvbdu.handler ;
    reachability_state: ('static, 'dynamic) reachability_result option ;
    subviews_info: subviews_info option ;
    dead_rules:  dead_rules option ;
    dead_agents: dead_agents option ;
    ode_flow: Ode_fragmentation_type.ode_frag option ;
    ctmc_flow: flow option ;
    errors        : Exception.method_handler ;
    (*TODO*)
    internal_constraints_list : internal_constraints_list option;
    constraints_list : constraints_list option;
  }

let create_state ?errors parameters init =
  let error =
    match
      errors
    with
    | None -> Exception.empty_error_handler
    | Some error -> error
  in
  let error, handler_bdu = Mvbdu_wrapper.Mvbdu.init parameters error in
  {
    parameters = parameters;
    log_info = StoryProfiling.StoryStats.init_log_info ();
    prehandler = None ;
    handler = None ;
    init = init ;
    compilation = None ;
    refined_compilation = None ;
    c_compil = None ;
    quark_map = None ;
    internal_influence_map = AccuracyMap.empty ;
    influence_map = AccuracyMap.empty ;
    internal_contact_map = AccuracyMap.empty ;
    contact_map = AccuracyMap.empty ;
    signature = None ;
    bdu_handler = handler_bdu ;
    ode_flow = None ;
    ctmc_flow = None ;
    reachability_state = None ;
    subviews_info = None ;
    dead_rules = None ;
    dead_agents = None ;
    errors = error ;
    internal_constraints_list = None;
    constraints_list = None
  }

let do_event_gen f phase n state =
  let error, log_info =
    f
      state.parameters
      state.errors
      phase
      n
      state.log_info
  in
  {state with errors = error ; log_info = log_info}

let add_event x y = do_event_gen StoryProfiling.StoryStats.add_event x y

let close_event x y = do_event_gen StoryProfiling.StoryStats.close_event x y

let set_parameters parameters state = {state with parameters = parameters}

let get_parameters state = state.parameters

let get_init state = state.init

let set_compilation compilation state =
  {state with compilation = Some compilation}

let get_compilation state = state.compilation

let set_prehandler handler state = {state with prehandler = Some handler}

let get_prehandler state = state.prehandler

let set_handler handler state = {state with handler = Some handler}

let get_handler state = state.handler

let set_compil compil state = {state with compilation = compil}

let get_compil state = state.compilation

let set_c_compil c_compil state = {state with c_compil = Some c_compil}

let get_c_compil state = state.c_compil

let set_refined_compil refined_compil state =
  {state with refined_compilation = Some refined_compil}

let get_refined_compil state = state.refined_compilation

let set_errors errors state = {state with errors = errors }

let get_errors state = state.errors

let set_quark_map quark_map state =
  {state with quark_map = Some quark_map}

let get_quark_map state = state.quark_map

let set_contact_map accuracy map state =
  {state with contact_map = AccuracyMap.add accuracy map state.contact_map}

let get_contact_map accuracy state =
  AccuracyMap.find_option accuracy state.contact_map

let set_signature signature state = {state with signature = Some signature}

let get_signature state = state.signature

let set_influence_map accuracy map state =
  {state with influence_map = AccuracyMap.add accuracy map state.influence_map}

let get_influence_map accuracy state =
  AccuracyMap.find_option accuracy state.influence_map

let set_internal_influence_map accuracy map state =
  {state
   with internal_influence_map =
          AccuracyMap.add accuracy map state.internal_influence_map}

let get_internal_influence_map accuracy state =
  AccuracyMap.find_option accuracy state.internal_influence_map

let set_internal_contact_map accuracy int_contact_map state =
  {state
   with internal_contact_map = AccuracyMap.add accuracy int_contact_map state.internal_contact_map}

let get_internal_contact_map accuracy state =
  AccuracyMap.find_option accuracy state.internal_contact_map

let get_reachability_result state = state.reachability_state

let set_reachability_result reachability_state state =
  {state with reachability_state = Some reachability_state}

let get_dead_rules state = state.dead_rules

let set_dead_rules dead_rules state =
  {state with dead_rules = Some dead_rules}

let get_dead_agents state = state.dead_agents

let set_dead_agents dead_agents state =
  {state with dead_agents = Some dead_agents}

let get_subviews_info state = state.subviews_info

let set_subviews_info subviews state =
  {state with subviews_info = Some subviews}

let set_bdu_handler bdu_handler state =
  {state with bdu_handler = bdu_handler}

let get_bdu_handler state = state.bdu_handler

let set_ode_flow flow state = {state with ode_flow = Some flow}

let get_ode_flow state = state.ode_flow

let set_ctmc_flow flow state = {state with ctmc_flow = Some flow}

let get_ctmc_flow state = state.ctmc_flow

let get_influence_map_map state = state.influence_map

let get_contact_map_map state = state.contact_map

let get_internal_contact_map_map state = state.internal_contact_map

let get_internal_influence_map_map state = state.internal_influence_map

let get_log_info state = state.log_info

let set_log_info log state = {state with log_info = log}

let get_internal_constraints_list state =
  state.internal_constraints_list

let set_internal_constraints_list list state =
  {state with internal_constraints_list = Some list}

let get_constraints_list state = state.constraints_list

let set_constraints_list list state =
  {state with constraints_list = Some list}
