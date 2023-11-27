(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type agent_name = int
type site_name = int
type internal_state = int
type binding_type = agent_name * site_name
type abstract = Matching.Agent.t
type concrete = Agent.t
type 'a site = 'a * site_name

type 'a test =
  | Is_Here of 'a
  | Has_Internal of 'a site * internal_state
  | Is_Free of 'a site
  | Is_Bound of 'a site
  | Has_Binding_type of 'a site * binding_type
  | Is_Bound_to of 'a site * 'a site

type 'a action =
  | Create of 'a * (site_name * internal_state option) list (* pourquoi ça *)
  | Mod_internal of 'a site * internal_state
  | Bind of 'a site * 'a site
  | Bind_to of 'a site * 'a site
  | Free of 'a site
  | Remove of 'a

(* The semantics of concrete actions seems to be the following one.

   - When an agent is removed, no other action is stored about it (including bond releasing).
   - Created agents are created without default binding/internal states.
   - Bonds are inserted thanks to two symmetric actions (Bind_to ...) *)

let weight action =
  match action with
  | Create _ -> 1
  | Mod_internal _ | Bind _ | Bind_to _ | Free _ -> 2
  | Remove _ -> 0

let weight_reverse action =
  match action with
  | Create _ -> 1
  | Mod_internal _ | Bind _ | Bind_to _ | Free _ -> 0
  | Remove _ -> 2

let sort_concrete_action_list = Tools.sort_by_priority weight 2
let sort_concrete_action_list_reverse = Tools.sort_by_priority weight_reverse 2
let sort_abstract_action_list = Tools.sort_by_priority weight 2
let sort_abstract_action_list_reverse = Tools.sort_by_priority weight_reverse 2

type 'a binding_state =
  | ANY
  | FREE
  | BOUND
  | BOUND_TYPE of binding_type
  | BOUND_to of 'a site

type 'a event = {
  tests: 'a test list list;
  actions: 'a action list;
  side_effects_src: ('a site * 'a binding_state) list;
  side_effects_dst: 'a site list;
  connectivity_tests: 'a test list;
}

let empty_event =
  {
    tests = [];
    actions = [];
    side_effects_src = [];
    side_effects_dst = [];
    connectivity_tests = [];
  }

let concretize_binding_state ~debugMode inj2graph = function
  | ANY -> ANY
  | FREE -> FREE
  | BOUND -> BOUND
  | BOUND_TYPE bt -> BOUND_TYPE bt
  | BOUND_to (pl, s) ->
    BOUND_to (Matching.Agent.concretize ~debugMode inj2graph pl, s)

let concretize_test ~debugMode inj2graph = function
  | Is_Here pl -> Is_Here (Matching.Agent.concretize ~debugMode inj2graph pl)
  | Has_Internal ((pl, s), i) ->
    Has_Internal ((Matching.Agent.concretize ~debugMode inj2graph pl, s), i)
  | Is_Free (pl, s) ->
    Is_Free (Matching.Agent.concretize ~debugMode inj2graph pl, s)
  | Is_Bound (pl, s) ->
    Is_Bound (Matching.Agent.concretize ~debugMode inj2graph pl, s)
  | Has_Binding_type ((pl, s), t) ->
    Has_Binding_type ((Matching.Agent.concretize ~debugMode inj2graph pl, s), t)
  | Is_Bound_to ((pl, s), (pl', s')) ->
    Is_Bound_to
      ( (Matching.Agent.concretize ~debugMode inj2graph pl, s),
        (Matching.Agent.concretize ~debugMode inj2graph pl', s') )

let concretize_action ~debugMode inj2graph = function
  | Create (pl, i) ->
    Create (Matching.Agent.concretize ~debugMode inj2graph pl, i)
  | Mod_internal ((pl, s), i) ->
    Mod_internal ((Matching.Agent.concretize ~debugMode inj2graph pl, s), i)
  | Bind ((pl, s), (pl', s')) ->
    Bind
      ( (Matching.Agent.concretize ~debugMode inj2graph pl, s),
        (Matching.Agent.concretize ~debugMode inj2graph pl', s') )
  | Bind_to ((pl, s), (pl', s')) ->
    Bind_to
      ( (Matching.Agent.concretize ~debugMode inj2graph pl, s),
        (Matching.Agent.concretize ~debugMode inj2graph pl', s') )
  | Free (pl, s) -> Free (Matching.Agent.concretize ~debugMode inj2graph pl, s)
  | Remove pl -> Remove (Matching.Agent.concretize ~debugMode inj2graph pl)

let try_concretize_action ~debugMode inj2graph actions =
  try Some (concretize_action ~debugMode inj2graph actions)
  with Not_found -> None
(* The action is dealing with a fresh agent *)

let concretize_event ~debugMode inj2graph e =
  {
    tests =
      List.map (List.rev_map (concretize_test ~debugMode inj2graph)) e.tests;
    actions =
      (* actions are reordered the following way:
         1) Remove actions
         2) Creation actions
         3) Anything else.*)
      sort_abstract_action_list
        (List.rev_map (concretize_action ~debugMode inj2graph) e.actions);
    side_effects_src =
      List.rev_map
        (fun ((pl, s), b) ->
          ( (Matching.Agent.concretize ~debugMode inj2graph pl, s),
            concretize_binding_state ~debugMode inj2graph b ))
        e.side_effects_src;
    side_effects_dst =
      List.rev_map
        (fun (pl, s) -> Matching.Agent.concretize ~debugMode inj2graph pl, s)
        e.side_effects_dst;
    connectivity_tests =
      List.rev_map (concretize_test ~debugMode inj2graph) e.connectivity_tests;
  }

let map_test f = function
  | Is_Here a
  | Has_Internal ((a, _), _)
  | Is_Free (a, _)
  | Is_Bound (a, _)
  | Has_Binding_type ((a, _), _)
  | Is_Bound_to ((a, _), _) ->
    f a

let map_action f = function
  | Create (a, _)
  | Mod_internal ((a, _), _)
  | Bind ((a, _), _)
  | Bind_to ((a, _), _)
  | Free (a, _)
  | Remove a ->
    f a

let match_tests = function
  (* abstract, concrete*)
  | Is_Here a, Is_Here b -> Matching.Agent.get_type a = Agent.sort b
  | Has_Internal ((a, s), i), Has_Internal ((b, t), j) ->
    Matching.Agent.get_type a = Agent.sort b && s = t && i = j
  | Has_Binding_type ((a, s), c), Has_Binding_type ((b, t), d) ->
    Matching.Agent.get_type a = Agent.sort b && s = t && c = d
  | Is_Free (a, s), Is_Free (b, t) | Is_Bound (a, s), Is_Bound (b, t) ->
    Matching.Agent.get_type a = Agent.sort b && s = t
  | Is_Bound_to ((a, s), (c, u)), Is_Bound_to ((b, t), (d, v)) ->
    Matching.Agent.get_type a = Agent.sort b
    && s = t
    && Matching.Agent.get_type c = Agent.sort d
    && u = v
  | Is_Here _, _
  | Has_Internal _, _
  | Is_Free _, _
  | Is_Bound _, _
  | Is_Bound_to _, _
  | Has_Binding_type _, _ ->
    false

let match_actions = function
  (* abstract, concrete*)
  | Create (a, als), Create (b, bls) ->
    Matching.Agent.get_type a = Agent.sort b
    && List.fold_left2
         (fun ok (s, i) (t, j) -> ok && s = t && i = j)
         true als bls
  | Mod_internal ((a, s), i), Mod_internal ((b, t), j) ->
    Matching.Agent.get_type a = Agent.sort b && s = t && i = j
  | Bind ((a, s), (c, u)), Bind ((b, t), (d, v))
  | Bind_to ((a, s), (c, u)), Bind_to ((b, t), (d, v)) ->
    Matching.Agent.get_type a = Agent.sort b
    && s = t
    && Matching.Agent.get_type c = Agent.sort d
    && u = v
  | Free (a, s), Free (b, t) ->
    Matching.Agent.get_type a = Agent.sort b && s = t
  | Remove a, Remove b -> Matching.Agent.get_type a = Agent.sort b
  | Create _, _
  | Mod_internal _, _
  | Bind _, _
  | Bind_to _, _
  | Free _, _
  | Remove _, _ ->
    false

let get_ids f aux =
  List.fold_left
    (fun acc a ->
      let id = f a in
      if List.mem id acc then
        acc
      else
        id :: acc)
    aux

let rec match_quarks a_quarks c_quarks fmatch =
  match a_quarks with
  | aq :: aqs ->
    let cqs, rest = List.partition (fun cq -> fmatch (aq, cq)) c_quarks in
    if cqs = [] then
      false
    else
      match_quarks aqs rest fmatch
  | [] -> c_quarks = []

let rec find_match tests actions ctests cactions = function
  | [] ->
    raise
      (ExceptionDefn.Internal_Error
         (Locality.dummy_annot "abstract and concret quarks don't match"))
  | cid :: tl ->
    let ctests' =
      List.filter (fun test -> map_test (fun a -> Agent.id a = cid) test) ctests
    in
    let cactions' =
      List.filter
        (fun act -> map_action (fun a -> Agent.id a = cid) act)
        cactions
    in
    if
      match_quarks tests ctests' match_tests
      && match_quarks actions cactions' match_actions
    then
      cid
    else
      find_match tests actions ctests cactions tl

let matching_abstract_concrete ~debugMode ae ce =
  let ae_tests = List.flatten ae.tests in
  let ce_tests = List.flatten ce.tests in
  let abstract_ids =
    get_ids
      (map_action Matching.Agent.get_id)
      (get_ids (map_test Matching.Agent.get_id) [] ae_tests)
      ae.actions
  in
  let concrete_ids =
    get_ids (map_action Agent.id)
      (get_ids (map_test Agent.id) [] ce_tests)
      ce.actions
  in
  let available_ids used =
    List.filter
      (fun i -> not (Mods.IntSet.mem i (Renaming.image used)))
      concrete_ids
  in
  let partition fmap i =
    List.partition (fun q -> fmap (fun a -> Matching.Agent.get_id a = i) q)
  in
  let matching = Renaming.empty () in
  let injective =
    List.fold_left
      (fun acc i ->
        acc
        &&
        let tests, _ = partition map_test i ae_tests in
        let actions, _ = partition map_action i ae.actions in
        let j =
          find_match tests actions ce_tests ce.actions (available_ids matching)
        in
        Renaming.imperative_add ~debugMode i j matching)
      true abstract_ids
  in
  if injective then
    Some matching
  else
    None

let subst_map_concrete_agent f ((id, na) as agent) =
  try
    if f id == id then
      agent
    else
      f id, na
  with Not_found -> agent

let subst_map_site f ((ag, s) as site) =
  let ag' = f ag in
  if ag == ag' then
    site
  else
    ag', s

let subst_map_agent_in_test f = function
  | Is_Here agent as x ->
    let agent' = f agent in
    if agent == agent' then
      x
    else
      Is_Here agent'
  | Has_Internal (site, internal_state) as x ->
    let site' = subst_map_site f site in
    if site == site' then
      x
    else
      Has_Internal (site', internal_state)
  | Is_Free site as x ->
    let site' = subst_map_site f site in
    if site == site' then
      x
    else
      Is_Free site'
  | Is_Bound site as x ->
    let site' = subst_map_site f site in
    if site == site' then
      x
    else
      Is_Bound site'
  | Has_Binding_type (site, binding_type) as x ->
    let site' = subst_map_site f site in
    if site == site' then
      x
    else
      Has_Binding_type (site', binding_type)
  | Is_Bound_to (site1, site2) as x ->
    let site1' = subst_map_site f site1 in
    let site2' = subst_map_site f site2 in
    if site1 == site1' && site2 == site2' then
      x
    else
      Is_Bound_to (site1', site2')

let subst_map_agent_in_concrete_test f x =
  subst_map_agent_in_test (subst_map_concrete_agent f) x

let subst_agent_in_concrete_test id id' x =
  subst_map_agent_in_concrete_test
    (fun j ->
      if j = id then
        id'
      else
        j)
    x

let rename_abstract_test ~debugMode id inj x =
  subst_map_agent_in_test (Matching.Agent.rename ~debugMode id inj) x

let subst_map2_agent_in_action f f' = function
  | Create (agent, list) as x ->
    let agent' = f' agent in
    if agent == agent' then
      x
    else
      Create (agent', list)
  | Mod_internal (site, i) as x ->
    let site' = subst_map_site f' site in
    if site == site' then
      x
    else
      Mod_internal (site', i)
  | Bind (s1, s2) as x ->
    let s1' = subst_map_site f' s1 in
    let s2' = subst_map_site f' s2 in
    if s1 == s1' && s2 == s2' then
      x
    else
      Bind (s1', s2')
  | Bind_to (s1, s2) as x ->
    let s1' = subst_map_site f' s1 in
    let s2' = subst_map_site f' s2 in
    if s1 == s1' && s2 == s2' then
      x
    else
      Bind_to (s1', s2')
  | Free site as x ->
    let site' = subst_map_site f' site in
    if site == site' then
      x
    else
      Free site'
  | Remove agent as x ->
    let agent' = f agent in
    if agent == agent' then
      x
    else
      Remove agent'

let subst_map_agent_in_action f x = subst_map2_agent_in_action f f x

let subst_map_agent_in_concrete_action f x =
  subst_map_agent_in_action (subst_map_concrete_agent f) x

let subst_agent_in_concrete_action id id' x =
  subst_map_agent_in_concrete_action
    (fun j ->
      if j = id then
        id'
      else
        j)
    x

let rename_abstract_action ~debugMode id inj x =
  subst_map_agent_in_action (Matching.Agent.rename ~debugMode id inj) x

let subst_map_binding_state f = function
  | (ANY | FREE | BOUND | BOUND_TYPE _) as x -> x
  | BOUND_to (ag, s) as x ->
    let ag' = f ag in
    if ag == ag' then
      x
    else
      BOUND_to (ag', s)

let subst_map_agent_in_side_effect f ((site, bstate) as x) =
  let site' = subst_map_site f site in
  let bstate' = subst_map_binding_state f bstate in
  if site == site' && bstate == bstate' then
    x
  else
    site', bstate'

let subst_map_agent_in_concrete_side_effect f x =
  subst_map_agent_in_side_effect (subst_map_concrete_agent f) x

let subst_agent_in_concrete_side_effect id id' x =
  subst_map_agent_in_concrete_side_effect
    (fun j ->
      if j = id then
        id'
      else
        j)
    x

let rename_abstract_side_effect ~debugMode id inj x =
  subst_map_agent_in_side_effect (Matching.Agent.rename ~debugMode id inj) x

let subst_map_agent_in_event f e =
  {
    tests =
      List_util.smart_map
        (List_util.smart_map (subst_map_agent_in_test f))
        e.tests;
    actions = List_util.smart_map (subst_map_agent_in_action f) e.actions;
    side_effects_src =
      List_util.smart_map (subst_map_agent_in_side_effect f) e.side_effects_src;
    side_effects_dst = List_util.smart_map (subst_map_site f) e.side_effects_dst;
    connectivity_tests =
      List_util.smart_map (subst_map_agent_in_test f) e.connectivity_tests;
  }

let subst_map2_agent_in_event f f' e =
  {
    tests =
      List_util.smart_map
        (List_util.smart_map (subst_map_agent_in_test f))
        e.tests;
    actions = List_util.smart_map (subst_map2_agent_in_action f f') e.actions;
    side_effects_src =
      List_util.smart_map (subst_map_agent_in_side_effect f) e.side_effects_src;
    side_effects_dst = List_util.smart_map (subst_map_site f) e.side_effects_dst;
    connectivity_tests =
      List_util.smart_map (subst_map_agent_in_test f) e.connectivity_tests;
  }

let subst_map_agent_in_concrete_event f x =
  subst_map_agent_in_event (subst_map_concrete_agent f) x

let subst_map2_agent_in_concrete_event f f' x =
  subst_map2_agent_in_event
    (subst_map_concrete_agent f)
    (subst_map_concrete_agent f')
    x

let subst_agent_in_concrete_event id id' x =
  subst_map_agent_in_concrete_event
    (fun j ->
      if j = id then
        id'
      else
        j)
    x

let rename_abstract_event ~debugMode id inj x =
  subst_map_agent_in_event (Matching.Agent.rename ~debugMode id inj) x

let print_concrete_agent_site ?sigs f (agent, id) =
  Format.fprintf f "%a.%a"
    (Agent.print ?sigs ~with_id:true)
    agent
    (Agent.print_site ?sigs agent)
    id

let print_concrete_test ?sigs f = function
  | Is_Here agent ->
    Format.fprintf f "Is_Here(%a)" (Agent.print ?sigs ~with_id:true) agent
  | Has_Internal ((ag, id), int) ->
    Format.fprintf f "Has_Internal(%a.%a)"
      (Agent.print ?sigs ~with_id:true)
      ag
      (Agent.print_internal ?sigs ag id)
      int
  | Is_Free site ->
    Format.fprintf f "Is_Free(%a)" (print_concrete_agent_site ?sigs) site
  | Is_Bound site ->
    Format.fprintf f "Is_Bound(%a)" (print_concrete_agent_site ?sigs) site
  | Has_Binding_type (site, (ty, sid)) ->
    Format.fprintf f "Btype(%a,%t)" (print_concrete_agent_site ?sigs) site
      (fun f ->
        match sigs with
        | None -> Format.fprintf f "%i.%i" ty sid
        | Some sigs ->
          Format.fprintf f "%a.%a"
            (Signature.print_agent sigs)
            ty
            (Signature.print_site sigs ty)
            sid)
  | Is_Bound_to (site1, site2) ->
    Format.fprintf f "Is_Bound(%a,%a)"
      (print_concrete_agent_site ?sigs)
      site1
      (print_concrete_agent_site ?sigs)
      site2

let print_concrete_action ?sigs f = function
  | Create (((_, ty) as agent), list) ->
    Format.fprintf f "Create(%a[@[<h>%a@]])"
      (Agent.print ?sigs ~with_id:true)
      agent
      (Pp.list Pp.comma (fun f (x, y) ->
           match sigs with
           | Some sigs -> Signature.print_site_internal_state sigs ty x f y
           | None ->
             (match y with
             | None -> Format.pp_print_int f x
             | Some y -> Format.fprintf f "%i.%i" x y)))
      list
  | Mod_internal ((ag, id), int) ->
    Format.fprintf f "Mod(%a.%a)"
      (Agent.print ?sigs ~with_id:true)
      ag
      (Agent.print_internal ?sigs ag id)
      int
  | Bind (site1, site2) ->
    Format.fprintf f "Bind(%a,%a)"
      (print_concrete_agent_site ?sigs)
      site1
      (print_concrete_agent_site ?sigs)
      site2
  | Bind_to (site1, site2) ->
    Format.fprintf f "Bind_to(%a,%a)"
      (print_concrete_agent_site ?sigs)
      site1
      (print_concrete_agent_site ?sigs)
      site2
  | Free site ->
    Format.fprintf f "Free(%a)" (print_concrete_agent_site ?sigs) site
  | Remove agent ->
    Format.fprintf f "Remove(%a)" (Agent.print ?sigs ~with_id:true) agent

let print_concrete_binding_state ?sigs f = function
  | ANY -> Format.pp_print_string f "*"
  | FREE -> ()
  | BOUND -> Format.pp_print_string f "!_"
  | BOUND_TYPE (s, a) ->
    Format.fprintf f "!%a.%a"
      (match sigs with
      | Some sigs -> Signature.print_site sigs s
      | None -> Format.pp_print_int)
      a
      (match sigs with
      | Some sigs -> Signature.print_agent sigs
      | None -> Format.pp_print_int)
      s
  | BOUND_to ((ag_id, ag), s) ->
    Format.fprintf f "!%a_%i.%a"
      (match sigs with
      | Some sigs -> Signature.print_agent sigs
      | None -> Format.pp_print_int)
      ag ag_id
      (match sigs with
      | Some sigs -> Signature.print_site sigs ag
      | None -> Format.pp_print_int)
      s

let json_dictionnary =
  "\"binding_type\":{\"type\":0,\"site\":1},\"quark\":{\"agent\":0,\"site\":1},\"test\":[\"Is_here\",\"Has_Internal\",\"Is_Free\",\"Is_Bound\",\"Has_Binding_type\",\"Is_Bound_to\"],\"actions\":[\"Create\",\"Mod_internal\",\"Bind\",\"Bind_to\",\"Free\",\"Remove\"],\"binding_state\":[\"ANY\",\"FREE\",\"BOUND\",\"BOUND_TYPE\",\"BOUND_to\"],\"event\":{\"tests\":0,\"actions\":1,\"side_effect_src\":2,\"side_effect_dst\":3,\"connectivity_tests\":4}"

let write_binding_type ob a =
  JsonUtil.write_compact_pair Yojson.Basic.write_int Yojson.Basic.write_int ob a

let read_binding_type p lb =
  JsonUtil.read_compact_pair Yojson.Basic.read_int Yojson.Basic.read_int p lb

let binding_type_to_json (ty, s) = `List [ `Int ty; `Int s ]

let binding_type_of_json = function
  | `List [ `Int ty; `Int s ] -> ty, s
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a binding_type", x))

let write_quark f ob a =
  JsonUtil.write_compact_pair f Yojson.Basic.write_int ob a

let read_quark f p lb = JsonUtil.read_compact_pair f Yojson.Basic.read_int p lb
let quark_to_json f (ag, s) = `List [ f ag; `Int s ]

let quark_of_json f = function
  | `List [ ag; `Int s ] -> f ag, s
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect quark", x))

let write_test f ob t =
  JsonUtil.write_sequence ob
    (match t with
    | Is_Here a -> [ (fun o -> Yojson.Basic.write_int o 0); (fun o -> f o a) ]
    | Has_Internal (s, i) ->
      [
        (fun o -> Yojson.Basic.write_int o 1);
        (fun o -> write_quark f o s);
        (fun o -> Yojson.Basic.write_int o i);
      ]
    | Is_Free s ->
      [ (fun o -> Yojson.Basic.write_int o 2); (fun o -> write_quark f o s) ]
    | Is_Bound s ->
      [ (fun o -> Yojson.Basic.write_int o 3); (fun o -> write_quark f o s) ]
    | Has_Binding_type (s, b) ->
      [
        (fun ob -> Yojson.Basic.write_int ob 4);
        (fun ob -> write_quark f ob s);
        (fun ob -> write_binding_type ob b);
      ]
    | Is_Bound_to (s1, s2) ->
      [
        (fun ob -> Yojson.Basic.write_int ob 5);
        (fun ob -> write_quark f ob s1);
        (fun ob -> write_quark f ob s2);
      ])

let read_test f st b =
  JsonUtil.read_variant Yojson.Basic.read_int
    (fun st b -> function
      | 0 ->
        let y = JsonUtil.read_next_item f st b in
        Is_Here y
      | 1 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        let i = JsonUtil.read_next_item Yojson.Basic.read_int st b in
        Has_Internal (s, i)
      | 2 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        Is_Free s
      | 3 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        Is_Bound s
      | 4 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        let bi = JsonUtil.read_next_item read_binding_type st b in
        Has_Binding_type (s, bi)
      | 5 ->
        let s1 = JsonUtil.read_next_item (read_quark f) st b in
        let s2 = JsonUtil.read_next_item (read_quark f) st b in
        Is_Bound_to (s1, s2)
      | _ -> Yojson.json_error "Wrong test" (*st b*))
    st b

let test_to_json f = function
  | Is_Here a -> `List [ `Int 0; f a ]
  | Has_Internal (s, i) -> `List [ `Int 1; quark_to_json f s; `Int i ]
  | Is_Free s -> `List [ `Int 2; quark_to_json f s ]
  | Is_Bound s -> `List [ `Int 3; quark_to_json f s ]
  | Has_Binding_type (s, b) ->
    `List [ `Int 4; quark_to_json f s; binding_type_to_json b ]
  | Is_Bound_to (s1, s2) ->
    `List [ `Int 5; quark_to_json f s1; quark_to_json f s2 ]

let test_of_json f = function
  | `List [ `Int 0; a ] -> Is_Here (f a)
  | `List [ `Int 1; s; `Int i ] -> Has_Internal (quark_of_json f s, i)
  | `List [ `Int 2; s ] -> Is_Free (quark_of_json f s)
  | `List [ `Int 3; s ] -> Is_Bound (quark_of_json f s)
  | `List [ `Int 4; s; b ] ->
    Has_Binding_type (quark_of_json f s, binding_type_of_json b)
  | `List [ `Int 5; s1; s2 ] ->
    Is_Bound_to (quark_of_json f s1, quark_of_json f s2)
  | x -> raise (Yojson.Basic.Util.Type_error ("Wrong test", x))

let write_action f ob a =
  JsonUtil.write_sequence ob
    (match a with
    | Create (ag, info) ->
      [
        (fun o -> Yojson.Basic.write_int o 0);
        (fun o -> f o ag);
        (fun o ->
          JsonUtil.write_list
            (JsonUtil.write_compact_pair Yojson.Basic.write_int
               (JsonUtil.write_option Yojson.Basic.write_int))
            o info);
      ]
    | Mod_internal (s, i) ->
      [
        (fun o -> Yojson.Basic.write_int o 1);
        (fun o -> write_quark f o s);
        (fun o -> Yojson.Basic.write_int o i);
      ]
    | Bind (s1, s2) ->
      [
        (fun o -> Yojson.Basic.write_int o 2);
        (fun o -> write_quark f o s1);
        (fun o -> write_quark f o s2);
      ]
    | Bind_to (s1, s2) ->
      [
        (fun o -> Yojson.Basic.write_int o 3);
        (fun o -> write_quark f o s1);
        (fun o -> write_quark f o s2);
      ]
    | Free s ->
      [ (fun o -> Yojson.Basic.write_int o 4); (fun o -> write_quark f o s) ]
    | Remove a -> [ (fun o -> Yojson.Basic.write_int o 5); (fun o -> f o a) ])

let read_action f st b =
  JsonUtil.read_variant Yojson.Basic.read_int
    (fun st b -> function
      | 0 ->
        let ag = JsonUtil.read_next_item f st b in
        let info =
          JsonUtil.read_next_item
            (Yojson.Basic.read_list
               (JsonUtil.read_compact_pair Yojson.Basic.read_int
                  (JsonUtil.read_option Yojson.Basic.read_int)))
            st b
        in
        Create (ag, info)
      | 1 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        let i = JsonUtil.read_next_item Yojson.Basic.read_int st b in
        Mod_internal (s, i)
      | 2 ->
        let s1 = JsonUtil.read_next_item (read_quark f) st b in
        let s2 = JsonUtil.read_next_item (read_quark f) st b in
        Bind (s1, s2)
      | 3 ->
        let s1 = JsonUtil.read_next_item (read_quark f) st b in
        let s2 = JsonUtil.read_next_item (read_quark f) st b in
        Bind_to (s1, s2)
      | 4 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        Free s
      | 5 ->
        let a = JsonUtil.read_next_item f st b in
        Remove a
      | _ -> Yojson.json_error "Wrong action" (*st b*))
    st b

let action_to_json f = function
  | Create (ag, info) ->
    `List
      [
        `Int 0;
        f ag;
        `List
          (List.map
             (fun (s, i) ->
               `List
                 (`Int s
                 ::
                 (match i with
                 | None -> []
                 | Some i -> [ `Int i ])))
             info);
      ]
  | Mod_internal (s, i) -> `List [ `Int 1; quark_to_json f s; `Int i ]
  | Bind (s1, s2) -> `List [ `Int 2; quark_to_json f s1; quark_to_json f s2 ]
  | Bind_to (s1, s2) -> `List [ `Int 3; quark_to_json f s1; quark_to_json f s2 ]
  | Free s -> `List [ `Int 4; quark_to_json f s ]
  | Remove a -> `List [ `Int 5; f a ]

let action_of_json f = function
  | `List [ `Int 0; ag; `List info ] ->
    Create
      ( f ag,
        List.map
          (function
            | `List [ `Int s ] -> s, None
            | `List [ `Int s; `Int i ] -> s, Some i
            | x ->
              raise (Yojson.Basic.Util.Type_error ("Invalid action info", x)))
          info )
  | `List [ `Int 1; s; `Int i ] -> Mod_internal (quark_of_json f s, i)
  | `List [ `Int 2; s1; s2 ] -> Bind (quark_of_json f s1, quark_of_json f s2)
  | `List [ `Int 3; s1; s2 ] -> Bind_to (quark_of_json f s1, quark_of_json f s2)
  | `List [ `Int 4; s ] -> Free (quark_of_json f s)
  | `List [ `Int 5; a ] -> Remove (f a)
  | x -> raise (Yojson.Basic.Util.Type_error ("Wrong action", x))

let write_binding_state f ob bf =
  JsonUtil.write_sequence ob
    (match bf with
    | ANY -> [ (fun o -> Yojson.Basic.write_int o 0) ]
    | FREE -> [ (fun o -> Yojson.Basic.write_int o 1) ]
    | BOUND -> [ (fun o -> Yojson.Basic.write_int o 2) ]
    | BOUND_TYPE b ->
      [
        (fun o -> Yojson.Basic.write_int o 3); (fun o -> write_binding_type o b);
      ]
    | BOUND_to s ->
      [ (fun o -> Yojson.Basic.write_int o 4); (fun o -> write_quark f o s) ])

let read_binding_state f st b =
  JsonUtil.read_variant Yojson.Basic.read_int
    (fun st b -> function
      | 0 -> ANY
      | 1 -> FREE
      | 2 -> BOUND
      | 3 ->
        let b = JsonUtil.read_next_item read_binding_type st b in
        BOUND_TYPE b
      | 4 ->
        let s = JsonUtil.read_next_item (read_quark f) st b in
        BOUND_to s
      | _ -> Yojson.json_error "Wrong binding state" (*st b*))
    st b

let binding_state_to_json f = function
  | ANY -> `List [ `Int 0 ]
  | FREE -> `List [ `Int 1 ]
  | BOUND -> `List [ `Int 2 ]
  | BOUND_TYPE b -> `List [ `Int 3; binding_type_to_json b ]
  | BOUND_to s -> `List [ `Int 4; quark_to_json f s ]

let binding_state_of_json f = function
  | `List [ `Int 0 ] -> ANY
  | `List [ `Int 1 ] -> FREE
  | `List [ `Int 2 ] -> BOUND
  | `List [ `Int 3; `List [ `Int ty; `Int s ] ] -> BOUND_TYPE (ty, s)
  | `List [ `Int 4; `List [ ag; `Int s ] ] -> BOUND_to (f ag, s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect binding_state", x))

let write_event f ob e =
  JsonUtil.write_sequence ob
    [
      (fun o ->
        JsonUtil.write_list (JsonUtil.write_list (write_test f)) o e.tests);
      (fun o -> JsonUtil.write_list (write_action f) o e.actions);
      (fun o ->
        JsonUtil.write_list
          (JsonUtil.write_compact_pair (write_quark f) (write_binding_state f))
          o e.side_effects_src);
      (fun o -> JsonUtil.write_list (write_quark f) o e.side_effects_dst);
      (fun o -> JsonUtil.write_list (write_test f) o e.connectivity_tests);
    ]

let read_event f st b =
  JsonUtil.read_variant
    (Yojson.Basic.read_list (Yojson.Basic.read_list (read_test f)))
    (fun st b tests ->
      let actions =
        JsonUtil.read_next_item (Yojson.Basic.read_list (read_action f)) st b
      in
      let side_effects_src =
        JsonUtil.read_next_item
          (Yojson.Basic.read_list
             (JsonUtil.read_compact_pair (read_quark f) (read_binding_state f)))
          st b
      in
      let side_effects_dst =
        JsonUtil.read_next_item (Yojson.Basic.read_list (read_quark f)) st b
      in
      let connectivity_tests =
        JsonUtil.read_next_item (Yojson.Basic.read_list (read_test f)) st b
      in
      { tests; actions; side_effects_src; side_effects_dst; connectivity_tests })
    st b

let event_to_json f e =
  `List
    [
      `List
        (List.map (fun cct -> `List (List.map (test_to_json f) cct)) e.tests);
      `List (List.map (action_to_json f) e.actions);
      `List
        (List.map
           (fun (s, b) ->
             `List [ quark_to_json f s; binding_state_to_json f b ])
           e.side_effects_src);
      `List (List.map (quark_to_json f) e.side_effects_dst);
      `List (List.map (test_to_json f) e.connectivity_tests);
    ]

let event_of_json f = function
  | `List [ `List t; `List a; `List s_e_src; `List s_e_dst; `List c_t ] as x ->
    (try
       {
         tests =
           List.map
             (function
               | `List ccl -> List.map (test_of_json f) ccl
               | _ -> raise Not_found)
             t;
         actions = List.map (action_of_json f) a;
         side_effects_src =
           List.map
             (function
               | `List [ s; b ] -> quark_of_json f s, binding_state_of_json f b
               | _ -> raise Not_found)
             s_e_src;
         side_effects_dst = List.map (quark_of_json f) s_e_dst;
         connectivity_tests = List.map (test_of_json f) c_t;
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Incorrect event", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect event", x))
