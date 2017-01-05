(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type agent_name = int
type site_name = int
type internal_state  = int

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

type 'a binding_state =
  | ANY
  | FREE
  | BOUND
  | BOUND_TYPE of binding_type
  | BOUND_to of 'a site

type 'a event =
  'a test list *
  ('a action list * ('a site * 'a binding_state) list * 'a site list)

let concretize_binding_state inj2graph = function
  | ANY -> ANY
  | FREE -> FREE
  | BOUND -> BOUND
  | BOUND_TYPE bt -> BOUND_TYPE bt
  | BOUND_to (pl,s) -> BOUND_to (Matching.Agent.concretize inj2graph pl,s)

let concretize_test inj2graph = function
  | Is_Here pl -> Is_Here (Matching.Agent.concretize inj2graph  pl)
  | Has_Internal ((pl,s),i) ->
    Has_Internal((Matching.Agent.concretize inj2graph pl,s),i)
  | Is_Free (pl,s) -> Is_Free (Matching.Agent.concretize inj2graph pl,s)
  | Is_Bound (pl,s) -> Is_Bound (Matching.Agent.concretize inj2graph pl,s)
  | Has_Binding_type ((pl,s),t) ->
    Has_Binding_type ((Matching.Agent.concretize inj2graph pl,s),t)
  | Is_Bound_to ((pl,s),(pl',s')) ->
    Is_Bound_to ((Matching.Agent.concretize inj2graph pl,s),
                 (Matching.Agent.concretize inj2graph pl',s'))

let concretize_action inj2graph = function
  | Create (pl,i) -> Create (Matching.Agent.concretize inj2graph pl,i)
  | Mod_internal ((pl,s),i) ->
    Mod_internal ((Matching.Agent.concretize inj2graph pl,s),i)
  | Bind ((pl,s),(pl',s')) ->
    Bind ((Matching.Agent.concretize inj2graph pl,s),
          (Matching.Agent.concretize inj2graph pl',s'))
  | Bind_to ((pl,s),(pl',s')) ->
    Bind_to ((Matching.Agent.concretize inj2graph pl,s),
             (Matching.Agent.concretize inj2graph pl',s'))
  | Free (pl,s) -> Free (Matching.Agent.concretize inj2graph pl,s)
  | Remove pl -> Remove (Matching.Agent.concretize inj2graph pl)

let concretize_event inj2graph (tests,(actions,kasa_side,kasim_side)) =
  (List.rev_map (concretize_test inj2graph) tests,
   (List.rev_map (concretize_action inj2graph) actions,
    List.rev_map
      (fun ((pl,s),b) ->
         ((Matching.Agent.concretize inj2graph pl,s),
          concretize_binding_state inj2graph b))
      kasa_side,
    List.rev_map
      (fun (pl,s) -> (Matching.Agent.concretize inj2graph pl,s)) kasim_side))

let subst_map_concrete_agent f (id,na as agent) =
  try if f id == id then agent else (f id,na)
  with Not_found -> agent

let subst_map_site f (ag,s as site) =
  let ag' = f ag in
  if ag==ag' then site else (ag',s)

let subst_map_agent_in_test f = function
  | Is_Here agent as x ->
    let agent' = f agent in
    if agent == agent' then x else Is_Here agent'
  | Has_Internal (site,internal_state) as x ->
    let site' = subst_map_site f site in
    if site == site' then x else Has_Internal (site',internal_state)
  | Is_Free site as x ->
    let site' = subst_map_site f site in
    if site == site' then x else Is_Free site'
  | Is_Bound site as x ->
    let site' = subst_map_site f site in
    if site == site' then x else Is_Bound site'
  | Has_Binding_type (site,binding_type) as x ->
    let site' = subst_map_site f site in
    if site == site' then x else Has_Binding_type (site',binding_type)
  | Is_Bound_to (site1,site2) as x ->
    let site1' = subst_map_site f site1 in
    let site2' = subst_map_site f site2 in
    if site1 == site1' && site2 == site2' then x
    else Is_Bound_to (site1',site2')
let subst_map_agent_in_concrete_test f x =
  subst_map_agent_in_test (subst_map_concrete_agent f) x
let subst_agent_in_concrete_test id id' x =
  subst_map_agent_in_concrete_test
    (fun j -> if j = id then id' else j) x
let rename_abstract_test id inj x =
  subst_map_agent_in_test (Matching.Agent.rename id inj) x

let subst_map2_agent_in_action f f' = function
  | Create (agent,list) as x ->
    let agent' = f' agent in
    if agent == agent' then x else Create(agent',list)
  | Mod_internal (site,i) as x ->
    let site' = subst_map_site f' site in
    if site == site' then x else Mod_internal(site',i)
  | Bind (s1,s2) as x ->
    let s1' = subst_map_site f' s1 in
    let s2' = subst_map_site f' s2 in
    if s1==s1' && s2==s2' then x else Bind(s1',s2')
  | Bind_to (s1,s2) as x ->
    let s1' = subst_map_site f' s1 in
    let s2' = subst_map_site f' s2 in
    if s1==s1' && s2==s2' then x else Bind_to(s1',s2')
  | Free site as x ->
    let site' = subst_map_site f' site in
    if site == site' then x else Free site'
  | Remove agent as x ->
    let agent' = f agent in
    if agent==agent' then x else Remove agent'

let subst_map_agent_in_action f x = subst_map2_agent_in_action f f x

let subst_map_agent_in_concrete_action f x =
  subst_map_agent_in_action (subst_map_concrete_agent f) x
let subst_agent_in_concrete_action id id' x =
  subst_map_agent_in_concrete_action
    (fun j -> if j = id then id' else j) x
let rename_abstract_action id inj x =
  subst_map_agent_in_action (Matching.Agent.rename id inj) x

let subst_map_binding_state f = function
  | (ANY | FREE | BOUND | BOUND_TYPE _ as x) -> x
  | BOUND_to (ag,s) as x ->
    let ag' = f ag in if ag == ag' then x else BOUND_to (ag',s)
let subst_map_agent_in_side_effect f (site,bstate as x) =
  let site' = subst_map_site f site in
  let bstate' = subst_map_binding_state f bstate in
  if site == site' && bstate == bstate' then x else (site',bstate')
let subst_map_agent_in_concrete_side_effect f x =
  subst_map_agent_in_side_effect (subst_map_concrete_agent f) x
let subst_agent_in_concrete_side_effect id id' x =
  subst_map_agent_in_concrete_side_effect
    (fun j -> if j = id then id' else j) x
let rename_abstract_side_effect id inj x =
  subst_map_agent_in_side_effect (Matching.Agent.rename id inj) x

let subst_map_agent_in_event f (tests,(acs,kasa_side,kasim_side)) =
  (Tools.list_smart_map (subst_map_agent_in_test f) tests,
   (Tools.list_smart_map (subst_map_agent_in_action f) acs,
    Tools.list_smart_map (subst_map_agent_in_side_effect f) kasa_side,
    Tools.list_smart_map (subst_map_site f) kasim_side))
let subst_map2_agent_in_event f f' (tests,(acs,kasa_side,kasim_side)) =
  (Tools.list_smart_map (subst_map_agent_in_test f) tests,
   (Tools.list_smart_map (subst_map2_agent_in_action f f') acs,
    Tools.list_smart_map (subst_map_agent_in_side_effect f) kasa_side,
    Tools.list_smart_map (subst_map_site f) kasim_side))

let subst_map_agent_in_concrete_event f x =
  subst_map_agent_in_event (subst_map_concrete_agent f) x
let subst_map2_agent_in_concrete_event f f' x =
  subst_map2_agent_in_event
    (subst_map_concrete_agent f) (subst_map_concrete_agent f') x

let subst_agent_in_concrete_event id id' x =
  subst_map_agent_in_concrete_event
    (fun j -> if j = id then id' else j) x
let rename_abstract_event id inj x =
  subst_map_agent_in_event (Matching.Agent.rename id inj) x

let print_concrete_agent_site ?sigs f (agent,id) =
  Format.fprintf f "%a.%a" (Agent.print ?sigs ~with_id:true) agent
    (Agent.print_site ?sigs agent) id
let print_concrete_test ?sigs f = function
  | Is_Here agent ->
    Format.fprintf f "Is_Here(%a)" (Agent.print ?sigs ~with_id:true) agent
  | Has_Internal ((ag,id as site),int) ->
    Format.fprintf f "Has_Internal(%a~%a)"
      (print_concrete_agent_site ?sigs) site
      (Agent.print_internal ?sigs ag id) int
  | Is_Free site ->
    Format.fprintf f "Is_Free(%a)" (print_concrete_agent_site ?sigs) site
  | Is_Bound site ->
    Format.fprintf f "Is_Bound(%a)" (print_concrete_agent_site ?sigs) site
  | Has_Binding_type (site,(ty,sid)) ->
    Format.fprintf f "Btype(%a,%t)"
      (print_concrete_agent_site ?sigs) site
      (fun f ->
         match sigs with
         | None -> Format.fprintf f "%i.%i" ty sid
         | Some sigs ->
           Format.fprintf
             f "%a.%a" (Signature.print_agent sigs) ty
             (Signature.print_site sigs ty) sid)
  | Is_Bound_to (site1,site2) ->
    Format.fprintf f "Is_Bound(%a,%a)"
      (print_concrete_agent_site ?sigs) site1
      (print_concrete_agent_site ?sigs) site2
let print_concrete_action ?sigs f = function
  | Create ((_,ty as agent),list) ->
    Format.fprintf
      f "Create(%a[@[<h>%a@]])" (Agent.print ?sigs ~with_id:true) agent
      (Pp.list Pp.comma
         (fun f (x,y) ->
            match sigs with
            | Some sigs ->
              Signature.print_site_internal_state sigs ty x f y
            | None ->
              match y with
              | None -> Format.pp_print_int f x
              | Some y ->
                Format.fprintf f "%i.%i" x y))
      list
  | Mod_internal ((ag,id as site),int) ->
    Format.fprintf f "Mod(%a~%a)" (print_concrete_agent_site ?sigs) site
      (Agent.print_internal ?sigs ag id) int
  | Bind (site1,site2) ->
    Format.fprintf f "Bind(%a,%a)" (print_concrete_agent_site ?sigs) site1
      (print_concrete_agent_site ?sigs) site2
  | Bind_to (site1,site2) ->
    Format.fprintf f "Bind_to(%a,%a)" (print_concrete_agent_site ?sigs) site1
      (print_concrete_agent_site ?sigs) site2
  | Free site ->
    Format.fprintf f "Free(%a)" (print_concrete_agent_site ?sigs) site
  | Remove agent ->
    Format.fprintf f "Remove(%a)" (Agent.print ?sigs ~with_id:true) agent
let print_concrete_binding_state ?sigs f = function
  | ANY -> Format.pp_print_string f "*"
  | FREE -> ()
  | BOUND -> Format.pp_print_string f "!_"
  | BOUND_TYPE (s,a) ->
    Format.fprintf
      f "!%a.%a"
      (match sigs with
       | Some sigs -> Signature.print_site sigs s
       | None -> Format.pp_print_int) a
      (match sigs with
       | Some sigs -> Signature.print_agent sigs
       | None -> Format.pp_print_int) s
  | BOUND_to ((ag_id,ag),s) ->
    Format.fprintf
      f "!%a_%i.%a"
      (match sigs with
       | Some sigs -> Signature.print_agent sigs
       | None -> Format.pp_print_int) ag ag_id
      (match sigs with
       | Some sigs -> Signature.print_site sigs ag
       | None -> Format.pp_print_int) s

let binding_type_to_json (ty,s) = `Assoc ["type", `Int ty; "site", `Int s]
let binding_type_of_json = function
  | `Assoc ["type", `Int ty; "site", `Int s]
  | `Assoc ["site", `Int s; "type", `Int ty] -> (ty,s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a binding_type",x))

let quark_to_json f (ag,s) = `Assoc ["agent", f ag; "site", `Int s]
let quark_of_json f = function
  | `Assoc ["agent", ag; "site", `Int s]
  | `Assoc ["site", `Int s; "agent", ag] -> (f ag,s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect quark",x))

let test_to_json f = function
  | Is_Here a -> `List [`String "Is_Here"; f a]
  | Has_Internal (s,i) ->
    `List [`String "Has_Internal"; quark_to_json f s; `Int i]
  | Is_Free s -> `List [`String "Is_Free"; quark_to_json f s]
  | Is_Bound s -> `List [`String "Is_Bound"; quark_to_json f s]
  | Has_Binding_type (s,b) ->
    `List [`String "Has_Binding_type";quark_to_json f s;binding_type_to_json b]
  | Is_Bound_to (s1,s2) ->
    `List [`String "Is_Bound_to"; quark_to_json f s1; quark_to_json f s2]
let test_of_json f = function
  | `List [`String "Is_Here"; a] -> Is_Here (f a)
  | `List [`String "Has_Internal"; s; `Int i] ->
    Has_Internal (quark_of_json f s,i)
  | `List [`String "Is_Free"; s] -> Is_Free (quark_of_json f s)
  | `List [`String "Is_Bound"; s] -> Is_Bound (quark_of_json f s)
  | `List [`String "Has_Binding_type"; s; b] ->
    Has_Binding_type (quark_of_json f s, binding_type_of_json b)
  | `List [`String "Is_Bound_to"; s1; s2] ->
    Is_Bound_to (quark_of_json f s1, quark_of_json f s2)
  | x -> raise (Yojson.Basic.Util.Type_error ("Wrong test",x))

let action_to_json f = function
  | Create (ag,info) ->
    `List [`String "Create"; f ag;
           `List (List.map (fun (s,i) ->
               `List (`Int s ::
                      (match i with None -> [] | Some i -> [`Int i]))) info)]
  | Mod_internal (s,i) ->
    `List [`String "Mod_internal"; quark_to_json f s; `Int i]
  | Bind (s1,s2) ->
    `List [`String "Bind"; quark_to_json f s1; quark_to_json f s2]
  | Bind_to (s1,s2) ->
    `List [`String "Bind_to"; quark_to_json f s1; quark_to_json f s2]
  | Free s -> `List [`String "Free"; quark_to_json f s]
  | Remove a -> `List [`String "Remove"; f a]
let action_of_json f = function
  | `List [`String "Create"; ag; `List info] ->
    Create (f ag,
            List.map (function
                | `List [ `Int s ] -> (s,None)
                | `List [ `Int s; `Int i ] -> (s, Some i)
                | x -> raise (Yojson.Basic.Util.Type_error
                                ("Invalid action info",x))
              ) info)
  | `List [`String "Mod_internal"; s; `Int i] ->
    Mod_internal (quark_of_json f s, i)
  | `List [`String "Bind"; s1; s2] ->
    Bind (quark_of_json f s1, quark_of_json f s2)
  | `List [`String "Bind_to"; s1; s2] ->
    Bind_to (quark_of_json f s1, quark_of_json f s2)
  | `List [`String "Free"; s] -> Free (quark_of_json f s)
  | `List [`String "Remove"; a] -> Remove (f a)
  | x -> raise (Yojson.Basic.Util.Type_error ("Wrong action",x))

let binding_state_to_json f = function
  | ANY -> `String "ANY"
  | FREE -> `String "FREE"
  | BOUND -> `String "BOUND"
  | BOUND_TYPE b -> binding_type_to_json b
  | BOUND_to s -> quark_to_json f s
let binding_state_of_json f = function
  | `String "ANY" -> ANY
  | `String "FREE" -> FREE
  | `String "BOUND" -> BOUND
  | `Assoc ["type", `Int ty; "site", `Int s]
  | `Assoc ["site", `Int s; "type", `Int ty] -> BOUND_TYPE (ty,s)
  | `Assoc ["agent", ag; "site", `Int s]
  | `Assoc ["site", `Int s; "agent", ag] -> BOUND_to (f ag,s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect binding_state",x))

let event_to_json f (tests,(actions,side_origin,side_dst)) =
  `Assoc [
    "tests", `List (List.map (test_to_json f) tests);
    "actions", `List (List.map (action_to_json f) actions);
    "side_effect_origins",
    `List (List.map
             (fun (s,b) -> `List [quark_to_json f s; binding_state_to_json f b])
             side_origin);
    "side_effect_destinations", `List (List.map (quark_to_json f) side_dst)
  ]
let event_of_json f = function
  | `Assoc l as x when List.length l = 4 ->
    begin
      try
        ((match List.assoc "tests" l
          with `List l -> List.map (test_of_json f) l | _ -> raise Not_found),
         ((match List.assoc "actions" l
           with `List l -> List.map (action_of_json f) l | _ -> raise Not_found),
          (match List.assoc "side_effect_origins" l with
           | `List l -> List.map (function
               | `List [s;b] -> (quark_of_json f s, binding_state_of_json f b)
               | _ -> raise Not_found) l
           | _ -> raise Not_found),
          (match List.assoc "side_effect_destinations" l
           with `List l -> List.map (quark_of_json f) l | _ -> raise Not_found)))
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect event",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect event",x))
