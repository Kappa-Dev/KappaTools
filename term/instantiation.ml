type agent_name = int
type site_name = int
type internal_state  = int

type binding_type = agent_name * site_name

type abstract = Agent_place.t
type concrete = int (*agent_id*) * agent_name

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
  | BOUND_to (pl,s) -> BOUND_to (Agent_place.concretize inj2graph pl,s)

let concretize_test inj2graph = function
  | Is_Here pl -> Is_Here (Agent_place.concretize inj2graph  pl)
  | Has_Internal ((pl,s),i) ->
     Has_Internal((Agent_place.concretize inj2graph pl,s),i)
  | Is_Free (pl,s) -> Is_Free (Agent_place.concretize inj2graph pl,s)
  | Is_Bound (pl,s) -> Is_Bound (Agent_place.concretize inj2graph pl,s)
  | Has_Binding_type ((pl,s),t) ->
     Has_Binding_type ((Agent_place.concretize inj2graph pl,s),t)
  | Is_Bound_to ((pl,s),(pl',s')) ->
     Is_Bound_to ((Agent_place.concretize inj2graph pl,s),
		  (Agent_place.concretize inj2graph pl',s'))

let concretize_action inj2graph = function
  | Create (pl,i) -> Create (Agent_place.concretize inj2graph pl,i)
  | Mod_internal ((pl,s),i) ->
     Mod_internal ((Agent_place.concretize inj2graph pl,s),i)
  | Bind ((pl,s),(pl',s')) ->
     Bind ((Agent_place.concretize inj2graph pl,s),
	   (Agent_place.concretize inj2graph pl',s'))
  | Bind_to ((pl,s),(pl',s')) ->
     Bind_to ((Agent_place.concretize inj2graph pl,s),
	      (Agent_place.concretize inj2graph pl',s'))
  | Free (pl,s) -> Free (Agent_place.concretize inj2graph pl,s)
  | Remove pl -> Remove (Agent_place.concretize inj2graph pl)

let concretize_event inj2graph (tests,(actions,kasa_side,kasim_side)) =
  (List.map (concretize_test inj2graph) tests,
   (List.map (concretize_action inj2graph) actions,
    List.map
      (fun ((pl,s),b) ->
       ((Agent_place.concretize inj2graph pl,s),
	concretize_binding_state inj2graph b))
      kasa_side,
    List.map
      (fun (pl,s) -> (Agent_place.concretize inj2graph pl,s)) kasim_side))

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
let rename_abstract_test wk id cc inj x =
  subst_map_agent_in_test (Agent_place.rename wk id cc inj) x


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
let rename_abstract_action wk id cc inj x =
  subst_map_agent_in_action (Agent_place.rename wk id cc inj) x

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
let rename_abstract_side_effect wk id cc inj x =
  subst_map_agent_in_side_effect (Agent_place.rename wk id cc inj) x

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
let rename_abstract_event wk id cc inj x =
  subst_map_agent_in_event (Agent_place.rename wk id cc inj) x

let with_sigs f = function
  | None -> Format.pp_print_int
  | Some sigs -> f sigs
let print_concrete_agent ?sigs f (id,ty) =
  Format.fprintf
    f "%a_%i" (with_sigs Signature.print_agent sigs) ty id
let print_concrete_agent_site ?sigs f ((_,ty as agent),id) =
  Format.fprintf f "%a.%a" (print_concrete_agent ?sigs) agent
		 (with_sigs (fun s -> Signature.print_site s ty) sigs) id
let print_concrete_test ?sigs f = function
  | Is_Here agent ->
     Format.fprintf f "Is_Here(%a)" (print_concrete_agent ?sigs) agent
  | Has_Internal (((_,ty),id as site),int) ->
     Format.fprintf f "Has_Internal(%a~%a)"
		    (print_concrete_agent_site ?sigs) site
		    (with_sigs
		       (fun s -> Signature.print_internal_state s ty id)
		       sigs) int
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
       f "Create(%a[@[<h>%a@]])" (print_concrete_agent ?sigs) agent
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
  | Mod_internal (((_,ty),id as site),int) ->
     Format.fprintf f "Mod(%a~%a)" (print_concrete_agent_site ?sigs) site
		    (with_sigs
		       (fun s -> Signature.print_internal_state s ty id)
		       sigs) int
  | Bind (site1,site2) ->
     Format.fprintf f "Bind(%a,%a)" (print_concrete_agent_site ?sigs) site1
		    (print_concrete_agent_site ?sigs) site2
  | Bind_to (site1,site2) ->
     Format.fprintf f "Bind_to(%a,%a)" (print_concrete_agent_site ?sigs) site1
		    (print_concrete_agent_site ?sigs) site2
  | Free site ->
     Format.fprintf f "Free(%a)" (print_concrete_agent_site ?sigs) site
  | Remove agent ->
     Format.fprintf f "Remove(%a)" (print_concrete_agent ?sigs) agent
