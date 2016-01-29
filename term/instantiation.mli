(** Trace of a rule exactly as the user wrote it (before
compilation) *)

type agent_name = int
type site_name = int
type internal_state  = int

type binding_type = agent_name * site_name

type abstract = Agent_place.t
type concrete = Edges.agent

type 'a site = 'a * site_name

type 'a test =
  | Is_Here of 'a
  | Has_Internal of 'a site * internal_state
  | Is_Free of 'a site
  | Is_Bound of 'a site
  | Has_Binding_type of 'a site * binding_type
  | Is_Bound_to of 'a site * 'a site

type 'a action =
  | Create of 'a * (site_name * internal_state option) list
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

val rename_abstract_test :
  Connected_component.work -> int ->
  Connected_component.cc -> Renaming.t -> abstract test -> abstract test
val rename_abstract_action :
  Connected_component.work -> int ->
  Connected_component.cc -> Renaming.t -> abstract action -> abstract action
val rename_abstract_event :
  Connected_component.work -> int ->
  Connected_component.cc -> Renaming.t -> abstract event -> abstract event
val rename_abstract_side_effect:
  Connected_component.work -> int -> Connected_component.cc -> Renaming.t ->
  (Agent_place.t * 'a) * Agent_place.t binding_state ->
  (Agent_place.t * 'a) * Agent_place.t binding_state
val concretize_test :
  (Connected_component.Matching.t * int Mods.IntMap.t) ->
  abstract test -> concrete test
val concretize_action :
  (Connected_component.Matching.t * int Mods.IntMap.t) ->
  abstract action -> concrete action
val concretize_event :
  (Connected_component.Matching.t * int Mods.IntMap.t) ->
  abstract event -> concrete event

val subst_map_agent_in_concrete_test :
  (int -> int) -> concrete test -> concrete test
val subst_agent_in_concrete_test :
  int -> int -> concrete test -> concrete test
val subst_map_agent_in_concrete_action :
  (int -> int) -> concrete action -> concrete action
val subst_agent_in_concrete_action :
  int -> int -> concrete action -> concrete action
val subst_map_agent_in_concrete_side_effect:
  (int -> int) -> (concrete site * concrete binding_state) ->
  (concrete site * concrete binding_state)
val subst_agent_in_concrete_side_effect:
  int -> int -> (concrete site * concrete binding_state) ->
  (concrete site * concrete binding_state)
val subst_map_agent_in_concrete_event:
  (int -> int) -> concrete event -> concrete event
val subst_map2_agent_in_concrete_event:
  (int -> int) -> (int -> int) -> concrete event -> concrete event
val subst_agent_in_concrete_event:
  int -> int -> concrete event -> concrete event

val print_concrete_test :
  ?sigs:Signature.s -> Format.formatter -> concrete test -> unit
val print_concrete_action :
  ?sigs:Signature.s -> Format.formatter -> concrete action -> unit
