(** What users wrote when writing its rules *)

(** All names refers to the one from the signature *)

type agent_name = int
type site_name = int
type internal_state  = int

type binding_type = agent_name * site_name

type abstract = Matching.Agent.t (** in a rule *)

type concrete = Agent.t (** in a simulation state *)

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
(** (The tests asked by the user, (The modifications asked by the
    user, the site of the agents mentioned by the users where trere is
    a side effects, the site of agents not mentionned by the users
    that have been freed by side effect)) *)

val rename_abstract_test :
  int -> Renaming.t -> abstract test -> abstract test
val rename_abstract_action :
  int -> Renaming.t -> abstract action -> abstract action
val rename_abstract_event :
  int -> Renaming.t -> abstract event -> abstract event
val rename_abstract_side_effect:
  int -> Renaming.t ->
  (Matching.Agent.t * 'a) * Matching.Agent.t binding_state ->
  (Matching.Agent.t * 'a) * Matching.Agent.t binding_state
val concretize_test :
  (Matching.t * int Mods.IntMap.t) -> abstract test -> concrete test
val concretize_action :
  (Matching.t * int Mods.IntMap.t) -> abstract action -> concrete action
val concretize_event :
  (Matching.t * int Mods.IntMap.t) -> abstract event -> concrete event

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
val print_concrete_binding_state :
  ?sigs:Signature.s -> Format.formatter -> concrete binding_state -> unit

val test_to_json : ('a -> Yojson.Basic.json) -> 'a test -> Yojson.Basic.json
val test_of_json : (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a test
val action_to_json : ('a -> Yojson.Basic.json) -> 'a action -> Yojson.Basic.json
val action_of_json : (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a action
val event_to_json : ('a -> Yojson.Basic.json) -> 'a event -> Yojson.Basic.json
val event_of_json : (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a event
