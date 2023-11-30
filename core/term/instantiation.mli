(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** What users wrote when writing its rules *)

(** All names refers to the one from the signature *)

type agent_name = int
type site_name = int
type internal_state = int
type binding_type = agent_name * site_name

type abstract = Matching.Agent.t
(** in a rule *)

type concrete = Agent.t
(** in a simulation state *)

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

val sort_concrete_action_list : concrete action list -> concrete action list

val sort_concrete_action_list_reverse :
  concrete action list -> concrete action list

val sort_abstract_action_list : concrete action list -> concrete action list

val sort_abstract_action_list_reverse :
  concrete action list -> concrete action list

type 'a binding_state =
  | ANY
  | FREE
  | BOUND
  | BOUND_TYPE of binding_type
  | BOUND_to of 'a site

type 'a event = {
  tests: 'a test list list;
      (** The tests written in the rule (pattern by pattern) *)
  actions: 'a action list;  (** The modifications written in the rule *)
  side_effects_src: ('a site * 'a binding_state) list;
      (** the site of the agents mentioned in the rule where there is a side
    effects *)
  side_effects_dst: 'a site list;
      (** the site of agents not mentionned in the rule that have been freed
    by side effect *)
  connectivity_tests: 'a test list;
      (** witness that patterns where connected (unary instances only of course) *)
}

val empty_event : 'a event

val rename_abstract_test :
  debugMode:bool -> int -> Renaming.t -> abstract test -> abstract test

val rename_abstract_action :
  debugMode:bool -> int -> Renaming.t -> abstract action -> abstract action

val rename_abstract_event :
  debugMode:bool -> int -> Renaming.t -> abstract event -> abstract event

val rename_abstract_side_effect :
  debugMode:bool ->
  int ->
  Renaming.t ->
  (Matching.Agent.t * 'a) * Matching.Agent.t binding_state ->
  (Matching.Agent.t * 'a) * Matching.Agent.t binding_state

val concretize_test :
  debugMode:bool ->
  Matching.t * int Mods.IntMap.t ->
  abstract test ->
  concrete test

val concretize_action :
  debugMode:bool ->
  Matching.t * int Mods.IntMap.t ->
  abstract action ->
  concrete action

val try_concretize_action :
  debugMode:bool ->
  Matching.t * int Mods.IntMap.t ->
  abstract action ->
  concrete action option
(** Same than [concretize_action], except that it returns [None]
    if the provided injection's domain does not contain a fresh agent
    that is involved in the action that is being concretized. *)

val concretize_event :
  debugMode:bool ->
  Matching.t * int Mods.IntMap.t ->
  abstract event ->
  concrete event

val matching_abstract_concrete :
  debugMode:bool -> abstract event -> concrete event -> Renaming.t option

val subst_map_agent_in_concrete_test :
  (int -> int) -> concrete test -> concrete test

val subst_agent_in_concrete_test : int -> int -> concrete test -> concrete test

val subst_map_agent_in_concrete_action :
  (int -> int) -> concrete action -> concrete action

val subst_agent_in_concrete_action :
  int -> int -> concrete action -> concrete action

val subst_map_agent_in_concrete_side_effect :
  (int -> int) ->
  concrete site * concrete binding_state ->
  concrete site * concrete binding_state

val subst_agent_in_concrete_side_effect :
  int ->
  int ->
  concrete site * concrete binding_state ->
  concrete site * concrete binding_state

val subst_map_agent_in_concrete_event :
  (int -> int) -> concrete event -> concrete event

(* In subst_map2_agent_in_concrete_event, the first renaming concerns the ids of the agent before the event, the second renaming the ones after the event.
   In the case when a removed agent and a created one have the same id, then
   the first renaming will be applied to anything related to the removed agent,
   the second one will be applied to anything related to the second one *)
val subst_map2_agent_in_concrete_event :
  (int -> int) -> (int -> int) -> concrete event -> concrete event

val subst_agent_in_concrete_event :
  int -> int -> concrete event -> concrete event

val print_concrete_test :
  ?sigs:Signature.s -> Format.formatter -> concrete test -> unit

val print_concrete_action :
  ?sigs:Signature.s -> Format.formatter -> concrete action -> unit

val print_concrete_binding_state :
  ?sigs:Signature.s -> Format.formatter -> concrete binding_state -> unit

val json_dictionnary : string
val test_to_json : ('a -> Yojson.Basic.t) -> 'a test -> Yojson.Basic.t
val test_of_json : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a test
val write_test : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a test -> unit

val read_test :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a test

val action_to_json : ('a -> Yojson.Basic.t) -> 'a action -> Yojson.Basic.t
val action_of_json : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a action
val write_action : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a action -> unit

val read_action :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a action

val event_to_json : ('a -> Yojson.Basic.t) -> 'a event -> Yojson.Basic.t
val event_of_json : (Yojson.Basic.t -> 'a) -> Yojson.Basic.t -> 'a event
val write_event : (Buffer.t -> 'a -> unit) -> Buffer.t -> 'a event -> unit

val read_event :
  (Yojson.Basic.lexer_state -> Lexing.lexbuf -> 'a) ->
  Yojson.Basic.lexer_state ->
  Lexing.lexbuf ->
  'a event
