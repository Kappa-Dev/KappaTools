(** Compiled kappa model unit *)

(** An agent in a connected component *)
module Place :
sig
  type t =
      Existing of Connected_component.ContentAgent.t * int (* node, id *)
    | Fresh of int * int (* type, id *)

  val rename :
    Connected_component.work -> int -> Connected_component.cc ->
    Renaming.t -> t -> t

  val is_site_from_fresh : (t * int) -> bool
end

(** Elementary rule transformations *)
module Transformation :
sig
  type t =
      Freed of Place.t * int
    | Linked of (Place.t * int) * (Place.t * int)
    | Internalized of Place.t * int * int

  val rename :
    Connected_component.work -> int ->
    Connected_component.cc -> Renaming.t -> t -> t

  val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
end

(** Trace of a rule exactly as the user wrote it (before
compilation) *)
module Instantiation :
sig
  type agent_name = int
  type site_name = int
  type internal_state  = int

  type binding_type = agent_name * site_name

  type abstract = Place.t
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
  val concretize_test : (Place.t -> int) -> abstract test -> concrete test
  val concretize_action : (Place.t -> int) -> abstract action -> concrete action
  val concretize_event : (Place.t -> int) -> abstract event -> concrete event

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
  val subst_agent_in_concrete_event:
    int -> int -> concrete event -> concrete event

  val print_concrete_test :
    ?sigs:Signature.s -> Format.formatter -> concrete test -> unit
  val print_concrete_action :
    ?sigs:Signature.s -> Format.formatter -> concrete action -> unit
end

type elementary_rule = {
  rate : Alg_expr.t;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  instantiations : Instantiation.abstract Instantiation.event;
}

type modification =
    ITER_RULE of Alg_expr.t Location.annot * elementary_rule
  | UPDATE of Operator.rev_dep * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr Location.annot list
  | STOP of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOW of
      Connected_component.t * Instantiation.abstract Instantiation.test list
  | FLUX of Alg_expr.t Ast.print_expr Location.annot list
  | FLUXOFF of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOWOFF of Connected_component.t
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr Location.annot list *
	 Alg_expr.t Ast.print_expr Location.annot list)

type perturbation =
    { precondition: Alg_expr.t Ast.bool_expr;
      effect : modification list;
      abort : Alg_expr.t Ast.bool_expr option;
      stopping_time : Nbr.t list
    }
