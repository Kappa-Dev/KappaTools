module Place :
sig
  type t =
      Existing of Connected_component.ContentAgent.t * int (* node, id *)
    | Fresh of int * int (* type, id *)

  val rename :
    Connected_component.work -> int -> Connected_component.cc ->
    Renaming.t -> t -> t
end

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
    | Free of 'a site
    | Remove of 'a

  val rename_abstract_test :
    Connected_component.work -> int ->
    Connected_component.cc -> Renaming.t -> abstract test -> abstract test
  val rename_abstract_action :
    Connected_component.work -> int ->
    Connected_component.cc -> Renaming.t -> abstract action -> abstract action
  val abstract_action_of_transformation : Transformation.t -> abstract action
  val concretize_test : (Place.t -> int) -> abstract test -> concrete test
  val concretize_action : (Place.t -> int) -> abstract action -> concrete action
end

type elementary_rule = {
  rate : Alg_expr.t;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  instantiations :
    Instantiation.abstract Instantiation.test list *
      Instantiation.abstract Instantiation.action list;
}

type modification =
    ITER_RULE of Alg_expr.t Term.with_pos * elementary_rule
  | UPDATE of Term.dep_type * Alg_expr.t Term.with_pos
  | SNAPSHOT of Alg_expr.t Ast.print_expr Term.with_pos list
  | STOP of Alg_expr.t Ast.print_expr Term.with_pos list
  | CFLOW of Connected_component.t
  | FLUX of Alg_expr.t Ast.print_expr Term.with_pos list
  | FLUXOFF of Alg_expr.t Ast.print_expr Term.with_pos list
  | CFLOWOFF of Connected_component.t
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr Term.with_pos list *
	 Alg_expr.t Ast.print_expr Term.with_pos list)

type perturbation =
    { precondition: Alg_expr.t Ast.bool_expr;
      effect : modification list;
      abort : Alg_expr.t Ast.bool_expr option;
      stopping_time : Nbr.t list
    }
