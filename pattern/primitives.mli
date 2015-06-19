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

module Compilation_info :
sig
  type t = {
    sites_tested_unmodified : (Place.t * int) list;
    sites_tested_modified : (Place.t * int) list;
    sites_untested_modified : (Place.t * int) list;
    internal_states_tested_unmodified : (Place.t * int) list;
    internal_states_tested_modified : (Place.t * int) list;
    internal_states_untested_modified : (Place.t * int) list;
  }

  val of_empty_rule : t
  val add_site_tested_only : Place.t -> int -> t -> t
  val add_site_modified : tested:bool -> Place.t -> int -> t -> t
  val add_internal_state_tested_only : Place.t -> int -> t -> t
  val add_internal_state_modified : tested:bool -> Place.t -> int -> t -> t

  val rename :
    Connected_component.work -> int ->
    Connected_component.cc -> Renaming.t -> t -> t
end

type elementary_rule = {
  rate : Alg_expr.t;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  infos : Compilation_info.t;
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
