(** Compiled kappa model unit *)

(** Elementary rule transformations *)
module Transformation :
sig
  type t =
    | Agent of Agent_place.t
    | Freed of Agent_place.t * int
    | Linked of (Agent_place.t * int) * (Agent_place.t * int)
    | PositiveInternalized of Agent_place.t * int * int
    | NegativeInternalized of Agent_place.t * int

  val rename :
    Connected_component.work -> int ->
    Connected_component.cc -> Renaming.t -> t -> t

  val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
end

type elementary_rule = {
  rate : Alg_expr.t;
  unary_rate : Alg_expr.t option;
  connected_components : Connected_component.t array;
  removed : Transformation.t list;
  inserted : Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  syntactic_rule : int;
  (** negative number [n] means opposite of rule |[n]|,
[0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
}

type modification =
    ITER_RULE of Alg_expr.t Location.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr Location.annot list
  | STOP of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOW of string option * Connected_component.t array *
	       Instantiation.abstract Instantiation.test list
  | FLUX of Alg_expr.t Ast.print_expr Location.annot list
  | FLUXOFF of Alg_expr.t Ast.print_expr Location.annot list
  | CFLOWOFF of Connected_component.t array
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
