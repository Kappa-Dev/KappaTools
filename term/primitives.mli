(** Compiled kappa model unit *)

(** Elementary rule transformations *)
module Transformation :
sig
  type 'a t =
    | Agent of 'a
    | Freed of 'a Instantiation.site
    | Linked of 'a Instantiation.site * 'a Instantiation.site
    | NegativeWhatEver of 'a Instantiation.site
    | PositiveInternalized of
	'a * Instantiation.site_name * Instantiation.internal_state
    | NegativeInternalized of 'a Instantiation.site

  val rename :
    Connected_component.work -> int -> Connected_component.cc ->
    Renaming.t -> Instantiation.abstract t -> Instantiation.abstract t

  val concretize :
    Connected_component.Matching.t * int Mods.IntMap.t ->
    Instantiation.abstract t -> Instantiation.concrete t
  val print :
    ?sigs:Signature.s -> Format.formatter -> Instantiation.abstract t -> unit
end

type elementary_rule = {
  rate : Alg_expr.t;
  unary_rate : (Alg_expr.t * int option) option;
  connected_components : Connected_component.t array;
  removed : Instantiation.abstract Transformation.t list;
  inserted : Instantiation.abstract Transformation.t list;
  consumed_tokens : (Alg_expr.t * int) list;
  injected_tokens : (Alg_expr.t * int) list;
  syntactic_rule : int;
  (** [0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
}

type modification =
    ITER_RULE of Alg_expr.t Location.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr list
  | STOP of Alg_expr.t Ast.print_expr list
  | CFLOW of string option * Connected_component.t array *
	       Instantiation.abstract Instantiation.test list
  | FLUX of Alg_expr.t Ast.print_expr list
  | FLUXOFF of Alg_expr.t Ast.print_expr list
  | CFLOWOFF of Connected_component.t array
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr list *
	 Alg_expr.t Ast.print_expr list)

type perturbation =
    { precondition: Alg_expr.t Ast.bool_expr;
      effect : modification list;
      abort : Alg_expr.t Ast.bool_expr option;
      stopping_time : Nbr.t list
    }

val exists_modification : (modification -> bool) -> perturbation list -> bool
