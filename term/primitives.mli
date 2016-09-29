(** Compiled kappa model unit *)

type contact_map = ((int list) * (int*int) list) array array

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

  val fresh_bindings :
    short_branch_agents:int list -> Instantiation.abstract t list ->
    (Instantiation.abstract Instantiation.site *
     Instantiation.abstract Instantiation.site) list
end

type elementary_rule = {
  rate : Alg_expr.t Location.annot;
  unary_rate : (Alg_expr.t Location.annot * int option) option;
  connected_components : Connected_component.t array;
  removed : Instantiation.abstract Transformation.t list;
  inserted : Instantiation.abstract Transformation.t list;
  fresh_bindings :
    (Instantiation.abstract Instantiation.site * Instantiation.abstract Instantiation.site) list;
  consumed_tokens : (Alg_expr.t Location.annot * int) list;
  injected_tokens : (Alg_expr.t Location.annot * int) list;
  syntactic_rule : int;
  (** [0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
  (** In the reverse order on purpose so that we rev_map when we
      concretize *)
}

type modification =
  | ITER_RULE of Alg_expr.t Location.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Location.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr list
  | STOP of Alg_expr.t Ast.print_expr list
  | CFLOW of string option * Connected_component.t array *
             Instantiation.abstract Instantiation.test list
  | FLUX of bool * Alg_expr.t Ast.print_expr list
  | FLUXOFF of Alg_expr.t Ast.print_expr list
  | CFLOWOFF of Connected_component.t array
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr list *
       Alg_expr.t Ast.print_expr list)

type perturbation =
  { precondition:
      (Connected_component.t array list,int) Alg_expr.bool_expr Location.annot;
    effect : modification list;
    abort : (Connected_component.t array list,int)
        Alg_expr.bool_expr Location.annot option;
  }

val exists_modification : (modification -> bool) -> perturbation array -> bool

val extract_connected_components_modifications :
  modification list -> Connected_component.t list

val map_expr_rule : (Alg_expr.t Location.annot -> Alg_expr.t Location.annot) ->
  elementary_rule -> elementary_rule
val map_expr_perturbation :
  (Alg_expr.t Location.annot -> Alg_expr.t Location.annot) ->
  ((Connected_component.t array list,int) Alg_expr.bool_expr Location.annot ->
   (Connected_component.t array list,int) Alg_expr.bool_expr Location.annot) ->
  perturbation -> perturbation

val stops_of_perturbation :
  (Operator.DepSet.t * Operator.DepSet.t *
   Operator.DepSet.t array * Operator.DepSet.t array) ->
  perturbation -> Nbr.t list
