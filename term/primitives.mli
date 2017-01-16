(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

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
    int -> Renaming.t -> Instantiation.abstract t -> Instantiation.abstract t

  val concretize :
    Matching.t * int Mods.IntMap.t ->
    Instantiation.abstract t -> Instantiation.concrete t
  val print :
    ?sigs:Signature.s -> Format.formatter -> Instantiation.abstract t -> unit

  val fresh_bindings :
    short_branch_agents:int list -> Instantiation.abstract t list ->
    (Instantiation.abstract Instantiation.site *
     Instantiation.abstract Instantiation.site) list
end

type elementary_rule = {
  rate : Alg_expr.t Locality.annot;
  unary_rate : (Alg_expr.t Locality.annot * Alg_expr.t option) option;
  connected_components : Pattern.id array;
  removed : Instantiation.abstract Transformation.t list;
  inserted : Instantiation.abstract Transformation.t list;
  fresh_bindings :
    (Instantiation.abstract Instantiation.site * Instantiation.abstract Instantiation.site) list;
  consumed_tokens : (Alg_expr.t Locality.annot * int) list;
  injected_tokens : (Alg_expr.t Locality.annot * int) list;
  syntactic_rule : int;
  (** [0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
  (** In the reverse order on purpose so that we rev_map when we
      concretize *)
}

type modification =
  | ITER_RULE of Alg_expr.t Locality.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Locality.annot
  | SNAPSHOT of Alg_expr.t Ast.print_expr list
  | STOP of Alg_expr.t Ast.print_expr list
  | CFLOW of string option * Pattern.id array *
             Instantiation.abstract Instantiation.test list list
  | FLUX of bool * Alg_expr.t Ast.print_expr list
  | FLUXOFF of Alg_expr.t Ast.print_expr list
  | CFLOWOFF of Pattern.id array
  | PLOTENTRY
  | PRINT of
      (Alg_expr.t Ast.print_expr list *
       Alg_expr.t Ast.print_expr list)

type perturbation =
  { precondition:
      (Pattern.id array list,int) Alg_expr.bool Locality.annot;
    effect : modification list;
    abort : (Pattern.id array list,int)
        Alg_expr.bool Locality.annot option;
  }

val exists_modification : (modification -> bool) -> perturbation array -> bool

val extract_connected_components_modifications :
  modification list -> Pattern.id list

val map_expr_rule : (Alg_expr.t Locality.annot -> Alg_expr.t Locality.annot) ->
  elementary_rule -> elementary_rule
val map_expr_perturbation :
  (Alg_expr.t Locality.annot -> Alg_expr.t Locality.annot) ->
  ((Pattern.id array list,int) Alg_expr.bool Locality.annot ->
   (Pattern.id array list,int) Alg_expr.bool Locality.annot) ->
  perturbation -> perturbation

val stops_of_perturbation :
  (Operator.DepSet.t * Operator.DepSet.t *
   Operator.DepSet.t array * Operator.DepSet.t array) ->
  perturbation -> Nbr.t list
