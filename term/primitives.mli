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

  val raw_mixture_of_fresh :
    Signature.s -> Instantiation.abstract t list ->
    (int * Raw_mixture.agent) list *
    (Instantiation.abstract * Instantiation.site_name * int) list *
    Instantiation.abstract t list

  val negative_transformations_of_actions :
    Signature.s -> Edges.t -> Instantiation.concrete Instantiation.action list ->
    Instantiation.concrete t list
  val positive_transformations_of_actions :
    Signature.s -> Instantiation.concrete Instantiation.site list ->
    Instantiation.concrete Instantiation.action list ->
    Instantiation.concrete t list
  (** [positive_transformations_of_actions signature side_effect_dsts actions] *)
end

type elementary_rule = {
  rate : Alg_expr.t Locality.annot;
  unary_rate : (Alg_expr.t Locality.annot * Alg_expr.t option) option;
  connected_components : Pattern.id array;
  removed : Instantiation.abstract Transformation.t list;
  inserted : Instantiation.abstract Transformation.t list;
  delta_tokens : (Alg_expr.t Locality.annot * int) list;
  syntactic_rule : int;
  (** [0] means generated for perturbation. *)
  instantiations : Instantiation.abstract Instantiation.event;
  (** In the reverse order on purpose so that we rev_map when we
      concretize *)
}
(** If [unary_rate] is [Some _], [rate] means binary rate. Else [rate]
    is the rate independently of the connectivity *)

val extract_cc_ids : elementary_rule -> Pattern.id array
val extract_abstract_event :
  elementary_rule -> Instantiation.abstract Instantiation.event
val rule_to_yojson : elementary_rule -> Yojson.Basic.json

val rule_of_yojson : Yojson.Basic.json -> elementary_rule

type 'alg_expr print_expr =
    Str_pexpr of string Locality.annot
  | Alg_pexpr of 'alg_expr Locality.annot

val print_expr_to_yojson :
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a,'b) Alg_expr.e print_expr -> Yojson.Basic.json
val print_expr_of_yojson :
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> ('a,'b) Alg_expr.e print_expr

type flux_kind = ABSOLUTE | RELATIVE | PROBABILITY

val flux_kind_to_yojson : flux_kind -> Yojson.Basic.json
val flux_kind_of_yojson : Yojson.Basic.json -> flux_kind

val write_flux_kind :
  Bi_outbuf.t -> flux_kind -> unit
  (** Output a JSON value of type {!flux_kind}. *)

val string_of_flux_kind :
  ?len:int -> flux_kind -> string
  (** Serialize a value of type {!flux_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_flux_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> flux_kind
  (** Input JSON data of type {!flux_kind}. *)

val flux_kind_of_string :
  string -> flux_kind
  (** Deserialize JSON data of type {!flux_kind}. *)

type modification =
  | ITER_RULE of Alg_expr.t Locality.annot * elementary_rule
  | UPDATE of int * Alg_expr.t Locality.annot
  | SNAPSHOT of Alg_expr.t print_expr list
  | STOP of Alg_expr.t print_expr list
  | CFLOW of string option * Pattern.id array *
             Instantiation.abstract Instantiation.test list list
  (** First string is the named used by the user *)
  | FLUX of flux_kind * Alg_expr.t print_expr list
  | FLUXOFF of Alg_expr.t print_expr list
  | CFLOWOFF of string option * Pattern.id array
  | PLOTENTRY
  | PRINT of Alg_expr.t print_expr list * Alg_expr.t print_expr list
  | SPECIES of Alg_expr.t print_expr list * Pattern.id array *
             Instantiation.abstract Instantiation.test list list
  | SPECIES_OFF of Alg_expr.t print_expr list

type perturbation =
  { precondition:
      Nbr.t option * (Pattern.id array list,int) Alg_expr.bool Locality.annot;
    effect : modification list;
    abort : (Pattern.id array list,int)
        Alg_expr.bool Locality.annot option;
  }

val perturbation_to_yojson : perturbation -> Yojson.Basic.json
val perturbation_of_yojson : Yojson.Basic.json -> perturbation

val exists_modification : (modification -> bool) -> perturbation array -> bool

val extract_connected_components_modifications :
  modification list -> Pattern.id list

val extract_connected_components_bool :
  (Pattern.id array list,int) Alg_expr.bool Locality.annot -> Pattern.id list

val map_expr_rule : (Alg_expr.t Locality.annot -> Alg_expr.t Locality.annot) ->
  elementary_rule -> elementary_rule

val map_expr_perturbation :
  (Alg_expr.t Locality.annot -> Alg_expr.t Locality.annot) ->
  ((Pattern.id array list,int) Alg_expr.bool Locality.annot ->
   (Pattern.id array list,int) Alg_expr.bool Locality.annot) ->
  perturbation -> perturbation

val stops_of_perturbation :
  (Operator.DepSet.t * Operator.DepSet.t *
   Operator.DepSet.t array * Operator.DepSet.t array) -> bool ->
  perturbation -> (Nbr.t option * Nbr.t) list
