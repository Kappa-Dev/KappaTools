(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(** Compiled kappa model unit *)

(** Elementary rule transformations *)
module Transformation : sig
  type 'a t =
    | Agent of 'a
    | Freed of 'a Instantiation.site
    | Linked of 'a Instantiation.site * 'a Instantiation.site
    | NegativeWhatEver of 'a Instantiation.site
    | PositiveInternalized of
        'a * Instantiation.site_name * Instantiation.internal_state
    | NegativeInternalized of 'a Instantiation.site

  val map_agent : ('a -> 'b) -> 'a t -> 'b t
  val fold_agent : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map_fold_agent : ('a -> 'b -> 'c * 'b) -> 'a t -> 'b -> 'c t * 'b
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

  val rename :
    debugMode:bool ->
    int ->
    Renaming.t ->
    Instantiation.abstract t ->
    Instantiation.abstract t

  val concretize :
    debugMode:bool ->
    Matching.t * int Mods.IntMap.t ->
    Instantiation.abstract t ->
    Instantiation.concrete t

  val print :
    ?sigs:Signature.s -> Format.formatter -> Instantiation.abstract t -> unit

  val negative_transformations_of_actions :
    Signature.s ->
    (Instantiation.concrete Instantiation.site ->
    Instantiation.concrete Instantiation.site option) ->
    Instantiation.concrete Instantiation.action list ->
    Instantiation.concrete t list
  (** [negative_transformation_of_actions signature link_dst actions] *)

  val positive_transformations_of_actions :
    Signature.s ->
    Instantiation.concrete Instantiation.site list ->
    Instantiation.concrete Instantiation.action list ->
    Instantiation.concrete t list
  (** [positive_transformations_of_actions signature side_effect_dsts actions] *)
end

type alg_expr = (Pattern.id array list, int) Alg_expr.e

type elementary_rule = {
  rate: alg_expr Locality.annot;
  unary_rate: (alg_expr Locality.annot * alg_expr option) option;
  connected_components: Pattern.id array;
  removed: Instantiation.abstract Transformation.t list;
  inserted: Instantiation.abstract Transformation.t list;
  delta_tokens: (alg_expr Locality.annot * int) list;
  syntactic_rule: int;  (** [0] means generated for perturbation. *)
  instantiations: Instantiation.abstract Instantiation.event;
      (** In the reverse order on purpose so that we rev_map when we
      concretize *)
}
(** If [unary_rate] is [Some _], [rate] means binary rate. Else [rate]
    is the rate independently of the connectivity *)

val extract_cc_ids : elementary_rule -> Pattern.id array

val extract_abstract_event :
  elementary_rule -> Instantiation.abstract Instantiation.event

val rule_to_yojson :
  filenames:int Mods.StringMap.t -> elementary_rule -> Yojson.Basic.t

val rule_of_yojson : filenames:string array -> Yojson.Basic.t -> elementary_rule

val fully_specified_pattern_to_positive_transformations :
  Pattern.cc -> Instantiation.concrete Transformation.t list

type 'alg_expr print_expr =
  | Str_pexpr of string Locality.annot
  | Alg_pexpr of 'alg_expr Locality.annot

val print_expr_to_yojson :
  filenames:int Mods.StringMap.t ->
  ('a -> Yojson.Basic.t) ->
  ('b -> Yojson.Basic.t) ->
  ('a, 'b) Alg_expr.e print_expr ->
  Yojson.Basic.t

val print_expr_of_yojson :
  filenames:string array ->
  (Yojson.Basic.t -> 'a) ->
  (Yojson.Basic.t -> 'b) ->
  Yojson.Basic.t ->
  ('a, 'b) Alg_expr.e print_expr

type din_kind = ABSOLUTE | RELATIVE | PROBABILITY

val din_kind_to_yojson : din_kind -> Yojson.Basic.t
val din_kind_of_yojson : Yojson.Basic.t -> din_kind

val write_din_kind : Buffer.t -> din_kind -> unit
(** Output a JSON value of type {!din_kind}. *)

val string_of_din_kind : ?len:int -> din_kind -> string
(** Serialize a value of type {!din_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_din_kind : Yojson.Safe.lexer_state -> Lexing.lexbuf -> din_kind
(** Input JSON data of type {!din_kind}. *)

val din_kind_of_string : string -> din_kind
(** Deserialize JSON data of type {!din_kind}. *)

type modification =
  | ITER_RULE of alg_expr Locality.annot * elementary_rule
  | UPDATE of int * alg_expr Locality.annot
  | SNAPSHOT of bool * alg_expr print_expr list
  | STOP of alg_expr print_expr list
  | CFLOW of
      string option
      * Pattern.id array
      * Instantiation.abstract Instantiation.test list list
      (** First string is the named used by the user *)
  | DIN of din_kind * alg_expr print_expr list
  | DINOFF of alg_expr print_expr list
  | CFLOWOFF of string option * Pattern.id array
  | PLOTENTRY
  | PRINT of alg_expr print_expr list * alg_expr print_expr list
  | SPECIES of
      alg_expr print_expr list
      * Pattern.id array
      * Instantiation.abstract Instantiation.test list list
  | SPECIES_OFF of alg_expr print_expr list

type perturbation = {
  alarm: Nbr.t option;
  precondition: (Pattern.id array list, int) Alg_expr.bool Locality.annot;
  effect: modification list;
  repeat: (Pattern.id array list, int) Alg_expr.bool Locality.annot;
  needs_backtrack: bool;
}

val perturbation_to_yojson :
  filenames:int Mods.StringMap.t -> perturbation -> Yojson.Basic.t

val perturbation_of_yojson :
  filenames:string array -> Yojson.Basic.t -> perturbation

val exists_modification : (modification -> bool) -> perturbation array -> bool

val extract_connected_components_modifications :
  modification list -> Pattern.id list

val extract_connected_components_bool :
  (Pattern.id array list, int) Alg_expr.bool Locality.annot -> Pattern.id list

val map_expr_rule :
  (alg_expr Locality.annot -> alg_expr Locality.annot) ->
  elementary_rule ->
  elementary_rule

val map_expr_perturbation :
  (alg_expr Locality.annot -> alg_expr Locality.annot) ->
  ((Pattern.id array list, int) Alg_expr.bool Locality.annot ->
  (Pattern.id array list, int) Alg_expr.bool Locality.annot) ->
  perturbation ->
  perturbation

val fold_expr_rule :
  ('a -> alg_expr Locality.annot -> 'a) -> 'a -> elementary_rule -> 'a

val fold_expr_perturbation :
  ('a -> alg_expr Locality.annot -> 'a) ->
  ('a -> (Pattern.id array list, int) Alg_expr.bool Locality.annot -> 'a) ->
  'a ->
  perturbation ->
  'a
