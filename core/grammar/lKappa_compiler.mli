(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val bool_expr_of_ast :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  syntax_version:Ast.syntax_version ->
  Signature.s ->
  int Mods.StringMap.t ->
  int Mods.StringMap.t ->
  ?max_allowed_var:int ->
  (Ast.mixture, string) Alg_expr.bool Locality.annot ->
  (LKappa.rule_agent list, int) Alg_expr.bool Locality.annot

val modif_expr_of_ast :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  syntax_version:Ast.syntax_version ->
  Signature.s ->
  int Mods.StringMap.t ->
  int Mods.StringMap.t ->
  Contact_map.t ->
  (Ast.mixture, Ast.mixture, string, Ast.rule) Ast.modif_expr ->
  int list ->
  (LKappa.rule_agent list, Raw_mixture.t, int, LKappa.rule) Ast.modif_expr
  * int list

val init_of_ast :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  syntax_version:Ast.syntax_version ->
  Signature.s ->
  Contact_map.t ->
  int Mods.StringMap.t ->
  int Mods.StringMap.t ->
  (Ast.mixture, Ast.mixture, string) Ast.init_statment list ->
  (LKappa.rule_agent list, Raw_mixture.t, int) Ast.init_statment list

val compil_of_ast :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  debugMode:bool ->
  syntax_version:Ast.syntax_version ->
  (string * Nbr.t) list ->
  Ast.parsing_compil ->
  Signature.s
  * Contact_map.t
  * unit NamedDecls.t
  * int Mods.StringMap.t
  * int list
  * ( Ast.agent,
      LKappa.rule_agent list,
      Raw_mixture.t,
      int,
      LKappa.rule )
    Ast.compil
(** [compil_of_ast variable_overwrite ast]

    @return the signature of agent, the contact map, the signature of
    tokens, an algebraic variable finder, algebraic variable on which
    constant propagation is forbidden, and an Ast.compil where identifiers
    are integers and not string, syntactic sugar on rules are expansed
    (syntactic sugar on mixture are not)

    This function sorts out longest prefix convention as well as ensure a
    lot of sanity on mixtures:
    - agent exists
    - sites exist
    - unique site occurence / agent
    - internal_states exist
    - unique internal_state / site
    - links appear exactly twice

    The sanity checks on rates consists in ensuring that
    - either absolute or unary rates are provided;
    - if the algebraic expression of the rate contains a mixture then
    a new variable is declared called rulelabel_un_rate; it is
    necessary in the update phase.

    After this step, [Ast.ANY_FREE] is a synonym of "an [Ast.LNK_ANY]
    explicitely given by the user". *)
