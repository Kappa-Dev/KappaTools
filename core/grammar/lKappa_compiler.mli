(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val bool_expr_of_ast :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  syntax_version:Ast.syntax_version ->
  Signature.s ->
  int Mods.StringMap.t ->
  int Mods.StringMap.t ->
  ?max_allowed_var:int ->
  (Ast.mixture, string) Alg_expr.bool Loc.annoted ->
  (LKappa.rule_agent list, int) Alg_expr.bool Loc.annoted

val modif_expr_of_ast :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
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
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  syntax_version:Ast.syntax_version ->
  Signature.s ->
  Contact_map.t ->
  int Mods.StringMap.t ->
  int Mods.StringMap.t ->
  (Ast.mixture, Ast.mixture, string) Ast.init_statement list ->
  (LKappa.rule_agent list, Raw_mixture.t, int) Ast.init_statement list

type ast_compiled_data = {
  agents_sig: Signature.s;
  contact_map: Contact_map.t;
  token_names: unit NamedDecls.t;
  alg_vars_finder: int Mods.StringMap.t;
  updated_alg_vars: int list;  (** alg vars with forbidden constant prop *)
  result:
    ( Ast.agent,
      LKappa.rule_agent list,
      Raw_mixture.t,
      int,
      LKappa.rule )
    Ast.compil;
      (** Compiled data where identifiers are i Ast.compil where identifiers
     * are integers and not string, syntactic sugar on rules are expansed
     * (syntactic sugar on mixture are not) *)
}

val compil_of_ast :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  debug_mode:bool ->
  syntax_version:Ast.syntax_version ->
  var_overwrite:(string * Nbr.t) list ->
  Ast.parsing_compil ->
  ast_compiled_data
(** [compil_of_ast variable_overwrite ast]

    @return a [ast_compiled_data] instance:

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

(** {2 Utils to build signatures} *)

type site_sig_with_links_as_lists =
  (string Loc.annoted * string Loc.annoted) list Signature.site_sig
(** Util type to store site signature with list links instead of array array links *)

val agent_sigs_of_agent_sigs_with_links_as_lists :
  build_contact_map:bool ->
  site_sig_with_links_as_lists NamedDecls.t NamedDecls.t ->
  Signature.t NamedDecls.t
(** Helper to build signatures: for each entry, translate [(string Loc.annoted * string Loc.annoted) list] into [bool array array option] *)
