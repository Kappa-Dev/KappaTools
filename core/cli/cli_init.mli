(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type preprocessed_ast = {
  conf: Configuration.t;
  story_compression: bool * bool * bool;
  formatCflow: string;
  cflowFile: string option;
  ast_compiled_data: LKappa_compiler.ast_compiled_data;
  overwrite_init:
    (LKappa.rule_mixture, Raw_mixture.t, int) Ast.init_statement list option;
  overwrite_t0: float option;
}

(* TODO contact map is also in env *)
type compilation_result = {
  conf: Configuration.t;
  env: Model.t;
  contact_map: Contact_map.t;
  updated_alg_vars: int list;
  story_compression: (bool * bool * bool) option;
  formatCflow: string;
  cflowFile: string option;
  init_l: (Primitives.alg_expr * Primitives.elementary_rule) list;
  counter_opt: Counter.t option;
      (* TODO Should we keep counter_opt here, or create another type? *)
}

(* TODO change calls to this *)
val get_compilation :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  debug_mode:bool ->
  ?compile_mode_on:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  compilation_result

val get_ast_from_list_of_files :
  Ast.syntax_version -> string list -> Ast.parsing_compil

val get_ast_from_cli_args : Run_cli_args.t -> Ast.parsing_compil

val get_preprocessed_ast_from_cli_args :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  debug_mode:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  preprocessed_ast

val preprocess_ast :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  debug_mode:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  Ast.parsing_compil ->
  preprocessed_ast

val get_compilation_from_preprocessed_ast :
  warning:(pos:Loc.t -> (Format.formatter -> unit) -> unit) ->
  ?compile_mode_on:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  preprocessed_ast ->
  compilation_result
