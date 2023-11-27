(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
type preprocessed_ast

val get_compilation :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  debugMode:bool ->
  ?compileModeOn:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  (Configuration.t
  * Model.t
  * Contact_map.t
  * int list
  * (bool * bool * bool) option
  * string
  * string option
  * (Primitives.alg_expr * Primitives.elementary_rule) list)
  * Counter.t

val get_ast_from_list_of_files :
  Ast.syntax_version -> string list -> Ast.parsing_compil

val get_ast_from_cli_args : Run_cli_args.t -> Ast.parsing_compil

val get_preprocessed_ast_from_cli_args :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  debugMode:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  preprocessed_ast

val preprocess :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  debugMode:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  Ast.parsing_compil ->
  preprocessed_ast

val get_compilation_from_preprocessed_ast :
  warning:(pos:Locality.t -> (Format.formatter -> unit) -> unit) ->
  ?compileModeOn:bool ->
  ?kasim_args:Kasim_args.t ->
  Run_cli_args.t ->
  preprocessed_ast ->
  (Configuration.t
  * Model.t
  * Contact_map.t
  * int list
  * (bool * bool * bool) option
  * string
  * string option
  * (Primitives.alg_expr * Primitives.elementary_rule) list)
  * Counter.t
