(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
type preprocessed_ast

val get_compilation :
  ?unit:Kasim_args.directive_unit -> ?max_sharing:bool ->
  ?bwd_bisim:LKappa_auto.bwd_bisim_info ->
  ?compileModeOn:bool -> ?kasim_args:Kasim_args.t -> Run_cli_args.t ->
  (Configuration.t * Counter.progressBar * Model.t * Contact_map.t * int list *
   (bool*bool*bool) option * string * string option *
   (Alg_expr.t * Primitives.elementary_rule * Locality.t) list) *
  Counter.t

val get_ast_from_list_of_files:
  string list -> Ast.parsing_compil

val get_ast_from_cli_args:
  Run_cli_args.t -> Ast.parsing_compil

val get_preprocessed_ast_from_cli_args:
  ?kasim_args:Kasim_args.t -> Run_cli_args.t -> preprocessed_ast

val preprocess:
    ?kasim_args:Kasim_args.t -> Run_cli_args.t ->
    Ast.parsing_compil -> preprocessed_ast

val get_compilation_from_preprocessed_ast :
  ?unit:Kasim_args.directive_unit -> ?max_sharing:bool ->
  ?bwd_bisim:LKappa_auto.bwd_bisim_info ->
  ?compileModeOn:bool -> ?kasim_args:Kasim_args.t ->
  Run_cli_args.t -> preprocessed_ast ->
  (Configuration.t * Counter.progressBar * Model.t * Contact_map.t * int list *
   (bool*bool*bool) option * string * string option *
   (Alg_expr.t * Primitives.elementary_rule * Locality.t) list) *
  Counter.t
