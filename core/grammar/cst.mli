(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)
val compute_ws_values :
  all_rules_in_ws:bool ->
  rules_in_ws:int list ->
  Ast.rule Ast.compil_rule list ->
  (int option * (Ast.mixture, Ast.mixture, string) Ast.init_statement) list ->
  Ast.parsing_compil ->
  Ast.parsing_compil

val append_to_ast_compil :
  Ast.parsing_instruction list ->
  ?all_rules_in_ws:bool ->
  ?rules_in_ws:int list ->
  Ast.parsing_compil ->
  Ast.parsing_compil
