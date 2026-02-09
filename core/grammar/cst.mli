(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val append_to_ast_compil :
  Ast.parsing_instruction list ->
  ?all_rules_in_ws:bool ->
  ?rules_in_ws:int list ->
  ?removed_rules:int list ->
  Ast.parsing_compil ->
  Ast.parsing_compil
