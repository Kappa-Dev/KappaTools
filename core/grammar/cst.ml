(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let append_to_ast_compil rev_instr compil =
  List.fold_left
    (fun r -> function
      | Ast.RULE ru -> { r with Ast.rules = ru :: r.Ast.rules }
      | Ast.SIG ag -> { r with Ast.signatures = ag :: r.Ast.signatures }
      | Ast.TOKENSIG str_pos -> { r with Ast.tokens = str_pos :: r.Ast.tokens }
      | Ast.VOLSIG (vol_type, vol, vol_param) ->
        { r with Ast.volumes = (vol_type, vol, vol_param) :: r.Ast.volumes }
      | Ast.INIT (alg, init_t) ->
        { r with Ast.init = (alg, init_t) :: r.Ast.init }
      | Ast.DECLARE var -> { r with Ast.variables = var :: r.Ast.variables }
      | Ast.OBS (((lbl, pos), _) as var) ->
        (*for backward compatibility, shortcut for %var + %plot*)
        {
          r with
          Ast.variables = var :: r.Ast.variables;
          Ast.observables = (Alg_expr.ALG_VAR lbl, pos) :: r.Ast.observables;
        }
      | Ast.PLOT expr -> { r with Ast.observables = expr :: r.Ast.observables }
      | Ast.PERT ((alarm, pre, effect, opt), pos) ->
        {
          r with
          Ast.perturbations =
            ((alarm, pre, effect, opt), pos) :: r.Ast.perturbations;
        }
      | Ast.CONFIG (param_name, value_list) ->
        {
          r with
          Ast.configurations = (param_name, value_list) :: r.Ast.configurations;
        })
    compil (List.rev rev_instr)
