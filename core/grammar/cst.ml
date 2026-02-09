(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let append_to_ast_compil rev_instr ?(all_rules_in_ws = false)
    ?(rules_in_ws = []) ?(removed_rules = []) compil =
  let compil =
    List.fold_left
      (fun r -> function
        | Ast.RULE (label, guard, (rule, loc), is_in_working_set) ->
          if is_in_working_set then
            {
              r with
              Ast.rules = (Some 1, label, guard, (rule, loc)) :: r.Ast.rules;
            }
          else
            {
              r with
              Ast.rules = (None, label, guard, (rule, loc)) :: r.Ast.rules;
            }
        | Ast.SIG ag -> { r with Ast.signatures = ag :: r.Ast.signatures }
        | Ast.TOKENSIG str_pos ->
          { r with Ast.tokens = str_pos :: r.Ast.tokens }
        | Ast.VOLSIG (vol_type, vol, vol_param) ->
          { r with Ast.volumes = (vol_type, vol, vol_param) :: r.Ast.volumes }
        | Ast.INIT (guard, alg, init_t) ->
          { r with Ast.init = (guard, alg, init_t) :: r.Ast.init }
        | Ast.DECLARE var -> { r with Ast.variables = var :: r.Ast.variables }
        | Ast.OBS (((lbl, pos), _) as var) ->
          (*for backward compatibility, shortcut for %var + %plot*)
          {
            r with
            Ast.variables = var :: r.Ast.variables;
            Ast.observables = (Alg_expr.ALG_VAR lbl, pos) :: r.Ast.observables;
          }
        | Ast.PLOT expr ->
          { r with Ast.observables = expr :: r.Ast.observables }
        | Ast.PERT ((alarm, pre, effect, opt), pos) ->
          {
            r with
            Ast.perturbations =
              ((alarm, pre, effect, opt), pos) :: r.Ast.perturbations;
          }
        | Ast.CONFIG (param_name, value_list) ->
          {
            r with
            Ast.configurations =
              (param_name, value_list) :: r.Ast.configurations;
          }
        | Ast.GUARD_PARAM ((params_sig, _), b) ->
          {
            r with
            Ast.guard_param_values =
              Mods.StringMap.add params_sig b r.Ast.guard_param_values;
          }
        | Ast.CONFLICT (agent, site1, site2) ->
          { r with Ast.conflicts = (agent, site1, site2) :: r.Ast.conflicts }
        | Ast.SEQUENTIAL_BOND (agent, site1, site2) ->
          {
            r with
            Ast.sequential_bonds =
              (agent, site1, site2) :: r.Ast.sequential_bonds;
          })
      compil (List.rev rev_instr)
  in
  let rules_rev, ws_values, _, _ =
    List.fold_left
      (fun (rules, ws_values, k, i) (ws_index, label, guard, rule) ->
        if List.exists (( = ) i) removed_rules then
          rules, ws_values, k, i + 1
        else (
          match ws_index with
          | None ->
            if all_rules_in_ws || List.exists (( = ) i) rules_in_ws then
              ( (Some k, label, guard, rule) :: rules,
                Mods.IntMap.add k true ws_values,
                k + 1,
                i + 1 )
            else
              (ws_index, label, guard, rule) :: rules, ws_values, k, i + 1
          | Some _ ->
            ( (Some k, label, guard, rule) :: rules,
              Mods.IntMap.add k true ws_values,
              k + 1,
              i + 1 )
        ))
      ([], Mods.IntMap.empty, 0, 0)
      compil.Ast.rules
  in
  {
    compil with
    Ast.working_set_values = ws_values;
    Ast.rules = List.rev rules_rev;
  }
