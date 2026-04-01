(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let compute_ws_values ~all_rules_in_ws ~rules_in_ws rules inits compil =
  let init_rev, working_set_values, nr_working_set_params =
    List.fold_left
      (fun (inits, ws_values, k) (ws_index, (guard, alg, init_t)) ->
        match ws_index with
        | None ->
          if all_rules_in_ws then
            ( (Some k, (guard, alg, init_t)) :: inits,
              Mods.IntMap.add k true ws_values,
              k + 1 )
          else
            (ws_index, (guard, alg, init_t)) :: inits, ws_values, k
        | Some _ ->
          ( (Some k, (guard, alg, init_t)) :: inits,
            Mods.IntMap.add k true ws_values,
            k + 1 ))
      ([], compil.Ast.working_set_values, compil.nr_working_set_params)
      inits
  in
  let rules_rev, working_set_values, nr_working_set_params, _ =
    List.fold_left
      (fun (rules, ws_values, k, i) (ws_index, label, guard, rule) ->
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
            i + 1 ))
      ([], working_set_values, nr_working_set_params, List.length compil.rules)
      rules
  in
  ( {
      compil with
      rules = compil.Ast.rules @ List.rev rules_rev;
      init = compil.Ast.init @ List.rev init_rev;
      working_set_values;
      nr_working_set_params;
    },
    List.rev rules_rev,
    List.rev init_rev )

let append_to_ast_compil rev_instr ?(all_rules_in_ws = false)
    ?(rules_in_ws = []) compil =
  let compil, rules, inits =
    List.fold_left
      (fun (r, rules, inits) -> function
        | Ast.RULE (label, guard, (rule, loc), is_in_working_set) ->
          if is_in_working_set then
            r, (Some 1, label, guard, (rule, loc)) :: rules, inits
          else
            r, (None, label, guard, (rule, loc)) :: rules, inits
        | Ast.SIG ag ->
          { r with Ast.signatures = ag :: r.Ast.signatures }, rules, inits
        | Ast.TOKENSIG str_pos ->
          { r with Ast.tokens = str_pos :: r.Ast.tokens }, rules, inits
        | Ast.VOLSIG (vol_type, vol, vol_param) ->
          ( { r with Ast.volumes = (vol_type, vol, vol_param) :: r.Ast.volumes },
            rules,
            inits )
        | Ast.INIT ((guard, alg, init_t), is_in_working_set) ->
          if is_in_working_set then
            r, rules, (Some 1, (guard, alg, init_t)) :: inits
          else
            r, rules, (None, (guard, alg, init_t)) :: inits
        | Ast.DECLARE var ->
          { r with Ast.variables = var :: r.Ast.variables }, rules, inits
        | Ast.OBS (((lbl, pos), _) as var) ->
          (*for backward compatibility, shortcut for %var + %plot*)
          ( {
              r with
              Ast.variables = var :: r.Ast.variables;
              Ast.observables = (Alg_expr.ALG_VAR lbl, pos) :: r.Ast.observables;
            },
            rules,
            inits )
        | Ast.PLOT expr ->
          { r with Ast.observables = expr :: r.Ast.observables }, rules, inits
        | Ast.PERT ((alarm, pre, effect, opt), pos) ->
          ( {
              r with
              Ast.perturbations =
                ((alarm, pre, effect, opt), pos) :: r.Ast.perturbations;
            },
            rules,
            inits )
        | Ast.CONFIG (param_name, value_list) ->
          ( {
              r with
              Ast.configurations =
                (param_name, value_list) :: r.Ast.configurations;
            },
            rules,
            inits )
        | Ast.GUARD_PARAM ((params_sig, _), b) ->
          ( {
              r with
              Ast.guard_param_values =
                Mods.StringMap.add params_sig b r.Ast.guard_param_values;
            },
            rules,
            inits )
        | Ast.CONFLICT (agent, site1, site2) ->
          ( { r with Ast.conflicts = (agent, site1, site2) :: r.Ast.conflicts },
            rules,
            inits )
        | Ast.SEQUENTIAL_BOND (agent, site1, site2) ->
          ( {
              r with
              Ast.sequential_bonds =
                (agent, site1, site2) :: r.Ast.sequential_bonds;
            },
            rules,
            inits ))
      (compil, [], []) (List.rev rev_instr)
  in
  let compil, _, _ =
    compute_ws_values ~all_rules_in_ws ~rules_in_ws rules inits compil
  in
  compil
