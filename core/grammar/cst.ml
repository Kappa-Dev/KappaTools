(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let add_working_set_guard guard k loc =
  let guard_name = Ast.working_set_index_to_string k in
  let guard_param = Logical_formulae.P (guard_name, loc) in
  match guard with
  | None -> Some guard_param
  | Some guard -> Some (Logical_formulae.AND (guard_param, guard))

let append_to_ast_compil (nr_working_set_rules, rev_instr) compil =
  fst
  @@ List.fold_left
       (fun (r, k) -> function
         | Ast.RULE (label, guard, (rule, loc), is_in_working_set) ->
           if is_in_working_set then (
             let updated_guard = add_working_set_guard guard k loc in
             ( {
                 r with
                 Ast.rules =
                   (Some k, label, updated_guard, (rule, loc)) :: r.Ast.rules;
                 Ast.working_set_values =
                   Mods.IntMap.add k true r.Ast.working_set_values;
               },
               k - 1 )
           ) else
             ( {
                 r with
                 Ast.rules = (None, label, guard, (rule, loc)) :: r.Ast.rules;
               },
               k )
         | Ast.SIG ag -> { r with Ast.signatures = ag :: r.Ast.signatures }, k
         | Ast.TOKENSIG str_pos ->
           { r with Ast.tokens = str_pos :: r.Ast.tokens }, k
         | Ast.VOLSIG (vol_type, vol, vol_param) ->
           ( { r with Ast.volumes = (vol_type, vol, vol_param) :: r.Ast.volumes },
             k )
         | Ast.INIT (guard, alg, init_t) ->
           { r with Ast.init = (guard, alg, init_t) :: r.Ast.init }, k
         | Ast.DECLARE var ->
           { r with Ast.variables = var :: r.Ast.variables }, k
         | Ast.OBS (((lbl, pos), _) as var) ->
           (*for backward compatibility, shortcut for %var + %plot*)
           ( {
               r with
               Ast.variables = var :: r.Ast.variables;
               Ast.observables =
                 (Alg_expr.ALG_VAR lbl, pos) :: r.Ast.observables;
             },
             k )
         | Ast.PLOT expr ->
           { r with Ast.observables = expr :: r.Ast.observables }, k
         | Ast.PERT ((alarm, pre, effect, opt), pos) ->
           ( {
               r with
               Ast.perturbations =
                 ((alarm, pre, effect, opt), pos) :: r.Ast.perturbations;
             },
             k )
         | Ast.CONFIG (param_name, value_list) ->
           ( {
               r with
               Ast.configurations =
                 (param_name, value_list) :: r.Ast.configurations;
             },
             k )
         | Ast.GUARD_PARAM ((params_sig, _), b) ->
           ( {
               r with
               Ast.guard_param_values =
                 Mods.StringMap.add params_sig b r.Ast.guard_param_values;
             },
             k )
         | Ast.CONFLICT (agent, site1, site2) ->
           ( { r with Ast.conflicts = (agent, site1, site2) :: r.Ast.conflicts },
             k )
         | Ast.SEQUENTIAL_BOND (agent, site1, site2) ->
           ( {
               r with
               Ast.sequential_bonds =
                 (agent, site1, site2) :: r.Ast.sequential_bonds;
             },
             k ))
       (compil, nr_working_set_rules - 1)
       (List.rev rev_instr)
