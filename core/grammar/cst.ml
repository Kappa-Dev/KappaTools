(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let add_working_set_guard guard k loc =
  let guard_name = "@rule-" ^ string_of_int k in
  let guard_param = LKappa.Param (guard_name, loc) in
  match guard with
  | None -> Some guard_param
  | Some guard -> Some (LKappa.And (guard_param, guard))

let append_to_ast_compil rev_instr compil =
  fst
  @@ List.fold_left
       (fun (r, k) -> function
         | Ast.RULE (label, guard, (rule, loc), is_in_working_set) ->
           if is_in_working_set then
             ( {
                 r with
                 Ast.rules =
                   (label, add_working_set_guard guard k loc, (rule, loc))
                   :: r.Ast.rules;
               },
               k + 1 )
           else
             ( { r with Ast.rules = (label, guard, (rule, loc)) :: r.Ast.rules },
               k )
         | Ast.SIG ag -> { r with Ast.signatures = ag :: r.Ast.signatures }, k
         | Ast.TOKENSIG str_pos ->
           { r with Ast.tokens = str_pos :: r.Ast.tokens }, k
         | Ast.VOLSIG (vol_type, vol, vol_param) ->
           ( { r with Ast.volumes = (vol_type, vol, vol_param) :: r.Ast.volumes },
             k )
         | Ast.INIT (alg, init_t) ->
           { r with Ast.init = (alg, init_t) :: r.Ast.init }, k
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
                 Ast.StringMap.add params_sig b r.Ast.guard_param_values;
             },
             k )
         | Ast.CONFLICT (agent, site1, site2) ->
           ( { r with Ast.conflicts = (agent, site1, site2) :: r.Ast.conflicts },
             k ))
       (compil, 0) (List.rev rev_instr)
