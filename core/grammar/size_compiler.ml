(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type size_predicate_sites = Ast.size_cons Loc.annoted 
type 'a with_size_predicates = { agent: 'a ; thresholds: Ast.size_cons}

type rule_mixture_with_size_predicates =
  LKappa.rule_agent with_size_predicates list

type raw_mixture_with_size_predicates =
  Raw_mixture.agent with_size_predicates list

(** [split_cvar_counter_in_rules_per_value var_name annot counter_delta counter_def] translates a counter CVAR whose value acts upon the rate expression into a rule per possible value, that are selected by a CEQ expression.
 * *)

let has_size_predicates compil = compil.Ast.thresholds <> ([],[])

(** Compiles the counter precondition in a left hand side mixture of a rule into a mixture which tests dummy positions
 * rule_agent_ - agent with counters in a rule
   lnk_nb - the max link number used in the rule;
   sigs.counter_agent_info - info on the counter agent
   returns: agent with explicit counters; created incr agents;
            the next link number to use *)

let add_test_in_rule_agent sigs (agent : LKappa.rule_agent) (t_agent_name,(cmp,_),(i,_)) =
  let agent_id = agent.LKappa.ra_type in
  let t_agent_id = Tools.map_opt (fun a -> Signature.num_of_agent a sigs) t_agent_name in 
  let t = Size_info.compute_threshold (cmp,i)  in 
  let site = Size_info.get_size_predicate_site agent_id t_agent_id t sigs in
  let () =
    Array.set agent.LKappa.ra_ports site
      ((LKappa.LNK_FREE, Loc.dummy), LKappa.Maintained)
  in
  let int = 
    match fst cmp with 
    | Operator.SMALLER -> Size_info.get_internal_state_true agent_id t_agent_id t sigs 
    | Operator.GREATER -> Size_info.get_internal_state_false agent_id t_agent_id t sigs
    | Operator.EQUAL | Operator.DIFF -> assert false 
  in
  let () =
    Array.set agent.LKappa.ra_ints site (LKappa.I_VAL_CHANGED (int, int))
  in

  agent

let compile_size_predicates_in_rule_agent (sigs : Signature.s)
    (_size_info : Size_info.t) (threshold : size_predicate_sites option)
    (rule_agent : LKappa.rule_agent with_size_predicates) : LKappa.rule_agent =
  let agent = (* cc from rates *)
    match threshold with
    | None -> rule_agent.agent
    | Some (t, _) ->
      List.fold_left 
       (add_test_in_rule_agent sigs ) 
       rule_agent.agent t
  in
  List.fold_left 
    (add_test_in_rule_agent sigs ) 
    agent rule_agent.thresholds

(** Compiles the counter value change in the right hand side of a rule into dummy chain changes *)
let compile_size_predicates_in_raw_agent (_sigs : Signature.s)
    (_size_info : Size_info.t)
    (raw_agent_ : Raw_mixture.agent with_size_predicates) : Raw_mixture.agent =
  raw_agent_.agent

(** [compile_counter_in_rule sigs mix created] takes the intial mixture from a rule [mix],
 * and the mixture obtained from the application of the rule [created],
 * both with counter information, and returns two mixtures for a new rule without counters, having compiled the counter logic inside the rule.
 *
 * - adds increment agents to the rule_agent mixture
   - adds increment agents to the raw mixture
   - links the agents in the mixture(lhs,rhs,mix) or in the raw mixture(created)
     to the increments *)
let compile_size_predicate_in_rule (sigs : Signature.s)
    (size_info : Size_info.size_sig option Array.t Array.t)
    (mix : rule_mixture_with_size_predicates)
    (created : raw_mixture_with_size_predicates)
    (t : size_predicate_sites option) : LKappa.rule_mixture * Raw_mixture.t =
  let mix =
    List.rev_map
      (compile_size_predicates_in_rule_agent sigs size_info t)
      (List.rev mix)
  in

  let raw_mix : Raw_mixture.t =
    List.rev_map
      (compile_size_predicates_in_raw_agent sigs size_info)
      (List.rev created)
  in
  mix, raw_mix

let make_size_predicate_site_sig sigs agent_name i =
  let agent_id = 
    match agent_name with None -> None | Some name -> 
      let agent_name = Loc.annot_with_dummy name in
      (Some (Signature.num_of_agent agent_name sigs))  
  in 
  let name = Size_info.name_of_size_predicate sigs agent_id i in
  {
    Size_info.threshold_sig_name = Loc.annot_with_dummy name;
    threshold_sig_value = [ Loc.annot_with_dummy (Some "true") ];
    threshold = i;
    threshold_sig_agent_name = agent_name; 
  }

let make_size_predicate_site sigs id agent_id i =
  let name = Size_info.name_of_size_predicate sigs id i in
  {
    Ast.threshold_name = Loc.annot_with_dummy name;
    threshold_value =
      [
        Loc.annot_with_dummy (Some "true"); Loc.annot_with_dummy (Some "false");
      ];
    threshold = i;
    threshold_agent_id = agent_id; 
  }

let _ = make_size_predicate_site, make_size_predicate_site_sig

let annotate_dropped_size_predicates _sign _ast_counters ra _arity _agent_name
    _aux cc =
  { agent = ra ; thresholds = cc }

let annotate_edit_size_predicates _sigs ((_agent_name, _) as _agent_type)
    _counters ra _add_link_contact_map cc =
  { agent = ra ; thresholds = cc}

let annotate_size_predicates_with_diff _sigs
    ((_agent_name, _loc) as _agent_type) _lc _rc ra _add_link_contact_map  cc =
  { agent = ra ; thresholds = cc}

let annotate_created_size_predicates _sigs ((_agent_name, _) as _agent_type)
    _counter_list _add_link_contact_map ra _lcc =
  { agent = ra ; thresholds = [] }

let compute_between_thresholds_matrix thresholds =
  Connected.init_between_thresholds thresholds