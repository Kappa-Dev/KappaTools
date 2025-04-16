(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type size_predicate_site = int * Loc.t
type 'a with_size_predicates = { agent: 'a }

type rule_mixture_with_size_predicates =
  LKappa.rule_agent with_size_predicates list

type raw_mixture_with_size_predicates =
  Raw_mixture.agent with_size_predicates list

(** [split_cvar_counter_in_rules_per_value var_name annot counter_delta counter_def] translates a counter CVAR whose value acts upon the rate expression into a rule per possible value, that are selected by a CEQ expression.
 * *)

let has_size_predicates compil = compil.Ast.thresholds <> []

(** Compiles the counter precondition in a left hand side mixture of a rule into a mixture which tests dummy positions
 * rule_agent_ - agent with counters in a rule
   lnk_nb - the max link number used in the rule;
   sigs.counter_agent_info - info on the counter agent
   returns: agent with explicit counters; created incr agents;
            the next link number to use *)
let compile_size_predicates_in_rule_agent (_sigs : Signature.s)
    (_size_info : Size_info.t) (_threshold : size_predicate_site option)
    (rule_agent : LKappa.rule_agent with_size_predicates) : LKappa.rule_agent =
  let agent = rule_agent.agent in
  (* to do: ajouter le test threshold: true *)
  agent

(** Compiles the counter value change in the right hand side of a rule into dummy chain changes *)
let compile_size_predicates_in_raw_agent (_sigs : Signature.s)
    (_size_info : Size_info.t)
    (raw_agent_ : Raw_mixture.agent with_size_predicates) : Raw_mixture.agent =
  let raw_agent : Raw_mixture.agent = raw_agent_.agent in
  (* to do: ajouter le test threshold: true *)
  raw_agent

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
    (t : size_predicate_site option) : LKappa.rule_mixture * Raw_mixture.t =
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

let make_size_predicate_site_sig i =
  let name = Size_info.name_of_size_predicate i in
  {
    Size_info.threshold_sig_name = Loc.annot_with_dummy name;
    threshold_sig_value = [ Loc.annot_with_dummy (Some "true") ];
    threshold = i;
  }

let _ = make_size_predicate_site_sig

let make_size_predicate_site i =
  let name = Size_info.name_of_size_predicate i in
  {
    Ast.threshold_name = Loc.annot_with_dummy name;
    threshold_value =
      [
        Loc.annot_with_dummy (Some "true"); Loc.annot_with_dummy (Some "false");
      ];
    threshold = i;
  }

let _ = make_size_predicate_site

let annotate_dropped_size_predicates _sign _ast_counters ra _arity _agent_name
    _aux =
  (*let ra_counters = Array.make arity None in
    let _ =
      List.fold_left
        (fun pset c ->
          let port_name = c.Ast.counter_name in
          let port_id = Signature.num_of_site ~agent_name port_name sign in
          let () =
            match Signature.counter_of_site_id port_id sign with
            | None -> LKappa.raise_counter_misused agent_name c.Ast.counter_name
            | Some _ -> ()
          in
          let pset' = Mods.IntSet.add port_id pset in
          let () =
            if pset == pset' then
              LKappa.raise_several_occurence_of_site agent_name c.Ast.counter_name
          in
          let () = raise_if_modification c.Ast.counter_delta in
          let () =
            match aux with
            | Some f -> f port_id
            | None -> ()
          in
          ra_counters.(port_id) <- Some (c, LKappa.Erased);
          pset')
        Mods.IntSet.empty ast_counters
    in*)
  { agent = ra (*; counters = ra_counters *) }

let annotate_edit_size_predicates _sigs ((_agent_name, _) as _agent_type)
    _counters ra _add_link_contact_map =
  (* let ag_id = Signature.num_of_agent agent_type sigs in
     let sign = Signature.get sigs ag_id in
     let arity = Signature.arity sigs ag_id in
     let ra_counters = Array.make arity None in
     let register_counter_modif c_id =
       let counter_agent_info = Signature.get_counter_agent_info sigs in
       let port_b = fst counter_agent_info.ports in
       add_link_contact_map ag_id c_id counter_agent_info.id port_b
     in
     let _ =
       List.fold_left
         (fun pset c ->
           let port_name = c.Ast.counter_name in
           let port_id = Signature.num_of_site ~agent_name port_name sign in
           let () =
             match Signature.counter_of_site_id port_id sign with
             | None -> LKappa.raise_counter_misused agent_name c.Ast.counter_name
             | Some _ -> ()
           in
           let pset' = Mods.IntSet.add port_id pset in
           let () =
             if pset == pset' then
               LKappa.raise_several_occurence_of_site agent_name c.Ast.counter_name
           in
           let () = register_counter_modif port_id in
           let () = ra_counters.(port_id) <- Some (c, LKappa.Maintained) in
           pset')
         Mods.IntSet.empty counters
     in*)
  { agent = ra (*; counters = ra_counters *) }

let annotate_size_predicates_with_diff _sigs
    ((_agent_name, _loc) as _agent_type) _lc _rc ra _add_link_contact_map =
  (*let ag_id = Signature.num_of_agent agent_type sigs in
    let sign = Signature.get sigs ag_id in
    let arity = Signature.arity sigs ag_id in
    let register_counter_modif c c_id =
      let counter_agent_info = Signature.get_counter_agent_info sigs in
      let port_b = fst counter_agent_info.ports in
      let () = add_link_contact_map ag_id c_id counter_agent_info.id port_b in
      c, LKappa.Maintained
    in
    let ra_counters = Array.make arity None in
    let rc_r, _ =
      List.fold_left
        (fun (rc, cset) c ->
          let ((na, _) as counter_name) = c.Ast.counter_name in
          let c_id = Signature.num_of_site ~agent_name counter_name sign in
          let cset' = Mods.IntSet.add c_id cset in
          let () =
            if cset == cset' then
              LKappa.raise_several_occurence_of_site agent_name counter_name
          in
          let c', rc' =
            List.partition
              (fun p -> String.compare (Loc.v p.Ast.counter_name) na = 0)
              rc
          in
          let c'' =
            match c' with
            | _ :: [] | [] -> register_counter_modif c c_id
            | _ :: _ ->
              LKappa.raise_several_occurence_of_site agent_name counter_name
          in
          let () = ra_counters.(c_id) <- Some c'' in
          rc', cset')
        (rc, Mods.IntSet.empty) lc
    in
    let _ =
      (* test if counter of rhs is in the signature *)
      List.map
        (fun c -> Signature.num_of_site ~agent_name c.Ast.counter_name sign)
        rc_r
    in
    if (not (rc = [])) && not (rc_r = []) then
      raise
        (ExceptionDefn.Internal_Error
           ("Counters in " ^ agent_name ^ " should have tests by now", loc));*)
  { agent = ra (*; counters = ra_counters*) }

let annotate_created_size_predicates _sigs ((_agent_name, _) as _agent_type)
    _counter_list _add_link_contact_map ra =
  (*let agent_id : int = Signature.num_of_agent agent_type sigs in
    let agent_signature : Signature.t = Signature.get sigs agent_id in
    let arity : int = Signature.arity sigs agent_id in
    let ra_counters : (Ast.counter * LKappa.switching) option array =
      Array.make arity None
    in

    (* register all counters (specified or not) with default value *)
    Array.iteri
      (fun port_id _ ->
        match Signature.counter_of_site_id port_id agent_signature with
        | Some counter_info ->
          let counter_name = Signature.site_of_num port_id agent_signature in
          (try
             (* find counter matching port *)
             let c : Ast.counter =
               List.find
                 (fun c' ->
                   String.compare (Loc.v c'.Ast.counter_name) counter_name = 0)
                 counter_list
             in
             ra_counters.(port_id) <-
               Some
                 ( {
                     Ast.counter_name = c.Ast.counter_name;
                     Ast.counter_test = c.Ast.counter_test;
                     Ast.counter_delta = 0, Loc.dummy;
                   },
                   LKappa.Maintained )
           with Not_found ->
             ra_counters.(port_id) <-
               Some
                 ( {
                     Ast.counter_name = counter_name |> Loc.annot_with_dummy;
                     Ast.counter_test =
                       Some
                         (Ast.CEQ counter_info.counter_default_value
                        |> Loc.annot_with_dummy);
                     Ast.counter_delta = 0, Loc.dummy;
                   },
                   LKappa.Maintained ))
        | None -> ())
      ra_counters;

    let register_size_predicate_modif c_id =
     (** let counter_agent_info = Signature.get_counter_agent_info sigs in
      let port_b = fst counter_agent_info.ports in
      add_link_contact_map agent_id c_id counter_agent_info.id port_b
    in
    let _ : Mods.IntSet.t =
      List.fold_left
        (fun pset c ->
          let port_name = c.Ast.counter_name in
          let port_id =
            Signature.num_of_site ~agent_name port_name agent_signature
          in
          match Signature.counter_of_site_id port_id agent_signature with
          | None -> LKappa.raise_counter_misused agent_name c.Ast.counter_name
          | Some _ ->
            ();
            let pset' = Mods.IntSet.add port_id pset in
            if pset == pset' then
              LKappa.raise_several_occurence_of_site agent_name c.Ast.counter_name;
            register_counter_modif port_id;
            ra_counters.(port_id) <- Some (c, LKappa.Maintained);
            pset')
        Mods.IntSet.empty counter_list
    in*) *)
  { agent = ra (*; counters = ra_counters *) }
