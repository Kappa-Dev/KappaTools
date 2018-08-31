(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let generate_destructed_monomers contact_map =
  Tools.array_fold_lefti
    (fun ra_type acc array ->
       let internals =
         Tools.array_fold_lefti
           (fun site_id list (internals,_links) ->
              if Mods.IntSet.is_empty internals then list
              else
                Mods.IntSet.fold
                  (fun state pack ->
                     List_util.rev_map_append
                       (fun ed ->
                          let ed' = Array.copy ed in
                          let () = ed'.(site_id) <-
                              LKappa.I_VAL_ERASED state in
                          ed')
                       list pack)
                  internals [])
           [Array.map (fun _ -> LKappa.I_ANY) array]
           array in
       let ports =
         Array.map
           (fun _ -> (Locality.dummy_annot Ast.LNK_FREE, LKappa.Erased))
           array in
       List_util.rev_map_append (fun ra_ints ->
           let ra_ports = Array.copy ports in [{
             LKappa.ra_type;
             LKappa.ra_erased = true;
             LKappa.ra_ports;
             LKappa.ra_ints;
             LKappa.ra_syntax = Some (Array.copy ra_ports, Array.copy ra_ints);
           }]) internals acc
    ) [] contact_map

let destruct_rule_of_ltoken
    contact_map preenv ~syntactic_rule ?origin tk_id mix =
  let outs,(preenv',origin') =
    Snip.connected_components_sum_of_ambiguous_rule
      ~compileModeOn:false contact_map preenv ?origin mix [] in
  let (cc,rule) =  match outs with
    | [ origin'', [|cc_id,_ as cc|], instantiations, (removed,inserted)] ->
      let () = assert (origin = origin'') in
      ((tk_id,cc),{
          Primitives.rate = Alg_expr.const (Nbr.F infinity);
          Primitives.unary_rate = None;
          Primitives.connected_components = [|cc_id|];
          Primitives.removed; Primitives.inserted;
          Primitives.delta_tokens = [Alg_expr.const Nbr.one,tk_id];
          Primitives.syntactic_rule;
          Primitives.instantiations;
        })
    | _ -> failwith "Something went wrong in destruct_rule_of_ltoken"
  in
  (preenv',origin',cc,rule)

let destruct_lrule_of_ltoken tk_id r_mix = {
  LKappa.r_mix; LKappa.r_created = [];
  LKappa.r_delta_tokens = [Alg_expr.const Nbr.one,tk_id];
  LKappa.r_rate = Alg_expr.const (Nbr.F infinity);
  LKappa.r_un_rate = None; LKappa.r_editStyle = true;
}

let reabstract =
    Primitives.Transformation.map_agent
    (fun (x,ty) -> Matching.Agent.Fresh (ty,x))

let put_transf_in_its_cc (cc_of_agent,ccs,exists) = function
  | Primitives.Transformation.Agent ag as x ->
    if Matching.Agent.is_fresh ag then
      let id = Matching.Agent.get_id ag in
      let concrete =
        Primitives.Transformation.Agent (id, Matching.Agent.get_type ag) in
      (Mods.IntMap.add id id cc_of_agent,
       Mods.IntMap.add id [concrete] ccs,
       exists)
    else (cc_of_agent,ccs,x::exists)
  | Primitives.Transformation.Freed (ag,site) as x->
    if Matching.Agent.is_fresh ag then
      let id = Matching.Agent.get_id ag in
      let cc_id = Mods.IntMap.find_default (-1) id cc_of_agent in
      if cc_id < 0 then (cc_of_agent,ccs,x::exists) else
        let l = Mods.IntMap.find_default [] cc_id ccs in
        let concrete =
          Primitives.Transformation.Freed
            ((id, Matching.Agent.get_type ag),site) in
        (cc_of_agent, Mods.IntMap.add cc_id (concrete::l) ccs,exists)
    else (cc_of_agent,ccs,x::exists)
  | Primitives.Transformation.PositiveInternalized (ag,site,state) as x ->
    if Matching.Agent.is_fresh ag then
      let id = Matching.Agent.get_id ag in
      let cc_id = Mods.IntMap.find_default (-1) id cc_of_agent in
      if cc_id < 0 then (cc_of_agent,ccs,x::exists) else
        let l = Mods.IntMap.find_default [] cc_id ccs in
        let concrete =
          Primitives.Transformation.PositiveInternalized
            ((id, Matching.Agent.get_type ag),site,state) in
        (cc_of_agent, Mods.IntMap.add cc_id (concrete::l) ccs,exists)
    else (cc_of_agent,ccs,x::exists)
  | Primitives.Transformation.Linked ((ag1,site1),(ag2,site2)) as x ->
    let id1 = Matching.Agent.get_id ag1 in
    let cc_id1 = Mods.IntMap.find_default (-1) id1 cc_of_agent in
    let id2 = Matching.Agent.get_id ag2 in
    let cc_id2 = Mods.IntMap.find_default (-1) id2 cc_of_agent in
    if Matching.Agent.is_fresh ag1 && cc_id1 >= 0 then
      if Matching.Agent.is_fresh ag2 && cc_id2 >= 0 then
        let l1 = Mods.IntMap.find_default [] cc_id1 ccs in
        let concrete =
          Primitives.Transformation.Linked
            (((id1, Matching.Agent.get_type ag1),site1),
             ((id2, Matching.Agent.get_type ag2),site2)) in
        if cc_id1 = cc_id2 then
          (cc_of_agent, Mods.IntMap.add cc_id1 (concrete::l1) ccs,exists)
        else
          let l2,ccs' = Mods.IntMap.pop cc_id2 ccs in
          let l = concrete :: Option_util.unsome [] l2 @ l1 in (* DANGER *)
          let cc_of_agent' =
            Mods.IntMap.map
              (fun cc_id -> if cc_id = cc_id2 then cc_id1 else cc_id)
              cc_of_agent in
          (cc_of_agent', Mods.IntMap.add cc_id1 l ccs',exists)
      else
        let l1,ccs' = Mods.IntMap.pop cc_id1 ccs in
        let tmp = List.rev_map reabstract (Option_util.unsome [] l1) in
        (cc_of_agent,ccs',x::List.rev_append tmp exists)
    else if Matching.Agent.is_fresh ag2 && cc_id2 >= 0 then
      let l2,ccs' = Mods.IntMap.pop cc_id2 ccs in
      let tmp = List.rev_map reabstract (Option_util.unsome [] l2) in
      (cc_of_agent,ccs',x::List.rev_append tmp exists)
    else (cc_of_agent,ccs,x::exists)
  | Primitives.Transformation.NegativeWhatEver _
  | Primitives.Transformation.NegativeInternalized _ ->
    failwith "Clearly not a positive transformation"

let recognize_token model tokens transfs =
  let pats = Evaluator.find_all_embeddings model transfs in
  List.fold_left
    (fun acc (x,_) ->
       List.fold_left
         (fun acc (tk_id,(cc_id,_)) ->
            if x = cc_id then
              let () = assert
                (match acc with None -> true | Some y -> y = tk_id) in
              Some tk_id else acc)
         acc tokens)
    None pats

let tokenify_fresh_transfs model tokens transfs =
  let (_,ccs,existings) =
    List.fold_left
      put_transf_in_its_cc (Mods.IntMap.empty, Mods.IntMap.empty, []) transfs in
  Mods.IntMap.fold (fun _ x (remains,toks) ->
      match recognize_token model tokens (List.rev x) with
      | None -> (List_util.rev_map_append reabstract x remains,toks)
      | Some tk -> (remains,tk::toks))
    ccs (List.rev existings,[])

let matchings_of_a_token model (tk_id,(_,pattern)) =
  let transfs =
    Primitives.fully_specified_pattern_to_positive_transformations pattern in
  let pats = Evaluator.find_all_embeddings model transfs in
  (tk_id,pats,transfs)

let product_alg_of_alg_product = function
  | [||] -> failwith "split_mix_in_alg invarient violation"
  | a ->
    let rec aux i acc =
      if i >= Array.length a then acc else
        Alg_expr.mult acc (Locality.dummy_annot (Alg_expr.KAPPA_INSTANCE a.(i)))
    in aux 1 (Locality.dummy_annot (Alg_expr.KAPPA_INSTANCE a.(0)))

let split_mix = function
  | [] -> failwith "split_mix_in_alg invarient violation"
  | [x] -> product_alg_of_alg_product x |> fst
  | h::t ->
    List.fold_left
      (fun acc e -> Alg_expr.add acc (product_alg_of_alg_product e))
      (product_alg_of_alg_product h) t |> fst

let rewrite_in_alg map alg =
  Alg_expr.map_on_mixture
    (fun i ->
       Pattern.Map.find_default (Alg_expr.KAPPA_INSTANCE [[|i|]]) i map)
    (Alg_expr.map_on_mixture split_mix alg)

let rewrite_in_bool map alg =
  Alg_expr.map_bool_on_mixture
    (fun i ->
       Pattern.Map.find_default (Alg_expr.KAPPA_INSTANCE [[|i|]]) i map)
    (Alg_expr.map_bool_on_mixture split_mix alg)

let replace_cc_in_rule tk_id transfs cc_pos inj rule =
  let rate =
    Alg_expr.mult
      rule.Primitives.rate
      (Locality.dummy_annot (Alg_expr.TOKEN_ID tk_id)) in
  let () =
    match rule.Primitives.unary_rate with
    | None -> ()
    | Some _ ->
      failwith "Tokenification is imcompatible with molecular ambiguity" in
  let connected_components =
    let t = rule.Primitives.connected_components in
    Array.append
      (Array.sub t 0 cc_pos)
      (Array.sub t (succ cc_pos) (Array.length t - cc_pos - 1)) in
  let delta_tokens =
    (Alg_expr.const (Nbr.I (-1)),tk_id) :: rule.Primitives.delta_tokens in
  let part_of_dropped_cc =
    Primitives.Transformation.fold_agent
      (fun b x ->
         b || match x with
         | Matching.Agent.Fresh _ -> false
         | Matching.Agent.Existing (_,i) -> i = cc_pos)
      false in
  let obselete,pre_removed =
    List.partition part_of_dropped_cc rule.Primitives.removed in
  let removed =
    List.map
      (Primitives.Transformation.map_agent
         (function
           | Matching.Agent.Fresh _ as x -> x
           | Matching.Agent.Existing (n,i) as x ->
             if i < cc_pos then x else Matching.Agent.Existing (n,pred i)))
      pre_removed in
  let concrete_obselete =
    List.rev_map
      (Primitives.Transformation.concretize
         (Option_util.unsome
            Matching.empty (Matching.add_cc Matching.empty cc_pos inj),
          Mods.IntMap.empty))
      obselete in
  let max_fresh_id =
    List.fold_left
      (Primitives.Transformation.fold_agent
         (fun m -> function
            | Matching.Agent.Fresh (_,i) -> max m i
            | Matching.Agent.Existing _ -> m))
      (-1) rule.Primitives.inserted in
  let new_transfs =
    List.filter
      (fun x -> not (List.exists
                       (Primitives.Transformation.is_the_opposite_of (=) x)
                       concrete_obselete)) transfs in
  let find_abstract_id (inj,fresh as p) i =
    match Mods.IntMap.find_option i inj with
    | Some j -> (p,j)
    | None -> ((Mods.IntMap.add i fresh inj, succ fresh),fresh) in
  let new_inserted,pack =
    List_util.fold_right_map
      (Primitives.Transformation.map_fold_agent
         (fun (id,ty) pack ->
            let (pack',id') = find_abstract_id pack id in
            Matching.Agent.Fresh (ty,id'),pack'))
       new_transfs (Mods.IntMap.empty,succ max_fresh_id) in
  let refreshed_inserted =
    List.map
      (Primitives.Transformation.map_agent
         (function
           | Matching.Agent.Existing ((id,ty as ag),cc) as x ->
             if cc < cc_pos then x
             else if cc > cc_pos then Matching.Agent.Existing (ag,pred cc)
             else Matching.Agent.Fresh (ty,snd (find_abstract_id pack id))
           | Matching.Agent.Fresh _ as x -> x))
      rule.Primitives.inserted in
  let inserted = new_inserted @ refreshed_inserted in
  {
    Primitives.rate;
    Primitives.unary_rate = rule.Primitives.unary_rate;
    Primitives.connected_components;
    Primitives.removed;
    Primitives.inserted;
    Primitives.delta_tokens;
    Primitives.syntactic_rule = rule.Primitives.syntactic_rule;
    Primitives.instantiations =
      (*TODO: super hyper wrong*) rule.Primitives.instantiations;
  }

let tokenify_fresh_inserted model tokens r =
  let (inserted,inserted_toks) =
    tokenify_fresh_transfs model tokens r.Primitives.inserted in
  let delta_tokens =
    List_util.rev_map_append
      (fun tk_id -> ((Alg_expr.const Nbr.one),tk_id))
      inserted_toks r.Primitives.delta_tokens in
  {
    Primitives.rate = r.Primitives.rate;
    Primitives.unary_rate = r.Primitives.unary_rate;
    Primitives.connected_components = r.Primitives.connected_components;
    Primitives.removed = r.Primitives.removed;
    Primitives.inserted;
    Primitives.delta_tokens;
    Primitives.syntactic_rule = r.Primitives.syntactic_rule;
    Primitives.instantiations = r.Primitives.instantiations;
  }


let new_rules_involving_a_token rule_list (tk_id,to_replace,transfs) =
  List.fold_left
    (fun acc rule ->
       let cc_to_drop =
         Tools.array_fold_lefti
           (fun i acc id ->
              let tmp = List.filter (fun (i,_) -> i=id) to_replace in
              List_util.rev_map_append (fun (_,inj) -> (i,inj)) tmp acc)
           [] rule.Primitives.connected_components in
       List_util.rev_map_append
         (fun (cc_pos,inj) -> replace_cc_in_rule tk_id transfs cc_pos inj rule)
         cc_to_drop acc)
    rule_list rule_list

let print_result conf env inputs_form init =
  let () = Format.fprintf inputs_form
      "%a@.%a@." Configuration.print conf Kappa_printer.decompiled_env env in
  let sigs = Model.signatures env in
  Format.fprintf inputs_form "@.@[<v>%a@]@."
    (Pp.list Pp.space
       (fun f (n,r) ->
          let _,ins_fresh =
            Snip.lkappa_of_elementary_rule sigs (Model.domain env) r in
          let () =
            if ins_fresh <> [] then
              Format.fprintf f "@[<h>%%init: %a %a@]"
                (Kappa_printer.alg_expr ~env) n
                (Raw_mixture.print ~created:false ~sigs)
                ins_fresh in
          Pp.list Pp.space (fun f (nb,tk) ->
              Format.fprintf f "@[<h>%%init: %a %a@]"
                (Kappa_printer.alg_expr ~env)
                (fst (Alg_expr.mult (Locality.dummy_annot n) nb))
                (Model.print_token ~env) tk)
            f r.Primitives.delta_tokens)) init

let output_result ?filename conf model init =
  let filename = match filename with
    | None -> "_tokenify.ka"
    | Some p -> p in
  let inputs = Kappa_files.open_out_fresh filename "" ".ka" in
  let inputs_form = Format.formatter_of_out_channel inputs in
  let () = print_result conf model inputs_form init in
  close_out inputs

let main () =
  let common_args = Common_args.default in
  let cli_args = Run_cli_args.default in
  let kasim_args = Kasim_args.default in
  let options = Common_args.options common_args @
                Run_cli_args.options cli_args @
                Kasim_args.options kasim_args in
  let () =
    Arg.parse
      options
      (fun fic -> cli_args.Run_cli_args.inputKappaFileNames <-
          fic::(cli_args.Run_cli_args.inputKappaFileNames))
      (Sys.argv.(0) ^ "\n Kappa source to source compiler") in
  let () = Printexc.record_backtrace common_args.Common_args.backtrace in
  let (conf,_,model,_contact_map,_,_,_,_,init),_counter =
    Cli_init.get_compilation
      ~warning:(fun ~pos msg -> Data.print_warning ~pos Format.err_formatter msg)
      ?bwd_bisim:None ~compileModeOn:false ~kasim_args cli_args in
  let (filenames,domain,tokens,algs,all_deps,
       (ast_rules,rules),obs,interventions,contact_map) =
    Model.deconstruct model in
  let sigs = Model.signatures model in
  let monomers = generate_destructed_monomers contact_map in
  let (preenv',_,_,_,ccs,destruct_rules) =
    List.fold_left
      (fun (env,syntactic_rule,origin,tk_id,ccs,rules) mono ->
         let (env',origin',cc,r) =
           destruct_rule_of_ltoken
             contact_map env ?origin ~syntactic_rule tk_id mono in
         (env',succ syntactic_rule, origin', succ tk_id,cc::ccs,r::rules))
      (Pattern.PreEnv.of_env domain,succ (Model.nb_syntactic_rules model),
       Some (Operator.RULE (Model.nb_rules model)),Model.nb_tokens model,[],[])
      monomers in
  let (destruct_lrules,_) =
    List.fold_left
      (fun (acc,tk_id) mono ->
         ((None,Locality.dummy_annot (destruct_lrule_of_ltoken tk_id mono))::acc,
          succ tk_id))
      ([],Model.nb_tokens model) monomers in
  let domain',_ = Pattern.finalize ~max_sharing:false preenv' contact_map in
  let tokens' =
    NamedDecls.create
      (Array.append
         (Array.map
            (fun (x,y) -> (Locality.dummy_annot x,y))
            tokens.NamedDecls.decls)
         (Tools.array_rev_map_of_list
            (fun (_,(_,cc)) ->
               (Locality.dummy_annot
                  (Format.asprintf "%a" (Pattern.print_cc_as_id sigs) cc),()))
            ccs)) in
  let to_be_rewritten = List.rev_map (matchings_of_a_token model) ccs in
  let patterns_to_rewrite =
  List.fold_left
    (fun set (tk,repl,_) ->
       List.fold_left
         (fun set (id,_) ->
            let v = Pattern.Map.find_default
                (Alg_expr.KAPPA_INSTANCE [[|id|]]) id set in
            let out,_ =
              Alg_expr.add
                (Locality.dummy_annot v)
                (Locality.dummy_annot (Alg_expr.TOKEN_ID tk)) in
            Pattern.Map.add id out set)
         set repl)
    Pattern.Map.empty to_be_rewritten in
  let obs' = Array.map (fun v -> rewrite_in_alg patterns_to_rewrite v) obs in
  let algs' =
    NamedDecls.mapi (fun _ _ v -> rewrite_in_alg patterns_to_rewrite v) algs in
  let rules' = Array.fold_right
      (fun v acc ->
         Primitives.map_expr_rule (rewrite_in_alg patterns_to_rewrite) v :: acc)
      rules [] in
  let interventions' = Array.map
      (fun v -> Primitives.map_expr_perturbation
          (rewrite_in_alg patterns_to_rewrite)
          (rewrite_in_bool patterns_to_rewrite) v)
      interventions in
  (*TODO extract itself*)
  let ast_rules'' =
    Array.append ast_rules (Tools.array_rev_of_list destruct_lrules) in
  let exploded_rules =
    List.fold_left new_rules_involving_a_token rules' to_be_rewritten in
let rules'' = Array.append
    (Tools.array_rev_of_list destruct_rules)
    (Tools.array_map_of_list
       (tokenify_fresh_inserted (Model.new_domain domain' model) ccs)
       exploded_rules) in
  let env =
    Model.init ~filenames domain' tokens' algs' all_deps
      (ast_rules'',rules'') obs' interventions' contact_map in
  let init' =
    List.rev_map (fun (a,r) -> (a,tokenify_fresh_inserted env ccs r)) init in
  let () = output_result
      ?filename:cli_args.Run_cli_args.outputDataFile
      conf env init' in
  ()

let () = main ()
