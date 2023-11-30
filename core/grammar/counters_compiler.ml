(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type 'a rule_agent_counters = {
  ra: 'a;
  ra_counters: (Ast.counter * LKappa.switching) option array;
}

let combinations ls1 ls2 =
  if ls1 = [] then
    List.fold_left (fun acc (b, ds) -> ([ b ], ds) :: acc) [] ls2
  else
    List.fold_left
      (fun acc (a, cs) ->
        List.fold_left (fun acc' (b, ds) -> (b :: a, ds @ cs) :: acc') acc ls2)
      [] ls1

let update_rate counters (k, a) =
  let update_id s k =
    let a, _ =
      List.partition (fun (s', _) -> String.compare s s' = 0) counters
    in
    match a with
    | [ (_, x) ] -> Alg_expr.CONST (Nbr.I x)
    | [] -> k
    | _ :: _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Counter variable " ^ s ^ " appears twice in rule", Locality.dummy))
  in
  let rec update_bool k =
    match k with
    | Alg_expr.TRUE | Alg_expr.FALSE -> k
    | Alg_expr.BIN_BOOL_OP (op, (k1, a1), (k2, a2)) ->
      Alg_expr.BIN_BOOL_OP (op, (update_bool k1, a1), (update_bool k2, a2))
    | Alg_expr.UN_BOOL_OP (op, (k, a)) ->
      Alg_expr.UN_BOOL_OP (op, (update_bool k, a))
    | Alg_expr.COMPARE_OP (op, (k1, a1), (k2, a2)) ->
      Alg_expr.COMPARE_OP (op, (update_expr k1, a1), (update_expr k2, a2))
  and update_expr k =
    match k with
    | Alg_expr.BIN_ALG_OP (op, (k1, a1), (k2, a2)) ->
      Alg_expr.BIN_ALG_OP (op, (update_expr k1, a1), (update_expr k2, a2))
    | Alg_expr.UN_ALG_OP (op, (k1, a1)) ->
      Alg_expr.UN_ALG_OP (op, (update_expr k1, a1))
    | Alg_expr.IF ((k1, a1), (k2, a2), (k3, a3)) ->
      Alg_expr.IF
        ((update_bool k1, a1), (update_expr k2, a2), (update_expr k3, a3))
    | Alg_expr.DIFF_TOKEN ((k1, a1), k2) ->
      Alg_expr.DIFF_TOKEN ((update_expr k1, a1), k2)
    | Alg_expr.DIFF_KAPPA_INSTANCE ((k, a), m) ->
      Alg_expr.DIFF_KAPPA_INSTANCE ((update_expr k, a), m)
    | Alg_expr.ALG_VAR id | Alg_expr.TOKEN_ID id -> update_id id k
    | Alg_expr.STATE_ALG_OP _ | Alg_expr.CONST _ | Alg_expr.KAPPA_INSTANCE _ ->
      k
  in
  update_expr k, a

let collect_ids expr_list expr2_list =
  let rec aux_expr expr acc =
    match expr with
    | Alg_expr.BIN_ALG_OP (_, (k1, _), (k2, _)) -> aux_expr k2 (aux_expr k1 acc)
    | Alg_expr.UN_ALG_OP (_, (k1, _))
    | Alg_expr.DIFF_TOKEN ((k1, _), _)
    | Alg_expr.DIFF_KAPPA_INSTANCE ((k1, _), _) ->
      aux_expr k1 acc
    | Alg_expr.IF ((k1, _), (k2, _), (k3, _)) ->
      aux_expr k3 (aux_expr k2 (aux_bool k1 acc))
    | Alg_expr.ALG_VAR id | Alg_expr.TOKEN_ID id -> aux_id id acc
    | Alg_expr.STATE_ALG_OP _ | Alg_expr.CONST _ | Alg_expr.KAPPA_INSTANCE _ ->
      acc
  and aux_id id acc = Mods.StringSet.add id acc
  and aux_bool expr acc =
    match expr with
    | Alg_expr.TRUE | Alg_expr.FALSE -> acc
    | Alg_expr.BIN_BOOL_OP (_, (k1, _), (k2, _)) ->
      aux_bool k2 (aux_bool k1 acc)
    | Alg_expr.UN_BOOL_OP (_, (k, _)) -> aux_bool k acc
    | Alg_expr.COMPARE_OP (_, (k1, _), (k2, _)) -> aux_expr k2 (aux_expr k1 acc)
  in
  List.fold_left
    (fun acc expr2_opt ->
      match expr2_opt with
      | None -> acc
      | Some ((expr1, _), None) -> aux_expr expr1 acc
      | Some ((expr1, _), Some (expr2, _)) ->
        aux_expr expr2 (aux_expr expr1 acc))
    (List.fold_left
       (fun acc expr_opt ->
         match expr_opt with
         | None -> acc
         | Some (expr, _) -> aux_expr expr acc)
       Mods.StringSet.empty expr_list)
    expr2_list

let name_match (s, _) (s', _) = String.compare s s' = 0

let prepare_agent rsites lsites =
  let rec prepare_site sites c =
    match sites with
    | [] -> [ Ast.Counter c ]
    | hd :: tl ->
      (match hd with
      | Ast.Counter c' when name_match c.Ast.count_nme c'.Ast.count_nme ->
        Ast.Counter { c' with Ast.count_delta = c.Ast.count_delta } :: tl
      | Ast.Counter _ | Ast.Port _ -> hd :: prepare_site tl c)
  in
  let counters =
    List.fold_left
      (fun acc' rsite ->
        match rsite with
        | Ast.Port _ -> acc'
        | Ast.Counter c -> c :: acc')
      [] rsites
  in
  List.fold_left prepare_site lsites counters

(* - add in the lhs : (i) counters only mentioned in the rhs and (ii) the deltas
   - syntactic checks of no test in rhs; no modif in lhs *)
let prepare_counters rules =
  let syntax sites f error =
    List.iter
      (function
        | Ast.Port _ -> ()
        | Ast.Counter c ->
          if f c then
            raise
              (ExceptionDefn.Malformed_Decl
                 ("Counter " ^ fst c.Ast.count_nme ^ error, snd c.Ast.count_nme)))
      sites
  in

  let rec fold rhs lhs =
    match rhs, lhs with
    | Ast.Present (rna, rsites, _) :: r, Ast.Present (lna, lsites, b) :: l ->
      let () =
        syntax lsites
          (fun c -> not (fst c.Ast.count_delta = 0))
          " has a modif in the lhs";
        syntax rsites
          (fun c -> not (c.Ast.count_test = None))
          " has a test in the rhs"
      in
      if String.compare (fst rna) (fst lna) = 0 then (
        let lsites' = prepare_agent rsites lsites in
        Ast.Present (lna, lsites', b) :: fold r l
      ) else
        lhs
      (*TODO we stop our job here. LKappa_compiler will detect
        later that there is a problem *)
    | _ :: r, (Ast.Absent _ as lagent) :: l ->
      (*created agent*)
      (* TODO Maybe some syntax check on rhs are necessary here *)
      lagent :: fold r l
    | Ast.Absent _ :: r, (Ast.Present (_, lsites, _) as lagent) :: l ->
      (*deleted  agent*)
      let () =
        syntax lsites
          (fun c -> not (fst c.Ast.count_delta = 0))
          " has a modif in the lhs"
      in
      lagent :: fold r l
    | [], x ->
      x
      (* TODO x must be [] but it is for now LKappa_compiler
         duty to complain *)
    | _x, [] -> (*TODO let () = assert (_x = []) in*) []
  in

  let aux r =
    match r.Ast.rewrite with
    | Ast.Edit _ -> r
    | Ast.Arrow a ->
      {
        r with
        Ast.rewrite =
          Ast.Arrow
            {
              a with
              Ast.lhs =
                [ fold (List.flatten a.Ast.rhs) (List.flatten a.Ast.lhs) ];
            };
      }
  in
  List.map (fun (s, (r, a)) -> s, (aux r, a)) rules

let counters_signature s agents =
  match
    List.find
      (function
        | Ast.Absent _ -> false
        | Ast.Present (s', _, _) -> name_match s s')
      agents
  with
  | Ast.Absent _ -> assert false
  | Ast.Present (_, sites', _) ->
    List.fold_left
      (fun acc s ->
        match s with
        | Ast.Counter c -> c :: acc
        | Ast.Port _ -> acc)
      [] sites'

(* c': counter declaration, returns counter in rule*)
let enumerate_counter_tests x a ((delta, _) as count_delta) c' =
  let max, _ = c'.Ast.count_delta in
  let min =
    match c'.Ast.count_test with
    | None | Some (Ast.CGTE _, _) | Some (Ast.CVAR _, _) ->
      raise
        (ExceptionDefn.Malformed_Decl
           ( "Invalid counter signature - have to specify min bound",
             snd c'.Ast.count_nme ))
    | Some (Ast.CEQ min, _) -> min
  in

  let rec enum v =
    if v > max then
      []
    else if v + delta <= max && v + delta >= 0 then
      ( Ast.Counter
          {
            Ast.count_nme = c'.Ast.count_nme;
            count_test = Some (Ast.CEQ v, a);
            count_delta;
          },
        [ x, v ] )
      :: enum (v + 1)
    else
      enum (v + 1)
  in
  enum min

let enumerate rules f =
  List.rev
    (List.fold_left
       (fun acc (s, ((rc, _) as r)) ->
         let enumerate_r =
           if
             match rc.Ast.rewrite with
             | Ast.Edit _ -> false
             | Ast.Arrow a -> a.Ast.lhs = []
           then
             [ None, r ]
           else
             List.map
               (fun (s', r') ->
                 match s, s' with
                 | None, _ -> None, r'
                 | Some _, None -> s, r'
                 | Some (s1, a1), Some s2 -> Some (s1 ^ "__" ^ s2, a1), r')
               (f r)
         in
         enumerate_r @ acc)
       [] rules)

let remove_variable_in_counters ~warning rules signatures =
  let counter_gte_delta c delta =
    let count_delta =
      { c with Ast.count_test = Some (Ast.CGTE (abs delta), Locality.dummy) }
    in
    [ Ast.Counter count_delta, [] ]
  in
  let counter_gte_zero c =
    [
      ( Ast.Counter { c with Ast.count_test = Some (Ast.CGTE 0, Locality.dummy) },
        [] );
    ]
  in

  let remove_var_site ids counters = function
    | Ast.Port p -> [ Ast.Port p, [] ]
    | Ast.Counter c ->
      let delta, _ = c.Ast.count_delta in
      (match c.Ast.count_test with
      | Some (Ast.CEQ v, _) ->
        if delta > 0 || abs delta <= v then
          [ Ast.Counter c, [] ]
        else
          raise
            (ExceptionDefn.Malformed_Decl
               ( "Counter " ^ fst c.Ast.count_nme ^ " becomes negative",
                 snd c.Ast.count_nme ))
      | Some (Ast.CGTE v, pos) ->
        let () =
          if v + delta < 0 then
            raise
              (ExceptionDefn.Malformed_Decl
                 ( "Counter " ^ fst c.Ast.count_nme ^ " becomes negative",
                   snd c.Ast.count_nme ))
        in
        let () =
          if v = 0 then (
            let error = "Counter " ^ fst c.Ast.count_nme ^ ":>0 always holds" in
            warning ~pos (fun f -> Format.pp_print_string f error)
          )
        in
        [ Ast.Counter c, [] ]
      | Some (Ast.CVAR x, a) when Mods.StringSet.mem x ids ->
        enumerate_counter_tests x a c.Ast.count_delta
          (List.find
             (fun c' -> name_match c.Ast.count_nme c'.Ast.count_nme)
             counters)
      | None | Some (Ast.CVAR _, _) ->
        if delta < 0 then
          counter_gte_delta c delta
        else
          counter_gte_zero c)
  in
  let rec remove_var_sites ids counters = function
    | [] -> []
    | s :: t ->
      combinations
        (remove_var_sites ids counters t)
        (remove_var_site ids counters s)
  in
  let remove_var_agent ids = function
    | Ast.Absent l -> [ Ast.Absent l, [] ]
    | Ast.Present (s, sites, m) ->
      let counters = counters_signature s signatures in
      let enumerate_sites = remove_var_sites ids counters sites in
      List.map
        (fun (sites', c) -> Ast.Present (s, sites', m), c)
        enumerate_sites
  in
  let rec remove_var_mixture ids = function
    | [] -> []
    | ag :: t ->
      combinations (remove_var_mixture ids t) (remove_var_agent ids ag)
  in

  let update_opt_rate counters = function
    | None -> None
    | Some r -> Some (update_rate counters r)
  in
  let update_pair_rate counters = function
    | None -> None
    | Some (r1, r2) ->
      Some (update_rate counters r1, update_opt_rate counters r2)
  in

  let remove_var_rule (r, a) =
    let mix =
      match r.Ast.rewrite with
      | Ast.Edit r -> r.Ast.mix
      | Ast.Arrow r -> r.Ast.lhs
    in
    let ids =
      collect_ids [ Some r.Ast.k_def; r.Ast.k_op ] [ r.Ast.k_un; r.Ast.k_op_un ]
    in
    List.map
      (fun (lhs, counters) ->
        let k_def = update_rate counters r.Ast.k_def in
        let k_un = update_pair_rate counters r.Ast.k_un in
        let k_op = update_opt_rate counters r.Ast.k_op in
        let k_op_un = update_pair_rate counters r.Ast.k_op_un in
        let lhs = [ lhs ] in
        let append =
          if counters = [] then
            None
          else
            Some
              (List.fold_left
                 (fun acc (_, i) -> string_of_int i ^ acc)
                 "" counters)
        in
        ( append,
          ( {
              Ast.rewrite =
                (match r.Ast.rewrite with
                | Ast.Edit e -> Ast.Edit { e with Ast.mix = lhs }
                | Ast.Arrow a -> Ast.Arrow { a with Ast.lhs });
              Ast.bidirectional = r.Ast.bidirectional;
              Ast.k_def;
              Ast.k_un;
              Ast.k_op;
              Ast.k_op_un;
            },
            a ) ))
      (remove_var_mixture ids (List.flatten mix))
  in
  let rules = prepare_counters rules in

  enumerate rules remove_var_rule

let with_counters c =
  let with_counters_mix mix =
    List.exists
      (function
        | Ast.Absent _ -> false
        | Ast.Present (_, ls, _) ->
          List.exists
            (function
              | Ast.Counter _ -> true
              | Ast.Port _ -> false)
            ls)
      mix
  in
  with_counters_mix c.Ast.signatures

let compile ~warning ~debugMode c =
  if with_counters c then (
    let rules =
      remove_variable_in_counters ~warning c.Ast.rules c.Ast.signatures
    in
    let () =
      if debugMode then (
        let () = Format.printf "@.ast rules@." in
        List.iter
          (fun (s, (r, _)) ->
            let label =
              match s with
              | None -> ""
              | Some (l, _) -> l
            in
            Format.printf "@.%s = %a" label Ast.print_ast_rule r)
          rules
      )
    in
    { c with Ast.rules }, true
  ) else
    c, false

let make_counter_agent sigs (first, (dst, ra_erased)) (last, equal) i j pos
    created =
  let ra_type, arity, incr_b, incr_a = Signature.incr_agent sigs in
  let ra_ports = Array.make arity ((LKappa.LNK_FREE, pos), LKappa.Maintained) in
  let before_switch =
    if first && created then
      LKappa.Linked i
    else
      LKappa.Maintained
  in
  let before =
    if first then
      LKappa.LNK_VALUE (i, dst), pos
    else
      LKappa.LNK_VALUE (i, (ra_type, incr_a)), pos
  in
  let () = ra_ports.(incr_b) <- before, before_switch in
  let after =
    if last && equal then
      LKappa.LNK_FREE, pos
    else if last then
      LKappa.LNK_ANY, pos
    else
      LKappa.LNK_VALUE (j, (ra_type, incr_b)), pos
  in
  let () = ra_ports.(incr_a) <- after, LKappa.Maintained in
  let ra_ints = Array.make arity LKappa.I_ANY in
  {
    LKappa.ra_type;
    ra_erased;
    ra_ports;
    ra_ints;
    ra_syntax = Some (Array.copy ra_ports, Array.copy ra_ints);
  }

let raw_counter_agent (first, first_lnk) (last, last_lnk) i j sigs equal =
  let incr_type, arity, incr_b, incr_a = Signature.incr_agent sigs in
  let ports = Array.make arity Raw_mixture.FREE in
  let internals =
    Array.init arity (fun i ->
        Signature.default_internal_state incr_type i sigs)
  in
  let before =
    if first then
      Raw_mixture.VAL first_lnk
    else
      Raw_mixture.VAL i
  in
  let () = ports.(incr_b) <- before in
  let after =
    if last && equal then
      Raw_mixture.FREE
    else if last then
      Raw_mixture.VAL last_lnk
    else
      Raw_mixture.VAL j
  in
  let () = ports.(incr_a) <- after in
  {
    Raw_mixture.a_type = incr_type;
    Raw_mixture.a_ports = ports;
    Raw_mixture.a_ints = internals;
  }

let rec add_incr i first_lnk last_lnk delta equal sigs =
  if i = delta then
    []
  else (
    let first = i = 0 in
    let last = i = delta - 1 in
    let raw_incr =
      raw_counter_agent (first, first_lnk) (last, last_lnk) (first_lnk + i)
        (first_lnk + i + 1)
        sigs equal
    in
    raw_incr :: add_incr (i + 1) first_lnk last_lnk delta equal sigs
  )

let rec link_incr sigs i nb ag_info equal lnk pos delta =
  if i = nb then
    []
  else (
    let first = i = 0 in
    let last = i = nb - 1 in
    let ra_agent =
      make_counter_agent sigs (first, ag_info) (last, equal) (lnk + i)
        (lnk + i + 1)
        pos (delta > 0)
    in
    ra_agent :: link_incr sigs (i + 1) nb ag_info equal lnk pos delta
  )

let rec erase_incr sigs i incrs delta lnk =
  let _, _, incr_b, _ = Signature.incr_agent sigs in
  match incrs with
  | hd :: tl ->
    if i = abs delta then (
      let before, _ = hd.LKappa.ra_ports.(incr_b) in
      let () = hd.LKappa.ra_ports.(incr_b) <- before, LKappa.Linked lnk in
      hd :: tl
    ) else (
      let () =
        Array.iteri
          (fun i (a, _) -> hd.LKappa.ra_ports.(i) <- a, LKappa.Erased)
          hd.LKappa.ra_ports
      in
      let ag = { hd with LKappa.ra_erased = true } in
      ag :: erase_incr sigs (i + 1) tl delta lnk
    )
  | [] -> []

let counter_becomes_port sigs ra p_id (delta, pos') pos equal test start_lnk_nb
    =
  let incr_type, _, incr_b, _ = Signature.incr_agent sigs in
  let start_lnk_for_created = start_lnk_nb + test + 1 in
  let lnk_for_erased = start_lnk_nb + abs delta in
  let ag_info = (p_id, ra.LKappa.ra_type), ra.LKappa.ra_erased in

  let test_incr =
    link_incr sigs 0 (test + 1) ag_info equal start_lnk_nb pos delta
  in
  let adjust_delta =
    if delta < 0 then
      erase_incr sigs 0 test_incr delta lnk_for_erased
    else
      test_incr
  in
  let created =
    if delta > 0 then
      add_incr 0 start_lnk_for_created start_lnk_nb delta false sigs
    else
      []
  in

  let () =
    if test + delta < 0 then
      raise
        (ExceptionDefn.Internal_Error
           ("Counter test should be greater then abs(delta)", pos'))
  in
  let switch =
    if delta = 0 then
      LKappa.Maintained
    else if delta > 0 then
      LKappa.Linked start_lnk_for_created
    else
      LKappa.Linked lnk_for_erased
  in
  let p = (LKappa.LNK_VALUE (start_lnk_nb, (incr_b, incr_type)), pos), switch in
  let () = ra.LKappa.ra_ports.(p_id) <- p in
  adjust_delta, created

let pos_part i =
  if i < 0 then
    0
  else
    i

(* ag - agent with counters in a rule
   lnk_nb - the max link number used in the rule;
   incr_info - info on the incr agent from the signature
   returns: agent with explicit counters; created incr agents;
            the next link number to use *)
let remove_counter_agent sigs ag lnk_nb =
  let incrs, lnk_nb' =
    Tools.array_fold_lefti
      (fun id (acc_incrs, lnk_nb) -> function
        | None -> acc_incrs, lnk_nb
        | Some (counter, _) ->
          let s, pos = counter.Ast.count_nme in
          (match counter.Ast.count_test, counter.Ast.count_delta with
          | None, _ ->
            raise
              (ExceptionDefn.Internal_Error
                 ("Counter " ^ s ^ " should have a test by now", pos))
          | Some (test, pos'), delta ->
            (match test with
            | Ast.CEQ j ->
              ( counter_becomes_port sigs ag.ra id delta pos true j lnk_nb
                :: acc_incrs,
                lnk_nb + 1 + j + pos_part (fst delta) )
              (* JF: link ids were colliding after counter decrementations -> I do not think that we should add delta when negative *)
            | Ast.CGTE j ->
              ( counter_becomes_port sigs ag.ra id delta pos false j lnk_nb
                :: acc_incrs,
                lnk_nb + 1 + j + pos_part (fst delta) )
              (* JF: link ids were colliding after counter decrementations -> I do not think that we should add delta when negative *)
            | Ast.CVAR _ ->
              raise
                (ExceptionDefn.Internal_Error
                   ("Counter " ^ s ^ " should not have a var by now", pos')))))
      ([], lnk_nb) ag.ra_counters
  in
  let als, bls =
    List.fold_left (fun (als, bls) (a, b) -> a @ als, b @ bls) ([], []) incrs
  in
  als, bls, lnk_nb'

let remove_counter_created_agent sigs ag lnk_nb =
  let raw_ag = ag.ra in
  let ports = raw_ag.Raw_mixture.a_ports in
  Tools.array_fold_lefti
    (fun p_id (acc, lnk) -> function
      | None -> acc, lnk
      | Some (c, _) ->
        (match c.Ast.count_test with
        | None ->
          let agent_name =
            Format.asprintf "@[%a@]"
              (Signature.print_agent sigs)
              raw_ag.Raw_mixture.a_type
          in
          LKappa.not_enough_specified ~status:"counter" ~side:"left" agent_name
            c.Ast.count_nme
        | Some (test, _) ->
          (match test with
          | Ast.CEQ j ->
            let p = Raw_mixture.VAL lnk in
            let () = ports.(p_id) <- p in
            let incrs = add_incr 0 lnk (lnk + j) (j + 1) true sigs in
            acc @ incrs, lnk + j + 1
          | Ast.CGTE _ | Ast.CVAR _ ->
            let agent_name =
              Format.asprintf "@[%a@]"
                (Signature.print_agent sigs)
                raw_ag.Raw_mixture.a_type
            in
            LKappa.not_enough_specified ~status:"counter" ~side:"left"
              agent_name c.Ast.count_nme)))
    ([], lnk_nb) ag.ra_counters

let raw_agent_with_counters ag =
  Array.fold_left (fun ok x -> x <> None || ok) false ag.ra_counters

let agent_with_counters ag sigs =
  let sign = Signature.get sigs ag.LKappa.ra_type in
  Signature.has_counter sign

(* - adds increment agents to the rule_agent mixture
   - adds increment agents to the raw mixture
   - links the agents in the mixture(lhs,rhs,mix) or in the raw mixture(created)
     to the increments *)
let remove_counter_rule sigs mix created =
  let with_counters =
    List.exists (fun ag -> agent_with_counters ag.ra sigs) mix
    || List.exists (fun ag -> raw_agent_with_counters ag) created
  in
  if with_counters then (
    let lnk_nb =
      List.fold_left (fun m ag -> max m (LKappa.max_link_id [ ag.ra ])) 0 mix
    in

    let incrs, incrs_created, lnk_nb' =
      List.fold_left
        (fun (a, b, lnk) ag ->
          let a', b', lnk' = remove_counter_agent sigs ag lnk in
          a' @ a, b' @ b, lnk' + 1)
        ([], [], lnk_nb + 1)
        mix
    in
    let incrs_created', _ =
      List.fold_left
        (fun (acc, lnk) ag ->
          let a, lnk' = remove_counter_created_agent sigs ag lnk in
          a @ acc, lnk')
        ([], lnk_nb' + 1)
        created
    in

    let rule_agent_mix = List_util.rev_map_append (fun ag -> ag.ra) mix incrs in
    let raw_mix =
      List_util.rev_map_append
        (fun ag -> ag.ra)
        created
        (incrs_created @ incrs_created')
    in
    rule_agent_mix, raw_mix
  ) else
    ( List.rev_map (fun ag -> ag.ra) (List.rev mix),
      List.rev_map (fun ag -> ag.ra) (List.rev created) )

let agent_with_max_counter sigs c ((agent_name, _) as ag_ty) =
  let incr_type, _, incr_b, _ = Signature.incr_agent sigs in
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ports =
    Array.make arity (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained)
  in
  let internals = Array.make arity LKappa.I_ANY in
  let c_na = c.Ast.count_nme in
  let c_id = Signature.num_of_site ~agent_name c_na sign in
  let max_val, pos = c.Ast.count_delta in
  let max_val' = max_val + 1 in
  let incrs =
    link_incr sigs 0 (max_val' + 1) ((c_id, ag_id), false) false 1 pos (-1)
  in
  let p = LKappa.LNK_VALUE (1, (incr_b, incr_type)), pos in
  let () = ports.(c_id) <- p, LKappa.Maintained in
  let ra =
    {
      LKappa.ra_type = ag_id;
      ra_ports = ports;
      ra_ints = internals;
      ra_erased = false;
      ra_syntax = Some (Array.copy ports, Array.copy internals);
    }
  in
  ra :: incrs

let counter_perturbation sigs c ag_ty =
  let filename =
    [ Primitives.Str_pexpr ("counter_perturbation.ka", snd c.Ast.count_nme) ]
  in
  let stop_message =
    "Counter " ^ fst c.Ast.count_nme ^ " of agent " ^ fst ag_ty
    ^ " reached maximum"
  in
  let mods =
    [
      Ast.PRINT ([], [ Primitives.Str_pexpr ("", snd c.Ast.count_nme) ]);
      Ast.PRINT
        ([], [ Primitives.Str_pexpr (stop_message, snd c.Ast.count_nme) ]);
      Ast.STOP filename;
    ]
  in
  let val_of_counter =
    Alg_expr.KAPPA_INSTANCE (agent_with_max_counter sigs c ag_ty)
  in
  let pre =
    Alg_expr.COMPARE_OP
      ( Operator.EQUAL,
        (val_of_counter, snd c.Ast.count_nme),
        (Alg_expr.CONST (Nbr.I 1), snd c.Ast.count_nme) )
  in
  None, Some (pre, snd ag_ty), mods, Some (Locality.dummy_annot Alg_expr.FALSE)

let counters_perturbations sigs ast_sigs =
  List.fold_left
    (List.fold_left (fun acc -> function
       | Ast.Absent _ -> acc
       | Ast.Present (ag_ty, sites, _) ->
         List.fold_left
           (fun acc' site ->
             match site with
             | Ast.Port _ -> acc'
             | Ast.Counter c ->
               (counter_perturbation sigs c ag_ty, snd ag_ty) :: acc')
           acc sites))
    [] ast_sigs

let make_counter i name =
  {
    Ast.count_nme = name, Locality.dummy;
    count_test = Some (Ast.CEQ i, Locality.dummy);
    count_delta = 0, Locality.dummy;
  }

let add_counter_to_contact_map sigs add_link_contact_map =
  let incr_id, _, incr_b, incr_a = Signature.incr_agent sigs in
  add_link_contact_map incr_id incr_a incr_id incr_b

let forbid_modification (delta, pos) =
  if delta != 0 then LKappa.forbid_modification pos (Some delta)

let annotate_dropped_counters sign counts ra arity agent_name aux =
  let ra_counters = Array.make arity None in
  let _ =
    List.fold_left
      (fun pset c ->
        let p_na = c.Ast.count_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let () =
          match Signature.counter_of_site p_id sign with
          | None -> LKappa.counter_misused agent_name c.Ast.count_nme
          | Some _ -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.several_occurence_of_site agent_name c.Ast.count_nme
        in
        let () = forbid_modification c.Ast.count_delta in
        let () =
          match aux with
          | Some f -> f p_id
          | None -> ()
        in
        let () = ra_counters.(p_id) <- Some (c, LKappa.Erased) in
        pset')
      Mods.IntSet.empty counts
  in
  { ra; ra_counters }

let annotate_edit_counters sigs ((agent_name, _) as ag_ty) counts ra
    add_link_contact_map =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ra_counters = Array.make arity None in
  let register_counter_modif c_id =
    let incr_id, _, incr_b, _ = Signature.incr_agent sigs in
    add_link_contact_map ag_id c_id incr_id incr_b
  in
  let _ =
    List.fold_left
      (fun pset c ->
        let p_na = c.Ast.count_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let () =
          match Signature.counter_of_site p_id sign with
          | None -> LKappa.counter_misused agent_name c.Ast.count_nme
          | Some _ -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.several_occurence_of_site agent_name c.Ast.count_nme
        in
        let () = register_counter_modif p_id in
        let () = ra_counters.(p_id) <- Some (c, LKappa.Maintained) in
        pset')
      Mods.IntSet.empty counts
  in
  { ra; ra_counters }

let annotate_counters_with_diff sigs ((agent_name, pos) as ag_ty) lc rc ra
    add_link_contact_map =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let register_counter_modif c c_id =
    let incr_id, _, incr_b, _ = Signature.incr_agent sigs in
    let () = add_link_contact_map ag_id c_id incr_id incr_b in
    c, LKappa.Maintained
  in
  let ra_counters = Array.make arity None in
  let rc_r, _ =
    List.fold_left
      (fun (rc, cset) c ->
        let ((na, _) as c_na) = c.Ast.count_nme in
        let c_id = Signature.num_of_site ~agent_name c_na sign in
        let cset' = Mods.IntSet.add c_id cset in
        let () =
          if cset == cset' then LKappa.several_occurence_of_site agent_name c_na
        in
        let c', rc' =
          List.partition
            (fun p -> String.compare (fst p.Ast.count_nme) na = 0)
            rc
        in
        let c'' =
          match c' with
          | _ :: [] | [] -> register_counter_modif c c_id
          | _ :: _ -> LKappa.several_occurence_of_site agent_name c_na
        in
        let () = ra_counters.(c_id) <- Some c'' in
        rc', cset')
      (rc, Mods.IntSet.empty) lc
  in
  let _ =
    (* test if counter of rhs is in the signature *)
    List.map
      (fun c -> Signature.num_of_site ~agent_name c.Ast.count_nme sign)
      rc_r
  in
  let () =
    if (not (rc = [])) && not (rc_r = []) then
      raise
        (ExceptionDefn.Internal_Error
           ("Counters in " ^ agent_name ^ " should have tests by now", pos))
  in
  { ra; ra_counters }

let annotate_created_counters sigs ((agent_name, _) as ag_ty) counts
    add_link_contact_map ra =
  let ag_id = Signature.num_of_agent ag_ty sigs in
  let sign = Signature.get sigs ag_id in
  let arity = Signature.arity sigs ag_id in
  let ra_counters = Array.make arity None in

  (* register all counters (specified or not) with min value *)
  let () =
    Array.iteri
      (fun p_id _ ->
        match Signature.counter_of_site p_id sign with
        | Some (min, _) ->
          let c_name = Signature.site_of_num p_id sign in
          (try
             let c =
               List.find
                 (fun c' -> String.compare (fst c'.Ast.count_nme) c_name = 0)
                 counts
             in
             ra_counters.(p_id) <-
               Some
                 ( {
                     Ast.count_nme = c.Ast.count_nme;
                     Ast.count_test = c.Ast.count_test;
                     Ast.count_delta = 0, Locality.dummy;
                   },
                   LKappa.Maintained )
           with Not_found ->
             ra_counters.(p_id) <-
               Some
                 ( {
                     Ast.count_nme = c_name, Locality.dummy;
                     Ast.count_test = Some (Ast.CEQ min, Locality.dummy);
                     Ast.count_delta = 0, Locality.dummy;
                   },
                   LKappa.Maintained ))
        | None -> ())
      ra_counters
  in

  let register_counter_modif c_id =
    let incr_id, _, incr_b, _ = Signature.incr_agent sigs in
    add_link_contact_map ag_id c_id incr_id incr_b
  in
  let _ =
    List.fold_left
      (fun pset c ->
        let p_na = c.Ast.count_nme in
        let p_id = Signature.num_of_site ~agent_name p_na sign in
        let () =
          match Signature.counter_of_site p_id sign with
          | None -> LKappa.counter_misused agent_name c.Ast.count_nme
          | Some _ -> ()
        in
        let pset' = Mods.IntSet.add p_id pset in
        let () =
          if pset == pset' then
            LKappa.several_occurence_of_site agent_name c.Ast.count_nme
        in
        let () = register_counter_modif p_id in
        let () = ra_counters.(p_id) <- Some (c, LKappa.Maintained) in
        pset')
      Mods.IntSet.empty counts
  in
  { ra; ra_counters }
