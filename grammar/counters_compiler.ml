(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let combinations ls1 ls2 =
  if (ls1 = []) then List.fold_left (fun acc (b,ds) -> ([b],ds)::acc) [] ls2
  else
    List.fold_left
      (fun acc (a,cs) ->
        List.fold_left (fun acc' (b,ds) -> ((b::a),ds@cs)::acc') acc ls2)
      [] ls1

let update_rate counters (k,a) =
  let update_id s k =
    let (a,_) =
      List.partition (fun (s',_) -> (String.compare s s') = 0) counters in
    match a with
    | [(_,x)] -> Alg_expr.CONST (Nbr.I x)
    | [] -> k
    | _::_ -> raise (ExceptionDefn.Malformed_Decl
                       ("Counter variable "^s^" appears twice in rule",
                        Locality.dummy)) in
  let rec update_bool k = match k with
    | Alg_expr.TRUE | Alg_expr.FALSE -> k
    | Alg_expr.BIN_BOOL_OP (op,(k1,a1),(k2,a2)) ->
       Alg_expr.BIN_BOOL_OP (op,((update_bool k1),a1),((update_bool k2),a2))
    | Alg_expr.UN_BOOL_OP (op,(k,a)) ->
      Alg_expr.UN_BOOL_OP (op,(update_bool k,a))
    | Alg_expr.COMPARE_OP (op,(k1,a1),(k2,a2)) ->
      Alg_expr.COMPARE_OP (op,((update_expr k1),a1),((update_expr k2),a2))
  and update_expr k = match k with
    | Alg_expr.BIN_ALG_OP (op,(k1,a1),(k2,a2)) ->
       Alg_expr.BIN_ALG_OP (op,((update_expr k1),a1),((update_expr k2),a2))
    | Alg_expr.UN_ALG_OP (op,(k1,a1)) ->
       Alg_expr.UN_ALG_OP (op,((update_expr k1),a1))
    | Alg_expr.IF ((k1,a1),(k2,a2),(k3,a3)) ->
       Alg_expr.IF
         (((update_bool k1),a1),((update_expr k2),a2),((update_expr k3),a3))
    | Alg_expr.DIFF_TOKEN ((k1,a1),k2) ->
       Alg_expr.DIFF_TOKEN (((update_expr k1),a1),k2)
    | Alg_expr.DIFF_KAPPA_INSTANCE ((k,a),m) ->
       Alg_expr.DIFF_KAPPA_INSTANCE (((update_expr k),a),m)
    | Alg_expr.ALG_VAR id| Alg_expr.TOKEN_ID id -> update_id id k
    | Alg_expr.STATE_ALG_OP _| Alg_expr.CONST _| Alg_expr.KAPPA_INSTANCE _ -> k
  in
  ((update_expr k),a)

let name_match (s,_) (s',_) = (String.compare s s') = 0

let prepare_agent rsites lsites =
  let rec prepare_site sites c =
    match sites with
    | [] -> [Ast.Counter c]
    | hd::tl ->
       match hd with
         Ast.Counter c' when (name_match c.Ast.count_nme c'.Ast.count_nme) ->
         (Ast.Counter {c' with Ast.count_delta = c.Ast.count_delta})::tl
       | Ast.Counter _ | Ast.Port _ -> hd::(prepare_site tl c) in
  let counters =
    List.fold_left
      (fun acc' rsite ->
        match rsite with Ast.Port _ -> acc' | Ast.Counter c -> c::acc')
      [] rsites in
  List.fold_left prepare_site lsites counters

(* - add in the lhs : (i) counters only mentioned in the rhs and (ii) the deltas
   - syntactic checks of no test in rhs; no modif in lhs *)
let prepare_counters rules =
  let syntax sites f error =
    List.iter
      (function Ast.Port _ -> ()
              | Ast.Counter c ->
                if (f c) then
                  raise (ExceptionDefn.Malformed_Decl
                           ("Counter "^(fst c.Ast.count_nme)^error,
                            (snd c.Ast.count_nme)))) sites in

  let rec fold rhs lhs = match rhs,lhs with
    | Ast.Present (rna,rsites,_)::r, Ast.Present (lna,lsites,b)::l->
      let () = syntax lsites (fun c -> not((fst c.Ast.count_delta)=0))
          " has a modif in the lhs";
        syntax rsites (fun c -> not(c.Ast.count_test=None))
          " has a test in the rhs" in
      if String.compare (fst rna) (fst lna) = 0 then
        let lsites' = prepare_agent rsites lsites in
        Ast.Present (lna,lsites',b)::(fold r l)
      else lhs (*TODO we stop our job here. LKappa_compiler will detect
                 later that there is a problem *)
    | _::r, (Ast.Absent _ as lagent)::l ->
      (*created agent*)
      (* TODO Maybe some syntax check on rhs are necessary here *)
      lagent::fold r l
    | Ast.Absent _::r, (Ast.Present (_,lsites,_) as lagent)::l ->
      (*deleted  agent*)
      let () = syntax lsites (fun c -> not((fst c.Ast.count_delta)=0))
          " has a modif in the lhs" in
      lagent::fold r l
    | [], x -> x (* TODO x must be [] but it is for now LKappa_compiler
                    duty to complain *)
    | _x, [] -> (*TODO let () = assert (_x = []) in*) [] in

  let aux r = match r.Ast.rewrite with
    | Ast.Edit _ -> r
    | Ast.Arrow a ->
      {r with Ast.rewrite =
                Ast.Arrow {a with Ast.lhs = (fold a.Ast.rhs a.Ast.lhs)}} in
  List.map (fun (s,(r,a)) -> (s,(aux r,a))) rules

let counters_signature s agents =
  match List.find (function
      | Ast.Absent _ -> false
      | Ast.Present (s',_,_) -> name_match s s') agents with
  | Ast.Absent _ -> assert false
  | Ast.Present (_,sites',_) ->
    List.fold_left
      (fun acc s -> match s with
           Ast.Counter c -> c::acc
         | Ast.Port _ -> acc) [] sites'

(* c': counter declaration, returns counter in rule*)
let enumerate_counter_tests x a ((delta,_) as count_delta) c' =
  let (max,_) = c'.Ast.count_delta in
  let min =
    match c'.Ast.count_test with
      None| Some (Ast.CGTE _,_)| Some (Ast.CVAR _,_) ->
             raise
               (ExceptionDefn.Malformed_Decl
                  ("Invalid counter signature - have to specify min bound",
                   (snd c'.Ast.count_nme)))
      | Some (Ast.CEQ min,_) -> min in

  let rec enum v =
    if (v>max) then []
    else
      if (v+delta <= max)&&(v+delta >= 0)
      then (Ast.Counter
              {Ast.count_nme=c'.Ast.count_nme;
               count_test = Some(Ast.CEQ v,a);
               count_delta},
            [x,v])::(enum (v+1))
      else enum (v+1) in
  enum min

let enumerate rules f =
  List.rev
    (List.fold_left
       (fun acc (s,(rc,_ as r)) ->
          let enumerate_r =
            if (match rc.Ast.rewrite with Ast.Edit _ -> false | Ast.Arrow a -> a.Ast.lhs = [])
            then [(None,r)]
            else
              List.map
                (fun (s',r') ->
                   match s,s' with
                     None, _ -> None,r'
                   | Some _, None -> s,r'
                   | Some (s1,a1), Some s2 -> Some (s1^"__"^s2,a1),r') (f r) in
          enumerate_r@acc) [] rules)

let remove_variable_in_counters ~warning rules signatures =
  let counter_gte_delta c delta =
    let count_delta =
      {c with Ast.count_test=Some (Ast.CGTE (abs(delta)),Locality.dummy)} in
    [(Ast.Counter count_delta,[])] in
  let counter_gte_zero c =
    [(Ast.Counter {c with Ast.count_test=Some (Ast.CGTE 0,Locality.dummy)}), []]
  in

  let remove_var_site counters =
    function
      Ast.Port p -> [(Ast.Port p,[])]
    | Ast.Counter c ->
       let (delta,_) = c.Ast.count_delta in
       match c.Ast.count_test with
         None ->
         if (delta <0) then counter_gte_delta c delta else counter_gte_zero c
       | Some (Ast.CEQ v,_) ->
          if (delta >0 || abs(delta) <= v) then [(Ast.Counter c,[])]
          else
            raise
              (ExceptionDefn.Malformed_Decl
                 ("Counter "^(fst c.Ast.count_nme)^" becomes negative",
                  (snd c.Ast.count_nme)))
       | Some (Ast.CGTE v,pos) ->
          let () = if (v+delta <0) then
                     raise
                       (ExceptionDefn.Malformed_Decl
                          ("Counter "^(fst c.Ast.count_nme)^" becomes negative",
                           (snd c.Ast.count_nme))) in
          let () = if (v=0) then
                     let error = "Counter "^(fst c.Ast.count_nme)^":>0 always holds" in
                     warning
                       ~pos (fun f -> Format.pp_print_string f error) in
          [(Ast.Counter c,[])]
       | Some (Ast.CVAR x,a) ->
          enumerate_counter_tests x a c.Ast.count_delta
               (List.find
                  (fun c' -> name_match c.Ast.count_nme c'.Ast.count_nme)
                  counters) in
  let rec remove_var_sites counters = function
    | [] -> []
    | s::t ->
       combinations
         (remove_var_sites counters t) (remove_var_site counters s) in
  let remove_var_agent = function
    | Ast.Absent l -> [Ast.Absent l,[]]
    | Ast.Present (s,sites,m) ->
      let counters = counters_signature s signatures in
      let enumerate_sites = remove_var_sites counters sites in
      List.map
        (fun (sites',c) -> (Ast.Present (s,sites',m),c)) enumerate_sites in
  let rec remove_var_mixture = function
    | [] -> []
    | ag::t -> combinations (remove_var_mixture t) (remove_var_agent ag) in

  let update_opt_rate counters = function
    | None -> None
    | Some r -> Some (update_rate counters r) in
  let update_pair_rate counters = function
    | None -> None
    | Some (r1,r2) ->
       Some ((update_rate counters r1),(update_opt_rate counters r2)) in

  let remove_var_rule (r,a) =
    let mix =
      match r.Ast.rewrite with
      | Ast.Edit r -> r.Ast.mix
      | Ast.Arrow r -> r.Ast.lhs in
    List.map
      (fun (lhs,counters) ->
         let k_def = update_rate counters r.Ast.k_def in
         let k_un = update_pair_rate counters r.Ast.k_un in
         let k_op = update_opt_rate counters r.Ast.k_op in
         let k_op_un = update_pair_rate counters r.Ast.k_op_un in
         let append =
           if (counters = []) then None
           else
             Some (List.fold_left
                     (fun acc (_,i) -> (string_of_int i)^acc) "" counters) in
         (append,
          ({Ast.rewrite = (match r.Ast.rewrite with
               | Ast.Edit e -> Ast.Edit {e with Ast.mix = lhs}
               | Ast.Arrow a -> Ast.Arrow {a with Ast.lhs});
            Ast.bidirectional = r.Ast.bidirectional;
            Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un},a)))
      (remove_var_mixture mix) in
  let rules = prepare_counters rules in

   enumerate rules remove_var_rule

let with_counters c =
  let with_counters_mix mix =
    List.exists
      (function
        | Ast.Absent _ -> false
        | Ast.Present (_,ls,_) ->
        List.exists (function Ast.Counter _ -> true | Ast.Port _ -> false) ls)
      mix in
  with_counters_mix c.Ast.signatures

let remove_variables_in_counters ~warning ~debugMode c =
  if (with_counters c) then
    let rules =
      remove_variable_in_counters ~warning c.Ast.rules c.Ast.signatures in
    let () =
      if debugMode then
        let () = Format.printf "@.ast rules@." in
        List.iter (fun (s,(r,_)) ->
            let label = match s with None -> "" | Some (l,_) -> l in
            Format.printf "@.%s = %a" label Ast.print_ast_rule r)
          rules in
    ({c with Ast.rules},true)
  else (c,false)

let incr_type = 0

let one_fresh_incr_agent is_zero is_value b_link_id a_link_id =
  let a_type = incr_type in
  let a_ints = [|None; Some (if is_zero then 1 else 0); None|] in
  let a_ports =
    [|Raw_mixture.VAL b_link_id; is_value; Raw_mixture.VAL a_link_id|] in
  { Raw_mixture.a_type; Raw_mixture.a_ports; Raw_mixture.a_ints; }

let build_created_counter min_value nb_agents value_link_id test acc =
  let value_lnk =
    match test with
    | None ->
      fun id ->
        if id = min_value then Raw_mixture.VAL value_link_id
        else Raw_mixture.FREE
    | Some (Ast.CEQ value,_) ->
      fun id ->
        if id = value then Raw_mixture.VAL value_link_id
        else Raw_mixture.FREE
    | Some ((Ast.CGTE _ | Ast.CVAR _),pos) ->
      raise (ExceptionDefn.Malformed_Decl
               ("Ambiguous definition of created counter",pos)) in
  let rec aux_fresh_ring id link_id acc =
    if id = 0 then
      (one_fresh_incr_agent true (value_lnk id) (succ value_link_id) link_id
       :: acc,
       succ value_link_id)
    else
      let ag =
        one_fresh_incr_agent false (value_lnk id) (succ link_id) link_id in
      aux_fresh_ring (pred id) (succ link_id) (ag::acc) in
  if nb_agents <= 0 then assert false
  else aux_fresh_ring (pred nb_agents) (succ value_link_id) acc

let one_incr_agent ra_erased zero_state value_val b_link_id a_link_id =
  let ra_type = incr_type in
  let int_state = LKappa.I_ANY in
  let ra_ints = [|int_state; zero_state; int_state|] in
  let switch = if ra_erased then LKappa.Erased else LKappa.Maintained in
  let b_val = match b_link_id with
    | None -> LKappa.LNK_ANY
    | Some lnk_id -> LKappa.LNK_VALUE (lnk_id,(2,ra_type)) in
  let a_val = match a_link_id with
    | None -> LKappa.LNK_ANY
    | Some lnk_id -> LKappa.LNK_VALUE (lnk_id,(0,ra_type)) in
  let ra_ports = [|
    (Locality.dummy_annot b_val,switch);
    value_val;
    (Locality.dummy_annot a_val,switch)|] in
  { LKappa.ra_type; ra_erased; ra_ports; ra_ints;
    ra_syntax = Some (Array.copy ra_ports,Array.copy ra_ints) }

let build_erased_counter nb_agents value_link_id link_value_info test acc =
  let value_lnk id =
    match test with
    | None | Some (Ast.CGTE 0,_) ->
      ((if id = 0 then LKappa.LNK_VALUE (value_link_id,link_value_info)
        else LKappa.LNK_ANY),
       LKappa.I_ANY_ERASED)
    | Some (Ast.CEQ v,_) ->
      ((if id = 0 then LKappa.LNK_VALUE (value_link_id,link_value_info)
        else LKappa.LNK_ANY),
       if id = v then LKappa.I_VAL_ERASED 1 else LKappa.I_ANY_ERASED)
    | Some (Ast.CGTE v,_) ->
      ((if id = v - 1 then LKappa.LNK_VALUE (value_link_id,link_value_info)
        else LKappa.LNK_ANY),
       if id < v then LKappa.I_VAL_ERASED 0 else LKappa.I_ANY_ERASED)
    | Some (Ast.CVAR _,pos) ->
      raise (ExceptionDefn.Malformed_Decl
               ("A counter variable cannot occur here",pos)) in
  let rec aux_destruct_ring id link_id acc =
    let (test_value,test_is_zero) = value_lnk id in
    let value_val = (Locality.dummy_annot test_value,LKappa.Erased) in
    if id = 0 then
      (one_incr_agent
         true test_is_zero value_val (Some link_id) (Some (succ value_link_id))
       :: acc,
       succ link_id)
    else
      let ag =
        one_incr_agent
          true test_is_zero value_val (Some link_id) (Some (succ link_id)) in
      aux_destruct_ring (pred id) (succ link_id) (ag::acc) in
  let () = assert (nb_agents > 0) in
  aux_destruct_ring (pred nb_agents) (succ value_link_id) acc

let build_maintained_counter_test_eq
    fresh_link_id link_value_id link_value_info test diff acc =
  let rec aux_chain_test_eq old_val new_val next_link_id first_ag acc =
    let (test_is_zero, old_val', new_val') =
      match old_val, new_val with
      | Some 0, Some 0 -> (LKappa.I_VAL_CHANGED (1,1),None,None)
      | Some 0, b -> (LKappa.I_VAL_CHANGED (1,0),None,Option_util.map pred b)
      | a, Some 0 -> (LKappa.I_VAL_CHANGED (0,1),Option_util.map pred a,None)
      | a, b -> (LKappa.I_ANY,Option_util.map pred a,Option_util.map pred b) in
    if old_val' = None && new_val' = None then
      if first_ag then
        let ag = one_incr_agent
            false test_is_zero
            (Locality.dummy_annot
               (LKappa.LNK_VALUE (link_value_id,link_value_info)),
             LKappa.Maintained)
            None None in
        (ag::acc,next_link_id,false)
      else
        let ag = one_incr_agent
            false test_is_zero
            (Locality.dummy_annot LKappa.LNK_ANY,LKappa.Maintained)
            None (Some next_link_id) in
        (ag::acc,succ next_link_id,false)
    else
      if first_ag then
        let ag = one_incr_agent
            false test_is_zero
            (Locality.dummy_annot
               (LKappa.LNK_VALUE (link_value_id,link_value_info)),
             LKappa.Maintained)
            (Some next_link_id) None in
        aux_chain_test_eq old_val' new_val' next_link_id false (ag::acc)
      else
        let ag = one_incr_agent
            false test_is_zero
            (Locality.dummy_annot LKappa.LNK_ANY,LKappa.Maintained)
            (Some (succ next_link_id)) (Some next_link_id) in
        aux_chain_test_eq
          old_val' new_val' (succ next_link_id) false (ag::acc) in
  let () = assert (test >= - diff) in
  aux_chain_test_eq (Some test) (Some (test+diff)) fresh_link_id true acc

let rec build_diff_only_negative new_link_value_id todo next_link_id acc =
  if todo < 1 then
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot LKappa.LNK_FREE, LKappa.Linked new_link_value_id)
        None (Some next_link_id) in
    (ag::acc, succ next_link_id, true)
  else
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained)
        (Some (succ next_link_id)) (Some next_link_id) in
    build_diff_only_negative
      new_link_value_id (pred todo) (succ next_link_id) (ag::acc)

let rec build_diff_only_positive new_link_value_id todo next_link_id acc =
  if todo < 1 then
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot LKappa.LNK_FREE, LKappa.Linked new_link_value_id)
        (Some next_link_id) None in
    (ag::acc, succ next_link_id, true)
  else
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained)
        (Some next_link_id) (Some (succ next_link_id)) in
    build_diff_only_positive
      new_link_value_id (pred todo) (succ next_link_id) (ag::acc)

let build_maintained_counter_test_gte
    fresh_link_id link_value_id link_value_info new_link_value_id test diff acc=
  let rec build_not_zero_maybe_diff_negative
      todo diff link_moved next_link_id acc =
    if todo < 1 then
      match diff with
      | None -> (acc,next_link_id,link_moved)
      | Some x ->
        build_diff_only_negative new_link_value_id x next_link_id acc
    else
      let (diff',value_val) =
        match diff with
        | Some 0 ->
          (None,
           (Locality.dummy_annot LKappa.LNK_FREE,
            LKappa.Linked new_link_value_id))
        | x ->
          (Option_util.map pred x,
           (Locality.dummy_annot LKappa.LNK_ANY, LKappa.Maintained)) in
      let ag =
        one_incr_agent
          false (LKappa.I_VAL_CHANGED (0,0)) value_val
          (if todo = 1 && diff' = None then None else (Some (succ next_link_id)))
          (Some next_link_id) in
      build_not_zero_maybe_diff_negative
        (pred todo) diff' link_moved (succ next_link_id) (ag::acc) in
  if diff > 0 then
    let (l,fresh_link_id',_) =
      build_not_zero_maybe_diff_negative
        (pred test) None false fresh_link_id acc in
    let ag =
      one_incr_agent
        false (LKappa.I_VAL_CHANGED (0,0))
        (Locality.dummy_annot
           (LKappa.LNK_VALUE (link_value_id,link_value_info)),
         LKappa.Freed)
        (if test = 1 then None else Some fresh_link_id) (Some fresh_link_id') in
    build_diff_only_positive
      new_link_value_id (pred diff) fresh_link_id' (ag::l)
  else
    let diff' = if diff = 0 then None else Some (-1 - diff) in
    let (l,fresh_link_id',new_link_value_id') =
      build_not_zero_maybe_diff_negative
        (pred test) diff' (diff <> 0) fresh_link_id acc in
    let ag =
      one_incr_agent
        false (LKappa.I_VAL_CHANGED (0,0))
        (Locality.dummy_annot
           (LKappa.LNK_VALUE (link_value_id,link_value_info)),
         if diff = 0 then LKappa.Maintained else LKappa.Freed)
        (if test = 1 && diff = 0 then None else Some fresh_link_id) None in
    (ag::l,fresh_link_id',new_link_value_id')

let build_untested_counter_chain
    fresh_link_id link_value_id link_value_info new_link_value_id diff acc =
  if diff < 0 then
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot
           (LKappa.LNK_VALUE (link_value_id,link_value_info)),
         LKappa.Freed)
        (Some fresh_link_id) None in
    build_diff_only_negative
      new_link_value_id (-1 - diff) fresh_link_id (ag::acc)
  else if diff = 0 then
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot
           (LKappa.LNK_VALUE (link_value_id,link_value_info)),
         LKappa.Maintained)
        None None in
(ag::acc,fresh_link_id,false)
  else
    let ag =
      one_incr_agent
        false LKappa.I_ANY
        (Locality.dummy_annot
           (LKappa.LNK_VALUE (link_value_id,link_value_info)),
         LKappa.Freed)
        None (Some fresh_link_id) in
    build_diff_only_positive
      new_link_value_id (pred diff) fresh_link_id (ag::acc)

let build_maintained_counter
    value_link_id link_value_info new_value_link_id test diff acc =
  match test with
  | None ->
    build_untested_counter_chain
      (succ value_link_id) value_link_id
      link_value_info new_value_link_id diff acc
  | Some (Ast.CEQ test,_) ->
    build_maintained_counter_test_eq
      (succ value_link_id) value_link_id link_value_info
      test diff acc
  | Some (Ast.CGTE test,_) ->
    if test > 0 then
      build_maintained_counter_test_gte
        (succ value_link_id) value_link_id
        link_value_info new_value_link_id test diff acc
    else
      build_untested_counter_chain
        (succ value_link_id) value_link_id
        link_value_info new_value_link_id diff acc
  | Some (Ast.CVAR _,pos) ->
    raise (ExceptionDefn.Malformed_Decl
             ("A counter variable cannot occur here",pos))
