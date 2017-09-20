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

  let rec fold rhs lhs = match (rhs,lhs) with
    | ((rna,rsites,_)::r, ((lna,lsites,b) as lagent)::l) ->
       let () = syntax lsites (fun c -> not((fst c.Ast.count_delta)=0))
                       " has a modif in the lhs";
                syntax rsites (fun c -> not(c.Ast.count_test=None))
                       " has a test in the rhs" in
       if ((String.compare (fst rna) (fst lna)) = 0) then
         let lsites' = prepare_agent rsites lsites in
         (lna,lsites',b)::(fold r l)
       else lagent::(fold r l) (*what does this subcase mean?*)
    | [], _ | _, [] -> [] in

  let aux r = {r with Ast.lhs = (fold r.Ast.rhs r.Ast.lhs)} in
  List.map (fun (s,(r,a)) -> (s,(aux r,a))) rules

let counters_signature s agents =
  let (_,sites',_) = List.find (fun (s',_,_) -> name_match s s') agents in
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
       (fun acc (s,r) ->
         let enumerate_r =
           if ((fst r).Ast.lhs = []) then [(None,r)] else
             List.map
               (fun (s',r') ->
                 match s,s' with
                   None, _ -> None,r'
                 | Some _, None -> s,r'
                 | Some (s1,a1), Some s2 -> Some (s1^"__"^s2,a1),r') (f r) in
         enumerate_r@acc) [] rules)

let enumerate_edit rules f =
  List.rev
    (List.fold_left
       (fun acc (s,r) ->
         let enumerate_r =
           List.map
             (fun (s',r') ->
               match s,s' with
                 None, _ -> None,r'
               | Some _, None -> s,r'
               | Some (s1,a1), Some s2 -> Some (s1^"__"^s2,a1),r') (f r) in
         enumerate_r@acc) [] rules)

let remove_variable_in_counters rules edit_rules signatures =
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
                     ExceptionDefn.warning
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
  let remove_var_agent (s,sites,m) =
    let counters = counters_signature s signatures in
    let enumerate_sites = remove_var_sites counters sites in
    List.map (fun (sites',c) -> ((s,sites',m),c)) enumerate_sites in
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

  let merge get_mix f r =
    List.map
      (fun (mix,counters) -> f mix counters r)
      (remove_var_mixture (get_mix r)) in
  let remove_var_rule r =
    merge (fun (r,_) -> r.Ast.lhs)
          (fun lhs counters (r,a) ->
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
             ({r with Ast.lhs; Ast.k_def; Ast.k_un; Ast.k_op; Ast.k_op_un},a)))
          r in
  let remove_var_edit_rule r =
    merge (fun (r,_) -> r.Ast.mix)
          (fun mix counters (r,pos) ->
            let act = update_rate counters r.Ast.act in
            let un_act = update_pair_rate counters r.Ast.un_act in
            let append = None in
            (append,({r with Ast.mix; Ast.act; Ast.un_act},pos))) r in
  let rules = prepare_counters rules in

  ((enumerate_edit edit_rules remove_var_edit_rule),
   (enumerate rules remove_var_rule))

let with_counters c =
  let with_counters_mix mix =
    List.exists
      (fun (_,ls,_) ->
        List.exists (function Ast.Counter _ -> true | Ast.Port _ -> false) ls)
      mix in
  with_counters_mix c.Ast.signatures

let compile c =
  if (with_counters c) then
    let (edit_rules,rules) =
      remove_variable_in_counters c.Ast.rules c.Ast.edit_rules c.Ast.signatures
    in
    let () =
      if (!Parameter.debugModeOn) then
      (Format.printf "@.ast rules@.";
      List.iter (fun (s,(r,_)) ->
                  let label = match s with None -> "" | Some (l,_) -> l in
                  Format.printf "@.%s = %a" label Ast.print_ast_rule r)
                rules;
      Format.printf "@.ast edit_rules@.";
      List.iter (fun (s,(r,_)) ->
                  let label = match s with None -> "" | Some (l,_) -> l in
                  Format.printf "@.%s = %a" label Ast.print_ast_edit_rule r)
                edit_rules) in
    ({c with Ast.rules;Ast.edit_rules},true)
  else (c,false)

let empty_counter =
  {Ast.count_nme = ("",Locality.dummy);
   count_test = None; count_delta =(0,Locality.dummy)}
