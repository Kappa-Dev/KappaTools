(**
 * preprocess.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 01/17/2011
 * Last modification: Time-stamp: <Oct 18 2017>
 * *
 * Translation from kASim ast to ckappa representation,
 *
 * Copyright 2010,2011,2012,2013,2014, 2015 Institut National
 * de Recherche en Informatique et en Automatique.
 * All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let local_trace = false

let check_freshness parameters error str id id_set =
  let error,id_set =
    if Mods.StringSet.mem id id_set
    then
      begin
        Exception.warn
          parameters error __POS__ ~message:(str^" '"^id^"' is already used") Exit id_set
      end
    else
      error,Mods.StringSet.add id id_set
  in
  error,id_set


let add_entry parameters id agent site index (error,map) =
  let error,old_list =
    Ckappa_sig.Agent_id_map_and_set.Map.find_default_without_logs
      parameters
      error
      []
      id
      map
      (* this is a partial map which stores the occurrences of binding
         labels *)
  in
  Ckappa_sig.Agent_id_map_and_set.Map.add_or_overwrite
    parameters
    error
    id
    ((agent, site, index) :: old_list)
    map

let rev_ast = List.rev
(*mixture =
  let rec aux mixture sol =
  match mixture with
   | [] -> sol
  (* | Ast.DOT(i,agent,mixture) -> aux mixture (Ast.DOT(i,agent,sol))*)
  (* | Ast.PLUS(i,agent,mixture) -> aux mixture (Ast.PLUS(i,agent,sol))*)
   | agent :: mixture -> aux mixture (agent :: sol)
  in aux mixture []*)

let pop_entry parameters error id (map,set) =
  let error,list =
    Ckappa_sig.Agent_id_map_and_set.Map.find_option
      parameters
      error
      id
      map
  in
  if
    Ckappa_sig.Agent_id_map_and_set.Set.mem
      id
      set
  then
    match list with
    | Some [_] ->
      let error,map =
        Ckappa_sig.Agent_id_map_and_set.Map.remove
          parameters
          error
          id
          map
      in
      Exception.warn
        parameters error __POS__
        ~message:"dandling bond detected"
        Exit (None,map)
    | Some [] ->
      Exception.warn
        parameters error __POS__
        ~message:"internal bug, link id is ignored"
        Exit (None,map)
    | Some (_::t) ->
      let error,map =
        Ckappa_sig.Agent_id_map_and_set.Map.overwrite
          parameters
          error
          id
          t
          map
      in
      Exception.warn
        parameters error __POS__
        ~message:"internal bug, link id is ignored"
        Exit (None,map)
    | None ->
      Exception.warn
        parameters error __POS__
        ~message:"internal bug, link id is ignored"
        Exit (None,map)
  else
    match list with
    | Some [a] ->
      let error,map =
        Ckappa_sig.Agent_id_map_and_set.Map.remove
          parameters
          error
          id
          map
      in
      error,(Some a,map)
    | Some [b;a] ->
      let error,map =
        Ckappa_sig.Agent_id_map_and_set.Map.overwrite
          parameters
          error
          id
          [a]
          map
      in
      error,(Some b,map)
    | Some (_::t) ->
      let error,map =
        Ckappa_sig.Agent_id_map_and_set.Map.overwrite
          parameters
          error
          id
          t
          map
      in
      Exception.warn
        parameters error __POS__
        ~message:"too many instances of a link identifier, ignore them"
        Exit (None,map)
    | Some [] ->
      Exception.warn
        parameters error __POS__
        ~message:"internal bug, link identifier"
        Exit (None,map)
    | None ->
      Exception.warn parameters error __POS__ Exit (None,map)

let rec scan_interface parameters k agent interface ((error,a),set as remanent)=
  match interface with
  | [] -> remanent
  | Ast.Counter _::_ ->
    (Exception.warn
        parameters error __POS__
        ~message:"Do not deal with counters yet"
        Exit a,set)
  | Ast.Port port::interface ->
    let error,set =
      check_freshness parameters error "Site" (fst port.Ast.port_nme) set
    in
    let remanent = error,a in
    scan_interface parameters k agent interface
      ((match port.Ast.port_lnk with
          | [Ast.LNK_VALUE (i,()),_] ->
            add_entry
              parameters
              (Ckappa_sig.agent_id_of_int i)
              agent
              (fst port.Ast.port_nme)
              k
              remanent
          | [] | ((Ast.LNK_ANY | Ast.LNK_FREE | Ast.LNK_TYPE _ | Ast.LNK_SOME
                  | Ast.ANY_FREE | Ast.LNK_VALUE (_,())),_) :: _ -> remanent),set)

let scan_agent parameters k ((name,_),intf,_modif) remanent =
  fst (scan_interface parameters k name intf (remanent,Mods.StringSet.empty))

let rec collect_binding_label
    parameters mixture f (k:Ckappa_sig.c_agent_id) remanent =
  match mixture with
  | agent :: mixture (*| Ast.DOT (_,agent,mixture) | Ast.PLUS(_,agent,mixture)*) ->
    collect_binding_label
      parameters
      mixture
      f
      (Ckappa_sig.next_agent_id k)
      (scan_agent parameters (f k) agent remanent)
  | [] -> remanent

let collect_binding_label parameters mixture f (k:Ckappa_sig.c_agent_id) remanent =
  let error,map =
    collect_binding_label
      parameters
      mixture
      f
      k
      remanent
  in
  Ckappa_sig.Agent_id_map_and_set.Map.fold
    (fun x l (error,(map,set)) ->
       if (List.length l = 1)
       then
         let error,map =
           Ckappa_sig.Agent_id_map_and_set.Map.remove
             parameters
             error
             x
             map
         in
         let error,set =
           Ckappa_sig.Agent_id_map_and_set.Set.add
             parameters
             error
             x
             set
         in
         Exception.warn
           parameters error __POS__
           ~message:"dangling bond detected"
           Exit (map,set)
       else
         (error,(map,set)))
    map
    (error, (map,
             Ckappa_sig.Agent_id_map_and_set.Set.empty
            ))

let translate_lnk_state parameters lnk_state remanent =
  match lnk_state with
  | [Ast.LNK_VALUE (id,()),pos] ->
    begin
      let error, remanent = remanent in
      let error, (triple, map) =
        pop_entry
          parameters
          error
          (Ckappa_sig.agent_id_of_int id) (*NOTE: I don't want to change the type in Ast*)
          remanent
      in
      match triple with
      | None ->
        let site = Ckappa_sig.LNK_SOME pos in
        let remanent =
          Exception.warn parameters error __POS__
            ~message:"one dandling bond has been replaced by a wild card"
            ~pos
            Exit
            remanent
        in
        site, remanent
      | Some (agent,site,index) ->
        if (agent,site,index) = ("", "", (*0*)Ckappa_sig.dummy_agent_id)
        then
          let site = Ckappa_sig.LNK_SOME pos in
          let remanent =
            Exception.warn parameters error __POS__ Exit remanent
          in
          site,remanent
        else
          Ckappa_sig.LNK_VALUE
            (index,
             agent,
             site,
             (Ckappa_sig.agent_id_of_int id),
             pos),
          (error, (map, (snd remanent)))
    end
  | [(Ast.LNK_FREE|Ast.ANY_FREE),_] | [] -> Ckappa_sig.FREE,remanent
  | [Ast.LNK_ANY,position] -> Ckappa_sig.LNK_ANY position,remanent
  | [Ast.LNK_SOME,position] -> Ckappa_sig.LNK_SOME position,remanent
  | [Ast.LNK_TYPE (x,y),_position] -> Ckappa_sig.LNK_TYPE (y,x),remanent
  | _::(_,pos)::_ ->
    let error, va = remanent in
    Ckappa_sig.LNK_ANY pos,
    Exception.warn parameters error __POS__
      ~message:"More than one link state for a single site" ~pos
      Exit va


let translate_port is_signature parameters int_set port remanent =
  let error,map = remanent in
  let error,_ =
    check_freshness parameters error "Site" (fst (port.Ast.port_nme)) int_set
  in
  let error',is_free =
    match port.Ast.port_lnk
    with [(Ast.LNK_FREE|Ast.ANY_FREE),_] | [] -> error,Some true
          | [Ast.LNK_ANY,_] -> error,None
          | ((Ast.LNK_SOME | Ast.LNK_TYPE _ | Ast.LNK_VALUE _ | Ast.ANY_FREE
            | Ast.LNK_FREE | Ast.LNK_ANY),_) :: _ -> error,Some false in
  let lnk,remanent =
    if is_signature then Ckappa_sig.FREE,remanent else
      translate_lnk_state parameters port.Ast.port_lnk (error',map) in
  {
    Ckappa_sig.port_nme = fst (port.Ast.port_nme) ;
    Ckappa_sig.port_int = List.rev_map fst (List.rev port.Ast.port_int) ;
    Ckappa_sig.port_lnk = lnk ;
    (*       port_pos = pos ; *)
    Ckappa_sig.port_free = is_free },
  remanent

let rec translate_interface parameters is_signature int_set interface remanent =
  match interface with
  | [] -> Ckappa_sig.EMPTY_INTF,remanent
  | Ast.Counter _::interface ->
    translate_interface parameters is_signature int_set interface remanent
  | Ast.Port port::interface ->
    let port,remanent =
      translate_port is_signature parameters int_set port remanent in
    let interface,remanent =
      translate_interface parameters is_signature int_set interface remanent in
    Ckappa_sig.PORT_SEP (port,interface),remanent

let translate_interface parameters is_signature =
  translate_interface parameters is_signature Mods.StringSet.empty

let translate_agent parameters is_signature
    ((ag_nme,ag_nme_pos),intf,_modif) remanent =
  let interface,remanent =
    translate_interface parameters is_signature intf remanent in
  {Ckappa_sig.ag_nme;
   Ckappa_sig.ag_intf = interface ;
   Ckappa_sig.ag_nme_pos;
   (*     Ckappa_sig.ag_pos = position ;*)
  },
  remanent

let rec build_skip k mixture =
  if k = 0
  then mixture
  else
    build_skip
      (k - 1)
      (Ckappa_sig.SKIP(mixture))

let rec translate_mixture_zero_zero  parameters mixture remanent tail_size =
  match mixture with
  | [] -> build_skip tail_size Ckappa_sig.EMPTY_MIX,remanent
  | agent :: mixture ->
    let agent,remanent =
      translate_agent parameters false agent remanent in
    let mixture,remanent =
      translate_mixture_zero_zero parameters mixture remanent tail_size  in
    Ckappa_sig.COMMA(agent,mixture),remanent
(*      | Ast.DOT(i,agent,mixture) ->
          let agent,remanent = translate_agent parameters agent remanent in
          let mixture,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size  in
            Ckappa_sig.DOT(i,agent,mixture),remanent
        | Ast.PLUS(i,agent,mixture) ->
          let agent,remanent = translate_agent parameters agent remanent in
          let mixture,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size  in
            Ckappa_sig.PLUS(i,agent,mixture),remanent*)

let rec translate_mixture_in_rule parameters mixture remanent prefix_size empty_size tail_size =
  if prefix_size = 0
  then
    let tail, remanent =
      translate_mixture_zero_zero
        parameters
        mixture
        remanent
        tail_size
    in
    build_skip
      empty_size
      tail, remanent
  else
    match mixture with
    | [] -> Ckappa_sig.EMPTY_MIX, remanent
    | agent :: mixture ->
      let agent, remanent =
        translate_agent parameters false agent remanent in
      let mixture, remanent =
        translate_mixture_in_rule
          parameters
          mixture
          remanent
          (prefix_size - 1)
          empty_size
          tail_size
      in
      Ckappa_sig.COMMA(agent,mixture),remanent

(* | Ast.DOT(i,agent,mixture) -> let agent,remanent = translate_agent
   parameters agent remanent in let mixture,remanent =
   translate_mixture_in_rule parameters mixture remanent
   (prefix_size-1) empty_size tail_size in
   Ckappa_sig.DOT(i,agent,mixture),remanent |
   Ast.PLUS(i,agent,mixture) -> let agent,remanent = translate_agent
   parameters agent remanent in let mixture,remanent =
   translate_mixture_in_rule parameters mixture remanent
   (prefix_size-1) empty_size tail_size in
   Ckappa_sig.PLUS(i,agent,mixture),remanent*)

let rec translate_mixture parameters mixture remanent  =
  match mixture with
  | [] -> Ckappa_sig.EMPTY_MIX,remanent
  | agent :: mixture ->
    let agent,remanent = translate_agent parameters false agent remanent in
    let mixture,remanent = translate_mixture parameters mixture remanent in
    Ckappa_sig.COMMA(agent,mixture),remanent
(*      | Ast.DOT(i,agent,mixture) ->
        let agent,remanent = translate_agent parameters agent remanent in
        let mixture,remanent = translate_mixture parameters mixture remanent in
        Ckappa_sig.DOT(i,agent,mixture),remanent
        | Ast.PLUS(i,agent,mixture) ->
        let agent,remanent = translate_agent parameters agent remanent in
        let mixture,remanent = translate_mixture parameters mixture remanent in
        Ckappa_sig.PLUS(i,agent,mixture),remanent*)

let support_agent ((name,_),intfs,_) =
  let list =
    let rec scan intf list =
      match intf with
      | [] -> List.sort compare list
      | Ast.Port port::intf ->
        scan intf ((fst port.Ast.port_nme)::list)
      | Ast.Counter _::intf -> scan intf list
    in
    scan intfs []
  in
  name,list

let compatible_agent ag1 ag2 =
  support_agent ag1 = support_agent ag2

let length mixture =
  let rec aux mixture k =
    match mixture with
    | [] -> k
    | _ :: mixture (*| Ast.DOT(_,_,mixture) | Ast.PLUS(_,_,mixture)*) -> aux mixture (k+1)
  in aux mixture 0


let longuest_prefix mixture1 mixture2 =
  let rec common_prefix mixture1 mixture2 k =
    match mixture1 with
    | [] -> (k,mixture1,mixture2)
    | agent :: mixture (*| Ast.DOT(_,agent,mixture) | Ast.PLUS(_,agent,mixture)*) ->
      begin
        match mixture2 with
        | [] -> (k,mixture1,mixture2)
        | agent' :: mixture' (*| Ast.DOT(_,agent',mixture') | Ast.PLUS(_,agent',mixture')*) ->
          begin
            if compatible_agent agent agent'
            then
              common_prefix mixture mixture' (k+1)
            else
              k,mixture1,mixture2
          end
      end
  in
  let common_size,tail_lhs,tail_rhs = common_prefix mixture1 mixture2 0 in
  common_size,length tail_lhs,length tail_rhs

let refine_mixture_in_rule parameters error prefix_size empty_size tail_size mixture =
  let f i =
    if
      Ckappa_sig.compare_agent_id
        i
        (Ckappa_sig.agent_id_of_int (prefix_size-1))
      > 0
    then
      Ckappa_sig.add_agent_id i empty_size
    else i
  in
  let remanent =
    collect_binding_label
      parameters
      mixture
      f
      Ckappa_sig.dummy_agent_id
      (error,
       Ckappa_sig.Agent_id_map_and_set.Map.empty
      )
  in
  let mixture,(error,_map) =
    translate_mixture_in_rule
      parameters
      mixture
      remanent
      prefix_size
      empty_size
      tail_size
  in
  error,mixture

let refine_mixture parameters error mixture =
  let remanent =
    collect_binding_label
      parameters
      mixture
      (fun i -> i)
      Ckappa_sig.dummy_agent_id
      (error,
       Ckappa_sig.Agent_id_map_and_set.Map.empty
      )
  in
  let mixture,(error,_map) = translate_mixture parameters mixture remanent in
  error, mixture


let rec alg_map f error alg =
  match
    alg
  with
  | Alg_expr.BIN_ALG_OP (op,(m1,pos1),(m2,pos2)) ->
    let error,m1' = alg_map f error m1 in
    let error,m2' = alg_map f error m2 in
    error,Alg_expr.BIN_ALG_OP (op,(m1',pos1),(m2',pos2))
  | Alg_expr.UN_ALG_OP (op,(m1,pos1)) ->
    let error,m1' = alg_map f error m1 in
    error,Alg_expr.UN_ALG_OP (op,(m1',pos1))
  | Alg_expr.DIFF_KAPPA_INSTANCE ((m1,pos1),pattern) ->
    let error,m1' = alg_map f error m1 in
    let error, pattern' = f error pattern in
    error,Alg_expr.DIFF_KAPPA_INSTANCE ((m1',pos1),pattern')
  | Alg_expr.DIFF_TOKEN ((m1,pos1),token) ->
    let error,m1' = alg_map f error m1 in
    error,Alg_expr.DIFF_TOKEN ((m1',pos1),token)
  | Alg_expr.STATE_ALG_OP s -> error,Alg_expr.STATE_ALG_OP s
  | Alg_expr.ALG_VAR s -> error,Alg_expr.ALG_VAR s
  | Alg_expr.TOKEN_ID s -> error,Alg_expr.TOKEN_ID s
  | Alg_expr.KAPPA_INSTANCE mixture ->
    let error,mixture' = f error mixture in
    error,Alg_expr.KAPPA_INSTANCE mixture'
  | Alg_expr.CONST x -> error,Alg_expr.CONST x
  | Alg_expr.IF ((cond,cond_pos),(yes,yes_pos),(no,no_pos)) ->
    let error,cond' = bool_map f error cond in
    let error,yes' = alg_map f error yes in
    let error,no' = alg_map f error no in
    (error,Alg_expr.IF ((cond',cond_pos),(yes',yes_pos),(no',no_pos)))
and bool_map f error alg =
  match
    alg
  with
  | Alg_expr.TRUE -> error,Alg_expr.TRUE
  | Alg_expr.FALSE -> error,Alg_expr.FALSE
  | Alg_expr.UN_BOOL_OP(Operator.NOT,(b1,pos1)) ->
    let error,b1' = bool_map f error b1 in
    error,Alg_expr.UN_BOOL_OP(Operator.NOT,(b1',pos1))
  | Alg_expr.BIN_BOOL_OP(Operator.AND,(b1,pos1),(b2,pos2)) ->
    let error,b1' = bool_map f error b1 in
    let error,b2' = bool_map f error b2 in
    error,Alg_expr.BIN_BOOL_OP(Operator.AND,(b1',pos1),(b2',pos2))
  | Alg_expr.BIN_BOOL_OP(Operator.OR,(b1,pos1),(b2,pos2)) ->
    let error,b1' = bool_map f error b1 in
    let error,b2' = bool_map f error b2 in
    error,Alg_expr.BIN_BOOL_OP(Operator.OR,(b1',pos1),(b2',pos2))
  | Alg_expr.COMPARE_OP(Operator.GREATER,(m1,pos1),(m2,pos2)) ->
    let error,m1' = alg_map f error m1 in
    let error,m2' = alg_map f error m2 in
    error,Alg_expr.COMPARE_OP(Operator.GREATER,(m1',pos1),(m2',pos2))
  | Alg_expr.COMPARE_OP(Operator.SMALLER,(m1,pos1),(m2,pos2)) ->
    let error,m1' = alg_map f error m1 in
    let error,m2' = alg_map f error m2 in
    error,Alg_expr.COMPARE_OP(Operator.SMALLER,(m1',pos1),(m2',pos2))
  | Alg_expr.COMPARE_OP(Operator.EQUAL,(m1,pos1),(m2,pos2)) ->
    let error,m1' = alg_map f error m1 in
    let error,m2' = alg_map f error m2 in
    error,Alg_expr.COMPARE_OP(Operator.EQUAL,(m1',pos1),(m2',pos2))
  | Alg_expr.COMPARE_OP(Operator.DIFF,(m1,pos1),(m2,pos2)) ->
    let error,m1' = alg_map f error m1 in
    let error,m2' = alg_map f error m2 in
    error,Alg_expr.COMPARE_OP(Operator.DIFF,(m1',pos1),(m2',pos2))

let print_expr_map  f error alg =
  match
    alg
  with
  | Primitives.Str_pexpr(s) -> error,Primitives.Str_pexpr(s)
  | Primitives.Alg_pexpr (alg,pos) ->
    let error,alg' = alg_map f error alg in
    error,Primitives.Alg_pexpr (alg',pos)

let map_with_pos map =
  (fun f error (x,pos) ->
     let error,x' = map f error x in
     error,(x',pos) )

let alg_with_pos_map = map_with_pos alg_map

let modif_map f_forbidding_question_marks f_allowing_question_marks error alg =
  match
    alg
  with
  | Ast.INTRO (alg,(mixture,pos)) ->
    let error,alg' = (map_with_pos alg_map) f_allowing_question_marks error alg in
    let error,mixture' = f_forbidding_question_marks error mixture in
    error,Ast.INTRO(alg',(mixture',pos))
  | Ast.DELETE (alg,(mixture,pos)) ->
    let error,alg' = (map_with_pos alg_map) f_allowing_question_marks error alg in
    let error,mixture' = f_allowing_question_marks error mixture in
    error,Ast.DELETE(alg',(mixture',pos))
  | Ast.UPDATE (pos,alg) ->
    let error,alg' = (map_with_pos alg_map) f_allowing_question_marks error alg in
    error,Ast.UPDATE (pos,alg')
  | Ast.UPDATE_TOK (pos,alg) ->
    let error,alg' = (map_with_pos alg_map) f_allowing_question_marks error alg in
    error,Ast.UPDATE_TOK (pos,alg')
  | Ast.STOP list ->
    let error,list' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list)
    in
    error,Ast.STOP list'
  | Ast.SNAPSHOT list ->
    let error,list' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list)
    in
    error,Ast.SNAPSHOT list'
  | Ast.PRINT (list1,list2) ->
    let error,list1' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list1)
    in
    let error,list2' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list2)
    in
    error,Ast.PRINT (list1',list2')
  | Ast.PLOTENTRY -> error,Ast.PLOTENTRY
  | Ast.CFLOWLABEL (a,b) -> error,Ast.CFLOWLABEL(a,b)
  | Ast.CFLOWMIX (a,(mix,pos)) ->
    let error,mix' = f_allowing_question_marks error mix in
    error,Ast.CFLOWMIX(a,(mix',pos))
  | Ast.FLUX (rel,list) ->
    let error,list' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list)
    in
    error,Ast.FLUX (rel,list')
  | Ast.FLUXOFF list ->
    let error,list' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list)
    in
    error,Ast.FLUXOFF list'
  | Ast.SPECIES_OF (a,list,(mix,pos)) ->
    let error,list' =
      List.fold_left
        (fun (error,list) elt ->
           let error,elt' = print_expr_map f_allowing_question_marks error elt in
           error,elt'::list)
        (error,[]) (List.rev list)
    in
    let error,mix' = f_allowing_question_marks error mix in
    error,Ast.SPECIES_OF(a,list',(mix',pos))



let bool_with_pos_map = map_with_pos bool_map

let with_option_map map f =
  (fun error alg ->
     match alg
     with
     | None -> error,None
     | Some alg ->
       let error,alg'=map f error alg in
       error,(Some alg'))

let alg_with_pos_with_option_map = with_option_map alg_with_pos_map
let bool_with_pos_with_option_map = with_option_map bool_with_pos_map

let refine_token parameters error token =
  let error,token =
    Exception.warn
      parameters error __POS__
      ~message:"Tokens are not implemented in KaSa yet"
      Exit token
  in
  error,token

let refine_init_t parameters error = function
  | Ast.INIT_MIX mixture,pos ->
    let error,mixture = refine_mixture parameters error mixture in
    error,(Ast.INIT_MIX mixture,pos)
  | Ast.INIT_TOK token,pos ->
    let error,(token,_) = refine_token parameters error (token,pos) in
    error,(Ast.INIT_TOK token,pos)

let refine_agent parameters error agent_set ((name,_),_,_ as agent) =
  let error,agent_set =
    check_freshness parameters error "Agent" name agent_set in
  let error, map =
    scan_agent
      parameters
      Ckappa_sig.dummy_agent_id
      agent
      (error,
       Ckappa_sig.Agent_id_map_and_set.Map.empty
      )
  in

  let agent,(error,_map) =
    translate_agent
      parameters true agent
      (error, (map,
               Ckappa_sig.Agent_id_map_and_set.Set.empty
              ))
  in
  error, agent_set, agent

let refine_var parameters error id_set var =
  match var with
  | ((string,pos),(alg,pos')) ->
    let error,id_set = check_freshness parameters error "Label" string id_set in
    let error,alg' = alg_map (refine_mixture parameters) error alg
    in error,id_set,((string,pos),(alg',pos'))


let dump_rule rule =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  let () =
   Ast.print_ast_rule fmt rule
  in
  let () = Format.pp_print_flush fmt () in
  Buffer.contents buf

let dump_rule_no_rate rule =
    let buf = Buffer.create 0 in
    let fmt = Format.formatter_of_buffer buf in
    let () =
     Ast.print_ast_rule_no_rate_kasa fmt rule
    in
    let () = Format.pp_print_flush fmt () in
    Buffer.contents buf

let dump_edit_rule rule =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  let () =
    Ast.print_ast_edit_rule fmt rule
  in
  let () = Format.pp_print_flush fmt () in
  Buffer.contents buf

let dump_edit_rule_no_rate rule =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  let () =
    Ast.print_ast_edit_rule_no_rate fmt rule
  in
  let () = Format.pp_print_flush fmt () in
  Buffer.contents buf



let translate_compil parameters error compil =
  let id_set = Mods.StringSet.empty in
  let agent_set = Mods.StringSet.empty in
  let error,id_set,var_rev =
    List.fold_left
      (fun (error,id_set,list) var ->
         let error,id_set,var = refine_var parameters error id_set var in
         error,id_set,(var::list))
      (error,id_set,[])
      compil.Ast.variables
  in
  let error,_agent_set,signatures_rev =
    List.fold_left
      (fun  (error,agent_set,list) agent->
         let error,agent_set,agent =
           refine_agent parameters error agent_set agent in
         error,agent_set,(agent::list))
      (error,agent_set,[])
      compil.Ast.signatures
  in
  let error,observables_rev =
    List.fold_left
      (fun (error,list) alg ->
         let error,alg' =
           alg_with_pos_map (refine_mixture parameters) error alg in
         error,alg'::list)
      (error,[])
      compil.Ast.observables
  in
  let error,id_set',chem_rules_rev =
    List.fold_left
      (fun (error,id_set,list) (id,(rule,p)) ->
         let error,id_set =
           match id with
           | None -> error,id_set
           | Some id -> check_freshness parameters error "Label" (fst id) id_set
         in
         let ast_lhs,ast_rhs = rule.Ast.lhs,rule.Ast.rhs in
         let prefix,tail_lhs,tail_rhs = longuest_prefix ast_lhs ast_rhs in
         let error,lhs =
           refine_mixture_in_rule parameters error prefix 0 tail_rhs ast_lhs in
         let error,rhs =
           refine_mixture_in_rule parameters error prefix tail_lhs 0 ast_rhs in
         let error,k_def =
           alg_with_pos_map (refine_mixture parameters) error rule.Ast.k_def in
         let error,k_un =
           alg_with_pos_with_option_map (refine_mixture parameters) error (Tools_kasa.fst_option rule.Ast.k_un) in
         let original_ast = dump_rule rule in
         let original_ast_no_rate = dump_rule_no_rate rule in
         let rule_direct = {rule with Ast.bidirectional = false} in

         let direct_ast = dump_rule rule_direct in
         let direct_ast_no_rate = dump_rule_no_rate rule_direct in
         let error,direct =
           error,
           {
             Ckappa_sig.position = p ;
             Ckappa_sig.prefix = prefix ;
             Ckappa_sig.interprete_delta = Ckappa_sig.Direct ;
             Ckappa_sig.delta = tail_lhs ;
             Ckappa_sig.lhs = lhs ;
             Ckappa_sig.rhs =  rhs ;
             Ckappa_sig.k_def = k_def ;
             Ckappa_sig.k_un = k_un ;
             Ckappa_sig.ast = direct_ast ;
             Ckappa_sig.ast_no_rate = direct_ast_no_rate ;
             Ckappa_sig.original_ast = original_ast ;
             Ckappa_sig.original_ast_no_rate = original_ast_no_rate ;
             Ckappa_sig.from_a_biderectional_rule = rule.Ast.bidirectional;
           }
         in
         if rule.Ast.bidirectional then
           let reverse_rule =
             {rule_direct with
              Ast.lhs = rule.Ast.rhs;
              Ast.rhs = rule.Ast.lhs;
              Ast.rm_token = rule.Ast.add_token;
              Ast.add_token = rule.Ast.rm_token;
              Ast.k_def =
                (match rule.Ast.k_op with
                | None -> Alg_expr.const Nbr.zero
                | Some k -> k);
              Ast.k_un = rule.Ast.k_op_un ;
              Ast.k_op_un = None ;
              Ast.k_op = None;
             }
           in
           let reverse_ast = dump_rule reverse_rule in
           let reverse_ast_no_rate = dump_rule_no_rate reverse_rule in
           let error,reverse =
             let error,k_op =
               alg_with_pos_map (refine_mixture parameters) error
                 (Option_util.unsome (Alg_expr.const Nbr.zero) rule.Ast.k_op) in
             let error,k_op_un =
               alg_with_pos_with_option_map (refine_mixture parameters) error
                 (Tools_kasa.fst_option rule.Ast.k_op_un) in
             error,
             {
               Ckappa_sig.position = p ;
               Ckappa_sig.prefix = prefix ;
               Ckappa_sig.delta = tail_lhs ;
               Ckappa_sig.interprete_delta = Ckappa_sig.Reverse ;
               Ckappa_sig.lhs = rhs ;
               Ckappa_sig.rhs =  lhs ;
               Ckappa_sig.k_def = k_op ;
               Ckappa_sig.k_un = k_op_un ;
               Ckappa_sig.ast = reverse_ast ;
               Ckappa_sig.ast_no_rate = reverse_ast_no_rate ;
               Ckappa_sig.original_ast = original_ast ;
               Ckappa_sig.original_ast_no_rate = original_ast_no_rate ;
               Ckappa_sig.from_a_biderectional_rule = rule.Ast.bidirectional ;
             }
           in
           error,id_set,
           (id,(direct,p))::
           (id,(reverse,p))::list
         else error,id_set,(id,(direct,p))::list)
      (error,id_set,[])
      compil.Ast.rules
  in
  let error,_id_set,rules_rev =
    List.fold_left
      (fun (error,id_set,list) (id,(rule,position)) ->
         let error,id_set =
           match id with
           | None -> error,id_set
           | Some id -> check_freshness parameters error "Label" (fst id) id_set
         in
         let raw_lhs,raw_rhs,add,del = Ast.split_mixture rule.Ast.mix in
         let ast_lhs,ast_rhs = raw_lhs@del,raw_rhs@add in
         let prefix,tail_lhs,tail_rhs =
           (List.length raw_lhs, List.length del, List.length add) in
         let error,lhs =
           refine_mixture_in_rule parameters error prefix 0 tail_rhs ast_lhs in
         let error,rhs =
           refine_mixture_in_rule parameters error prefix tail_lhs 0 ast_rhs in
         let error,k_def =
           alg_with_pos_map (refine_mixture parameters) error rule.Ast.act in
         let error,k_un =
           alg_with_pos_with_option_map (refine_mixture parameters) error (Tools_kasa.fst_option rule.Ast.un_act) in
         let ast = dump_edit_rule rule in
         let ast_no_rate = dump_edit_rule_no_rate rule in
         let error,direct =
           error,
           {
             Ckappa_sig.position ;
             Ckappa_sig.prefix = prefix ;
             Ckappa_sig.delta = tail_lhs ;
             Ckappa_sig.lhs = lhs ;
             Ckappa_sig.rhs =  rhs ;
             Ckappa_sig.k_def = k_def ;
             Ckappa_sig.k_un = k_un ;
             Ckappa_sig.ast = ast ;
             Ckappa_sig.original_ast = ast ;
             Ckappa_sig.ast_no_rate = ast_no_rate ;
             Ckappa_sig.original_ast_no_rate = ast_no_rate ;
             Ckappa_sig.from_a_biderectional_rule = false ;
             Ckappa_sig.interprete_delta = Ckappa_sig.Direct ;
           }
         in
         error,id_set,(id,(Locality.dummy_annot direct))::list)
      (error,id_set',chem_rules_rev)
      compil.Ast.edit_rules
  in
  let error,init_rev =
    List.fold_left
      (fun (error,list) (id,alg_ex,init_t) ->
         let error,alg =
           alg_with_pos_map (refine_mixture parameters) error alg_ex in
         let error,init = refine_init_t parameters error init_t in
         error,(id,alg,init)::list)
      (error,[])
      compil.Ast.init in
  let error,perturbations_rev =
    List.fold_left
      (fun (error,list) ((alarm,b,m,o),p) ->
         let error,b' = match b with
           | None -> error,None
           | Some b ->
              let error,b' =
                bool_with_pos_map (refine_mixture parameters) error b in
              error,Some b'
         in
         let error,o' =
           bool_with_pos_with_option_map (refine_mixture parameters) error o
         in
         let error,m' =
           List.fold_left
             (fun (error,list) m ->
                match m with
                | Ast.INTRO (a,(m,p)) ->
                  let error,a' = alg_with_pos_map (refine_mixture parameters) error a in
                  let error,m' = refine_mixture parameters error (rev_ast m) in
                  error,Ast.INTRO(a',(m',p))::list
                | Ast.DELETE (a,(m,p)) ->
                  let error,a' = alg_with_pos_map (refine_mixture parameters) error a in
                  let error,m' = refine_mixture parameters error (rev_ast m) in
                  error,Ast.DELETE(a',(m',p))::list
                | Ast.UPDATE (x,y) ->
                  let error,y' = alg_with_pos_map (refine_mixture parameters) error y in
                  error,(Ast.UPDATE (x,y'))::list
                | Ast.STOP l ->
                  let error,l' =
                    List.fold_left
                      (fun (error,l) x ->
                         let error,x' = print_expr_map (refine_mixture parameters) error x in
                         error,(x'::l)
                      )
                      (error,[]) (List.rev l)
                  in
                  error,(Ast.STOP l')::list
                | Ast.SNAPSHOT l ->
                  let error,l' =
                    List.fold_left
                      (fun (error,l) x ->
                         let error,x' = print_expr_map (refine_mixture parameters) error x in
                         error,(x'::l)
                      )
                      (error,[]) (List.rev l)
                  in
                  error,(Ast.SNAPSHOT l')::list
                | Ast.UPDATE_TOK _ | Ast.PRINT _ | Ast.FLUX _
                | Ast.FLUXOFF _ | Ast.CFLOWMIX _ | Ast.PLOTENTRY
                | Ast.CFLOWLABEL _ | Ast.SPECIES_OF _ ->
                  error,list (*to do*))
             (error,[])
             m
         in
         error,((alarm,b',List.rev m',o'),p)::list
      )
      (error,[])
      compil.Ast.perturbations
  in
  error,{
    Ast.filenames = compil.Ast.filenames;
    Ast.variables = List.rev var_rev;
    Ast.signatures = List.rev signatures_rev;
    Ast.rules = List.rev rules_rev ;
    Ast.edit_rules = [] ;
    Ast.observables  = List.rev observables_rev;
    Ast.init = List.rev init_rev ;
    Ast.perturbations = List.rev perturbations_rev ;
    Ast.configurations = compil.Ast.configurations ;
    Ast.tokens = compil.Ast.tokens ;
    Ast.volumes = compil.Ast.volumes
  }
