 (**
  * preprocess.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  * 
  * Creation: 01/17/2011
  * Last modification: Time-stamp: <2015-11-11 06:13:21 feret>
  * * 
  * Translation from kASim ast to ckappa representation,
  *  
  * Copyright 2010,2011,2012,2013,2014, 2015 Institut National 
  * de Recherche en Informatique et en Automatique.  
  * All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Prepreprocess.ml") message exn (fun () -> default) 
  

let local_trace = false 

let check_freshness parameters error str id id_set = 
  let error,id_set = 
    if Mods.StringSet.mem id id_set 
    then 
      begin 
	warn parameters error (Some (str^" '"^id^"' is already used")) Exit id_set
      end 
    else 
      error,Mods.StringSet.add id id_set
  in 
  error,id_set
    
  
let add_entry parameters id agent site index (error,map) =
  let error,old_list = 
    Ckappa_sig.Int_Set_and_Map.Map.find_default_without_logs parameters error [] id map (* this is a partial map which stores the occurrences of binding labels *)
  in 
  Ckappa_sig.Int_Set_and_Map.Map.add_or_overwrite parameters error id ((agent,site,index)::old_list) map 

let rev_ast = List.rev 
(*mixture = 
  let rec aux mixture sol = 
    match mixture with
      | [] -> sol
(*      | Ast.DOT(i,agent,mixture) -> aux mixture (Ast.DOT(i,agent,sol))*)
(*      | Ast.PLUS(i,agent,mixture) -> aux mixture (Ast.PLUS(i,agent,sol))*)
      | agent :: mixture -> aux mixture (agent :: sol)
  in aux mixture []*)
  
let pop_entry parameters error id (map,set) =
  let error,list =  Ckappa_sig.Int_Set_and_Map.Map.find_option parameters error id map in 	
  if Ckappa_sig.Int_Set_and_Map.Set.mem id set 
  then
    match list with       
    | Some [a] ->
       let error,map = Ckappa_sig.Int_Set_and_Map.Map.remove parameters error id map in
       warn parameters error (Some "line 55, dandling bond detected\n") Exit (None,map)
    | Some [] ->  warn parameters error (Some "line 56, internal bug, link id is ignored") Exit (None,map) 
    | Some (h::t) ->
       let error,map = Ckappa_sig.Int_Set_and_Map.Map.overwrite parameters error id t map in
       warn parameters error (Some "line 57, internal bug, link id is ignored") Exit (None,map)
    | None ->
       warn parameters error (Some "line 58, internal bug, link id is ignored") Exit (None,map)
  else
    match list with
    | Some [a] ->
       let error,map = Ckappa_sig.Int_Set_and_Map.Map.remove parameters error id map in
       error,(Some a,map)
    | Some [b;a] ->
       let error,map = Ckappa_sig.Int_Set_and_Map.Map.overwrite parameters error id [a] map in
       error,(Some b,map)
    | Some (h::t) ->
       let error,map = Ckappa_sig.Int_Set_and_Map.Map.overwrite parameters error id t map in 
       warn parameters error (Some "line 69, too many instances of a link identifier, ignore them") Exit (None,map)
    | Some [] -> warn parameters error (Some "line 70, internal bug, link identifier") Exit (None,map)
    | None -> warn parameters error (Some "line 70, internal bug, link identifier") Exit (None,map)
		      
let rec scan_interface parameters k agent interface remanent = 
      match interface with 
      | [] -> remanent
      | port::interface -> 
	let (error,a),set = remanent in 
	let error,set = 
	  check_freshness parameters error "Site" (fst port.Ast.port_nme) set 
	in 
        let remanent = error,a in 
	scan_interface parameters k agent interface 
          ((match port.Ast.port_lnk with 
            | Ast.LNK_VALUE (i,()),_ -> 
                  add_entry parameters i agent (fst port.Ast.port_nme) k remanent
            | _ -> remanent),set)
              
let scan_agent parameters k agent remanent = 
  fst (scan_interface parameters k (fst (fst agent)) (snd agent) (remanent,Mods.StringSet.empty))

let rec collect_binding_label parameters mixture f k remanent =
  match mixture with
  | agent :: mixture (*| Ast.DOT (_,agent,mixture) | Ast.PLUS(_,agent,mixture)*) ->
     collect_binding_label parameters mixture f (k+1) (scan_agent parameters (f k) agent remanent)
  | [] -> remanent

let collect_binding_label parameters mixture f k remanent = 
  let error,map = collect_binding_label parameters mixture f k remanent in 
  Ckappa_sig.Int_Set_and_Map.Map.fold
    (fun x l (error,(map,set)) -> 
     if (List.length l = 1)
      then 
	let error,map = Ckappa_sig.Int_Set_and_Map.Map.remove parameters error x map in 
	let error,set = Ckappa_sig.Int_Set_and_Map.Set.add parameters error x set in
	warn parameters error (Some "line 100, dangling bond detected") Exit (map,set) 
      else 
	(error,(map,set)))
    map 
    (error,(map,Ckappa_sig.Int_Set_and_Map.Set.empty))

let translate_lnk_state parameters lnk_state remanent = 
    match lnk_state with 
     | Ast.LNK_VALUE (id,()),position ->  
       begin 
	 let error,remanent = remanent in 
	 let error,(triple,map) = pop_entry parameters error id remanent  in 
	 match triple with 
	 | None ->
	   let site = Ckappa_sig.LNK_SOME position in 
	   let remanent = 
	     warn parameters error (Some ("line 116... "^(Location.to_string position)^"one dandling bond has been replaced by a wild card")) Exit remanent 
	   in 
	   site,remanent
	 | Some (agent,site,index) -> 
	   if (agent,site,index) = ("","",0) 
	   then 
	     let site = Ckappa_sig.LNK_SOME position in 
	     let remanent = 
	       warn parameters error (Some "line 119") Exit remanent
	     in 
	     site,remanent
	   else
	     Ckappa_sig.LNK_VALUE (index,agent,site,id,position),(error,(map,(snd remanent)))
       end 
     | Ast.FREE,_ -> Ckappa_sig.FREE,remanent
     | Ast.LNK_ANY,position -> Ckappa_sig.LNK_ANY position,remanent 
     | Ast.LNK_SOME,position -> Ckappa_sig.LNK_SOME position,remanent
     | Ast.LNK_TYPE (x,y),position -> Ckappa_sig.LNK_TYPE (y,x),remanent

let translate_port parameters int_set port remanent = 
  let error,map = remanent in 
  let error,int_set = 
    check_freshness parameters error "Site" (fst (port.Ast.port_nme)) int_set 
  in 
  let remanent = error,map in 
  let lnk,remanent = translate_lnk_state parameters port.Ast.port_lnk remanent in   
   {
    Ckappa_sig.port_nme = fst (port.Ast.port_nme) ;
    Ckappa_sig.port_int = List.rev_map fst (List.rev port.Ast.port_int) ; 
    Ckappa_sig.port_lnk = lnk ;
       (*       port_pos = pos ; *)
    Ckappa_sig.port_free = 
      (match port.Ast.port_lnk 
       with Ast.FREE,_ -> Some true 
       | Ast.LNK_ANY,_ -> None 
       | Ast.LNK_SOME,_ 
       | Ast.LNK_TYPE _,_ 
       | Ast.LNK_VALUE _,_ -> Some false ) 
  },
  remanent

let rec translate_interface parameters int_set interface remanent =  
    match interface with 
     | [] -> Ckappa_sig.EMPTY_INTF,remanent
     | port::interface -> 
          let port,remanent = translate_port parameters int_set port remanent in 
          let interface,remanent = translate_interface parameters int_set interface remanent in 
          Ckappa_sig.PORT_SEP (port,interface),remanent  

let translate_interface parameters = translate_interface parameters Mods.StringSet.empty

let translate_agent parameters agent remanent = 
    let interface,remanent = translate_interface parameters (snd agent) remanent in 
    {Ckappa_sig.ag_nme = fst (fst agent); 
     Ckappa_sig.ag_intf = interface ;
     Ckappa_sig.ag_nme_pos = snd (fst agent);
(*     Ckappa_sig.ag_pos = position ;*)
    },
    remanent 

let rec build_skip k mixture = 
  if k=0 
  then mixture 
  else build_skip (k-1) (Ckappa_sig.SKIP(mixture)) 
  
let rec translate_mixture_zero_zero  parameters mixture remanent tail_size = 
   match mixture with 
     | [] -> build_skip tail_size Ckappa_sig.EMPTY_MIX,remanent
     | agent :: mixture ->
       let agent,remanent = translate_agent parameters agent remanent in 
       let mixture,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size  in 
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
   if prefix_size=0 
    then 
      let tail,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size
      in 
        build_skip empty_size tail,remanent 
   else 
      match mixture with 
      | [] -> Ckappa_sig.EMPTY_MIX,remanent
      | agent :: mixture ->
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_in_rule parameters mixture remanent (prefix_size-1) empty_size tail_size  in 
            Ckappa_sig.COMMA(agent,mixture),remanent 
(*      | Ast.DOT(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_in_rule parameters mixture remanent (prefix_size-1) empty_size tail_size  in 
            Ckappa_sig.DOT(i,agent,mixture),remanent
      | Ast.PLUS(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_in_rule parameters mixture remanent (prefix_size-1) empty_size tail_size  in 
            Ckappa_sig.PLUS(i,agent,mixture),remanent*)

 let rec translate_mixture parameters mixture remanent  = 
    match mixture with 
      | [] -> Ckappa_sig.EMPTY_MIX,remanent
      | agent :: mixture ->
          let agent,remanent = translate_agent parameters agent remanent in 
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
 
let support_agent ag = 
  let name = fst (fst ag) in 
  let list = 
    let rec scan intf list = 
      match intf with 
      | [] -> List.sort compare list 
      | port::intf -> 
	scan intf ((fst port.Ast.port_nme)::list)
    in
    scan (snd ag) []
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
     let f i =  if i>prefix_size then i+empty_size else i in 
     let remanent = collect_binding_label parameters mixture f 0 (error,Ckappa_sig.Int_Set_and_Map.Map.empty) in  
     let mixture,(error,map) = 
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
     let remanent = collect_binding_label parameters mixture (fun i -> i) 0 (error,Ckappa_sig.Int_Set_and_Map.Map.empty) in  
     let mixture,(error,map) = translate_mixture parameters mixture remanent in
    error,mixture


let rec alg_map f error alg = 
  match 
    alg 
  with 
  | Ast.BIN_ALG_OP (op,(m1,pos1),(m2,pos2)) -> 
    let error,m1' = alg_map f error m1 in 
    let error,m2' = alg_map f error m2 in 
    error,Ast.BIN_ALG_OP (op,(m1',pos1),(m2',pos2))
  | Ast.UN_ALG_OP (op,(m1,pos1)) -> 
    let error,m1' = alg_map f error m1 in 
    error,Ast.UN_ALG_OP (op,(m1',pos1)) 
  | Ast.STATE_ALG_OP s -> error,Ast.STATE_ALG_OP s 
  | Ast.OBS_VAR s -> error,Ast.OBS_VAR s
  | Ast.TOKEN_ID s -> error,Ast.TOKEN_ID s 
  | Ast.KAPPA_INSTANCE mixture -> 
    let error,mixture' = f error mixture in 
    error,Ast.KAPPA_INSTANCE mixture'
  | Ast.CONST x -> error,Ast.CONST x
  | Ast.TMAX -> error,Ast.TMAX
  | Ast.EMAX -> error,Ast.EMAX 
  | Ast.PLOTNUM -> error,Ast.PLOTNUM 

let rec print_expr_map  f error alg = 
  match 
    alg 
  with 
  | Ast.Str_pexpr(s) -> error,Ast.Str_pexpr(s) 
  | Ast.Alg_pexpr (alg,pos) -> 
    let error,alg' = alg_map f error alg in 
    error,Ast.Alg_pexpr (alg',pos) 

let map_with_pos map = 
  (fun f error (x,pos) -> 
    let error,x' = map f error x in 
    error,(x',pos) )

let alg_with_pos_map = map_with_pos alg_map 

let rec bool_map f error alg = 
  match 
    alg 
  with 
  | Ast.TRUE -> error,Ast.TRUE
  | Ast.FALSE -> error,Ast.FALSE
  | Ast.BOOL_OP(Operator.AND,(b1,pos1),(b2,pos2)) -> 
    let error,b1' = bool_map f error b1 in 
    let error,b2' = bool_map f error b2 in 
    error,Ast.BOOL_OP(Operator.AND,(b1',pos1),(b2',pos2))
  | Ast.BOOL_OP(Operator.OR,(b1,pos1),(b2,pos2)) -> 
    let error,b1' = bool_map f error b1 in 
    let error,b2' = bool_map f error b2 in 
    error,Ast.BOOL_OP(Operator.OR,(b1',pos1),(b2',pos2))
  | Ast.COMPARE_OP(Operator.GREATER,(m1,pos1),(m2,pos2)) -> 
    let error,m1' = alg_map f error m1 in 
    let error,m2' = alg_map f error m2 in 
    error,Ast.COMPARE_OP(Operator.GREATER,(m1',pos1),(m2',pos2))
  | Ast.COMPARE_OP(Operator.SMALLER,(m1,pos1),(m2,pos2)) -> 
    let error,m1' = alg_map f error m1 in 
    let error,m2' = alg_map f error m2 in 
    error,Ast.COMPARE_OP(Operator.SMALLER,(m1',pos1),(m2',pos2))
  | Ast.COMPARE_OP(Operator.EQUAL,(m1,pos1),(m2,pos2)) -> 
    let error,m1' = alg_map f error m1 in 
    let error,m2' = alg_map f error m2 in 
    error,Ast.COMPARE_OP(Operator.EQUAL,(m1',pos1),(m2',pos2))
  | Ast.COMPARE_OP(Operator.DIFF,(m1,pos1),(m2,pos2)) -> 
    let error,m1' = alg_map f error m1 in 
    let error,m2' = alg_map f error m2 in 
    error,Ast.COMPARE_OP(Operator.DIFF,(m1',pos1),(m2',pos2))

let rec modif_map f_forbidding_question_marks f_allowing_question_marks error alg = 
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
   | Ast.FLUX list ->
     let error,list' = 
       List.fold_left 
	 (fun (error,list) elt -> 
	   let error,elt' = print_expr_map f_allowing_question_marks error elt in
	   error,elt'::list)
	 (error,[]) (List.rev list)
     in 
	error,Ast.FLUX list'
   | Ast.FLUXOFF list ->
     let error,list' = 
       List.fold_left 
	 (fun (error,list) elt -> 
	   let error,elt' = print_expr_map f_allowing_question_marks error elt in
	   error,elt'::list)
	 (error,[]) (List.rev list)
     in 
	error,Ast.FLUXOFF list'


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
  let error,token = warn parameters error (Some ("Line 431: Token are not implemented in KaSa yet")) Exit token in 
  error,token 
  
let refine_init_t parameters error init_t = 
  match 
    init_t 
  with 
  | Ast.INIT_MIX(alg_ex,(mixture,pos)) -> 
    let error,alg_ex = alg_with_pos_map (refine_mixture parameters) error alg_ex in 
    let error,mixture = refine_mixture parameters error mixture in 
      error,Some(Ast.INIT_MIX(alg_ex,(mixture,pos)))
  | Ast.INIT_TOK (alg_ex,token) -> 
    let error,alg_ex = alg_with_pos_map (refine_mixture parameters) error alg_ex in 
    let error,token = refine_token parameters error token in 
    error,Some(Ast.INIT_TOK(alg_ex,token))


let refine_agent parameters error agent_set agent =
  let error,agent_set = check_freshness parameters error "Agent" (fst (fst agent)) agent_set in 
  let error,map = scan_agent parameters 0 agent (error,Ckappa_sig.Int_Set_and_Map.Map.empty) in 
  
  let agent,(error,map) = translate_agent parameters agent (error,(map,Ckappa_sig.Int_Set_and_Map.Set.empty)) in 
  error,agent_set,agent 



let refine_var parameters error id_set var = 
  match var with 
  | ((string,pos),(alg,pos')) -> 
    let error,id_set = check_freshness parameters error "Label" string id_set in 
    let error,alg' = alg_map (refine_mixture parameters) error alg  
    in error,id_set,((string,pos),(alg',pos'))
 
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
  let error,agent_set,signatures_rev = 
    List.fold_left 
      (fun  (error,agent_set,list) agent-> 
        let error,agent_set,agent = refine_agent parameters error agent_set agent in 
        error,agent_set,(agent::list))
      (error,agent_set,[])
      compil.Ast.signatures       
  in
  let error,observables_rev = 
    List.fold_left 
      (fun (error,list) alg -> 
	let error,alg' = alg_with_pos_map (refine_mixture parameters) error alg in 
	error,alg'::list)
      (error,[])
      compil.Ast.observables
  in 
  let error,id_set,rules_rev = 
     List.fold_left
      (fun (error,id_set,list) (id,(rule,p)) ->
	let error,id_set = 
	  match id with 
	  | None -> error,id_set 
	  | Some id -> check_freshness parameters error "Label" (fst id) id_set 
	in 
        let ast_lhs,ast_rhs = rule.Ast.lhs,rule.Ast.rhs in 
        let prefix,tail_lhs,tail_rhs = longuest_prefix ast_lhs ast_rhs in 
        let error,lhs = refine_mixture_in_rule parameters error prefix 0 tail_rhs ast_lhs in 
        let error,rhs = refine_mixture_in_rule parameters error prefix tail_lhs 0 ast_rhs in 
	let error,k_def = alg_with_pos_map (refine_mixture parameters) error rule.Ast.k_def in 
	let error,k_un = alg_with_pos_with_option_map (refine_mixture parameters) error (Tools_kasa.fst_option rule.Ast.k_un) in 
        let error,direct = 
          error,
          {
	    Ckappa_sig.prefix = prefix ;
	    Ckappa_sig.delta = tail_lhs ;
            Ckappa_sig.lhs = lhs ;
            Ckappa_sig.rhs =  rhs ;
            Ckappa_sig.arrow = rule.Ast.arrow ;
            Ckappa_sig.k_def = k_def ;
            Ckappa_sig.k_un = k_un ; 
          }
        in 
        match rule.Ast.arrow 
        with 
        | Ast.RAR  -> 
          error,id_set,(id,((Ckappa_sig.Direct,direct),p))::list
        | Ast.LRAR  -> 
          let error,reverse = 
            error,{direct 
             with Ckappa_sig.lhs = rhs; 
               Ckappa_sig.rhs = lhs; 
               Ckappa_sig.arrow = Ast.RAR ; 
            }
          in 
            error,id_set,
	  (id,((Ckappa_sig.Reverse,reverse),p))::
	    (id,((Ckappa_sig.Direct,({direct with Ckappa_sig.arrow = Ast.RAR })),p))::list)
    (error,id_set,[])
    compil.Ast.rules     
  in 
  let error,init_rev = 
     List.fold_left
      (fun (error,list) (id,init_t) -> 
        let error,mixture = refine_init_t parameters error init_t in 
        match mixture 
        with 
        |  Some (init) ->  error,(id,init)::list
        | None -> error,list)
    (error,[])
    compil.Ast.init    
  in 
  let error,perturbations_rev = 
    List.fold_left
      (fun (error,list) ((b,m,o),p) -> 
	let error,b' = 
	  bool_with_pos_map (refine_mixture parameters) error b 
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
	      | _ -> error,list (*to do*))
            (error,[])
            m
        in
        error,((b',List.rev m'(*,p*),o'),p)::list
      )
    (error,[])
    compil.Ast.perturbations
  in 
  error,{
    Ast.variables = List.rev var_rev;
    Ast.signatures = List.rev signatures_rev;
    Ast.rules = List.rev rules_rev ;      
    Ast.observables  = List.rev observables_rev;
    Ast.init = List.rev init_rev ;      
    Ast.perturbations = List.rev perturbations_rev ;
    Ast.configurations = compil.Ast.configurations ;
    Ast.tokens = compil.Ast.tokens ;
    Ast.volumes = compil.Ast.volumes 
   }

   
