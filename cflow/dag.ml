(**
  * dag.ml
  *
  * Dag computation and canonical form
  *
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * KaSim
  * Jean Krivine, Université Paris Diderot, CNRS
  *
  * Creation: 22/03/2012
  * Last modification: 18/06/2013
  * *
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation
  *
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique
  * et en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module S = Generic_branch_and_cut_solver.Solver

let warn parameter error option exn default =
       Exception.warn (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error (Some "dag.ml") option exn (fun () -> default)

module type StoryTable =
  sig
    type table

    val fold_table:
      ((S.PH.B.PB.CI.Po.K.refined_step list,StoryProfiling.StoryStats.log_info Mods.simulation_info list,'a,'a) S.PH.B.PB.CI.Po.K.H.ternary, table, 'a, 'a) S.PH.B.PB.CI.Po.K.H.ternary
    val init_table: table S.PH.B.PB.CI.Po.K.H.zeroary
    val count_stories: table -> int
    val add_story: (Causal.grid,S.PH.B.PB.CI.Po.K.refined_step list,StoryProfiling.StoryStats.log_info Mods.simulation_info list,table,table) S.PH.B.PB.CI.Po.K.H.quaternary
    val hash_list: (table, table) S.PH.B.PB.CI.Po.K.H.unary

    val sort_list: (table, (Causal.grid * StoryProfiling.StoryStats.log_info Mods.simulation_info list) list) S.PH.B.PB.CI.Po.K.H.unary
  end

module H=S.PH.B.PB.CI.Po.K.H
module A=Mods.DynArray

type label = string
type node_kind = OBS | PERT | RULE | INIT | FICTITIOUS
type node = node_kind * label

type graph =
  {
    root: int;
    labels: node A.t ;
    pred: int list A.t ;
    conflict_pred: int list A.t;
   }

let dummy_graph =
  {
    root = 0 ;
    labels = A.make 1 (FICTITIOUS,"") ;
    pred = A.make 1 [] ;
    conflict_pred = A.make 1 [] ;
  }

type edge_kind = Succ | Conflict
type position = int
type key =
  | Fresh of node
  | Former of position
  | Stop

type canonical_form = key list
type prehash = (node*int) list

let dummy_cannonical_form = []
let dummy_prehash = []

let print_graph parameter handler error graph =
  let _ = Format.fprintf parameter.H.out_channel "****@\ngraph@\n****" in
  let _ = Format.fprintf parameter.H.out_channel "Root: %i@\n" graph.root in
  let _ = Format.fprintf parameter.H.out_channel "Labels:@\n" in
  let _ = A.iteri (fun i (_,j) -> Format.fprintf parameter.H.out_channel "Node %i,Label %s@\n" i j) graph.labels in
  let _ =
    A.iteri
      (fun i l   ->
       List.iter (Format.fprintf parameter.H.out_channel "%i <- %i@\n" i) l
      )
      graph.pred
  in
  let _ = Format.fprintf parameter.H.out_channel "Conflicts:@\n" in
  let _ =
    A.iteri
      (fun i l  ->
       List.iter
         (Format.fprintf parameter.H.out_channel "%i |--  %i@\n" i)
         l
      )
      graph.conflict_pred
  in
  let _ = Format.fprintf parameter.H.out_channel "****@\n@\n" in
  error

let print_elt log elt =
  match
    elt
  with
  | Stop -> Format.fprintf log "STOP@\n"
  | Former i -> Format.fprintf log "Pointer %i@\n" i
  | Fresh (_,s) -> Format.fprintf log "Event %s@\n" s

let print_canonical_form parameter handler error dag =
  let _ =
    List.iter
      (print_elt parameter.H.out_channel_err)
      dag
  in
  let _ = Format.fprintf parameter.H.out_channel_err "@." in
  error

let print_prehash parameter handler error representation =
  let _ =
    List.iter
      (fun ((_,b),i) -> Format.fprintf parameter.H.out_channel_err "%s:%i," b i)
      representation
  in
  let _ = Format.fprintf parameter.H.out_channel_err "@." in
  error

let label handler x =
  match x
  with
  | Causal.RULE i -> H.string_of_rule_id handler i
  | Causal.INIT [i] -> H.string_of_agent_id handler i
  | _ -> Causal.label ~env:handler.H.env x

let kind node =
  match node
  with
  | Causal.INIT _ -> INIT
  | Causal.RULE _ -> RULE
  | Causal.PERT _ -> PERT
  | Causal.OBS _ -> OBS

let compare_elt x y =
  match x,y with
  | Stop,Stop -> 0
  | Stop,_ -> -1
  | _,Stop -> +1
  | Former i, Former j -> compare i j
  | Former _,_ -> -1
  | _,Former _ -> +1
  | Fresh s,Fresh s' -> compare s s'

let quick_compare g t1 t2 = compare_elt (g t1) (g t2)

let rec aux compare_elt l1 l2 =
  match l1,l2
  with
  | [],[] -> 0
  | [], _ -> -1
  | _ ,[] -> +1
  | t::q,t'::q' ->
     let cmp = compare_elt t t' in
     if cmp = 0
     then aux compare_elt q q'
     else cmp

let compare_canonic = aux compare_elt
let compare_canonic_opt x y =
  match x,y
  with
  | None,None -> 0
  | None,_ -> -1
  | _,None -> +1
  |Some x,Some y -> compare_canonic x y

let compare_prehash = aux compare

let graph_of_grid parameter handler log_info error grid =
  let error,log_info = StoryProfiling.StoryStats.add_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error StoryProfiling.Graph_conversion None log_info in
  let ids = Hashtbl.fold (fun key _ l -> key::l) grid.Causal.flow [] in
  let label = label handler in
  let error,log_info,config = Causal.cut (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) handler log_info error ids grid in
  match Mods.IntMap.max_key config.Causal.prec_1
  with None -> error,log_info,dummy_graph
     | Some size ->
	let succ_size = succ size in
	let labels = A.make succ_size (FICTITIOUS,"") in
	let pred = A.make succ_size [] in
	let conflict_pred = A.make succ_size [] in
	let () =
	  Mods.IntMap.iter
	    (fun i atom_kind ->
	     A.set labels i (kind atom_kind,label atom_kind)
	    )
	    config.Causal.events_kind
	in
	let () =
	  Mods.IntMap.iter
	    (fun i s ->
	     if Mods.IntSet.is_empty s
	     then ()
	     else
	       A.set pred i (Mods.IntSet.elements s))
	    config.Causal.prec_1
	in
	let () =
	  Mods.IntMap.iter
	    (fun i s  ->
	     if Mods.IntSet.is_empty s
	     then ()
	     else
               A.set conflict_pred i (Mods.IntSet.elements s))
	    config.Causal.conflict
	in
	let root = size	in
	let error,log_info = StoryProfiling.StoryStats.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error StoryProfiling.Graph_conversion None log_info in
	  error,
	  log_info,
	  {
	    root = root;
	    labels = labels ;
	    pred = pred ;
	    conflict_pred = conflict_pred
	  }
	
let concat list1 list2 =
  let rec aux list1 list2 =
    match list2
    with
    | [] -> list1
    | t::q -> aux (t::list1) q
  in
  aux list2 (List.rev list1)

let compare_node (a,_) (b,_) = compare a b
				
let smash l =
  let rec aux l former weight output =
    match l
    with
    | [] -> List.rev ((former,weight)::output)
    | (t,wt)::q when t = former -> aux q former (wt+weight) output
    | (t,wt)::q -> aux q t wt ((former,weight)::output)
  in
  match l
  with
  | [] -> []
  | (t,wt)::q -> aux q t wt []
		
let prehash parameter handler error graph =
  error,
  smash
    (List.sort
       compare_node
       (let l=ref [] in
        let _ =
          A.iter
            (function
              | FICTITIOUS,_ -> ()
              | (OBS|PERT|RULE|INIT),_ as a -> l:=(a,1)::(!l))
            graph.labels
        in
        !l))

let canonicalize parameter handler log_info error graph =
  let asso = Mods.IntMap.empty in
  let label i =
    try
      A.get graph.labels i
    with
    | _ -> FICTITIOUS,""
  in
  let rec pop (candidate:key list) (to_beat:key list option) =
    match
      candidate,to_beat
    with
    | [],_ | _,None -> Some to_beat  (* candidate is a prefix of to_beat, we output the suffix *)
    | _::_, Some [] -> None (* the candidate is worse *)
    | t::q,Some (tr::qr) ->
       let cmp = compare_elt t tr in
       if cmp < 0
       then
         Some None (* the candidate is better *)
       else if cmp = 0 then
         pop q (Some qr) (* we do not know, we look further *)
       else
         None (* the candidate is worse *)
  in
  let rec visit (i:int) (map:int Mods.IntMap.t) (fresh_pos:int) (to_beat:key list option) =
    match
      Mods.IntMap.find_option i map
    with
    | Some i -> (* the node has already been seen, we put a pointer *)
       begin (*0*)
         match
           pop [Former i] to_beat
         with
         | None ->  (* to beat is better, we cut the construction *)
            None
         | Some to_beat -> (* to beat may be improved, we go on *)
            Some ([Former i],map,fresh_pos,to_beat)
       end (*0*)
    | None -> (* the node is seen for the first time *)
       let map = Mods.IntMap.add i fresh_pos map in
       let fresh_pos = fresh_pos + 1 in
       begin (*0*)
         match
           pop [Fresh (label i)] to_beat
         with
         | None -> None
         | Some (to_beat:key list option) ->
            begin (*1*)
              let sibbling1 =
                try
                  A.get graph.pred i
                with
                | Not_found -> []
              in
              let sibbling2 =
                try
                  A.get graph.conflict_pred i
                with
                | Not_found -> []
              in
              let rec best_sibbling (m:int Mods.IntMap.t) (f:int) (candidates:int list) (not_best:int list) (to_beat:key list option) (record: (int * (key list * int Mods.IntMap.t * int * key list option)) option) =
                match candidates
                with
                | [] ->
                   begin (*2*)
                     match
                       record
                     with
                     | None -> None
                     | Some record -> Some (not_best,record)
                   end (*2*)
                | t::q ->
                   let rep =  visit t m f to_beat in
                   begin (*2*)
                     match
                       rep
                     with
                     | None -> best_sibbling m f q (t::not_best) to_beat record
                     | Some ((encoding:key list),map,fresh,residue) ->
                        begin (*3*)
                          let (to_beat_after:key list option) =
                            match
                              residue
                            with
                            | None ->
                               Some encoding
                            | _ ->
                               to_beat
                          in
                          let (not_best:int list) =
                            match
                              record
                            with
                            | None -> not_best
                            | Some (best,_) -> best::not_best
                          in
                          best_sibbling
                            m
                            f
                            q
                            not_best
                            to_beat_after
                            (Some (t,(encoding,map,fresh,residue)))
                        end (*3*)
                   end (*2*)
              in
              let rec aux m f l sol to_beat =
                match
                  l
                with
                | [] -> Some (m,f,sol,to_beat)
                | _ ->
                   begin (*2*)
                     match
                       best_sibbling m f l [] to_beat None
                     with
                     | None -> None
                     | Some (not_best,record) ->
                        let (_,(best,map,fresh_pos,to_beat_after)) = record  in
                        aux map fresh_pos not_best (concat sol best) to_beat_after
                   end (*2*)
              in
              let list = [Fresh (label i)] in
              begin (*2*)
                let g x =
                  match Mods.IntMap.find_option x map with
		  | Some x -> Former x
                  | None -> Fresh (label x)
                in
                let sibbling1 = List.sort (quick_compare g) sibbling1 in
                match
                  aux map fresh_pos sibbling1 list to_beat
                with
                | None -> None
                | Some (map,fresh_pos,list,to_beat) ->
                   begin (*3*)
                     match
                       pop [Stop] to_beat
                     with
                     | None -> None
                     | Some to_beat ->
                        begin (*4*)
                          let list = concat list [Stop] in
                          begin (*5*)
			    let g x =
			      match Mods.IntMap.find_option x map with
			      | Some x -> Former x
			      | None -> Fresh (label x)
			    in
                            let sibbling2 = List.sort (quick_compare g) sibbling2 in
                            match
                              aux map fresh_pos sibbling2 list to_beat
                            with
                            | None -> None
                            | Some (map,fresh_pos,list,to_beat) ->
                               begin (*6*)
                                 match
                                   pop [Stop] to_beat
                                 with
                                 | None -> None
                                 | Some to_beat ->
                                    let list = concat list [Stop] in
                                    Some (list,map,fresh_pos,to_beat)
                               end (*6*)
                          end (*5*)
                        end (*4*)
                   end (*3*)
              end (*2*)
            end (*1*)
       end (*0*)
  in
  let error, log_info = StoryProfiling.StoryStats.add_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error StoryProfiling.Cannonic_form_computation None log_info in
  let error, output =
    match
      visit graph.root asso 0 None
    with
    | Some (rep,_,_,_) -> error,rep
    | None -> error,[]
  in
  let error, log_info = StoryProfiling.StoryStats.close_event (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error StoryProfiling.Cannonic_form_computation None log_info in
  error,log_info,output
		
let dot_of_graph parameter handler error graph = error
		
		
module ListTable =
  (
    struct
      type table = (prehash * (Causal.grid * graph * canonical_form option * S.PH.B.PB.CI.Po.K.refined_step list  * StoryProfiling.StoryStats.log_info Mods.simulation_info list) list) list
																					
      let init_table parameter handler log_info error =
	error,log_info,[]
		
      let add_story parameter handler log_info error grid pretrace info table =
	let error,log_info,graph = graph_of_grid parameter handler log_info error grid in
	let error,prehash = prehash parameter handler error graph in
	error,log_info,(prehash,[grid,graph,None,pretrace,info])::table
			

      let sort_outer =
        let compare (a,_) (b,_) = compare_prehash a b in
        List.sort compare

      let sort_inner =
        let compare (_,_,a,_,_) (_,_,b,_,_) = compare_canonic_opt a b in
        List.sort compare

      let hash_inner parameter handler error cmp list =
        let list = sort_inner list in
        let rec visit elements_to_store stored_elements last_element last_element_occurrences =
          match elements_to_store,last_element
          with
            | (_,_,t,_,list)::q,Some (_,_,old,_) when compare t old = 0 ->
              visit
                q
                stored_elements
                last_element
                (List.fold_left
                   (fun list a -> a::list)
                   list
                   last_element_occurrences)
            | (grid,graph,t,event,list)::q,Some (grid',graph',a,event') ->

              visit q ((grid',graph',a,event',List.sort cmp last_element_occurrences)::stored_elements) (Some (grid,graph,t,event)) ((*List.rev*) list)
            | (grid,graph,t,event,list)::q,None ->
              visit q stored_elements (Some (grid,graph,t,event)) (List.rev list)
            | [],None -> []
            | [],Some (grid,graph,a,event) ->
              List.rev ((grid,graph,a,event,List.sort cmp last_element_occurrences)::stored_elements)
        in
        let list = visit list [] None [] in
        error,list

      let hash_list parameter handler log_info error list =
        let list = sort_outer (List.rev list) in
        let rec visit elements_to_store stored_elements last_element last_element_occurrences =
          match elements_to_store,last_element
          with
            | ((t:prehash),list)::q,Some old when compare t old = 0 ->
              visit q stored_elements last_element
                (List.fold_left
                   (fun list a -> a::list) list last_element_occurrences)
            | (t,list)::q,Some a ->
              visit q ((a,last_element_occurrences)::stored_elements) (Some t) list
            | (t,list)::q,None ->
              visit q stored_elements (Some t) (List.rev list)
            | [],None -> []
            | [],Some a ->
              List.rev ((a,last_element_occurrences)::stored_elements)
        in
        let list = visit list [] None [] in
        let rec visit2 l log_info error acc =
          match l
          with
            | []   -> error,log_info,acc
            | (t,list)::q ->
              if List.length list = 1
              then visit2 q log_info error ((t,list)::acc)
              else
                let error,log_info,list' =
                  List.fold_left
                    (fun (error,log_info,list') (grid,graph,dag,b,c) ->
                      let error,log_info,dag' =
                        match dag
                        with
                          | None -> canonicalize parameter handler log_info error graph
                          | Some dag -> error,log_info,dag
                      in
                      (error,log_info,(grid,graph,Some dag',b,c)::list')
                    )
		    (error,log_info,[]) list
                in
                let error,list' =
                  hash_inner parameter handler error Mods.compare_profiling_info list'
                in
                visit2 q log_info error ((t,list')::acc)
        in
        let error,log_info,list = visit2 list log_info error [] in
        error,log_info,list

      let project_tuple (grid,_,_,_,list) =
        List.hd list,grid,list

      let sort_list parameter handler log_info error list =
        let flat_list =
          List.fold_left
            (fun list_out (prehash,list) ->
              List.fold_left
                (fun list_out tuple ->
                  (project_tuple tuple)::list_out
                )
                list_out list)
            [] list
        in
        let compare_pair (a,_,_) (c,_,_) = Mods.compare_profiling_info a c in
        let flat_list = List.sort compare_pair flat_list in
         error, log_info, List.rev_map (fun (a,b,c) -> b,c) (List.rev flat_list)

      let count_stories list =
	List.fold_left
	  (fun n l -> n + List.length (snd l))
	  0
	  list
	
      let fold_table parameter handler log_info (error:Exception.method_handler) g list a  =
	List.fold_left
	  (fun a (_,l) ->
	  List.fold_left
	    (fun (error,log_info,a) (_,_,_,x,y) -> g parameter handler log_info error x y a)
	    a
	    l)
	  (error,log_info,a)
	  (List.rev list)
	
	  end:StoryTable)

module BucketTable =
  (struct
      type story_id = int
      let first_story_id = 0
      let succ_story_id = succ
      type prehash_elt = node * int

      module KeyS = (SetMap.Make (struct type t = key let compare = compare end))
      module PreHashS = (SetMap.Make (struct type t = prehash_elt let compare = compare end))
      module KeyMap = KeyS.Map
      module PreHashMap = PreHashS.Map

      type inner_tree =
	| Inner_node of (inner_tree KeyMap.t * story_id option)
	| Inner_leave of (key list * story_id)
			

      type outer_tree =
      | Empty
      | Outer_node of (outer_tree PreHashMap.t * story_id option)
      | Outer_leave of (prehash_elt list * story_id)
      | To_inner of (outer_tree PreHashMap.t * inner_tree)
	
      type table =
	{
	  tree: outer_tree;
	  array: (Causal.grid * graph * canonical_form option * S.PH.B.PB.CI.Po.K.refined_step list * StoryProfiling.StoryStats.log_info Mods.simulation_info list)  Int_storage.Nearly_inf_Imperatif.t;
	  fresh_id: story_id
	}


      let init_table parameters handler log_info error=
	let error,array =  Int_storage.Nearly_inf_Imperatif.create (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameters) error 0 in
	error,log_info,{
	  tree= Empty;
	  array= array;
	  fresh_id= 0 }

      let get_cannonical_form parameter handler log_info error id table =
	let error,assoc =
	  Int_storage.Nearly_inf_Imperatif.get
	    (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
	    error
	    id
	    table.array
	in
	match assoc
	with
	  None ->
	  let error,a = warn parameter error (Some "get_cannonical_form, line 767, unknown story id") (Failure "Inconsistent story id") (table,[]) in
	  error,log_info,a
	| Some (_,_,Some cannonic,_,_) ->
	   error,log_info,(table,cannonic)
	| Some (grid,graph,None,trace,info) ->
	   let error,log_info,cannonic = canonicalize parameter handler log_info error graph in
	   let error,array' = Int_storage.Nearly_inf_Imperatif.set
	     (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters  parameter) error id (grid,graph,Some cannonic,trace,info) table.array in
	   let table = {table with array = array'} in
	   error,log_info,(table,cannonic)
			
      let add_story  parameter handler log_info error grid pretrace story_info table =
	let error,log_info,graph = graph_of_grid parameter handler log_info error grid in
	let error,prehash = prehash parameter handler error graph in
	let assoc = (grid,graph,None,pretrace,story_info) in
	let add_story error x table =
	  let error,array = Int_storage.Nearly_inf_Imperatif.set (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error table.fresh_id x table.array in
	  error,table.fresh_id,
	  {table
	  with
	    array = array ;
	    fresh_id = succ_story_id table.fresh_id}
	in
	let add_story_info error story_info id table =
	  let error,asso_opt = Int_storage.Nearly_inf_Imperatif.get (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error id table.array in
	  match
	    asso_opt
	  with
	  | None -> warn parameter error (Some "add_story_info, line 800, Unknown story id") (Failure "Unknown story id") table
	  | Some (grid,graph,canonic,trace,info) ->
	     let error,array = Int_storage.Nearly_inf_Imperatif.set (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter) error id (grid,graph,canonic,trace,story_info@info) table.array in
	     error,{table with array = array}
	in
	let update_assoc error canonic_form assoc =
	  match
	    assoc
	  with
	  | (grid,graph,None,trace,info) -> error,(grid,graph,Some canonic_form,trace,info)
	  | (_,_,Some _,_,_) ->
			warn parameter error (Some "update_assoc, line 812, the canonical form of this story should not have been computed yet") (Failure "the canonical form of stories  should not have been computed yet") assoc
	in
	let rec aux_inner2 log_info error canonic_form canonic_form' id' assoc table =
	  match canonic_form,canonic_form'
	  with
	  | [],[] ->
	     let error,table = add_story_info error story_info id' table in
	     error,log_info,table,Inner_leave ([],id')
	  | t::q,[] ->
	     let error,id,table = add_story error assoc table in
	     error, log_info, table, Inner_node (KeyMap.add t (Inner_leave(q,id))  KeyMap.empty,Some id')
	  | [],t'::q' ->
	     let error,id,table = add_story error assoc table in
	     error,log_info,table, Inner_node (KeyMap.add t' (Inner_leave(q',id')) KeyMap.empty,Some id)
	  | t::q,t'::q' when t=t' ->
	     let error,log_info,table,tree = aux_inner2 log_info error q q' id' assoc table in
	     error,log_info,table, Inner_node (KeyMap.add t tree KeyMap.empty,None)
	  | t::q,t'::q' ->
	     let error,id,table = add_story error assoc table in
	     error,log_info,table, Inner_node (KeyMap.add t (Inner_leave(q,id)) (KeyMap.add t' (Inner_leave(q',id')) KeyMap.empty),None)
	in
	let rec aux_outer2 log_info error prehash prehash' id' table =
	  match
	    prehash,prehash'
	  with
	  | [],[] ->
	     let error,log_info,cannonic_form = canonicalize parameter handler log_info error graph in
	     let error,assoc = update_assoc error cannonic_form assoc in
	     let error,log_info,(table,cannonic_form') = get_cannonical_form parameter handler log_info error id' table in
	     let error,log_info,table,inner = aux_inner2 log_info error cannonic_form cannonic_form' id' assoc table in
	     error,log_info,table,To_inner (PreHashMap.empty,  inner)
	  | t::q,[] ->
	     let error,id,table = add_story error assoc table in
	     error,log_info,table, Outer_node (PreHashMap.add t (Outer_leave(q,id)) PreHashMap.empty,Some id')
	  | [],t'::q' ->
	     let error,id,table = add_story error assoc table in
	     error,log_info,table, Outer_node (PreHashMap.add t' (Outer_leave(q',id')) PreHashMap.empty,Some id)
	  | t::q,t'::q' when t=t' ->
	     let error,log_info,table,tree = aux_outer2 log_info error q q' id' table in
	     error,log_info,table, Outer_node (PreHashMap.add t tree PreHashMap.empty,None)
	  | t::q,t'::q' ->
	     let error,id,table = add_story error assoc table in
	     error,log_info,table, Outer_node (PreHashMap.add t (Outer_leave(q,id)) (PreHashMap.add t' (Outer_leave(q',id')) PreHashMap.empty),None)
	in
	let rec aux_inner log_info error assoc story_info suffix inner_tree table =
	   match
	     suffix
	   with
	   | [] ->
	      begin
		match
		  inner_tree
		with
		| Inner_node (map,None) ->
		   let error,id,table = add_story error assoc table in
		   error,log_info,table,
		   Inner_node (map , (Some id))
		| Inner_node (_,Some id')
		| Inner_leave ([],id') ->
		   let error,table = add_story_info error story_info id' table in
		   error,log_info,table,inner_tree
		| Inner_leave (t'::q',id') ->
		   let error,id,table = add_story error assoc table in
		   error,log_info,table,Inner_node (KeyMap.add t' (Inner_leave (q',id')) KeyMap.empty,Some id)
	      end
	   | t::q ->
	      begin
		match
		  inner_tree
		with
		| Inner_node (map,assoc') ->
		   begin
		     match
		       KeyMap.find_option t map
		     with
		     | None ->
			let error,id,table = add_story error assoc table in
			let inner_tree =
			  Inner_node (KeyMap.add t (Inner_leave (q,id)) map,assoc')
			in
			error,log_info,table,inner_tree
		     | Some (inner_tree')  ->
			let error,log_info,table',inner_tree'' = aux_inner log_info error assoc story_info q inner_tree' table in
			if inner_tree'' == inner_tree'
			then error,log_info,table',inner_tree
			else
			  error,log_info,table',Inner_node(KeyMap.add t inner_tree'' map,assoc')
		   end
		| Inner_leave (l',id') ->
		   aux_inner2 log_info error suffix l' id' assoc table
	      end
	in
	let rec aux_outer log_info error assoc story_info suffix outer_tree table =
	   match
	     suffix
	   with
	   | [] ->
	      begin
		match
		  outer_tree
		with
		| Empty ->
		   let error,id,table = add_story error assoc table in
		   error,log_info,table,Outer_leave(suffix,id)
		| Outer_node (map,None) ->
		   let error,id,table = add_story error assoc table in
		    error,log_info,table,Outer_node (map , Some id)
    		| Outer_node (map,Some id') ->
		   let error,graph =
		     match
		       assoc
		     with
		     |	(_,graph,None,_,_) -> error,graph
		     | (_,graph,Some _,_,_) ->
			warn parameter error (Some "add_story, line 878, the canonical form of stories in the outer tree should not have been computed yet") (Failure "the canonical form of stories in the outer tree should not have been computed yet") graph
		   in
		   let error,log_info,cannonic_form = canonicalize parameter handler log_info error graph in
		   let error,assoc = update_assoc error cannonic_form assoc in
		   let error,log_info,(table,cannonic_form') = get_cannonical_form parameter handler log_info error id' table in
		   let error,log_info,table,inner = aux_inner2 log_info error cannonic_form cannonic_form' id' assoc table in
		   error,log_info,table,To_inner (map,  inner)
		| Outer_leave (q,id') ->
		   let error,table = add_story_info error story_info id' table in
		   error,log_info,table,outer_tree
		| To_inner (map,inner) ->
		   let error,log_info,suffix =  canonicalize parameter handler log_info error graph in
		   let error,assoc = update_assoc error suffix assoc in
		   let error,log_info,table,inner = aux_inner log_info error assoc story_info suffix inner table in
		   error,log_info,table,To_inner(map,inner)
	      end
	   | t::q ->
	      begin
		match
		  outer_tree
		with
		| Empty ->
		   let error,id,table = add_story error assoc table in
		   error,log_info,table,Outer_leave(suffix,id)
		| Outer_node (map,assoc') ->
		   begin
		     match
		       PreHashMap.find_option t map
		     with
		     | None ->
			let error,id,table = add_story error assoc table in
			let inner_tree =
			  Outer_node (PreHashMap.add t (Outer_leave (q,id)) map,assoc')
			in
			error,log_info,table,inner_tree
		     | Some (outer_tree')  ->
			let error,log_info,table',outer_tree'' = aux_outer log_info error assoc story_info q outer_tree' table in
			if outer_tree'' == outer_tree'
			then error,log_info,table',outer_tree
			else
			  error,log_info,table',Outer_node(PreHashMap.add t outer_tree'' map,assoc')
		   end	
		| Outer_leave (t'::q',id') when not (t = t') ->
		   let error,id,table = add_story error assoc table in
		   error,log_info,table, Outer_node (PreHashMap.add t (Outer_leave (q,id)) (PreHashMap.add t' (Outer_leave (q',id')) PreHashMap.empty),None)
		| Outer_leave (l',id') ->
		   aux_outer2 log_info error suffix l' id' table
		| To_inner (map,inner) ->
		   let error,id,table = add_story error assoc table in
		   error,log_info,table, To_inner (PreHashMap.add t (Outer_leave (q,id)) (PreHashMap.add t (Outer_leave (q,id)) PreHashMap.empty),inner)

		
	      end
	in
	let error,log_info,table,tree = aux_outer log_info error (grid,graph,None,pretrace,story_info) story_info prehash table.tree table in
	error,log_info,{table with tree = tree}

      let rec print_inner_tree parameter handler error prefix inner_tree =
	match
	  inner_tree
	with
	| Inner_node (map,assoc') ->
	   let () =
	     match
	       assoc'
	     with
	     | None ->
		Format.fprintf parameter.H.out_channel "%sUnfilled Node\n" prefix
	     | Some (id)  ->
		Format.fprintf parameter.H.out_channel "%sFilled Node: %i\n" prefix id
	   in
	   let prefix' = prefix^" " in
	   KeyMap.iter
	     (fun elt map ->
	      print_elt parameter.H.out_channel elt;
	      print_inner_tree parameter handler error prefix' map)
	     map
	| Inner_leave (l,id)  ->
	   let () = Format.fprintf parameter.H.out_channel "%sLEAVE:\n" prefix in
	   let _ = print_canonical_form parameter handler error l in
	   ()
	
      let rec print_outer_tree parameter handler error prefix outer_tree =
	match
	  outer_tree
	with
	| Empty ->
	   Format.fprintf parameter.H.out_channel "%sEMPTY\n" prefix
	| Outer_node (map,assoc') ->
	   let () =
	     match
	       assoc'
	     with
	     | None ->
		Format.fprintf parameter.H.out_channel "%sUnfilled Node\n" prefix
	     | Some (id)  ->
		Format.fprintf parameter.H.out_channel "%sFilled Node: %i\n" prefix id
	   in
	   let prefix' = prefix^" " in
	   PreHashMap.iter
	     (fun ((_,b),i)  map ->
	      Format.fprintf parameter.H.out_channel "%s%s:%i->\n" prefix b i ;
	      print_outer_tree parameter handler error prefix' map)
	     map
	| Outer_leave (l,id)  ->
	   let () = Format.fprintf parameter.H.out_channel "%sLEAVE:\n" prefix in
	   let _ = print_prehash parameter handler error l in
	   ()
	| To_inner (map,inner) ->
	   let () = Format.fprintf parameter.H.out_channel "%sTO INNER TREE:\n" prefix in
	   let prefix' = prefix^" " in
	   let () = PreHashMap.iter
		      (fun ((_,b),i)  map ->
		       Format.fprintf parameter.H.out_channel "%s%s:%i->\n" prefix b i ;
		       print_outer_tree parameter handler error prefix' map)
		      map
	   in
	   print_inner_tree parameter handler error prefix' inner
	
      let hash_list parameter _  log_info error table =
	let error,array =
	  Int_storage.Nearly_inf_Imperatif.fold
	    (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
	    error
	    (fun parameter' error i (a,b,c,d,e) array ->
	     Int_storage.Nearly_inf_Imperatif.set
	       parameter' error
	       i
	       (a,b,c,d,List.sort Mods.compare_profiling_info e)
	       array)
	    table.array
	    table.array
	in
	error,log_info,{table with array = array}

      let sort_list parameter _ log_info error table =
	let error,l =
	  Int_storage.Nearly_inf_Imperatif.fold
	    (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
	    error
	    (fun parameter error _ (a,b,c,d,e) l -> error,(a,e)::l)
	    table.array
	    []
	in error,log_info,l
			
      let count_stories table = table.fresh_id

      let fold_table parameter handler log_info error f table a =
	let a,(b,c) =
	  Int_storage.Nearly_inf_Imperatif.fold_with_interruption
	    (S.PH.B.PB.CI.Po.K.H.get_kasa_parameters parameter)
	    error
	    (fun parameter' error _ (_,_,_,d,e) (log_info,a) ->
	     let a,b,c = f (S.PH.B.PB.CI.Po.K.H.set_kasa_parameters parameter' parameter) handler log_info error d e a in
	     a,(b,c))
	    table.array
	    (log_info,a)
	in a,b,c

      end:StoryTable)

module type Selector =
  sig
    val choose_fst: H.parameter -> bool
  end


module Choice(S:Selector)(A:StoryTable)(B:StoryTable) =
  (struct
      type table =
	| A of A.table
	| B of B.table
	 	
      let init_table parameter handler log_info error =
	if S.choose_fst parameter
	then
	  let error,log_info,table = A.init_table parameter handler log_info error in
	  error,log_info,A(table)
	else
	  let error,log_info,table = B.init_table parameter handler log_info error in
	  error,log_info,B(table)
		
      let add_story parameter handler log_info error grid pretrace info table =
	match
	  table
	with
	| A(table) ->
	   let error,log_info,table = A.add_story parameter handler log_info error grid pretrace info table in
	   error,log_info,A(table)
	| B(table) ->
	   let error,log_info,table = B.add_story parameter handler log_info error grid pretrace info table in
	   error,log_info,B(table)
		
      let hash_list parameter handler log_info error table =
	match
	  table
	with
	| A(table) ->
	   let error,log_info,table = A.hash_list parameter handler log_info error table in
	   error,log_info,A(table)
	| B(table) ->
	   let error,log_info,table = B.hash_list parameter handler log_info error table in
	   error,log_info,B(table)

      let sort_list parameter handler log_info error table =
	match
	  table
	with
	| A(table) -> A.sort_list parameter handler log_info error table
	| B(table) -> B.sort_list parameter handler log_info error table

      let count_stories table =
	match
	  table
	with
	| A(table) -> A.count_stories table
	| B(table) -> B.count_stories table

      let fold_table parameter handler log_info error g table a  =
	match
	  table
	with
	| A(table) -> A.fold_table parameter handler log_info error g table a
	| B(table) -> B.fold_table parameter handler log_info error g table a


    end:StoryTable)

(* module StoryTable = ListTable*)
(* module StoryTable = BucketTable*)
module StoryTable = Choice(struct let choose_fst = S.PH.B.PB.CI.Po.K.H.do_we_use_bucket_sort end)(BucketTable)(ListTable)
