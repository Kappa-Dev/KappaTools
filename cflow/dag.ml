(**
  * dag.ml 
  *
  * Dag computation and canonical form 
  *
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 22/03/2012
  * Last modification: 25/04/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011,2012 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)


module type Dag = 
  sig
    module S:Generic_branch_and_cut_solver.Solver
      
    type graph 
    type canonical_form 

    val graph_of_grid: (Causal.grid -> S.PH.B.PB.CI.Po.K.H.error_channel * graph) S.PH.B.PB.CI.Po.K.H.with_handler
    val dot_of_graph: (graph -> S.PH.B.PB.CI.Po.K.H.error_channel) S.PH.B.PB.CI.Po.K.H.with_handler
    val canonicalize: (graph -> S.PH.B.PB.CI.Po.K.H.error_channel * canonical_form) S.PH.B.PB.CI.Po.K.H.with_handler
    val compare: canonical_form -> canonical_form -> int 
      
    val print_canonical_form: (canonical_form -> S.PH.B.PB.CI.Po.K.H.error_channel) S.PH.B.PB.CI.Po.K.H.with_handler
    val print_graph: (graph -> S.PH.B.PB.CI.Po.K.H.error_channel) S.PH.B.PB.CI.Po.K.H.with_handler 
     
    val hash_list: ((canonical_form * 'a) list -> S.PH.B.PB.CI.Po.K.H.error_channel * (((canonical_form * 'a * 'a list) list))) S.PH.B.PB.CI.Po.K.H.with_handler 

  end 


module Dag = 
  (
    struct 
      module S=Generic_branch_and_cut_solver.Solver
      module H=S.PH.B.PB.CI.Po.K.H
      module A=Mods.DynArray 

      type graph = 
          { 
            root: int; 
            labels: string A.t ;
            pred: int list A.t ;
            succ: int list A.t ;
            conflict_pred: int list A.t; 
            conflict_succ: int list A.t;
          }

      let dummy_graph = 
        {
          root = 0 ;
          labels = A.make 1 "" ;
          pred = A.make 1 [] ;
          succ = A.make 1 [] ;
          conflict_pred = A.make 1 [] ; 
          conflict_succ = A.make 1 [] ;
        }

      type edge_kind = Succ | Conflict 
      type label = string
      type position = int 
      type key = 
        | Fresh of label
        | Former of position 
        | Stop

      type canonical_form = key list

      let dummy_cannonical_form = []

          
      let print_graph parameter handler error graph = 
        let _ = Printf.fprintf parameter.H.out_channel "****\ngraph\n****" in 
        let _ = Printf.fprintf parameter.H.out_channel "Root: %i\n" graph.root in 
        let _ = Printf.fprintf parameter.H.out_channel "Labels:\n" in 
        let _ = A.iteri (Printf.fprintf parameter.H.out_channel "Node %i,Label %s\n") graph.labels in 
        let _ = Printf.fprintf parameter.H.out_channel "Succ:\n" in 
        let _ = 
          A.iteri 
            (fun i l -> 
              List.iter (Printf.fprintf parameter.H.out_channel "%i -> %i\n" i) l 
            ) 
            graph.succ 
        in 
        let _ = 
          A.iteri 
            (fun i l   -> 
              List.iter (Printf.fprintf parameter.H.out_channel "%i <- %i\n" i) l 
            ) 
            graph.pred
        in 
        let _ = Printf.fprintf parameter.H.out_channel "Conflicts:\n" in 
        let _ = 
          A.iteri 
            (fun i l ->  
              List.iter 
                (Printf.fprintf parameter.H.out_channel "%i --| %i\n" i)
                l
            )
            graph.conflict_succ
        in 
          let _ = 
          A.iteri 
            (fun i l  ->  
              List.iter 
                (Printf.fprintf parameter.H.out_channel "%i |--  %i\n" i)
                l
            )
            graph.conflict_pred
          in 
        let _ = Printf.fprintf parameter.H.out_channel "****\n\n" in 
        error 

      let print_elt log elt = 
        match 
          elt 
        with 
          | Stop -> Printf.fprintf log "STOP\n" 
          | Former i -> Printf.fprintf log "Pointer %i\n" i 
          | Fresh s -> Printf.fprintf log "Event %s\n" s 

      let print_canonical_form parameter handler error graph = 
        let _ =
          List.iter 
            (print_elt parameter.H.out_channel)
            graph 
        in error 

      let label handler e = 
	match e with
	  | Causal.OBS mix_id -> Environment.kappa_of_num mix_id handler.H.env
	  | Causal.PERT p_id -> Environment.pert_of_num p_id handler.H.env
	  | Causal.RULE r_id -> Dynamics.to_kappa (State.rule_of_id r_id handler.H.state) handler.H.env
	  | Causal.INIT -> "intro"

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

      let rec aux l1 l2 = 
        match l1,l2 
        with 
          | [],[] -> 0 
          | [], _ -> -1 
          | _ ,[] -> +1 
          | t::q,t'::q' -> 
            let cmp = compare_elt t t' in 
            if cmp = 0 
              then aux q q' 
            else cmp 
      
      let compare x y = aux x y 


      let graph_of_grid parameter handler error grid = 
        let ids = Hashtbl.fold (fun key _ l -> key::l) grid.Causal.flow [] in
        let label = label handler in 
        let config = Causal.cut ids grid in 
        let labels = A.make 1 "" in 
        let set =  
          Mods.IntMap.fold
            (fun i atom  -> 
              let _ = A.set labels i (label atom.Causal.kind) in 
              Mods.IntSet.add i 
            )
            config.Causal.events
            Mods.IntSet.empty 
        in 
        let add_to_list_array i j a = 
          try 
            let old = 
              try 
                A.get a i 
              with 
                | Not_found -> []
            in 
            A.set a i  (j::old) 
          with 
            | _ -> A.set a i [j]
        in 
        let add i j s p = 
          let _ = add_to_list_array i j s in 
          let _ = add_to_list_array j i p in 
          ()
        in 
        let succ  = A.make 1 [] in 
        let pred = A.make 1 [] in 
        let root = 
         Mods.IntMap.fold
           (fun i s set ->
             if Mods.IntSet.is_empty s
             then set 
             else 
               let set  = 
                 Mods.IntSet.fold
                   (fun j -> 
                     let _ = add j i succ pred in 
                   Mods.IntSet.remove j)
                   s
                   set
               in 
               set)
           config.Causal.prec_1
           set 
        in 
        let conflict_pred = A.make 1 [] in 
        let conflict_succ = A.make 1 [] in 
        let root = 
          Mods.IntMap.fold
            (fun i s root ->
              if Mods.IntSet.is_empty s 
              then set 
              else 
                let root = 
                  Mods.IntSet.fold 
                    (fun j -> 
                      let _ = add j i conflict_succ conflict_pred in 
                    Mods.IntSet.remove j)
                    s
                    root 
                in 
                root)
            config.Causal.conflict 
            root
        in 
        if Mods.IntSet.is_empty root
        then 
          error,dummy_graph 
        else 
          error,{ 
            root = Mods.IntSet.min_elt root ;
            labels = labels ;
            succ = succ ;
            pred = pred ;
            conflict_succ = conflict_succ ;
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


      let canonicalize parameter handler error graph = 
        let asso = Mods.IntMap.empty in 
        let label i = 
          try 
            A.get graph.labels i 
          with 
            | _ -> "" 
        in 
        let print_to_beat to_beat= 
          let _ = Printf.fprintf stderr "TO BEAT :" in 
          let _ = 
            match 
              to_beat 
            with 
              | None -> Printf.fprintf stderr "NONE  " 
              | Some l -> List.iter (print_elt stderr) l
          in 
          Printf.fprintf stderr "\n" 
        in 
        let rec pop (candidate:key list) (to_beat:key list option) = 
          match 
            candidate,to_beat
          with 
            | [],_ | _,None -> 
              Some to_beat  (* candidate is a prefix of to_beat, we output the suffix *) 
            | t::q, Some [] -> 
              None (* the candidate is worse *)
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
          let pos = 
            try 
              Some (Mods.IntMap.find i map)
            with 
                Not_found -> None 
          in 
          match 
            pos 
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
                          try 
                            Former (Mods.IntMap.find x map)
                          with 
                            | _ -> Fresh (label x)
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
                                        try 
                                          Former (Mods.IntMap.find x map)
                                        with 
                                          | _ -> Fresh (label x)
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
        match 
          visit graph.root asso 0 None 
        with 
          | Some (rep,_,_,_) -> error,rep
          | None -> error,[]
     
      let dot_of_graph parameter handler error graph = error  

      let sort l = 
        let compare (a,_) (b,_) = compare a b in 
        List.sort compare l 

      let hash_list parameter handler error list = 
        let list = sort list in 
        let rec visit elements_to_store stored_elements last_element first_asso last_element_occurrences = 
          match elements_to_store,last_element,first_asso
          with 
            | ((t:canonical_form),asso)::q,Some old,_ when compare t old = 0 ->
              visit q stored_elements last_element first_asso (asso::last_element_occurrences)
            | (t,asso)::q,Some a,Some first_asso ->
              
              visit q ((a,first_asso,List.rev last_element_occurrences)::stored_elements) (Some t) (Some asso) [asso]
            | (t,asso)::q,None,None -> 
              visit q stored_elements (Some t) (Some asso) [asso]
            | [],None,None -> 
              []
            | [],Some a,Some first_asso -> 
              List.rev ((a,first_asso,List.rev last_element_occurrences)::stored_elements)
        in
        let list = visit list [] None None [] in 
        error,list 
              
    end:Dag)
