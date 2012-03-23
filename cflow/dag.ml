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
  * Creation: 22/03/2011
  * Last modification: 22/03/2012
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)


module type Dag = 
  sig
    module H:Cflow_handler.Cflow_handler
      
    type graph 
    type canonical_form 

    val graph_of_grid: (Causal.grid -> H.error_channel * graph) H.with_handler
    val dot_of_graph: (graph -> H.error_channel) H.with_handler
    val canonicalize: (graph -> H.error_channel * canonical_form) H.with_handler
    val compare: canonical_form -> canonical_form -> int 
      
    val print_canonical_form: (canonical_form -> H.error_channel) H.with_handler
    val print_graph: (graph -> H.error_channel) H.with_handler 
  end 


module Dag = 
  (
    struct 
      module H=Cflow_handler.Cflow_handler 
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
        | Node of label
        | Stop_pred 
        | Stop_conflict 

      type canonical_form = 
          {
            span :key list;
            extra_edges: (edge_kind * int * int) list;
          }

      let dummy_cannonical_form = 
        {
          span = [] ; 
          extra_edges = []
        }
          
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
        let _ = Printf.fprintf parameter.H.out_channel "****\n\n" in 
        error 

      let print_canonical_form parameter handler error graph = 
        error 

      let label handler e = 
	match e with
	  | Causal.OBS mix_id -> Environment.kappa_of_num mix_id handler.Cflow_handler.Cflow_handler.env
	  | Causal.PERT p_id -> Environment.pert_of_num p_id handler.Cflow_handler.Cflow_handler.env
	  | Causal.RULE r_id -> Dynamics.to_kappa (State.rule_of_id r_id handler.Cflow_handler.Cflow_handler.state) handler.Cflow_handler.Cflow_handler.env
	  | Causal.INIT -> "intro"

      let graph_of_grid parameter handler error grid = 
        let env = handler.Cflow_handler.Cflow_handler.env in 
        let state = handler.Cflow_handler.Cflow_handler.state in 
        let ids = Hashtbl.fold (fun key _ l -> key::l) grid.Causal.flow [] in
        let label = label handler in 
	let config = Causal.cut ids grid in 
        let labels = A.make 1 "" in 
        let _ =  
          Mods.IntMap.iter
            (fun i atom  -> 
              A.set labels i (label atom.Causal.kind) 
            )
            config.Causal.events 
        in 
        let add_to_list_array i j a = 
          try 
            let old = A.get a i in 
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
           (fun i s root ->
             if Mods.IntSet.is_empty s
             then Mods.IntSet.add i root
             else 
               let _ = 
                 Mods.IntSet.iter 
                   (fun j -> add j i succ pred)
                   s
               in 
               root)
           config.Causal.prec_1
           Mods.IntSet.empty
        in 
        let conflict_pred = A.make 1 [] in 
        let conflict_succ = A.make 1 [] in 
        let root2 = 
          Mods.IntMap.fold
            (fun i s root ->
              if Mods.IntSet.is_empty s 
              then Mods.IntSet.add i root 
              else 
                let _ = 
                  Mods.IntSet.iter 
                    (fun j -> add j i conflict_succ conflict_pred)
                    s
                in 
                root)
            config.Causal.conflict 
            Mods.IntSet.empty 
        in 
        let root = Mods.IntSet.inter root root2 in 
        
        (*if not Mods.IntSet.is_singleton root 
        then 
          dummy_graph 
        else*) 
          error,{ 
            root = Mods.IntSet.min_elt root ;
            labels = labels ;
            succ = succ ;
            pred = pred ;
            conflict_succ = conflict_succ ;
            conflict_pred = conflict_pred 
          }

      let canonicalize parameter handler error graph = error,
        dummy_cannonical_form 
          
      let rec aux l1 l2 = 
          match l1,l2 
          with 
            | [],[] -> 0 
            | [], _ -> -1 
            | _ ,[] -> +1 
            | t::q,t'::q' -> 
              let cmp = compare t t' in 
              if cmp = 0 
              then aux q q' 
              else cmp 
      
      let compare x y = 
        let cmp = aux x.span y.span in 
        if cmp = 0 
        then aux x.extra_edges y.extra_edges 
        else cmp 

      let dot_of_graph parameter handler error graph = error  

        
    end:Dag)
