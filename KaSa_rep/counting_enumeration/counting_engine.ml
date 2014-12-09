  (**
    * counting_engine.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 01/10/2010
    * Last modification: 14/10/2010
    * * 
    * A generic module to count or enumerate the shapes that we can do with puzzle pieces 
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

let verbose_mode = false
(* Dump information about intermediary steps *)
                                                                                                                              
type ('hole, 'brick) hole_handler = 
  {
    dual: Exception.method_handler -> 'hole -> Exception.method_handler * 'hole list ;
    dual_and_self: Exception.method_handler -> 'hole -> Exception.method_handler * 'hole list * bool ;
    interface_of_brick: Exception.method_handler -> 'brick -> Exception.method_handler * ('hole list) ; 
    print_hole: out_channel -> 'hole -> unit ;
    }
  



module Count = 
  (functor (E:Counting_algebrae.Enumeration) -> 
    struct 

      
      
      module Puzzle_hole_map_and_set = 
          Set_and_map.Make
           (struct 
               type t = E.puzzle_hole 
               let compare = compare 
      end)  
  
      type hole_multiset = int Puzzle_hole_map_and_set.map
      
      module Interfaces = 
          (Set_and_map.Make
             (struct 
               type t = hole_multiset * hole_multiset  
               let compare = compare 
      end):Set_and_map.Set_and_Map 
            with type elt = hole_multiset * hole_multiset  
             and type key = hole_multiset * hole_multiset) 
                                   
      type dependence_graph = 
        {dependences:(hole_multiset*hole_multiset) list Puzzle_hole_map_and_set.map;
         interfaces:Interfaces.set}
              
      type induction_state =
        {error_handler:Exception.method_handler;
         dependence_graph:dependence_graph;
         to_visit: (E.puzzle_hole*E.abstract_species_set*Puzzle_hole_map_and_set.set*hole_multiset) list;
         species: (E.abstract_species_set*Puzzle_hole_map_and_set.set) Interfaces.map}
      
      let print_handler error_handler kappa_handler hole_handler = 
          {
              Counting_print.iter_map1=Puzzle_hole_map_and_set.iter_map;
              Counting_print.iter_map2=Interfaces.iter_map;
              Counting_print.iter_map3=Puzzle_hole_map_and_set.iter_map;
              Counting_print.iter_set=Puzzle_hole_map_and_set.iter_set;
              Counting_print.iter_set2=Interfaces.iter_set;              
              Counting_print.dependences=(fun a -> a.dependences);
              Counting_print.dependence_graph=(fun a -> a.dependence_graph);
              Counting_print.print_hole=hole_handler.print_hole; 
              Counting_print.print_short=(fun sdtout -> E.print_short error_handler kappa_handler stdout hole_handler.print_hole) ;
              Counting_print.interfaces=(fun a -> a.interfaces);
              Counting_print.species=(fun a -> a.species);
              Counting_print.to_visit=(fun a -> a.to_visit); 
              Counting_print.error=(fun a -> a.error_handler)}
          

        
      let trace_state title prefix hole_handler state = 
        if verbose_mode then 
          let _ = Printf.fprintf stdout "%s" title in 
          let _ = Counting_print.dump_state stdout prefix hole_handler state
          in () 
     
      let find_dependence parameters error hole graph = 
           match 
             Puzzle_hole_map_and_set.find_map_option parameters error hole graph 
           with 
             | error,Some i -> error,i
             | error,None -> error,[]
               
      let add_dependence parameters error_handler hole interface graph = 
         let error_handler,old_interface_list = 
            find_dependence parameters error_handler hole graph
         in 
         Puzzle_hole_map_and_set.add_map parameters error_handler hole (interface::old_interface_list) graph 
        
      let add_species parameters error_handler hole_handler species holeset interface interface_map = 
            let error_handler,(old,old_holeset) = 
            match 
               Interfaces.find_map_option parameters error_handler interface interface_map
            with 
             | error_handler,None -> error_handler,(E.nil,Puzzle_hole_map_and_set.empty_set)
             | error_handler,Some i -> error_handler,i
            in 
            let new_species = E.sum old species in 
            let error_handler,new_hole_set = Puzzle_hole_map_and_set.union parameters error_handler old_holeset holeset in 
            Interfaces.add_map parameters error_handler interface (new_species,new_hole_set) interface_map
        
      let remove_species parameters hole self state = 
        let species = state.species in 
        let error_handler,interface_other = Puzzle_hole_map_and_set.add_map parameters state.error_handler hole 1 Puzzle_hole_map_and_set.empty_map in 
        let error_handler,k = Interfaces.find_map_option parameters error_handler (interface_other,self) species in   
        match k with 
          | None -> state,(E.nil,Puzzle_hole_map_and_set.empty_set)
          | Some k -> 
             let error_handler,species = 
               Interfaces.remove_map 
                 parameters 
                 error_handler 
                 (interface_other,self)
                 species 
             in 
               {state with error_handler=error_handler ; species=species},k
          
      let add_interface parameters error_handler hole_handler interface species holeset state =
        let interface_other,interface_self = interface in 
        let empty_interface_other = 
               Puzzle_hole_map_and_set.forall_map 
                 (fun _ x -> x=0)
                 interface_other 
        in 
        let is_singleton_interface_other = 
            let min_elt = Puzzle_hole_map_and_set.min_elt_map (fun _ i -> i<>0) interface_other in
            match min_elt with 
               | None -> false 
               | Some hole -> 
                 Puzzle_hole_map_and_set.forall_map 
                   (fun x y -> (y=1 && x=hole) or y=0)
                   interface_other  
        in          
        begin (*1*)
          if empty_interface_other 
          then 
              let error_handler,species = add_species parameters state.error_handler hole_handler species holeset (interface_other,interface_self) state.species in
              let state = {
                state with 
                  error_handler = error_handler ; 
                  species = species
                          } in 
              state 
          else 
            begin (*2*)
              let state =
                  if is_singleton_interface_other 
                  then 
                      let hole =  Puzzle_hole_map_and_set.min_elt_map (fun _ i -> i<>0) interface_other in
                        match hole with 
                         | None -> 
                             let error_handler,state = Exception.warn parameters state.error_handler (Some "Counting_enumeration") (Some "line 91") Exit (fun () -> state) in 
                              {state with error_handler = error_handler }
                         | Some hole ->      
                            {
                              state with 
                                 to_visit = (hole,species,holeset,interface_self)::state.to_visit
                            } 
                  else 
                      state
              in 
              begin (*3*)
                if Interfaces.mem_set interface state.dependence_graph.interfaces 
                then 
                  begin (*4*)
                    let error_handler,species = add_species parameters error_handler hole_handler species holeset interface state.species in 
                    {state with 
                      species = species; 
                      error_handler = error_handler 
                    }
                  end (*4*)
                else
                  begin (*4*)
                    let error_handler,dependences = 
                      Puzzle_hole_map_and_set.fold_map 
                        (fun hole n (error_handler,graph) -> 
                            if n=0 
                            then error_handler,graph 
                            else add_dependence parameters error_handler hole interface graph)
                        interface_other
                        (error_handler,state.dependence_graph.dependences)
                    in       
                    let error_handler,interfaces = 
                      Interfaces.add_set parameters error_handler interface state.dependence_graph.interfaces
                    in 
                    let error_handler,species = add_species parameters error_handler hole_handler species holeset interface state.species in 
                      {state with 
                        error_handler = error_handler; 
                        species = species;
                        dependence_graph = 
                          {
                            dependences = dependences;
                            interfaces = interfaces
                          };}
                          end (*4*)
              end (*3*) 
            end (*2*)
        end (*1*)
        
    let empty_state error_handler = 
      {
        error_handler=error_handler;
        dependence_graph=
            {
              dependences =  Puzzle_hole_map_and_set.empty_map;
              interfaces = Interfaces.empty_set 
            };
        to_visit=[];
        species=Interfaces.empty_map;
              }

    let infinite_state parameters error_handler =
      let empty_state = empty_state error_handler in 
      let error_handler,species = Interfaces.add_map parameters empty_state.error_handler (Puzzle_hole_map_and_set.empty_map,Puzzle_hole_map_and_set.empty_map) (E.infinity,Puzzle_hole_map_and_set.empty_set) Interfaces.empty_map in 
        {empty_state with species = species ; error_handler = error_handler}
      
    let inc parameters error_handler x delta map = 
        let error_handler,old = 
          match
              Puzzle_hole_map_and_set.find_map_option parameters error_handler x map 
          with 
            | error,None -> error,0 
            | error,Some i -> error,i 
        in 
        let output = old + delta in 
        if output = 0 
        then Puzzle_hole_map_and_set.remove_map parameters error_handler x map
        else Puzzle_hole_map_and_set.add_map parameters error_handler x output map
          
    let init parameters hole_handler print_handler empty_state linear_combination = 
      let error_handler = empty_state.error_handler in 
      List.fold_left 
        (fun state (n,i) -> 
            let error_handler,interface = hole_handler.interface_of_brick error_handler i in
            let error_handler,partition = 
              let rec aux list error_handler other self_other self =  
                  match list with 
                    | [] -> error_handler,Some (other,self_other,self)
                    | (elt:E.puzzle_hole)::tail -> 
                      let error_handler,dual_other,can_self = hole_handler.dual_and_self error_handler elt in 
                      let can_other = dual_other <> [] in  
                      match can_other,can_self with  
                      | false,false -> 
                          error_handler,None  
                      | true,false -> 
                          let error_handler,other = inc parameters error_handler elt 1 other in 
                          aux tail error_handler other self_other self   
                      | false,true -> 
                          let error_handler,self = inc parameters error_handler elt 1 self in
                          aux tail error_handler other self_other self 
                      | true,true -> 
                          aux tail error_handler other (elt::self_other) self
              in aux interface error_handler Puzzle_hole_map_and_set.empty_map [] Puzzle_hole_map_and_set.empty_map
            in 
            match partition with 
                | None -> {state with error_handler = error_handler}
                | Some (other,self_other,self) -> 
                    let error_handler,interface_list = 
                    List.fold_left 
                        (fun (error_handler,interface_list) elt -> 
                            List.fold_left 
                               (fun (error_handler,list) (prefix1,prefix2) ->
                                 let error_handler,sol1 = inc parameters error_handler elt 1 prefix1 in 
                                 let error_handler,sol2 = inc parameters error_handler elt 1 prefix2 in 
                                   error_handler,(sol1,prefix2)::(prefix1,sol2)::list)
                         (error_handler,[])
                         interface_list)
                  (error_handler,[other,self])
                  self_other 
            in 
            List.fold_left 
               (fun state interface -> 
                  add_interface parameters error_handler hole_handler interface (E.promote [n,i]) Puzzle_hole_map_and_set.empty_set state)
                {state with error_handler = error_handler}
                interface_list)
        empty_state 
        linear_combination 
      
                        
   let conclude state = 
     Interfaces.fold_map 
       (fun (other,x) (new_abstract_species,_) abstract_species -> 
         if Puzzle_hole_map_and_set.forall_map (fun _ x -> x=0) other 
         then 
           let nhole = 
             Puzzle_hole_map_and_set.fold_map  
               (fun _ x y -> x+y)
               x
               0
           in 
           match nhole with 
             | 0 -> E.sum abstract_species new_abstract_species
             | 1 -> E.sum 
                     abstract_species
                     (E.square new_abstract_species)
             | _ -> E.infinity
         else 
           abstract_species)  
       state.species 
       E.nil
           
           
   let induction parameters error_handler hole_handler print_handler state = 
     let rec aux state = 
       let _ = trace_state "Induction\n" " " print_handler state in 
       match state.to_visit with 
          | [] -> state   
          | (hole,formula,forbidden,self)::q -> 
            let error_handler,dual_list = hole_handler.dual error_handler hole in
            let state = {state with error_handler = error_handler} in 
            let state,abstract_species = remove_species parameters hole self state in 
            let state = {state with to_visit = q} in 
            let state = 
                begin 
                  let rec aux2 dual_list state = 
                    match dual_list with 
                      | [] -> state 
                      | dual::dual_tail -> 
                        let error_handler,partner_set = find_dependence parameters error_handler dual state.dependence_graph.dependences in 
                         let state = {state with error_handler = error_handler} in
                         let state = 
                            let rec aux3 list state = 
                               match list with 
                              | [] -> state
                              | interface::tail -> 
                                begin 
                                match 
                                  Interfaces.find_map_option parameters error_handler interface state.species 
                                with 
                              | error,None -> aux3 tail state 
                              | error_handler,Some (abstract_species_set,hole_set) -> 
                                 begin 
                                 if Puzzle_hole_map_and_set.mem_set hole hole_set 
                                   or Puzzle_hole_map_and_set.mem_set dual forbidden  
                                 then 
                                   infinite_state parameters error_handler  
                                 else 
                                   let new_abstract_species = E.combine formula hole dual abstract_species_set in 
                                   let error_handler,new_interface = 
                                      let error_handler,new_other = inc parameters error_handler dual (-1) (fst interface) in  
                                      let error_handler,new_self = 
                                        Puzzle_hole_map_and_set.map2_map 
                                          parameters 
                                          error_handler
                                          (fun x y -> x+y)
                                          (snd interface) 
                                          self
                                      in 
                                      error_handler,(new_other,new_self)
                                   in 
                                   let error_handler,new_hole_set = Puzzle_hole_map_and_set.add_set parameters error_handler dual hole_set in 
                                   let state =  add_interface parameters error_handler hole_handler new_interface new_abstract_species new_hole_set state in 
                                    aux3 tail state
end end 
                           in aux3 partner_set state   
                       in aux2 dual_tail state
             in aux2 dual_list state
        end 
        in aux state 
     in  aux state          
                   
   let count parameters error_handler hole_handler print_handler linear_combination =   
       let empty_state = empty_state error_handler in    
       let print_handler = print_handler error_handler Kappa.kappa_handler hole_handler in 
       let _ = trace_state "Empty state\n" " " print_handler empty_state  in 
       let init_state = init parameters hole_handler print_handler empty_state linear_combination in   
       let _ = trace_state "\nInitial state\n" " " print_handler init_state in 
       let final_state = (induction parameters error_handler hole_handler print_handler init_state) in 
       let _ = trace_state "\nFinal state\n" " " print_handler final_state in
       let sol = conclude final_state in 
       let _ = E.print error_handler Kappa.kappa_handler stdout hole_handler.print_hole sol in 
       () 
        
        
end)
       
       
       
