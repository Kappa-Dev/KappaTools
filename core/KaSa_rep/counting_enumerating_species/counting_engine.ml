(**
   * counting_engine.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 01/10/2010
   * Last modification: Time-stamp: <Dec 09 2018>
   * *
   * A generic module to count or enumerate the shapes that we can do with puzzle pieces
   *
   * Copyright 2010 Institut National de Recherche en Informatique et
   * en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let verbose_mode = false
(* Dump information about intermediary steps *)

type ('hole, 'brick) hole_handler = {
  dual:
    Exception.exceptions_caught_and_uncaught ->
    'hole ->
    Exception.exceptions_caught_and_uncaught * 'hole list;
  dual_and_self:
    Exception.exceptions_caught_and_uncaught ->
    'hole ->
    Exception.exceptions_caught_and_uncaught * 'hole list * bool;
  interface_of_brick:
    Exception.exceptions_caught_and_uncaught ->
    'brick ->
    Exception.exceptions_caught_and_uncaught * 'hole list;
  print_hole: out_channel -> 'hole -> unit;
}

module Count =
functor
  (E : Counting_algebrae.Enumeration)
  ->
  struct
    module Puzzle_hole_map_and_set = Map_wrapper.Make (SetMap.Make (struct
      type t = E.puzzle_hole

      let compare = compare
      let print _ _ = ()
    end))

    type hole_multiset = int Puzzle_hole_map_and_set.Map.t

    module Interfaces = Map_wrapper.Make (SetMap.Make (struct
      type t = hole_multiset * hole_multiset

      let compare = compare
      let print _ _ = ()
    end))

    type dependence_graph = {
      dependences:
        (hole_multiset * hole_multiset) list Puzzle_hole_map_and_set.Map.t;
      interfaces: Interfaces.Set.t;
    }

    type induction_state = {
      exceptions_caught_and_uncaught: Exception.exceptions_caught_and_uncaught;
      dependence_graph: dependence_graph;
      to_visit:
        (E.puzzle_hole
        * E.abstract_species_set
        * Puzzle_hole_map_and_set.Set.t
        * hole_multiset)
        list;
      species:
        (E.abstract_species_set * Puzzle_hole_map_and_set.Set.t)
        Interfaces.Map.t;
    }

    let print_handler exceptions_caught_and_uncaught kappa_handler hole_handler
        =
      {
        Counting_print.iter_map1 = Puzzle_hole_map_and_set.Map.iter;
        Counting_print.iter_map2 = Interfaces.Map.iter;
        Counting_print.iter_map3 = Puzzle_hole_map_and_set.Map.iter;
        Counting_print.iter_set = Puzzle_hole_map_and_set.Set.iter;
        Counting_print.iter_set2 = Interfaces.Set.iter;
        Counting_print.dependences = (fun a -> a.dependences);
        Counting_print.dependence_graph = (fun a -> a.dependence_graph);
        Counting_print.print_hole = hole_handler.print_hole;
        Counting_print.print_short =
          (fun stdout ->
            E.print_short exceptions_caught_and_uncaught kappa_handler stdout
              hole_handler.print_hole);
        Counting_print.interfaces = (fun a -> a.interfaces);
        Counting_print.species = (fun a -> a.species);
        Counting_print.to_visit = (fun a -> a.to_visit);
        Counting_print.error = (fun a -> a.exceptions_caught_and_uncaught);
      }

    let trace_state title prefix hole_handler state =
      if verbose_mode then (
        let _ = Printf.fprintf stdout "%s" title in
        let _ = Counting_print.dump_state stdout prefix hole_handler state in
        ()
      )

    let find_dependence parameters error hole graph =
      Puzzle_hole_map_and_set.Map.find_default parameters error [] hole graph

    let add_dependence parameters exceptions_caught_and_uncaught hole interface
        graph =
      let exceptions_caught_and_uncaught, old_interface_list =
        find_dependence parameters exceptions_caught_and_uncaught hole graph
      in
      Puzzle_hole_map_and_set.Map.add parameters exceptions_caught_and_uncaught
        hole
        (interface :: old_interface_list)
        graph

    let add_species parameters exceptions_caught_and_uncaught _hole_handler
        species holeset interface interface_map =
      let exceptions_caught_and_uncaught, (old, old_holeset) =
        Interfaces.Map.find_default_without_logs parameters
          exceptions_caught_and_uncaught
          (E.nil, Puzzle_hole_map_and_set.Set.empty)
          interface interface_map
      in
      let new_species = E.sum old species in
      let exceptions_caught_and_uncaught, new_hole_set =
        Puzzle_hole_map_and_set.Set.union parameters
          exceptions_caught_and_uncaught old_holeset holeset
      in
      Interfaces.Map.add parameters exceptions_caught_and_uncaught interface
        (new_species, new_hole_set)
        interface_map

    let remove_species parameters hole self state =
      let species = state.species in
      let exceptions_caught_and_uncaught =
        state.exceptions_caught_and_uncaught
      in
      let exceptions_caught_and_uncaught, interface_other =
        Puzzle_hole_map_and_set.Map.add parameters
          exceptions_caught_and_uncaught hole 1
          Puzzle_hole_map_and_set.Map.empty
      in
      let exceptions_caught_and_uncaught, k =
        Interfaces.Map.find_option parameters exceptions_caught_and_uncaught
          (interface_other, self) species
      in
      match k with
      | None ->
        let exceptions_caught_and_uncaught, state =
          Exception.warn parameters state.exceptions_caught_and_uncaught __POS__
            ~message:"unknown interface in remove_species" Exit state
        in
        ( { state with exceptions_caught_and_uncaught },
          (E.nil, Puzzle_hole_map_and_set.Set.empty) )
      | Some k ->
        let exceptions_caught_and_uncaught, species =
          Interfaces.Map.remove parameters exceptions_caught_and_uncaught
            (interface_other, self) species
        in
        { state with species; exceptions_caught_and_uncaught }, k

    let add_interface parameters hole_handler interface species holeset state =
      let exceptions_caught_and_uncaught =
        state.exceptions_caught_and_uncaught
      in
      let interface_other, interface_self = interface in
      let empty_interface_other =
        Puzzle_hole_map_and_set.Map.for_all (fun _ x -> x = 0) interface_other
      in
      let is_singleton_interface_other =
        let min_elt =
          Puzzle_hole_map_and_set.Map.filter_one
            (fun _ i -> i <> 0)
            interface_other
        in
        match min_elt with
        | None -> false
        | Some (hole, _) ->
          Puzzle_hole_map_and_set.Map.for_all
            (fun x y -> (y = 1 && x = hole) || y = 0)
            interface_other
      in
      (*1*)
      if empty_interface_other then (
        let exceptions_caught_and_uncaught, species =
          add_species parameters state.exceptions_caught_and_uncaught
            hole_handler species holeset
            (interface_other, interface_self)
            state.species
        in
        { state with exceptions_caught_and_uncaught; species }
      ) else (
        (*2*)
        let state =
          if is_singleton_interface_other then (
            let hole =
              Puzzle_hole_map_and_set.Map.filter_one
                (fun _ i -> i <> 0)
                interface_other
            in
            match hole with
            | None ->
              let exceptions_caught_and_uncaught, state =
                Exception.warn parameters state.exceptions_caught_and_uncaught
                  __POS__ Exit state
              in
              { state with exceptions_caught_and_uncaught }
            | Some (hole, _) ->
              {
                state with
                to_visit =
                  (hole, species, holeset, interface_self) :: state.to_visit;
              }
          ) else
            state
        in
        (*3*)
        if Interfaces.Set.mem interface state.dependence_graph.interfaces then (
          (*4*)
          let exceptions_caught_and_uncaught, species =
            add_species parameters exceptions_caught_and_uncaught hole_handler
              species holeset interface state.species
          in
          { state with species; exceptions_caught_and_uncaught }
        ) else (
          (*4*)

          (*4*)
          let exceptions_caught_and_uncaught, dependences =
            Puzzle_hole_map_and_set.Map.fold
              (fun hole n (exceptions_caught_and_uncaught, graph) ->
                if n = 0 then
                  exceptions_caught_and_uncaught, graph
                else
                  add_dependence parameters exceptions_caught_and_uncaught hole
                    interface graph)
              interface_other
              ( exceptions_caught_and_uncaught,
                state.dependence_graph.dependences )
          in
          let exceptions_caught_and_uncaught, interfaces =
            Interfaces.Set.add parameters exceptions_caught_and_uncaught
              interface state.dependence_graph.interfaces
          in
          let exceptions_caught_and_uncaught, species =
            add_species parameters exceptions_caught_and_uncaught hole_handler
              species holeset interface state.species
          in
          {
            state with
            exceptions_caught_and_uncaught;
            species;
            dependence_graph = { dependences; interfaces };
          }
          (*4*)
          (*3*)
          (*2*)
        )
      )
    (*1*)

    let empty_state exceptions_caught_and_uncaught =
      {
        exceptions_caught_and_uncaught;
        dependence_graph =
          {
            dependences = Puzzle_hole_map_and_set.Map.empty;
            interfaces = Interfaces.Set.empty;
          };
        to_visit = [];
        species = Interfaces.Map.empty;
      }

    let infinite_state parameters exceptions_caught_and_uncaught =
      let empty_state = empty_state exceptions_caught_and_uncaught in
      let exceptions_caught_and_uncaught, species =
        Interfaces.Map.add parameters exceptions_caught_and_uncaught
          (Puzzle_hole_map_and_set.Map.empty, Puzzle_hole_map_and_set.Map.empty)
          (E.infinity, Puzzle_hole_map_and_set.Set.empty)
          Interfaces.Map.empty
      in
      { empty_state with species; exceptions_caught_and_uncaught }

    let inc parameters exceptions_caught_and_uncaught x delta map =
      let exceptions_caught_and_uncaught, old =
        Puzzle_hole_map_and_set.Map.find_default_without_logs parameters
          exceptions_caught_and_uncaught 0 x map
      in
      let output = old + delta in
      if output = 0 then
        Puzzle_hole_map_and_set.Map.remove parameters
          exceptions_caught_and_uncaught x map
      else
        Puzzle_hole_map_and_set.Map.add parameters
          exceptions_caught_and_uncaught x output map

    let init parameters hole_handler _print_handler empty_state
        linear_combination =
      let exceptions_caught_and_uncaught =
        empty_state.exceptions_caught_and_uncaught
      in
      List.fold_left
        (fun state (n, i) ->
          let exceptions_caught_and_uncaught, interface =
            hole_handler.interface_of_brick exceptions_caught_and_uncaught i
          in
          let exceptions_caught_and_uncaught, partition =
            let rec aux list exceptions_caught_and_uncaught other self_other
                self =
              match list with
              | [] ->
                exceptions_caught_and_uncaught, Some (other, self_other, self)
              | (elt : E.puzzle_hole) :: tail ->
                let exceptions_caught_and_uncaught, dual_other, can_self =
                  hole_handler.dual_and_self exceptions_caught_and_uncaught elt
                in
                let can_other = dual_other <> [] in
                (match can_other, can_self with
                | false, false -> exceptions_caught_and_uncaught, None
                | true, false ->
                  let exceptions_caught_and_uncaught, other =
                    inc parameters exceptions_caught_and_uncaught elt 1 other
                  in
                  aux tail exceptions_caught_and_uncaught other self_other self
                | false, true ->
                  let exceptions_caught_and_uncaught, self =
                    inc parameters exceptions_caught_and_uncaught elt 1 self
                  in
                  aux tail exceptions_caught_and_uncaught other self_other self
                | true, true ->
                  aux tail exceptions_caught_and_uncaught other
                    (elt :: self_other) self)
            in
            aux interface exceptions_caught_and_uncaught
              Puzzle_hole_map_and_set.Map.empty []
              Puzzle_hole_map_and_set.Map.empty
          in
          match partition with
          | None -> { state with exceptions_caught_and_uncaught }
          | Some (other, self_other, self) ->
            let exceptions_caught_and_uncaught, interface_list =
              List.fold_left
                (fun (exceptions_caught_and_uncaught, interface_list) elt ->
                  List.fold_left
                    (fun (exceptions_caught_and_uncaught, list)
                         (prefix1, prefix2) ->
                      let exceptions_caught_and_uncaught, sol1 =
                        inc parameters exceptions_caught_and_uncaught elt 1
                          prefix1
                      in
                      let exceptions_caught_and_uncaught, sol2 =
                        inc parameters exceptions_caught_and_uncaught elt 1
                          prefix2
                      in
                      ( exceptions_caught_and_uncaught,
                        (sol1, prefix2) :: (prefix1, sol2) :: list ))
                    (exceptions_caught_and_uncaught, [])
                    interface_list)
                (exceptions_caught_and_uncaught, [ other, self ])
                self_other
            in
            List.fold_left
              (fun state interface ->
                add_interface parameters hole_handler interface
                  (E.promote [ n, i ])
                  Puzzle_hole_map_and_set.Set.empty state)
              { state with exceptions_caught_and_uncaught }
              interface_list)
        empty_state linear_combination

    let conclude state =
      Interfaces.Map.fold
        (fun (other, x) (new_abstract_species, _) abstract_species ->
          if Puzzle_hole_map_and_set.Map.for_all (fun _ x -> x = 0) other then (
            let nhole =
              Puzzle_hole_map_and_set.Map.fold (fun _ x y -> x + y) x 0
            in
            match nhole with
            | 0 -> E.sum abstract_species new_abstract_species
            | 1 -> E.sum abstract_species (E.square new_abstract_species)
            | _ -> E.infinity
          ) else
            abstract_species)
        state.species E.nil

    let induction parameters exceptions_caught_and_uncaught hole_handler
        print_handler state =
      let rec aux state =
        let _ = trace_state "Induction\n" " " print_handler state in
        match state.to_visit with
        | [] -> state
        | (hole, formula, forbidden, self) :: q ->
          let exceptions_caught_and_uncaught, dual_list =
            hole_handler.dual exceptions_caught_and_uncaught hole
          in
          let state = { state with exceptions_caught_and_uncaught } in
          let state, _ = remove_species parameters hole self state in
          let state = { state with to_visit = q } in
          let state =
            let rec aux2 dual_list state =
              match dual_list with
              | [] -> state
              | dual :: dual_tail ->
                let exceptions_caught_and_uncaught, partner_set =
                  find_dependence parameters exceptions_caught_and_uncaught dual
                    state.dependence_graph.dependences
                in
                let state = { state with exceptions_caught_and_uncaught } in
                let state =
                  let rec aux3 list state =
                    match list with
                    | [] -> state
                    | interface :: tail ->
                      (match
                         Interfaces.Map.find_option_without_logs parameters
                           exceptions_caught_and_uncaught interface
                           state.species
                       with
                      | exceptions_caught_and_uncaught, None ->
                        aux3 tail { state with exceptions_caught_and_uncaught }
                      | ( exceptions_caught_and_uncaught,
                          Some (abstract_species_set, hole_set) ) ->
                        if
                          Puzzle_hole_map_and_set.Set.mem hole hole_set
                          || Puzzle_hole_map_and_set.Set.mem dual forbidden
                        then
                          infinite_state parameters
                            exceptions_caught_and_uncaught
                        else (
                          let new_abstract_species =
                            E.combine formula hole dual abstract_species_set
                          in
                          let exceptions_caught_and_uncaught, new_interface =
                            let exceptions_caught_and_uncaught, new_other =
                              inc parameters exceptions_caught_and_uncaught dual
                                (-1) (fst interface)
                            in
                            let exceptions_caught_and_uncaught, new_self =
                              Puzzle_hole_map_and_set.Map.map2z parameters
                                exceptions_caught_and_uncaught
                                (fun _paramters exceptions_caught_and_uncaught x
                                     y -> exceptions_caught_and_uncaught, x + y)
                                (snd interface) self
                            in
                            exceptions_caught_and_uncaught, (new_other, new_self)
                          in
                          let exceptions_caught_and_uncaught, new_hole_set =
                            Puzzle_hole_map_and_set.Set.add parameters
                              exceptions_caught_and_uncaught dual hole_set
                          in
                          let state =
                            add_interface parameters hole_handler new_interface
                              new_abstract_species new_hole_set
                              { state with exceptions_caught_and_uncaught }
                          in
                          aux3 tail state
                        ))
                  in
                  aux3 partner_set state
                in
                aux2 dual_tail state
            in
            aux2 dual_list state
          in
          aux state
      in
      aux state

    let count parameters exceptions_caught_and_uncaught kappa_handler
        hole_handler print_handler linear_combination =
      let empty_state = empty_state exceptions_caught_and_uncaught in
      let print_handler =
        print_handler exceptions_caught_and_uncaught kappa_handler hole_handler
      in
      let _ = trace_state "Empty state\n" " " print_handler empty_state in
      let init_state =
        init parameters hole_handler print_handler empty_state
          linear_combination
      in
      let _ = trace_state "\nInitial state\n" " " print_handler init_state in
      let final_state =
        induction parameters exceptions_caught_and_uncaught hole_handler
          print_handler init_state
      in
      let _ = trace_state "\nFinal state\n" " " print_handler final_state in
      let sol = conclude final_state in
      let _ =
        E.print exceptions_caught_and_uncaught kappa_handler stdout
          hole_handler.print_hole sol
      in
      sol
  end
