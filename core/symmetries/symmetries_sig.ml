(**
   * symmetries_sig.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Antique, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 5th of December
   * Last modification: Time-stamp: <May 13 2017>
   *
   * Abstract domain to record relations between pair of sites in connected agents.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type 'a site_partition = {
  over_binding_states: 'a list list;
  over_internal_states: 'a list list;
  over_full_states: 'a list list;
}

let empty =
  { over_binding_states = []; over_internal_states = []; over_full_states = [] }

let map_list_list f list =
  List.rev_map (fun list -> List.rev_map f (List.rev list)) (List.rev list)

let map f site_partition =
  {
    over_binding_states = map_list_list f site_partition.over_binding_states;
    over_internal_states = map_list_list f site_partition.over_internal_states;
    over_full_states = map_list_list f site_partition.over_full_states;
  }

let at_least_two = function
  | [] | [ _ ] -> false
  | _ -> true

let clean site_partition =
  {
    over_binding_states =
      List.filter at_least_two site_partition.over_binding_states;
    over_internal_states =
      List.filter at_least_two site_partition.over_internal_states;
    over_full_states = List.filter at_least_two site_partition.over_full_states;
  }

let print_l print_site logger fmt agent s l =
  if l = [] then
    ()
  else (
    let () = Loggers.fprintf logger "%s" s in
    let () = Loggers.print_newline logger in
    List.iter
      (fun equ_class ->
        let () = Loggers.fprintf logger "      {" in
        let _ =
          List.fold_left
            (fun b site ->
              let () = if b then Loggers.fprintf logger "," in
              let () = print_site agent fmt site in
              true)
            false equ_class
        in
        let () = Loggers.fprintf logger "}" in
        Loggers.print_newline logger)
      l
  )

let print logger print_site print_agent agent partition =
  if partition = empty then
    ()
  else (
    let fmt = Loggers.formatter_of_logger logger in
    match fmt with
    | None -> ()
    | Some fmt ->
      let () = Loggers.fprintf logger "************" in
      let () = Loggers.print_newline logger in
      let () = Loggers.fprintf logger "Agent: " in
      let () = print_agent fmt agent in
      let () = Loggers.print_newline logger in
      let () =
        print_l print_site logger fmt agent
          "  -Equivalence classes of sites for internal states:"
          partition.over_internal_states
      in
      let () =
        print_l print_site logger fmt agent
          "  -Equivalence classes of sites for bindings states:"
          partition.over_binding_states
      in
      let () =
        print_l print_site logger fmt agent
          "  -Equivalence classes of sites (both):" partition.over_full_states
      in
      let () = Loggers.fprintf logger "************" in
      let () = Loggers.print_newline logger in
      ()
  )
