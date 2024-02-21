(**
    * counting_print.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 07/10/2010
    * Last modification: Time-stamp: <Aug 05 2016>
    * *
    * Printing primitives
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

type ('error,
       'species,
       'state,
       'dependence_graph,
       'dependence,
       'hole,
       'holeset,
       'hole_multiset,
       'species_interface_map,
       'hole_map_interface_list,
       'interface,
       'interfaceset)
     print_handler = {
  iter_map1: ('hole -> int -> unit) -> 'hole_multiset -> unit;
  iter_map2:
    ('interface -> 'species * 'holeset -> unit) ->
    'species_interface_map ->
    unit;
  iter_map3:
    ('hole -> 'interface list -> unit) -> 'hole_map_interface_list -> unit;
  iter_set: ('hole -> unit) -> 'holeset -> unit;
  iter_set2: ('interface -> unit) -> 'interfaceset -> unit;
  dependences: 'dependence_graph -> 'dependence;
  dependence_graph: 'state -> 'dependence_graph;
  print_hole: out_channel -> 'hole -> unit;
  print_short: out_channel -> 'species -> unit;
  interfaces: 'dependence_graph -> 'interfaceset;
  species: 'state -> 'species_interface_map;
  to_visit: 'state -> ('hole * 'species * 'holeset * 'hole_multiset) list;
  error: 'state -> 'error;
}

let dump_hole_set log print_handler hole_set =
  print_handler.iter_map1
    (fun x i ->
      match i with
      | 0 -> ()
      | 1 ->
        let _ = print_handler.print_hole log x in
        let _ = Printf.fprintf log ";" in
        ()
      | _ ->
        let _ = Printf.fprintf log "%i*" i in
        let _ = print_handler.print_hole log x in
        let _ = Printf.fprintf log ";" in
        ())
    hole_set

let dump_interface log prefix print_handler (other, self) =
  let _ =
    Printf.fprintf log "%s   * Hole connected to other kind of pieces: " prefix
  in
  let _ = dump_hole_set log print_handler other in
  let _ =
    Printf.fprintf log "\n%s   * Hole connected to identical kind of pieces: "
      prefix
  in
  let _ = dump_hole_set log print_handler self in
  let _ = Printf.fprintf log "\n" in
  ()

let dump_dependence_graph log prefix print_handler graph =
  let _ =
    Printf.fprintf log "%sDependence_graph:\n%s * Known interfaces:\n" prefix
      prefix
  in
  let _ =
    print_handler.iter_set2
      (dump_interface log prefix print_handler)
      (print_handler.interfaces graph)
  in
  let _ = Printf.fprintf log "%s * Dependences:\n" prefix in
  let _ =
    print_handler.iter_map3
      (fun hole list ->
        let _ = Printf.fprintf log "%s   * Hole :" prefix in
        let _ = print_handler.print_hole log hole in
        let _ = Printf.fprintf log "\n" in
        let _ =
          List.iter (dump_interface log ("     " ^ prefix) print_handler) list
        in
        ())
      (print_handler.dependences graph)
  in
  ()

let dump_state log prefix print_handler state =
  let _ = Printf.fprintf log "%sState:\n" prefix in
  let _ =
    dump_dependence_graph log (prefix ^ " ") print_handler
      (print_handler.dependence_graph state)
  in
  let _ = Printf.fprintf log "%s Available pieces:\n" prefix in
  let _ =
    print_handler.iter_map2
      (fun interface (species, forbidden) ->
        let _ = Printf.fprintf log "%s  * species\n" prefix in
        let _ = dump_interface log (prefix ^ "  ") print_handler interface in
        let _ =
          Printf.fprintf log "%s  * regular formula\n%s     " prefix prefix
        in
        let _ = print_handler.print_short log species in
        let _ =
          Printf.fprintf log "%s  * forbidden holes\n%s     " prefix prefix
        in
        let _ =
          print_handler.iter_set
            (fun x ->
              print_handler.print_hole log x;
              Printf.fprintf log ";")
            forbidden
        in
        let _ = Printf.fprintf log "\n" in
        ())
      (print_handler.species state)
  in
  let _ = Printf.fprintf log "\n" in
  let _ = Printf.fprintf log "%s To be visited:\n" prefix in
  let _ =
    List.iter
      (fun (hole, species, holeset, _hole_multiset) ->
        let _ = Printf.fprintf log "%s  * element\n" prefix in
        let _ = Printf.fprintf log "%s    - hole " prefix in
        let _ = print_handler.print_hole log hole in
        let _ = Printf.fprintf log "," in
        let _ = Printf.fprintf log "regular formula " in
        let _ = print_handler.print_short log species in
        let _ = Printf.fprintf log "forbidden holes " in
        let _ =
          print_handler.iter_set
            (fun x ->
              print_handler.print_hole log x;
              Printf.fprintf log ";")
            holeset
        in
        let _ = Printf.fprintf log "\n" in
        ())
      (print_handler.to_visit state)
  in
  ()
