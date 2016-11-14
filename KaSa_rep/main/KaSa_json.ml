(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Nov 14 2016>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let main () =
  let state = Export_to_json.init () in
  let state,cm = Export_to_json.get_contact_map state in
  let state,im = Export_to_json.get_influence_map state in
  let state,dr = Export_to_json.get_dead_rules state in
  let _,constraints = Export_to_json.get_constraints_list state in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string cm) in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string im) in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string dr) in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string constraints) in
  ()

let () = main ()
