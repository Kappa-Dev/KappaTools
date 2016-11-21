(**
 * main.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: December, the 18th of 2010
 * Last modification: Time-stamp: <Nov 21 2016>
 * *
 *
 * Copyright 2010,2011 Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

let main () =
  let errors = Exception.empty_error_handler in
  let _, parameters, _  = Get_option.get_option errors in
  let module A =
    (val Domain_selection.select_domain
      ~with_views_domain:(Remanent_parameters.get_view_analysis parameters)
      ~with_parallel_bonds_domain:(Remanent_parameters.get_parallel_bonds_analysis parameters)
      ~with_site_accross_bonds_domain:(Remanent_parameters.get_site_accross_bonds_analysis parameters)
      ()
    )
  in
  let export_to_json =
    (module Export_to_json.Export(A) : Export_to_json.Type)
  in
  let module Export_to_json =
    (val export_to_json : Export_to_json.Type)
  in
  let state = Export_to_json.init () in
  let state,cm = Export_to_json.get_contact_map state in
  let state,im = Export_to_json.get_influence_map state in
  let state,dr = Export_to_json.get_dead_rules state in
  let _,constraints = Export_to_json.get_constraints_list state in
  (*let _,internal_constraints = Export_to_json.get_internal_constraints_list state in*)
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string cm) in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string im) in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string dr) in
  let () = Printf.fprintf stdout "%s\n" (Yojson.Basic.to_string constraints) in
  (*let () = Printf.fprintf stdout "%s\n"
      (Yojson.Basic.to_string internal_constraints) in*)
  ()

let () = main ()
