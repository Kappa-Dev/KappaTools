(**
  * export_to_KaSim.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Aug 23 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

include Export
let init ?compil () =
  init ?compil ~called_from:Remanent_parameters_sig.Server ()

let get_contact_map ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
  let state, cm = get_contact_map ~accuracy_level state in
  state, Remanent_state.contact_map_to_json cm


let get_influence_map ?accuracy_level:(accuracy_level=Remanent_state.Low) state =
  let state, influence_map = get_influence_map ~accuracy_level state in
  state, Remanent_state.influence_map_to_json influence_map



let get_dead_rules state =
  let state, rules = get_dead_rules state in
  state, dead_rules_to_json rules
