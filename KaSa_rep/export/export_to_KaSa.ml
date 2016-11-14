(**
  * export_to_KaSa.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June 30 2016
  * Last modification: Time-stamp: <Nov 14 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

include Export
let init () = init ~called_from:Remanent_parameters_sig.KaSa ()

let get_contact_map = get_internal_contact_map
let get_influence_map = get_internal_influence_map
let output_contact_map = output_internal_contact_map
let output_influence_map = output_internal_influence_map

let get_constraint_list state = Remanent_state.get_constraint_list state

let set_constraint_list list state =
  Remanent_state.set_constraint_list list state

let get_internal_constraint_list state =
  Remanent_state.get_internal_constraint_list state

let set_internal_constraint_list list state =
    Remanent_state.set_internal_constraint_list list state

let get_constraint_list_to_json state =
  let state, constraint_list = Export.get_constraint_list_to_json state in
  state, Remanent_state.constraint_list_to_json constraint_list

let get_internal_constraint_list_to_json state =
  let state, internal_constraint_list =
    Export.get_internal_constraint_list_to_json state in
  state, Remanent_state.internal_constraint_list_to_json
    internal_constraint_list
