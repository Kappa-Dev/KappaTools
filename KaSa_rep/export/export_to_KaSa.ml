(**
  * export_to_KaSa.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June 30 2016
  * Last modification: Time-stamp: <Oct 25 2016>
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
