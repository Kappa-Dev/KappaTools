(**
  * export_to_KaSim.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June 30 2016
  * Last modification: Time-stamp: <Nov 27 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type =
sig
  type state

  type contact_map =
    (string list * (string * string) list) Mods.StringSetMap.Map.t Mods.StringSetMap.Map.t

  val init:
    ?compil:Ast.parsing_compil ->
    called_from:Remanent_parameters_sig.called_from ->
    unit -> state

  val get_contact_map:
    ?accuracy_level: Public_data.accuracy_level -> state -> state * contact_map

  val get_dead_rules:
    state -> state * Ckappa_sig.c_rule_id list

  val dump_errors_light: state -> unit

  val flush_errors: state -> state
end

include Export
