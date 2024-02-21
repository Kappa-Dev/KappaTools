(**
  * export_to_Trace_Checker.mli
  * openkappa
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * Creation: Apr 10 2017
  * Last modification: Time-stamp: <Dec 30 2018>
  * *
  *
  * Copyright 2010,2011,2012,2013,2015,2016,2017
  * Institut National de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type state

val init : ?compil:Ast.parsing_compil -> unit -> state

val query_inhibition_map :
  ?accuracy_level:Public_data.accuracy_level ->
  state ->
  Remanent_state.rule_id ->
  Remanent_state.rule_id ->
  state * (Public_data.location * Public_data.location) list

val dump_errors_light : state -> unit
val flush_errors : state -> state
