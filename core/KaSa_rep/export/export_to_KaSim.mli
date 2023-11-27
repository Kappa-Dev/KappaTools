(**
  * export.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: June 30 2016
  * Last modification: Time-stamp: <Dec 30 2018>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type = sig
  type state
  type contact_map = Public_data.contact_map

  val init :
    ?compil:Ast.parsing_compil ->
    called_from:Remanent_parameters_sig.called_from ->
    unit ->
    state

  val get_contact_map :
    ?accuracy_level:Public_data.accuracy_level -> state -> state * contact_map

  val get_dead_rules : state -> state * Public_data.dead_rules
  val dump_errors_light : state -> unit
  val flush_errors : state -> state
end

module Export : functor (Reachability : Analyzer.Analyzer) -> Type
