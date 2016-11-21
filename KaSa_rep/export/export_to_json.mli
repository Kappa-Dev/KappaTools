(**
  * export.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Nov 21 2016>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type =
  sig
    type state

    val init:
      ?compil:(string Location.annot * Ast.port list, Ast.mixture, string, Ast.rule) Ast.compil ->
      unit -> state

    val get_contact_map:
      ?accuracy_level:Remanent_state.accuracy_level ->
      state -> state * Yojson.Basic.json

    val get_influence_map:
      ?accuracy_level:Remanent_state.accuracy_level ->
      state -> state * Yojson.Basic.json

    val get_dead_rules: state -> state * Yojson.Basic.json

    val get_constraints_list: state -> state * Yojson.Basic.json
  end

module Export:
  functor (Reachability : Analyzer.Analyzer) ->
    Type

(*val get_internal_constraints_list: state -> state * Yojson.Basic.json*)
