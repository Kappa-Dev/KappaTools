(**
  * export_to_KaSim.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Feb 20 2017>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type =
sig
  type state

  val init: ?compil:Ast.parsing_compil -> unit -> state

  val get_parameters: state -> parameters

  val get_handler: state -> state * handler

  val get_errors: state -> errors

  val get_symmetric_sites:
    ?accuracy_level:Remanent_state.accuracy_level ->
    state -> state * Remanent_state.symmetric_sites

end

module Export =
functor (A:Analyzer.Analyzer) ->
  struct

    include Export.Export(A)

    let init ?compil () =
      init ?compil ~called_from:Remanent_parameters_sig.Server ()

  end
