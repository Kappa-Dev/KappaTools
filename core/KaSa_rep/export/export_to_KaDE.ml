(**
  * export_to_KaSim.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: Aug 23 2016
  * Last modification: Time-stamp: <Aug 07 2017>
  * *
  *
  * Copyright 2010,2011 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Type = sig
  type state
  type parameters = Remanent_parameters_sig.parameters
  type errors = Exception.method_handler
  type handler = Cckappa_sig.kappa_handler

  val init : ?compil:Ast.parsing_compil -> unit -> state
  val get_parameters : state -> parameters
  val set_parameters : parameters -> state -> state
  val get_handler : state -> state * handler
  val get_errors : state -> errors

  val get_contact_map :
    ?accuracy_level:Public_data.accuracy_level ->
    state ->
    state * Public_data.contact_map
end

module Export =
functor
  (A : Analyzer.Analyzer)
  ->
  struct
    include Export.Export (A)

    let init ?compil () =
      init ?compil ~called_from:Remanent_parameters_sig.Server ()
  end
