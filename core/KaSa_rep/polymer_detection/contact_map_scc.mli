(**
  * contact_map_scc.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2017, the 26th of October
  * Last modification: Time-stamp: <Nov 12 2017>
  *
  * Compute strongly connected component in contact map
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type edge
type converted_contact_map

val convert_contact_map :
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Remanent_state.internal_contact_map ->
  Exception.exceptions_caught_and_uncaught * converted_contact_map

val mixture_of_edge :
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  edge ->
  Exception.exceptions_caught_and_uncaught * Cckappa_sig.mixture

val filter_edges_in_converted_contact_map :
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  Cckappa_sig.kappa_handler ->
  'static ->
  'dynamic ->
  (Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  'static ->
  'dynamic ->
  Cckappa_sig.mixture ->
  Exception.exceptions_caught_and_uncaught * 'dynamic * bool) ->
  converted_contact_map ->
  Exception.exceptions_caught_and_uncaught * 'dynamic * converted_contact_map

val compute_graph_scc :
  Remanent_parameters_sig.parameters ->
  Exception.exceptions_caught_and_uncaught ->
  converted_contact_map ->
  Exception.exceptions_caught_and_uncaught * Remanent_state.internal_scc_decomposition
