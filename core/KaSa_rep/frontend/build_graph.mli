(**
  * build_graph.mli
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: November, the 12th of 2017
  * Last modification: Time-stamp: <Jun 07 2018>
  * *
  * Primitives to build site graph in Cckappa
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type in_progress

val init :
  ?cckappa_only:bool ->
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Cckappa_sig.kappa_handler ->
  Exception.method_handler * in_progress

val add_agent :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_name ->
  in_progress ->
  Exception.method_handler * Ckappa_sig.c_agent_id * in_progress

val add_site :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_id ->
  Ckappa_sig.c_site_name ->
  in_progress ->
  Exception.method_handler * in_progress

val add_free :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_id ->
  Ckappa_sig.c_site_name ->
  in_progress ->
  Exception.method_handler * in_progress

val add_internal_state :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_id ->
  Ckappa_sig.c_site_name ->
  Ckappa_sig.c_state ->
  in_progress ->
  Exception.method_handler * in_progress

val add_link :
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_id ->
  Ckappa_sig.c_site_name ->
  Ckappa_sig.c_agent_id ->
  Ckappa_sig.c_site_name ->
  in_progress ->
  Exception.method_handler * in_progress

val export : in_progress -> Cckappa_sig.mixture
