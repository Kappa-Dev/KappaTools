(**
     * parallel_bonds.ml
     * openkappa
     * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
     *
     * Creation: 2016, the 31th of March
     * Last modification: Time-stamp: <Oct 13 2016>
     *
     * Abstract domain to detect whether when two sites of an agent are bound,
     * they must be bound to the same agent.
     *
     * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
     * en Informatique et en Automatique.
     * All rights reserved.  This file is distributed
     * under the terms of the GNU Library General Public License *)

let local_trace = false

(******************************************************************)
(*parallel bonds in the initial states*)
(******************************************************************)

let collect_parallel_or_not_bonds_init parameters kappa_handler error
    tuple_of_interest init_state store_result =
  let tuple_of_interest = Some tuple_of_interest in
  let error, big_store =
    Parallel_bonds_static.collect_double_bonds_in_pattern parameters error
      ?tuple_of_interest init_state.Cckappa_sig.e_init_c_mixture
  in
  Parallel_bonds_static.project_away_ag_id parameters kappa_handler error
    big_store store_result
