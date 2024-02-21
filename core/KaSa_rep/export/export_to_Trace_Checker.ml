(**
  * export_to_KaSim.ml
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

module A =
  (val Domain_selection.select_domain
         ~reachability_parameters:
           {
             Remanent_parameters_sig.views = true;
             Remanent_parameters_sig.site_across_bonds = true;
             Remanent_parameters_sig.parallel_bonds = true;
             Remanent_parameters_sig.dynamic_contact_map = true;
             Remanent_parameters_sig.counters = true;
             Remanent_parameters_sig.counter_domain = Remanent_parameters_sig.Mi;
           }
         ())

include Export.Export (A)

let init ?compil () =
  init ?compil ~called_from:Remanent_parameters_sig.Server ()
