(**
  * domain_selection.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of June
  * Last modification: Time-stamp: <Aug 15 2016>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


module Reachability_analysis =
  Analyzer.Make
    (Composite_domain.Make
       (Product.Product
          (Parallel_bonds.Domain)
          (Product.Product
             (Site_accross_bonds_domain.Domain)
             (Product.Product
                (Views_domain.Domain)
                (Product.Product
                   (Contact_map_domain.Domain)
                   (Product.Product
                      (Agents_domain.Domain)
                      (Rules_domain.Domain)))))))
