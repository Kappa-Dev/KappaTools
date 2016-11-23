(*
  * domain_selection.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, project Antique, INRIA Paris
  *
  * Creation: 2016, the 30th of June
  * Last modification: Time-stamp: <Nov 23 2016>
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


let select_domain
    ?reachability_parameters
    ()
  =
  let parameters =
    match
      reachability_parameters
    with
    | None ->
      Remanent_parameters.get_reachability_parameters ()
    | Some p -> p
  in
  let base =
    (module
      Product.Product
         (Agents_domain.Domain)
         (Rules_domain.Domain):Analyzer_domain_sig.Domain)
  in
  let module Base = (val base: Analyzer_domain_sig.Domain) in
  let with_cm =
    if Remanent_parameters.get_dynamic_contact_map_1 parameters
    then
      (module
        Product.Product
        (Dynamic_contact_map_domain.Domain)
        (Base) : Analyzer_domain_sig.Domain)
    else
      (module
        Product.Product
          (Static_contact_map_domain.Domain)
          (Base) : Analyzer_domain_sig.Domain)
  in
  let module With_cm = (val with_cm: Analyzer_domain_sig.Domain) in
  let with_views =
    if Remanent_parameters.get_view_analysis_1 parameters
    then
      (module
        Product.Product(Views_domain.Domain)(With_cm) : Analyzer_domain_sig.Domain)
    else
      (module With_cm : Analyzer_domain_sig.Domain)
  in
  let module With_views = (val with_views: Analyzer_domain_sig.Domain) in
  let with_site_accross =
    if Remanent_parameters.get_site_accross_bonds_analysis_1 parameters
    then
      (module
        Product.Product(Site_accross_bonds_domain.Domain)(With_views) : Analyzer_domain_sig.Domain)
    else
      (module With_views : Analyzer_domain_sig.Domain)
  in
  let module With_site_accross =
    (val with_site_accross: Analyzer_domain_sig.Domain)
  in
  let with_parallel_bonds =
    if Remanent_parameters.get_parallel_bonds_analysis_1 parameters
    then
      (module
        Product.Product(Parallel_bonds.Domain)(With_site_accross) :
        Analyzer_domain_sig.Domain)
    else
      (module With_site_accross)
  in
  let module With_parallel_bonds =
    (val with_parallel_bonds: Analyzer_domain_sig.Domain)
  in
  let comp =
    (module Composite_domain.Make(With_parallel_bonds) : Composite_domain.Composite_domain)
  in
  let module Comp = (val comp: Composite_domain.Composite_domain) in
  let main =
    (module Analyzer.Make(Comp) : Analyzer.Analyzer)
  in
  main
