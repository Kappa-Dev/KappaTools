(**
  * build_graph.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: November, the 12th of 2017
  * Last modification: Time-stamp: <Aug 17 2018>
  * *
  * Primitives to build site graph in Cckappa
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let add_agent parameters error handler cckappa_only agent_id agent_type mixture
    =
  let error, agent_string =
    Handler.string_of_agent parameters error handler agent_type
  in
  let error, c_mixture =
    if cckappa_only then
      error, mixture.Cckappa_sig.c_mixture
    else
      Ckappa_sig.add_agent parameters error agent_id agent_string
        mixture.Cckappa_sig.c_mixture
  in
  let agent =
    {
      Cckappa_sig.agent_kasim_id = agent_id;
      Cckappa_sig.agent_name = agent_type;
      Cckappa_sig.agent_interface = Ckappa_sig.Site_map_and_set.Map.empty;
      Cckappa_sig.agent_position = Loc.dummy;
      Cckappa_sig.is_created = false;
    }
  in
  let error', views =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set parameters
      error agent_id (Cckappa_sig.Agent agent) mixture.Cckappa_sig.views
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  error, { mixture with Cckappa_sig.c_mixture; Cckappa_sig.views }

let empty_port =
  {
    Cckappa_sig.site_name = Ckappa_sig.dummy_site_name;
    Cckappa_sig.site_position = Loc.dummy;
    Cckappa_sig.site_free = None;
    Cckappa_sig.site_state =
      {
        Cckappa_sig.min = Some Ckappa_sig.dummy_state_index;
        Cckappa_sig.max = Some Ckappa_sig.dummy_state_index;
      };
  }

let add_site parameters error handler cckappa_only agent_id site_name mixture =
  let error', agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameters
      error agent_id mixture.Cckappa_sig.views
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  match agent with
  | Some (Cckappa_sig.Agent ag) ->
    let agent_name = ag.Cckappa_sig.agent_name in
    let error, site_string =
      Handler.string_of_site parameters error handler agent_name site_name
    in
    let error, c_mixture =
      if cckappa_only then
        error, mixture.Cckappa_sig.c_mixture
      else
        Ckappa_sig.add_site parameters error agent_id site_string
          mixture.Cckappa_sig.c_mixture
    in
    let error, max_state_index =
      Handler.last_state_of_site parameters error handler agent_name site_name
    in
    let site =
      {
        Cckappa_sig.site_name;
        Cckappa_sig.site_position = Loc.dummy;
        Cckappa_sig.site_free = None;
        Cckappa_sig.site_state =
          {
            Cckappa_sig.min = Some Ckappa_sig.dummy_state_index;
            Cckappa_sig.max = Some max_state_index;
          };
      }
    in
    let error', interface =
      Ckappa_sig.Site_map_and_set.Map.add parameters error site_name site
        ag.Cckappa_sig.agent_interface
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    let ag = { ag with Cckappa_sig.agent_interface = interface } in
    let agent = Cckappa_sig.Agent ag in
    let error', views =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set parameters
        error agent_id agent mixture.Cckappa_sig.views
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    error, { mixture with Cckappa_sig.c_mixture; Cckappa_sig.views }
  | Some
      ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
      | Cckappa_sig.Unknown_agent _ )
  | None ->
    Exception.warn parameters error __POS__ Exit mixture

let add_state parameters error agent_id site_name state mixture =
  let error', agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameters
      error agent_id mixture.Cckappa_sig.views
  in
  let error =
    Exception.check_point Exception.warn parameters error error' __POS__ Exit
  in
  match agent with
  | Some (Cckappa_sig.Agent ag) ->
    let error, site =
      Ckappa_sig.Site_map_and_set.Map.find_default_without_logs parameters error
        empty_port site_name ag.Cckappa_sig.agent_interface
    in
    let error, () =
      if
        Ckappa_sig.compare_state_index_option_min (Some state)
          site.Cckappa_sig.site_state.Cckappa_sig.min
        < 0
        || Ckappa_sig.compare_state_index_option_max
             site.Cckappa_sig.site_state.Cckappa_sig.max (Some state)
           < 0
      then
        Exception.warn parameters error __POS__ Exit ()
      else
        error, ()
    in
    let site =
      {
        site with
        Cckappa_sig.site_state =
          { Cckappa_sig.min = Some state; Cckappa_sig.max = Some state };
      }
    in
    let error', agent_interface =
      Ckappa_sig.Site_map_and_set.Map.overwrite parameters error site_name site
        ag.Cckappa_sig.agent_interface
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    let ag = { ag with Cckappa_sig.agent_interface } in
    let agent = Cckappa_sig.Agent ag in
    let error', views =
      Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set parameters
        error agent_id agent mixture.Cckappa_sig.views
    in
    let error =
      Exception.check_point Exception.warn parameters error error' __POS__ Exit
    in
    error, { mixture with Cckappa_sig.views }
  | Some
      ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
      | Cckappa_sig.Unknown_agent _ )
  | None ->
    Exception.warn parameters error __POS__ Exit mixture

let add_pointer parameter error agent_id site_name address mixture =
  let error, old =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
      parameter error agent_id mixture.Cckappa_sig.bonds
  in
  let old =
    match old with
    | None -> Ckappa_sig.Site_map_and_set.Map.empty
    | Some old -> old
  in
  let error', old' =
    Ckappa_sig.Site_map_and_set.Map.add parameter error site_name address old
  in
  let error =
    Exception.check_point Exception.warn parameter error error' __POS__ Exit
  in
  let error, bonds =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.set parameter
      error agent_id old' mixture.Cckappa_sig.bonds
  in
  error, { mixture with Cckappa_sig.bonds }

let add_link parameters error handler cckappa_only agent_id site_name agent_id'
    site_name' lnk_value mixture =
  let error, agent =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameters
      error agent_id mixture.Cckappa_sig.views
  in
  let error, agent' =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.get parameters
      error agent_id' mixture.Cckappa_sig.views
  in
  match agent, agent' with
  | Some (Cckappa_sig.Agent ag), Some (Cckappa_sig.Agent ag') ->
    let agent_type = ag.Cckappa_sig.agent_name in
    let error, agent_name =
      Handler.string_of_agent parameters error handler agent_type
    in
    let agent_type' = ag'.Cckappa_sig.agent_name in
    let error, agent_name' =
      Handler.string_of_agent parameters error handler agent_type'
    in
    let error, site_string =
      Handler.string_of_site parameters error handler agent_type site_name
    in
    let error, site_string' =
      Handler.string_of_site parameters error handler agent_type' site_name'
    in
    let error, c_mixture =
      if cckappa_only then
        error, mixture.Cckappa_sig.c_mixture
      else
        Ckappa_sig.add_link parameters error agent_id ~agent_name site_string
          agent_id' ~agent_name' site_string' lnk_value
          mixture.Cckappa_sig.c_mixture
    in
    let error, state =
      Handler.id_of_binding_type parameters error handler agent_type site_name
        agent_type' site_name'
    in
    let error, mixture =
      add_state parameters error agent_id site_name state mixture
    in
    let error, state' =
      Handler.id_of_binding_type parameters error handler agent_type' site_name'
        agent_type site_name
    in
    let error, mixture =
      add_state parameters error agent_id' site_name' state' mixture
    in
    let site_address =
      {
        Cckappa_sig.agent_index = agent_id';
        Cckappa_sig.site = site_name';
        Cckappa_sig.agent_type = agent_type';
      }
    in
    let error, mixture =
      add_pointer parameters error agent_id site_name site_address mixture
    in
    let site_address' =
      {
        Cckappa_sig.agent_index = agent_id;
        Cckappa_sig.site = site_name;
        Cckappa_sig.agent_type;
      }
    in
    let error, mixture =
      add_pointer parameters error agent_id' site_name' site_address' mixture
    in
    error, { mixture with Cckappa_sig.c_mixture }
  | ( ( Some
          ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
          | Cckappa_sig.Unknown_agent _ )
      | None ),
      _ )
  | ( _,
      ( Some
          ( Cckappa_sig.Ghost | Cckappa_sig.Dead_agent _
          | Cckappa_sig.Unknown_agent _ )
      | None ) ) ->
    Exception.warn parameters error __POS__ Exit mixture

type in_progress = {
  fresh_agent_id: Ckappa_sig.c_agent_id;
  fresh_bond_id: Ckappa_sig.c_link_value;
  cckappa_only: bool;
  mixture: Cckappa_sig.mixture;
  kappa_handler: Cckappa_sig.kappa_handler;
}

let init ?cckappa_only parameters error kappa_handler =
  let error, mixture = Preprocess.empty_mixture parameters error in
  ( error,
    let cckappa_only =
      match cckappa_only with
      | None | Some false -> false
      | Some true -> true
    in
    {
      fresh_agent_id = Ckappa_sig.dummy_agent_id;
      fresh_bond_id = Ckappa_sig.dummy_link_value;
      mixture;
      cckappa_only;
      kappa_handler;
    } )

let add_agent parameters error agent_type in_progress =
  let handler = in_progress.kappa_handler in
  let agent_id = in_progress.fresh_agent_id in
  let fresh_agent_id = Ckappa_sig.next_agent_id in_progress.fresh_agent_id in
  let error, mixture =
    add_agent parameters error handler in_progress.cckappa_only agent_id
      agent_type in_progress.mixture
  in
  error, agent_id, { in_progress with fresh_agent_id; mixture }

let add_site parameters error agent_id site_name in_progress =
  let handler = in_progress.kappa_handler in
  let error, mixture =
    add_site parameters error handler in_progress.cckappa_only agent_id
      site_name in_progress.mixture
  in
  error, { in_progress with mixture }

let add_internal_state parameters error agent_id site_name state in_progress =
  let error', mixture =
    add_state parameters error agent_id site_name state in_progress.mixture
  in
  ( Exception.check_point Exception.warn parameters error error' __POS__ Exit,
    { in_progress with mixture } )

let add_free parameters error agent_id site_name in_progress =
  add_internal_state parameters error agent_id site_name
    Ckappa_sig.dummy_state_index in_progress

let add_link parameters error agent_id site_name agent_id' site_name'
    in_progress =
  let handler = in_progress.kappa_handler in
  let lnk_id = in_progress.fresh_bond_id in
  let fresh_bond_id = Ckappa_sig.next_link_value in_progress.fresh_bond_id in
  let error, mixture =
    add_link parameters error handler in_progress.cckappa_only agent_id
      site_name agent_id' site_name' lnk_id in_progress.mixture
  in
  error, { in_progress with fresh_bond_id; mixture }

let export in_progress = in_progress.mixture
