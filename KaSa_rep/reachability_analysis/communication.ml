(**
   * communication.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 22th of February
   * Last modification: Time-stamp: <Aug 14 2016>
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

type path_defined_in =
  | LHS of Cckappa_sig.enriched_rule
  | RHS of Cckappa_sig.enriched_rule
  | Pattern

type event =
  | Dummy (* to avoid compilation warning *)
  | Check_rule of Ckappa_sig.c_rule_id
  | See_a_new_bond of
      ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) *
       (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state))

(*For example:
  E_1(x!1),R_2(x!1,y!2),R_3(y!2,x) -> E_1(x!1),R_2(x!1,y),R_3(y,x)

  - Agent_id: 1 (E)
  - relative_addres:
     [{
       site_out : x
       agent_type_in: R2
       site_in : x
      };
      {
        site_out: y
        agent_type_in: R3
        site_in : y
      }
      ]
  - site : y (denoted the site y of R3)

  Go to agent_id 1:
  Exit current agent E by site x, then enter an agent of type R2 via a site x.
  Exit current agent R2 by site y, then enter an agent of type R3 via a site y.
*)

type step =
  {
    site_out: Ckappa_sig.c_site_name;
    site_in: Ckappa_sig.c_site_name;
    agent_type_in: Ckappa_sig.c_agent_name
  }

type path =
  {
    defined_in: path_defined_in;
    agent_id: Ckappa_sig.c_agent_id;
    relative_address: step list;
    site: Ckappa_sig.c_site_name;
  }

module type PathMap =
sig
  type 'a t
  val empty: 'a -> 'a t
  val add: path -> 'a -> 'a t -> 'a t
  val find: path -> 'a t -> 'a option
end

module PathSetMap =
  SetMap.Make (struct type t = path let compare = compare let print _ _ = () end)

module PathMap =
  (struct
    include PathSetMap.Map

    let empty _ = empty
    let find = find_option
  end:PathMap)

type 'a fold =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Ckappa_sig.c_agent_name ->
  Ckappa_sig.c_site_name ->
  Exception.method_handler *
  ((Remanent_parameters_sig.parameters ->
    Ckappa_sig.c_state ->
    Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state ->
    Exception.method_handler * 'a ->
    Exception.method_handler * 'a) ->
   Exception.method_handler ->  'a ->
   Exception.method_handler * 'a) Usual_domains.flat_lattice

type prefold =
  {
    fold: 'a. 'a fold
  }

(*precondition is used to get the information between domains*)
type precondition =
  {
    precondition_dummy: unit (* to avoid compilation warning *);
    the_rule_is_applied_for_the_first_time: Usual_domains.maybe_bool ;
    state_of_sites_in_precondition:
      Remanent_parameters_sig.parameters ->
      Exception.method_handler ->
      Analyzer_headers.global_dynamic_information ->
      path ->
      Exception.method_handler *
      Analyzer_headers.global_dynamic_information *
      Ckappa_sig.c_state list Usual_domains.flat_lattice;
    cache_state_of_sites: Ckappa_sig.c_state list Usual_domains.flat_lattice PathMap.t ;
    partner_map:
      Exception.method_handler  -> Ckappa_sig.c_agent_name -> Ckappa_sig.c_site_name ->
      Ckappa_sig.c_state ->
      Exception.method_handler * (Ckappa_sig.c_agent_name *
                                  Ckappa_sig.c_site_name *
                                  Ckappa_sig.c_state) Usual_domains.flat_lattice;
    partner_fold: 'a. 'a fold }

let is_the_rule_applied_for_the_first_time precondition =
  precondition.the_rule_is_applied_for_the_first_time

let the_rule_is_or_not_applied_for_the_first_time bool parameter error precondition =
  match precondition.the_rule_is_applied_for_the_first_time with
  | Usual_domains.Maybe -> error,
                           {
                             precondition with
                             the_rule_is_applied_for_the_first_time = Usual_domains.Sure_value bool
                           }
  | Usual_domains.Sure_value b when b = bool -> error, precondition
  | Usual_domains.Sure_value _ ->
    Exception.warn
      parameter error __POS__
      ~message:"inconsistent computation in three-value logic"
      Exit precondition

let the_rule_is_applied_for_the_first_time p e wp =
  the_rule_is_or_not_applied_for_the_first_time true p e wp

let the_rule_is_not_applied_for_the_first_time p e wp =
  the_rule_is_or_not_applied_for_the_first_time false p e wp

let dummy_precondition =
  {
    precondition_dummy = ();
    the_rule_is_applied_for_the_first_time = Usual_domains.Maybe;
    state_of_sites_in_precondition =
      (fun _ error dynamic _ -> error, dynamic, Usual_domains.Any);
    cache_state_of_sites = PathMap.empty Usual_domains.Any;
    partner_map = (fun error _ _ _ -> error, Usual_domains.Any);
    partner_fold = (fun _ error _ _ -> error,Usual_domains.Any);
  }

(*let get_state_of_site error dynamic precondition path =
  match
    PathMap.find path precondition.cache_state_of_site
  with
  | Some output ->
    (*  let _ =
        match output with
        | Usual_domains.Val l ->
        List.iter
          (fun i -> Printf.fprintf stdout "state:%s\n" (Ckappa_sig.string_of_state_index i)) l
        | Usual_domains.Any | Usual_domains.Undefined -> ()
        in*)
    error, dynamic, precondition, output
  | None ->
    begin
      let error, dynamic, output =
        precondition.state_of_site error dynamic path
      in
      (*let _ =
        match output with
        | Usual_domains.Val l ->
          List.iter (fun i -> Printf.fprintf stdout "None_state:%i\n" (Ckappa_sig.int_of_state_index i)) l
        | _ -> ()
        in*)
      let precondition =
        {
          precondition with
          cache_state_of_site =
            PathMap.add path output precondition.cache_state_of_site
        }
      in
      error, dynamic, precondition, output
    end*)



let get_potential_partner precondition error agent_type site state =
  let error, rep = precondition.partner_map error agent_type site state in
  error, precondition, rep

let fold_over_potential_partners parameter error precondition agent_type site f init =
  match
    precondition.partner_fold parameter error agent_type site
  with
  | error, Usual_domains.Any -> error, precondition, Usual_domains.Top
  | error, Usual_domains.Undefined ->
    (* In theory, this could happen, but it would be worth being warned
       about it *)
    let error, () =
      Exception.warn
        parameter error __POS__ ~message:"bottom propagation" Exit ()
    in
    error, precondition, Usual_domains.Not_top init
  | error, Usual_domains.Val v ->
    let error, output = v f error init in
    error, precondition, Usual_domains.Not_top output

let overwrite_potential_partners_map
    (_parameters:Remanent_parameters_sig.parameters)
    (error:Exception.method_handler)
    precondition
    f
    (fold: prefold) =
  error,
  {
    precondition with
    partner_map = f;
    partner_fold =
      (fun parameters error agent_type site_type ->
         fold.fold parameters error agent_type site_type)
  }

type output =
  | Cannot_exist
  | May_exist
  | Located of Ckappa_sig.c_agent_id


let may_get_free_by_side_effect parameters kappa_handler error precondition rule path =
  let error, agent_name =
    match
      List.rev path.relative_address
    with
    | [] ->
      begin
        let agent =
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters
            error
            path.agent_id (*A*)
            rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        in
        match agent
        with
        | error, None ->
          Exception.warn
            parameters error __POS__ Exit Ckappa_sig.dummy_agent_name
        | error, Some agent ->
          begin
            match agent with
            | Cckappa_sig.Ghost
            | Cckappa_sig.Dead_agent _
            | Cckappa_sig.Unknown_agent _ ->
              Exception.warn
                parameters error __POS__ Exit
                Ckappa_sig.dummy_agent_name
            | Cckappa_sig.Agent proper_agent ->
              error,
              proper_agent.Cckappa_sig.agent_name
          end
      end
    | head::_ ->
      error, head.agent_type_in
  in
  let site_name = path.site in
  let error, is_binding_site =
    Handler.is_binding_site parameters error kappa_handler agent_name site_name
  in
  if is_binding_site
  then
    begin
      let list = rule.Cckappa_sig.actions.Cckappa_sig.half_break in
      let rec aux1 error list =
        match list
        with
        | [] -> error, false
        | (site, site_state_opt)::tail ->
          begin
            let error, site_state_list =
              match site_state_opt
              with
              | Some l ->
                begin
                  error,
                  let rec aux3 k output =
                    if Ckappa_sig.compare_state_index k l.Cckappa_sig.min < 0
                    then output
                    else aux3 (Ckappa_sig.pred_state_index k) (k::output)
                  in
                  aux3 l.Cckappa_sig.max []
                end
              | None ->
                begin
                  let error, partner =
                    precondition.partner_fold  parameters
                      error site.Cckappa_sig.agent_type
                      site.Cckappa_sig.site
                  in
                  match partner with
                  | Usual_domains.Undefined ->
                    Exception.warn parameters error __POS__ Exit
                      []
                  | Usual_domains.Val f ->
                    f
                      (fun _parameter state _ (error,list) ->
                         error, state::list)
                      error
                      []
                  | Usual_domains.Any ->
                    Exception.warn parameters error __POS__ Exit
                      []

                end
            in
            let rec aux2 error state_list =
              match state_list with
              | [] -> error, false
              | state::tail ->
                let error, opt =
                  Handler.dual
                    parameters error kappa_handler site.Cckappa_sig.agent_type
                    site.Cckappa_sig.site state
                in
                match opt with
                | None ->
                  Exception.warn parameters error __POS__ Exit true
                | Some (agent',site_name', _)
                  when agent_name = agent' && site_name = site_name'
                  -> error, true
                | Some _ ->
                  aux2 error tail
            in
            let error, output = aux2 error site_state_list in
            if output
            then
              error, output
            else
              aux1 error tail
          end

      in
      aux1 error list
    end
  else error, false


let rec post_condition parameters kappa_handler error rule precondition dynamic path =
  let cc = rule.Cckappa_sig.rule_rhs in
  (*---------------------------------------------------------*)
  (*inside the pattern, check the binding information in the lhs of the current agent*)
  let rec aux error path =
    match
      path.relative_address
    with
    | [] -> error, Located path.agent_id
    | head::tail ->
      begin
        match
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters
            error
            path.agent_id (*A*)
            cc.Cckappa_sig.bonds
        with
        | error, None -> error, May_exist
        | error, Some map ->
          begin
            match
              Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
                parameters
                error
                head.site_out (*A.x*)
                map
            with
            | error, None -> error, May_exist
            | error, Some site_add ->
              (*-----------------------------------------------*)
              (*A.x is bound to something*)
              let agent_type' = site_add.Cckappa_sig.agent_type in
              let site_type' = site_add.Cckappa_sig.site in
              (*check that A.x is bound to B.y*)
              if agent_type' = head.agent_type_in
              && site_type' = head.site_in
              then
                (*recursively apply to #i tail*)
                let next_path =
                  {
                    path with
                    agent_id = site_add.Cckappa_sig.agent_index;
                    relative_address = tail;
                  }
                in
                aux error next_path
              else
                let () = Loggers.fprintf (Remanent_parameters.get_logger parameters) "WRONG TARGET\n" in
                error, Cannot_exist
          end
      end
  in
  let error, potential_values =
    match aux error path
    with
    | error, Cannot_exist -> error, Usual_domains.Undefined
    | error, May_exist -> error, Usual_domains.Any
    | error, Located agent_id ->
      begin
        let agent =
          Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
            parameters
            error
            agent_id (*A*)
            cc.Cckappa_sig.views
        in
        match agent
        with
        | error, None ->
          Exception.warn
            parameters error __POS__ Exit Usual_domains.Undefined
        | error, Some agent ->
          begin
            match agent with
            | Cckappa_sig.Ghost
            | Cckappa_sig.Dead_agent _
            | Cckappa_sig.Unknown_agent _ ->
              Exception.warn
                parameters error __POS__ Exit Usual_domains.Undefined

            | Cckappa_sig.Agent proper_agent ->
              let error, state_opt =
                Ckappa_sig.Site_map_and_set.Map.find_option_without_logs
                  parameters
                  error
                  path.site
                  proper_agent.Cckappa_sig.agent_interface
              in
              begin
                match state_opt with
                | None -> error, Usual_domains.Any
                | Some interval ->
                  let interval = interval.Cckappa_sig.site_state in
                  let list =
                    let rec aux k output =
                      if Ckappa_sig.compare_state_index k interval.Cckappa_sig.min < 0
                      then output
                      else aux (Ckappa_sig.pred_state_index k) (k::output)
                    in
                    aux interval.Cckappa_sig.max []
                  in
                  error, Usual_domains.Val list
              end
          end
      end
  in
  match potential_values with
  | Usual_domains.Undefined | Usual_domains.Val _
    -> error, dynamic, potential_values
  | Usual_domains.Any ->
    begin
      let path =
        match path.defined_in with
        | RHS r ->
          { path with defined_in = LHS r}
        | LHS _ | Pattern -> path
      in
      let error, dynamic, precondition, values =
        get_state_of_site
          parameters kappa_handler error
          precondition dynamic path
      in
      let error, bool =
        may_get_free_by_side_effect parameters kappa_handler error precondition rule path in
      if bool
      then
        match values with
        | Usual_domains.Val l ->
          error, dynamic,
          (Usual_domains.Val
             ((Ckappa_sig.state_index_of_int 0)::l))
        | Usual_domains.Undefined | Usual_domains.Any ->
          error, dynamic, values
      else
        error, dynamic, values
    end
and get_state_of_site
    parameters kappa_handler error precondition dynamic path =
  begin
    match path.defined_in with
    | LHS _ | Pattern ->
      let error, dynamic, range =
        precondition.state_of_sites_in_precondition
          parameters error dynamic path
      in
      error, dynamic, precondition, range
    | RHS rule ->
      let error, dynamic, range =
        post_condition
          parameters kappa_handler error
          rule.Cckappa_sig.e_rule_c_rule precondition dynamic path
      in
      error, dynamic, precondition,  range
  end

let refine_information_about_state_of_sites_in_precondition
     precondition f =
  let new_f parameter error dynamic path =
    let error, dynamic, old_output =
      precondition.state_of_sites_in_precondition
        parameter error dynamic path in
    f parameter error dynamic path old_output
  in
  {
    precondition with
    cache_state_of_sites = PathMap.empty Usual_domains.Any;
    state_of_sites_in_precondition = new_f
  }

(*let get_state_of_site error dynamic precondition path =
  match
    PathMap.find path precondition.cache_state_of_sites
  with
  | Some output ->
    error, dynamic, precondition, output
  | None ->
    begin
      let error, dynamic, output =
        precondition.state_of_sites_in_precondition
          error dynamic path
      in
      let precondition =
        {
          precondition with
          cache_state_of_sites =
            PathMap.add path output precondition.cache_state_of_site
        }
      in
      error, dynamic, precondition, output
    end*)
