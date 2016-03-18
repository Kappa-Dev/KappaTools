(**
   * communication.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 22th of February
   * Last modification:
   *
   * Abstract domain to record live rules
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "communication") message exn
    (fun () -> default)

type event =
| Dummy (* to avoid compilation warning *)
| Check_rule of Ckappa_sig.c_rule_id
| See_a_new_bond of
    ((Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state) * 
        (Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name * Ckappa_sig.c_state))

type step =
  {
    site_out: Ckappa_sig.c_site_name;
    site_in: Ckappa_sig.c_site_name;
    agent_type_in: Ckappa_sig.c_agent_name
  }

type path =
  {
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
  SetMap.Make (struct type t = path let compare = compare end)

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
   state_of_site:
     Exception.method_handler ->
     Analyzer_headers.global_dynamic_information ->
     path ->
     Exception.method_handler * 
       Analyzer_headers.global_dynamic_information * 
       Ckappa_sig.c_state list Usual_domains.flat_lattice;
   cache_state_of_site: Ckappa_sig.c_state list Usual_domains.flat_lattice PathMap.t ;
   partner_map: Ckappa_sig.c_agent_name -> Ckappa_sig.c_site_name -> 
                Ckappa_sig.c_state -> (Ckappa_sig.c_agent_name * 
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
  | Usual_domains.Sure_value _ -> warn parameter error
    (Some "inconsistent computation in three-value logic") Exit precondition

let the_rule_is_applied_for_the_first_time p e wp =
  the_rule_is_or_not_applied_for_the_first_time true p e wp

let the_rule_is_not_applied_for_the_first_time p e wp =
  the_rule_is_or_not_applied_for_the_first_time false p e wp

let dummy_precondition =
  {
    precondition_dummy = ();
    the_rule_is_applied_for_the_first_time = Usual_domains.Maybe;
    state_of_site = (fun error dynamic _ -> error, dynamic, Usual_domains.Any);
    cache_state_of_site = PathMap.empty Usual_domains.Any;
    partner_map = (fun _ _ _ -> Usual_domains.Any);
    partner_fold = (fun _ error _ _ -> error,Usual_domains.Any);
  }
    
let get_state_of_site error dynamic precondition path =
  match
    PathMap.find path precondition.cache_state_of_site
  with
  | Some output -> error, dynamic, precondition, output
  | None ->
    begin
      let error, dynamic, output = precondition.state_of_site error dynamic path in
      let precondition =
	{
          precondition with
	    cache_state_of_site =
	    PathMap.add path output precondition.cache_state_of_site
        }
      in
      error, dynamic, precondition, output
    end

let refine_information_about_state_of_site precondition f =
  let new_f error dynamic path =
    let error, dynamic, _ ,old_output = get_state_of_site error dynamic precondition path in
    f error dynamic path old_output
  in
  {
    precondition with
      cache_state_of_site = PathMap.empty Usual_domains.Any;
      state_of_site = new_f
  }

let get_potential_partner precondition agent_type site state =
  precondition, precondition.partner_map agent_type site state

let fold_over_potential_partners parameter error precondition agent_type site f init =
  match
    precondition.partner_fold parameter error agent_type site
  with
  | error, Usual_domains.Any -> error, precondition, Usual_domains.Top
  | error, Usual_domains.Undefined ->
    (* In theory, this could happen, but it would be worth being warned
       about it *)
    let error, () =
      warn parameter error (Some "line 142, bottom propagation") Exit ()
    in
    error, precondition, Usual_domains.Not_top init
  | error, Usual_domains.Val v ->
    let error, output = v f error init in 
    error, precondition, Usual_domains.Not_top output

let overwrite_potential_partners_map
    (parameters:Remanent_parameters_sig.parameters)
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
