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

type rule_id = int
type agent_id = int
type agent_type = int
type site = int
type state = int

type event =
| Dummy (* to avoid compilation warning *)
| Check_rule of rule_id
| See_a_new_bond of ((agent_type * site * state) * (agent_type * site * state))

type step =
  {
    site_out: site;
    site_in: site;
    agent_type_in: agent_type
  }

type path =
  {
    agent_id: agent_id;
    relative_address: step list;
    site: site;
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
    type 'a t   = 'a PathSetMap.Map.t

    let empty _ = PathSetMap.Map.empty
    let add     = PathSetMap.Map.add
    let find    = PathSetMap.Map.find_option

   end:PathMap)

type precondition =
  {
    precondition_dummy: unit (* to avoid compilation warning *);
    the_rule_is_applied_for_the_first_time: Usual_domains.maybe_bool ;
    state_of_site:
      Exception.method_handler ->
      path -> Exception.method_handler * int list Usual_domains.top_or_not ;
    cache_state_of_site: int list Usual_domains.top_or_not PathMap.t ;
  }

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
    state_of_site = (fun error _ -> error, Usual_domains.Top);
    cache_state_of_site = PathMap.empty Usual_domains.Top;
  }

let get_state_of_site error precondition path =
  match
    PathMap.find path precondition.cache_state_of_site
  with
  | Some output -> error, precondition, output
  | None ->
    begin
      let error, output = precondition.state_of_site error path in
      let precondition =
	{
          precondition with
	    cache_state_of_site =
	    PathMap.add path output precondition.cache_state_of_site
        }
      in
      error, precondition, output
    end
      
let refine_information_about_state_of_site precondition f =
  let new_f error path =
    let error, _ ,old_output = get_state_of_site error precondition path in
    f error path old_output
  in
  {
    precondition with
      cache_state_of_site = PathMap.empty Usual_domains.Top;
      state_of_site = new_f
  }

