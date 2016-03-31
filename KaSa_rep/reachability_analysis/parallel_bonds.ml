(**
   * parallel_bonds.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 31th of March
   * Last modification:
   *
   * Abstract domain to detect whether when two sites of an agent are bound, they must be bound to the same agent.
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  (* the type of the struct that contains all static information as in the
     previous version of the analysis *)

  (* domain specific info: *)
  (* collect the set of tuples (A,x,y,B,z,t) such that there exists a rule with two agent of type A and B and two bonds between A.x and B.z,  and A.y and B.t  *)
  (* for each tuple, collect a map 
      -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain which rules can create a bond of type A.x.z.B (and at which position *)
  (* a map 
      -> (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain which rules can create a bond of type A.z.t.B (and at which position *) 
  (*    and a map (A,x,y,B,z,t) -> (Ag_id,Ag_id) list RuleIdMap to explain which rules can contain parallel bonds in their lhs *)
   type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : unit (* to be replaced *)
    }

  (*--------------------------------------------------------------------*)
  
      (* one map: for each tuple: Yes, No, Maybe, *)
      (* Yes: to say that when the sites x and y are bound with sites of the good type, then they are bound to the same B*)
      (* No: to say that when the sites x and y are bound with sites of the good type, then they are never bound to the same B*)
      (* Maybe: both case may happen*)


type local_dynamic_information = unit (* to be replaced *)

  type dynamic_information =
    {
      local  : local_dynamic_information ;
      global : Analyzer_headers.global_dynamic_information ;
    }

  (*--------------------------------------------------------------------*)
  (** global static information.
      explain how to extract the handler for kappa expressions from a value
      of type static_information. Kappa handler is static and thus it should
      never updated. *)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

 

  (*--------------------------------------------------------------------*)

  type 'a zeroary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> Exception.method_handler * dynamic_information * 'a

  type ('a, 'b) unary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> Exception.method_handler * dynamic_information * 'b

  type ('a, 'b, 'c) binary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> Exception.method_handler * dynamic_information * 'c

  (**************************************************************************)
  (** [get_scan_rule_set static] *)

  (* todo *)
  let initialize static dynamic error =
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = ();
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    let parameter = Analyzer_headers.get_parameter static in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = () ;
      }
    in
    error, init_global_static_information, init_global_dynamic_information

  (* to do *)
  (* take into account parallel bounds that may occur in initial states *)
  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  (* to do *)
  (* if a parallel boudn occur in a lhs, check that this is possible *)
  let is_enabled static dynamic error (rule_id:Ckappa_sig.c_rule_id) precondition =
      error, dynamic, Some precondition

  (* to do, when one bond is created, 
check in the precondition, whether the two other sites may be bound, check whether they must be bound to the same agents, whether they cannot be bound to the same agent, whether we cannot know, and deal with accordingly *)
  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    let parameter = get_parameter static in
    let kappa_handler = get_kappa_handler static in
    let compil = get_compil static in
    error, dynamic, (precondition, event_list)

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)

  (* to do *)
  let print static dynamic error loggers =
    error, dynamic, ()

  let lkappa_mixture_is_reachable static dynamic error lkappa =
    error, dynamic, Usual_domains.Maybe (* to do *)

  let cc_mixture_is_reachable static dynamic error ccmixture =
    error, dynamic, Usual_domains.Maybe (* to do *)

end
