(**
   * rule_domain.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 30th of January
   * Last modification:
   *
   * Abstract domain to record live rules
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

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : unit (* no domain-specific static information *)
    }

  (*--------------------------------------------------------------------*)
  (* put here the type of the struct that contains the rest of the
     dynamic information, including the result of the analysis *)

  type local_dynamic_information = bool array
    (* this array indicates whether a rule has already be applied, or not *)

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

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)

  let get_global_dynamic_information dynamic = dynamic.global

  (** dead rule local dynamic information*)
  let get_dead_rule dynamic = dynamic.local

  let set_dead_rule dead_rule dynamic =
    {
      dynamic with local = dead_rule
    }

  (*--------------------------------------------------------------------*)
  (** intialization function of global static & dynamic information of this
      domain*)



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

  let initialize static dynamic error =
    let parameter = Analyzer_headers.get_parameter static in
    (*global static information*)
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = ();
      }
    in
    let kappa_handler = Analyzer_headers.get_kappa_handler static in
    (*global dynamic information*)
    let nrules = Handler.nrules parameter error kappa_handler in
    let init_dead_rule_array = Array.make nrules false in
    let init_global_dynamic_information =
      {
	global = dynamic;
	local = init_dead_rule_array;
      }
    in
    error, init_global_static_information, init_global_dynamic_information

  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  let is_enabled static dynamic error rule_id precondition =
    error, dynamic, Some precondition

  let apply_rule static dynamic error rule_id precondition =
  (*TO DO: set the Boolean associated to the rule rule_id to true *)
    let event_list = [] in
    error, dynamic, event_list

  (* events enable communication between domains. At this moment, the
     global domain does not collect information *)

  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state

  (**************************************************************************)

  let print static dynamic error loggers =
    (* TO DO *)
    error, dynamic, ()

end
