(**
   * contact_map_domain.ml
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
  Exception.warn parameters mh (Some "Rule domain") message exn
    (fun () -> default)

let local_trace = false

module Domain =
struct

  module Set_triple =
    Map_wrapper.Make
      (SetMap.Make (
        struct
          type t = int * int * int
          let compare = compare
        end))

  module Int2Map_CM_state =
    Map_wrapper.Make 
      (SetMap.Make
         (struct 
           type t = int * int * int
           let compare = compare
          end))

  type static_information =
    {
      global_static_information : Analyzer_headers.global_static_information;
      domain_static_information : unit
    }
  
  type local_dynamic_information =
    {
      contact_map_dynamic   : Set_triple.Set.t Int2Map_CM_state.Map.t;
      contact_map_syntactic : Set_triple.Set.t Int2Map_CM_state.Map.t
    }
      
  type dynamic_information =
    {
      local  : local_dynamic_information;
      global : Analyzer_headers.global_dynamic_information
    }

  (**************************************************************************)

  let get_global_static_information static = static.global_static_information

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static

  let get_kappa_handler static = lift Analyzer_headers.get_kappa_handler static

  let get_compil static = lift Analyzer_headers.get_cc_code static

  (*--------------------------------------------------------------------*)
  (** global dynamic information*)
    
  let get_local_dynamic_information dynamic = dynamic.local

  let set_local_dynamic_information local dynamic =
    {
      dynamic with local = local
    }
    
  let get_contact_map_dynamic dynamic =
    (get_local_dynamic_information dynamic).contact_map_dynamic

  let set_contact_map_dynamic contact dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          contact_map_dynamic = contact
      } dynamic

  let get_contact_map_syntactic dynamic =
    (get_local_dynamic_information dynamic).contact_map_syntactic

  let set_contact_map_syntactic contact dynamic =
    set_local_dynamic_information
      {
        (get_local_dynamic_information dynamic) with
          contact_map_syntactic = contact
      } dynamic

  (**************************************************************************)

  let init_local_dynamic_information =
    let init_contact_map_dynamic = Int2Map_CM_state.Map.empty in
    let init_contact_map_syntactic = Int2Map_CM_state.Map.empty in
    {
      contact_map_dynamic   = init_contact_map_dynamic;
      contact_map_syntactic = init_contact_map_syntactic
    }

  let initialize static dynamic error =
    let init_global_static_information =
      {
        global_static_information = static;
        domain_static_information = ()
      }
    in
    let init_global_dynamic_information =
      {
        local  = init_local_dynamic_information;
        global = dynamic
      }
    in
    error, init_global_static_information, init_global_dynamic_information
      
  (**************************************************************************)

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

  let add_initial_state static dynamic error species =
    let event_list = [] in
    error, dynamic, event_list

  let is_enabled static dynamic error rule_id precondition =
    error, dynamic, Some precondition

  let apply_rule static dynamic error rule_id precondition =
    let event_list = [] in
    error, dynamic, (precondition, event_list)
      
  let rec apply_event_list static dynamic error event_list =
    let event_list = [] in
    error, dynamic, event_list

  let export static dynamic error kasa_state =
    error, dynamic, kasa_state
      
  let print static dynamic error loggers =
    error, dynamic, ()

end
