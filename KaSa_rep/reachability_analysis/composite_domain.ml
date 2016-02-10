(**
  * analyzer_headers.mli
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2016, the 30th of January
  * Last modification:
  *
  * Compute the relations between sites in the BDU data structures
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
  * en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module type Composite_domain =
  sig
    type static_information
    type dynamic_information

    val initialize:
      Analyzer_headers.global_static_information ->
      Analyzer_headers.global_dynamic_information ->
      Exception.method_handler ->
      Exception.method_handler * static_information * dynamic_information

    type 'a zeroary =
      static_information
      -> dynamic_information
      -> Exception.method_handler
      -> Exception.method_handler * dynamic_information * 'a

    type ('a,'b) unary =
      static_information
      -> dynamic_information
      -> Exception.method_handler
      -> 'a
      -> Exception.method_handler * dynamic_information * 'b

    type ('a,'b,'c) binary =
      static_information
      -> dynamic_information
      -> Exception.method_handler
      -> 'a
      -> 'b
      -> Exception.method_handler * dynamic_information * 'c

    val next_rule: Analyzer_headers.rule_id option zeroary

    val add_initial_state: (Analyzer_headers.initial_state, unit) unary

    val is_enabled: (Analyzer_headers.rule_id, Analyzer_headers.precondition option) unary

    val apply_rule: (Analyzer_headers.rule_id, Analyzer_headers.precondition,unit) binary

    val export: (Analyzer_headers.kasa_state, Analyzer_headers.kasa_state) unary

    val print: (Loggers.t list, unit) unary

  end

(*****************************************************************************************)
(*Analyzer is a functor takes a module Domain as its parameter.*)

module Make (Domain:Analyzer_domain_sig.Domain) =
struct

  type static_information =
    Analyzer_headers.global_static_information * Domain.static_information

  type working_list =
    Fifo.IntWL.WSetMap.elt list * Fifo.IntWL.WSetMap.elt list * Fifo.IntWL.WSetMap.Set.t

  type dynamic_information =
    {
      rule_working_list: working_list;
      domain           : Domain.dynamic_information
    }

  let get_global_static_information = fst

  let get_domain_static_information = snd

  let get_parameter static =
    Analyzer_headers.get_parameter (get_global_static_information static)

  let empty_working_list = Fifo.IntWL.empty

  

  type 'a zeroary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> Exception.method_handler * dynamic_information * 'a

  type ('a,'b) unary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> Exception.method_handler * dynamic_information * 'b

  type ('a,'b,'c) binary =
    static_information
    -> dynamic_information
    -> Exception.method_handler
    -> 'a
    -> 'b
    -> Exception.method_handler * dynamic_information * 'c

  (** push r_id in the working_list *)
  let push_rule (static:static_information) dynamic error r_id =
    let working_list = dynamic.rule_working_list in
    let parameter = get_parameter static in
    let error, rule_working_list =
      Fifo.IntWL.push parameter error r_id working_list
    in
    error,
    {
      dynamic with rule_working_list = rule_working_list
    }

  (**[next_rule static dynamic] returns a rule_id inside a working list
     if it is not empty*)

  let next_rule static dynamic error =
    let working_list = dynamic.rule_working_list in
    (* see if the working list is empty, if not pop an element *)
    if Fifo.IntWL.is_empty working_list
    then error, dynamic, None
    else
      let parameter = get_parameter static in
      let error, (rule_id_op, working_list_tail) =
        Fifo.IntWL.pop parameter error working_list
      in
      error,
      {
        dynamic with rule_working_list = working_list_tail
      }, rule_id_op

  (**[lift_unary f static dynamic] is a function lifted of unary type,
     returns information of dynamic information and its output*)

  let initialize global_static dynamic error =
    let error, domain_static, dynamic =
      Domain.initialize global_static dynamic error
    in
    let static_information = global_static, domain_static in
    let working_list = empty_working_list in
    (* to do *)
    (* for each rule with no lhs, push the rule in the working list *)
    error, static_information,
    {
      rule_working_list = working_list;
      domain            = dynamic
    }
	   
  let lift_unary f (static:static_information) dynamic error a =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a
    in
    error,
    {
      dynamic with domain = domain_dynamic
      }, output

  (**[pre_add_initial_state static dynamic error a] returns a pair of
     type unary where the output of static information [a] is an initial state
     of analyzer header, and the dynamic output [a list of event] is unit. *)

  let pre_add_initial_state static dynamic error a =
    lift_unary Domain.add_initial_state static dynamic error a

  let lift_binary f (static:static_information) dynamic error a b =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a b
    in
    error,
    {
      dynamic with domain = domain_dynamic
    }, output

  (**[is_enabled static dynamic error a] returns a triple of type binary
     when given a rule_id [a], check that if this rule is enable or not *)

  let is_enabled static dynamic error a =
    lift_binary
      Domain.is_enabled
      static
      dynamic
      error
      a
      Analyzer_headers.dummy_precondition

  (**[pre_apply_rule static dynamic error a b] *)
  let pre_apply_rule static dynamic error a b =
    lift_binary
      Domain.apply_rule
      static
      dynamic
      error
      a
      b

  (**apply a list of event if it is empty then do nothing, otherwise go
     through this list and at each event push rule_id inside a working
     list and apply this list of event with new dynamic information*)

  let rec apply_event_list (static:static_information) dynamic error event_list =
    if event_list = []
    then
      error, dynamic, ()
    else
      let error, dynamic, event_list' =
        lift_unary
          Domain.apply_event_list
          static
          dynamic
          error
          event_list
      in
      let error, dynamic =
	List.fold_left (fun (error, dynamic) event ->
	  match event with
	  | Analyzer_headers.Check_rule rule_id ->
            push_rule
              static
              dynamic
              error
              rule_id
	  | _ -> error, dynamic
        )(error, dynamic) event_list
      in
      apply_event_list static dynamic error event_list'

  (** add initial state then apply a list of event starts from this new
      list*)

  let add_initial_state static dynamic error initial_state =
    let error, dynamic, event_list =
      pre_add_initial_state
        static
        dynamic
        error
        initial_state
    in
    apply_event_list static dynamic error event_list

  (**if it has a precondition for this rule_id then apply a list of event
     starts from this new list*)

  let apply_rule static dynamic error r_id precondition =
    let error, dynamic, event_list =
      pre_apply_rule
        static
        dynamic
        error
        r_id
        precondition
    in
    apply_event_list static dynamic error event_list

  let export static dynamic error kasa_state =
    lift_unary Domain.export static dynamic error kasa_state

  let print static dynamic error loggers =
    lift_unary Domain.print static dynamic error loggers

end
