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

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Composite domain") message exn
    (fun () -> default)

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

  let lift f x = f (get_global_static_information x)

  let get_parameter static = lift Analyzer_headers.get_parameter static
      
  let get_compil static = lift Analyzer_headers.get_cc_code static

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
    
  let get_working_list dynamic = dynamic.rule_working_list

  let set_working_list rule_working_list dynamic =
    {
      dynamic with rule_working_list = rule_working_list
    }

  let get_domain dynamic = dynamic.domain
    
  let set_domain domain dynamic =
    {
      dynamic with domain = domain
    }
    
  let push_rule static dynamic error r_id =
    let working_list = get_working_list dynamic in
    let parameter = get_parameter static in
    let error, rule_working_list =
      Fifo.IntWL.push parameter error r_id working_list
    in
    let dynamic = set_working_list rule_working_list dynamic in
    error, dynamic

  (**[next_rule static dynamic] returns a rule_id inside a working list
     if it is not empty*)

  let next_rule static dynamic error =
    let working_list = get_working_list dynamic in
    (* see if the working list is empty, if not pop an element *)
    if Fifo.IntWL.is_empty working_list
    then error, dynamic, None
    else
      let parameter = get_parameter static in
      let error, (rule_id_op, working_list_tail) =
        Fifo.IntWL.pop parameter error working_list
      in
      let dynamic = set_working_list working_list_tail dynamic in
      error, dynamic, rule_id_op

  (*for each rule with no lhs, push the rule in the working list *)
  let push_rule_creation static dynamic error rule_id rule =
    let parameter = get_parameter static in
    let error, dynamic =
      List.fold_left (fun (error, dynamic) (agent_id, agent_type) ->
        let error, agent = 
          Bdu_analysis_type.AgentMap.get parameter error agent_id 
            rule.Cckappa_sig.rule_rhs.Cckappa_sig.views
        in
        match agent with
        | Some Cckappa_sig.Dead_agent _ 
        | Some Cckappa_sig.Ghost -> error, dynamic
        | None -> warn parameter error (Some "line 156") Exit dynamic
        | Some Cckappa_sig.Unknown_agent _ 
        | Some Cckappa_sig.Agent _ ->
          let error, dynamic =
            push_rule static dynamic error rule_id
          in
          error, dynamic 
      ) (error, dynamic) rule.Cckappa_sig.actions.Cckappa_sig.creation
    in
    error, dynamic
    
  let scan_rule_creation static dynamic error =
    let parameter = get_parameter static in
    let compil = get_compil static in
    let rules = compil.Cckappa_sig.rules in
    let error, dynamic =
      Int_storage.Nearly_inf_Imperatif.fold
        parameter error
        (fun parameter error rule_id rule dynamic ->
          let error, dynamic =
            push_rule_creation
              static
              dynamic
              error
              rule_id
              rule.Cckappa_sig.e_rule_c_rule
          in
          error, dynamic
        )
        rules dynamic
    in
    error, dynamic

  let initialize static dynamic error =
    let error, domain_static, domain_dynamic =
      Domain.initialize static dynamic error
    in
    let static = static, domain_static in
    let working_list = empty_working_list in
    let dynamic = 
      {
        rule_working_list = working_list;
        domain = domain_dynamic
      }
    in
    let error, dynamic =
      scan_rule_creation
        static
        dynamic
        error
    in
    error, static, dynamic 
	   
  (**[lift_unary f static dynamic] is a function lifted of unary type,
     returns information of dynamic information and its output*)

  let lift_unary f static dynamic error a =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a
    in
    let dynamic = set_domain domain_dynamic dynamic in
    error, dynamic, output
      
  (**[pre_add_initial_state static dynamic error a] returns a pair of
     type unary where the output of static information [a] is an initial state
     of analyzer header, and the dynamic output [a list of event] is unit. *)

  let pre_add_initial_state static dynamic error a =
    lift_unary Domain.add_initial_state static dynamic error a

  let lift_binary f static dynamic error a b =
    let error, domain_dynamic, output =
      f (get_domain_static_information static) dynamic.domain error a b
    in
    let dynamic = set_domain domain_dynamic dynamic in
    error, dynamic, output

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
