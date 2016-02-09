(**
   * analyzer_sig.mli
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


module Product 
  (New_domain:Analyzer_domain_sig.Domain)
  (Underlying_domain:Analyzer_domain_sig.Domain) =
  
  (struct
      
    type ('a, 'b) pair =
      {
	new_domain       : 'a;
	underlying_domain: 'b
      }
   	
    type static_information =
      (New_domain.static_information, Underlying_domain.static_information) pair

    type local_dynamic_information =
      (New_domain.local_dynamic_information,
       Underlying_domain.local_dynamic_information) pair
        
    type dynamic_information =
      {
	local : local_dynamic_information;
	global: Analyzer_headers.global_dynamic_information;
      }

    let smash_dynamic underlying_domain new_domain =
      {
	global = new_domain.New_domain.global;
	local =
	  {
	    new_domain = new_domain.New_domain.local;
	    underlying_domain = underlying_domain.Underlying_domain.local
	  }}

    let underlying_domain_dynamic_information dynamic =
      {
	Underlying_domain.global = dynamic.global;
	Underlying_domain.local = dynamic.local.underlying_domain
      }
    let new_domain_dynamic_information underlying_dynamic global_dynamic =
      {
	New_domain.global = underlying_dynamic.Underlying_domain.global;
	New_domain.local = global_dynamic.local.new_domain
      }

    let initialize global_static_information global_dynamic_information error =
      let error, underlying_domain_static_information,
        underlying_domain_dynamic_information =
        Underlying_domain.initialize global_static_information 
          global_dynamic_information 
          error 
      in
      let error, new_domain_static_information, new_domain_dynamic_information =
        New_domain.initialize global_static_information
          underlying_domain_dynamic_information.Underlying_domain.global 
          error 
      in
      error,
      {
        new_domain        = new_domain_static_information;
        underlying_domain = underlying_domain_static_information
      },
      smash_dynamic underlying_domain_dynamic_information new_domain_dynamic_information
	
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

    let add_initial_state static dynamic error initial_state =
      let error, underlying_domain_dynamic, event_list =
        Underlying_domain.add_initial_state 
          static.underlying_domain
          (underlying_domain_dynamic_information dynamic)
          error 
          initial_state
      in
      let error, new_domain_dynamic, event_list' =
        New_domain.add_initial_state
          static.new_domain
          (new_domain_dynamic_information underlying_domain_dynamic dynamic)
	  error
          initial_state
      in
      error,
      smash_dynamic underlying_domain_dynamic new_domain_dynamic,
      List.fold_left (fun list a -> a :: list) event_list event_list'
    (* be careful, the concatenation should be done in the correct order to
       get a linear time complexity instead of a quadratic one*)

    let is_enabled static dynamic error rule_id precondition =
      let error, underlying_domain_dynamic_information, output_opt =
        Underlying_domain.is_enabled
          static.underlying_domain
          (underlying_domain_dynamic_information dynamic)
	  error
          rule_id
          precondition
      in
      let new_domain_dynamic_information =
	new_domain_dynamic_information underlying_domain_dynamic_information dynamic
      in
      match output_opt with
      | None ->
	error,
	smash_dynamic underlying_domain_dynamic_information new_domain_dynamic_information,
	None
      | Some precondition ->
	let error, new_domain_dynamic_information, output_opt =
          New_domain.is_enabled
            static.new_domain
            new_domain_dynamic_information
            error
            rule_id
            precondition
        in
	let dynamic = 
          smash_dynamic 
            underlying_domain_dynamic_information 
            new_domain_dynamic_information 
        in
	error, dynamic, output_opt

    let apply_rule static dynamic error rule_id precondition =
      let error, underlying_domain_dynamic_information, event_list =
        Underlying_domain.apply_rule
          static.underlying_domain 
          (underlying_domain_dynamic_information dynamic)
	  error
          rule_id
          precondition
      in
      let error, new_domain_dynamic_information, event_list' =
        New_domain.apply_rule 
          static.new_domain
          (new_domain_dynamic_information
             underlying_domain_dynamic_information 
             dynamic)
	  error 
          rule_id
          precondition 
      in
      error,
      smash_dynamic
        underlying_domain_dynamic_information 
        new_domain_dynamic_information,
      List.fold_left (fun list a -> a :: list) event_list event_list'
    (* be careful, the concatenation should be done in the correct order to get
       a linear time complexity instead of a quadratic one*)       
        
    let apply_event_list static dynamic error event_list =
      let error, underlying_domain_dynamic_information, event_list' = 
        Underlying_domain.apply_event_list
          static.underlying_domain
          (underlying_domain_dynamic_information dynamic)
	  error
          event_list
      in
      let error, new_domain_dynamic_information, event_list'' = 
        New_domain.apply_event_list 
          static.new_domain
          (new_domain_dynamic_information
             underlying_domain_dynamic_information 
             dynamic)
	  error
          event_list
      in
      let event_list = List.fold_left (fun list a -> a :: list) event_list' event_list'' in 
      (* be careful, the concatenation should be done in the correct order to get
         a linear time complexity instead of a quadratic one*)
      error,
      smash_dynamic
        underlying_domain_dynamic_information 
        new_domain_dynamic_information,
      event_list
        
    let export static dynamic error kasa_state =
      let error, underlying_domain_dynamic_information, kasa_state =
        Underlying_domain.export 
          static.underlying_domain
          (underlying_domain_dynamic_information dynamic)
	  error 
          kasa_state
      in
      let error, new_domain_dynamic_information, kasa_state =
        New_domain.export
          static.new_domain 
          (new_domain_dynamic_information 
             underlying_domain_dynamic_information 
             dynamic)
	  error
          kasa_state
      in
      error,
      smash_dynamic 
        underlying_domain_dynamic_information 
        new_domain_dynamic_information,
      kasa_state
	
    let print static dynamic error loggers =
      let error, underlying_domain_dynamic_information, () =
        Underlying_domain.print
          static.underlying_domain 
          (underlying_domain_dynamic_information dynamic)
	  error
          loggers
      in
      let error, new_domain_dynamic_information, () =
        New_domain.print 
          static.new_domain
          (new_domain_dynamic_information 
             underlying_domain_dynamic_information 
             dynamic)
	  error
          loggers
      in
      error,
      smash_dynamic
        underlying_domain_dynamic_information 
        new_domain_dynamic_information,
      ()
        
   end:Analyzer_domain_sig.Domain)
    
    
    
