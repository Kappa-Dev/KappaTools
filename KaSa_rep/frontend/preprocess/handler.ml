 (**
  * print_handler.ml
  * openkappa
  * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
  *
  * Creation: 2011, the 16th of March
  * Last modification: 2015, the 4th of February
  * *
  * Primitives to use a kappa handler
  *
  * Copyright 2010,2011,2012,2013,2014,2015 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
     Exception.warn parameters mh (Some "Handler") message exn (fun () -> default)

let local_trace = true


let nrules parameter error handler = handler.Cckappa_sig.nrules
let nvars parameter error handler = handler.Cckappa_sig.nvars
let nagents parameter error handler = handler.Cckappa_sig.nagents

let translate_agent parameter error handler ag =
  let error,(a,_,_) =
    Misc_sa.unsome
      (Ckappa_sig.Dictionary_of_agents.translate parameter error ag handler.Cckappa_sig.agents_dic)
      (fun error -> warn parameter error (Some "line 36") Exit ("",(),()))
  in
    error,a

let translate_site parameter error handler agent site =
  let error,dic =
    Misc_sa.unsome
      (Int_storage.Nearly_inf_Imperatif.get parameter error agent handler.Cckappa_sig.sites)
      (fun error -> warn parameter error (Some "line 44") Exit (Ckappa_sig.Dictionary_of_sites.init ()))
  in
   let error,(a,_,_) =
    Misc_sa.unsome
      (Ckappa_sig.Dictionary_of_sites.translate parameter error site dic)
      (fun error -> warn parameter error (Some "line 36") Exit (Ckappa_sig.Internal "",(),()))
  in
   error,a

let translate_state parameter error handler agent site state =
  let error,dic =
    Misc_sa.unsome
      (Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get parameter error (agent,site) handler.Cckappa_sig.states_dic)
      (fun error -> warn parameter error (Some "line 44") Exit (Cckappa_sig.Dictionary_of_States.init ()))
  in
  let error,(a,_,_) =
    Misc_sa.unsome
      (Cckappa_sig.Dictionary_of_States.translate parameter error state dic)
      (fun error -> warn parameter error (Some "line 36") Exit (Ckappa_sig.Internal "",(),()))
  in
   error,a


let dual parameter error handler agent site state =
  Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.unsafe_get parameter error (agent,(site,state)) handler.Cckappa_sig.dual


let is_binding_site parameter error handler agent site =
    let error,site = translate_site parameter error handler agent site in
    match site
    with
     | Ckappa_sig.Internal _ -> error,false
     | Ckappa_sig.Binding _ -> error,true

let is_internal_site parameter error handler agent site =
    let error,site = translate_site parameter error handler agent site in
    match site
    with
     | Ckappa_sig.Internal _ -> error,true
     | Ckappa_sig.Binding _ -> error,false

let complementary_interface parameters error handler agent_type interface =
   let error,dic =
    Misc_sa.unsome
      (Int_storage.Nearly_inf_Imperatif.get parameters error agent_type handler.Cckappa_sig.sites)
      (fun error -> warn parameters error (Some "line 89") Exit (Ckappa_sig.Dictionary_of_sites.init ()))
   in
   let error,last_entry = Ckappa_sig.Dictionary_of_sites.last_entry parameters error dic in
   let l = Misc_sa.list_0_n last_entry in
    error,Misc_sa.list_minus l interface

let string_of_rule parameters error handler compiled rule_id =
  let rules = compiled.Cckappa_sig.rules in
  let vars = compiled.Cckappa_sig.variables in
  let nrules = nrules parameters error handler in
  if rule_id<nrules
  then
    begin
      let error,rule =
        Int_storage.Nearly_inf_Imperatif.get
          parameters
          error
          rule_id
          rules
      in
        match rule
        with
         | None -> warn parameters error (Some "line 103") Exit ""
         | Some rule ->
             let label = rule.Cckappa_sig.e_rule_label in
             let error, (m1, _) = Misc_sa.unsome (error,label)
	       (fun error -> error,Location.dummy_annot "") in
             let m1 =
	       if m1 = "" then m1
	       else
		 match
		   rule.Cckappa_sig.e_rule_initial_direction
		 with
		 | Ckappa_sig.Direct -> m1
		 | Ckappa_sig.Reverse -> Ast.flip_label m1
	     in
	     error,
             (if m1 = ""
              then ("rule "^ (string_of_int rule_id))
               else ("rule "^ string_of_int rule_id)^": "^ m1
             )
    end
  else
    begin
       let var_id = rule_id - nrules in
       let error,var =
        Int_storage.Nearly_inf_Imperatif.get
          parameters
          error
          var_id
          vars
      in
        match var
        with
         | None  -> warn parameters error (Some "line 12299") Exit ("VAR "^(string_of_int var_id))
         | Some _ ->
           (*TO DO*)
           error,"ALG"
(*
warn parameters error (Some "line 122") Exit "ALG"*)
(*         | Some Cckappa_sig.VAR_KAPPA(a,(b,c)) ->
             let m1 = b in
             let m2 = string_of_int  var_id in
           let m = m1^m2 in
               error,(if m="" then ("var"^(string_of_int var_id)) else ("var"^(string_of_int var_id)^":"^m))*)

    end

(*mapping agent of type int to string*)
let string_of_agent parameter error handler_kappa agent_type =
  let agents_dic = handler_kappa.Cckappa_sig.agents_dic in
  (*get sites dictionary*)
  let error, output =
    Ckappa_sig.Dictionary_of_agents.translate
      parameter
      error
      agent_type
      agents_dic
  in
  match output with
  | None -> warn parameter error (Some "line 162") Exit ""
  | Some (agent_name, _, _) -> error, agent_name

(*mapping site of type int to string*)
let print_site_compact site =
  match site with
  | Ckappa_sig.Internal a -> a ^ "~"
  | Ckappa_sig.Binding a -> a ^ "!"

let string_of_site_aux parameter error handler_kappa agent_name site_int =
  let error, sites_dic =
    match
      Int_storage.Nearly_inf_Imperatif.get
        parameter
        error
        agent_name
        handler_kappa.Cckappa_sig.sites
    with
    | error, None -> warn parameter error (Some "line 171") Exit
      (Ckappa_sig.Dictionary_of_sites.init())
    | error, Some i -> error, i
  in
  let error, site_type =
    match
      Ckappa_sig.Dictionary_of_sites.translate
         parameter
         error
         site_int
         sites_dic
    with
    | error, None -> warn parameter error (Some "line 194") Exit (Ckappa_sig.Internal "")
    | error, Some (value, _, _) -> error, value
  in
  error, site_type

let string_of_site parameter error handler_kappa agent_type site_int =
  let error, site_type =
    string_of_site_aux parameter error handler_kappa agent_type site_int
  in
  error, (print_site_compact site_type)

let string_of_site_in_natural_language parameter error handler_kapp agent_type site_int =
  let error, site_type =
    string_of_site_aux parameter error handler_kapp agent_type site_int
  in
  match
    site_type
  with
  | Ckappa_sig.Internal x -> error, ("the internal state of site "^x)
  | Ckappa_sig.Binding x -> error, ("the binding state of site "^x)

(*print function for contact map*)

let print_site_contact_map site =
  match site with
  | Ckappa_sig.Internal a -> a
  | Ckappa_sig.Binding a -> a



let string_of_site_contact_map parameter error handler_kappa agent_type site_int =
  let error, site_type =
    string_of_site_aux parameter error handler_kappa agent_type site_int
  in
  error, (print_site_contact_map site_type)

(*mapping state of type int to string*)

let print_state parameter error handler_kappa state =
  match state with
  | Ckappa_sig.Internal a -> error, a
  | Ckappa_sig.Binding Cckappa_sig.Free -> error, "free"
  | Ckappa_sig.Binding Cckappa_sig.Lnk_type (a, b) ->
     error, (string_of_int a) ^ "@" ^ (string_of_int b)

let print_state_fully_deciphered parameter error handler_kappa state =
  match state with
  | Ckappa_sig.Internal a -> error,a
  | Ckappa_sig.Binding Cckappa_sig.Free -> error, "free"
  | Ckappa_sig.Binding Cckappa_sig.Lnk_type (a, b) ->
     let error, ag = string_of_agent parameter error handler_kappa a in
     let error, site = string_of_site_contact_map parameter error handler_kappa a b in
     error, ag ^ "@" ^ site

let string_of_state_gen print_state parameter error handler_kappa agent_type site_int state =
  let error, state_dic =
    match Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.get
      parameter
      error
      (agent_type, site_int)
      handler_kappa.Cckappa_sig.states_dic
    with
    | error, None -> warn parameter error (Some "line 206") Exit
      (Cckappa_sig.Dictionary_of_States.init())
    | error, Some i -> error, i
  in
  let error, value =
    match
      Cckappa_sig.Dictionary_of_States.translate
        parameter
        error
        state
        state_dic
    with
    | error, None -> warn parameter error (Some "line 228") Exit (Ckappa_sig.Internal "")
    | error, Some (value, _, _) -> error, value
  in
  print_state parameter error handler_kappa value

let string_of_state = string_of_state_gen print_state
let string_of_state_fully_deciphered = string_of_state_gen print_state_fully_deciphered

let print_labels_txt parameters error handler couple =
   let _ = Quark_type.Labels.dump_couple parameters error handler couple
   in error

let print_labels_dot parameters error handler couple =
   let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "[label =\"" in
   let _ = Quark_type.Labels.dump_couple parameters error handler couple in
   let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"]" in
     error

let get_label_of_rule_txt parameters error rule = error, rule.Cckappa_sig.e_rule_label
let get_label_of_rule_dot parameters error rule = error, rule.Cckappa_sig.e_rule_label_dot


let get_label_of_var_txt parameters error rule = error,rule.Cckappa_sig.e_id
let get_label_of_var_dot parameters error rule = error,rule.Cckappa_sig.e_id_dot

let print_rule_txt parameters error rule_id m1 m2 rule =
  let m = "'"^ m1 ^"' " in
  let error, _ = error,
    Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s"
      (if m = ""
       then ("rule(" ^ (string_of_int rule_id) ^ "): ")
       else ("rule(" ^ string_of_int rule_id) ^ "):"^ m) in
  let error = Print_ckappa.print_rule parameters error rule in
  error

let print_var_txt parameters error var_id m1 m2 var =
  let m = "'"^m1^"' " in
  let error,_ = error,Loggers.fprintf (Remanent_parameters.get_logger parameters) "%s" (if m="" then ("var("^(string_of_int var_id)^")") else ("var("^string_of_int var_id)^"):"^m) in
   let error = Print_ckappa.print_alg parameters error var  in
     error

let print_rule_dot parameters error rule_id m1 m2 rule =
  let error =
     if m1<>"" && (not (Remanent_parameters.get_prompt_full_rule_def parameters))
     then
       let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"%s" m1 in
       error
     else
       let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"%s:" m2 in
       let error = Print_ckappa.print_rule parameters error rule in
       error
  in
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"" in
  error

 let print_var_dot parameters (error:Exception.method_handler)  var_id m1 m2 var =
   let error =
     if m1<>"" && (not (Remanent_parameters.get_prompt_full_var_def parameters))
     then
       let _ =
	 Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"%s" m1
       in error
     else
       let _ =
	 Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"%s:" m2 in
       let error = Print_ckappa.print_alg parameters error var
       in error
   in
   let _ = Loggers.fprintf (Remanent_parameters.get_logger parameters) "\"" in
   error

 let print_rule_or_var parameters error handler compiled print_rule print_var get_label_of_rule get_label_of_var rule_id =
  let rules = compiled.Cckappa_sig.rules in
  let vars = compiled.Cckappa_sig.variables in
  let nrules = nrules parameters error handler in
  if rule_id<nrules
  then
    begin
      let error,rule =
        Int_storage.Nearly_inf_Imperatif.get
          parameters
          error
          rule_id
          rules
      in
        match rule
        with
         | None ->
	   let a,b = warn parameters error (Some "line 103") Exit () in
	   a,false,b
         | Some rule ->
             let error,label = get_label_of_rule parameters error rule in
	     let error, (m1,_) =
               Misc_sa.unsome (error,label) (fun error -> error,(Location.dummy_annot "")) in
	     let m1 =
	       if m1 = ""
               then m1
	       else
		 match rule.Cckappa_sig.e_rule_initial_direction with
		   | Ckappa_sig.Direct -> m1
		   | Ckappa_sig.Reverse -> Ast.flip_label m1
	     in
             let error =
               print_rule
                 parameters
                 error
                 rule_id
                 m1
                 (string_of_int rule_id)
                 rule.Cckappa_sig.e_rule_rule
             in
             error,true,()
    end
  else
    begin
       let var_id = rule_id - nrules in
       let error,var =
        Int_storage.Nearly_inf_Imperatif.get
          parameters
          error
          var_id
          vars
      in
        match var
        with
         | None  ->
           let a,b = warn parameters error (Some "line 122") Exit ()
	   in a,false,b
         | Some var ->
	   let b = var.Cckappa_sig.c_variable in
	   let error,m1 = get_label_of_var parameters error var in
           let m2 = string_of_int var_id in
           let error =
             print_var parameters error var_id m1 m2 b
           in error,true,()
    end

