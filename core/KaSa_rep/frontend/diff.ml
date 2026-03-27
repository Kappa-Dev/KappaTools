type ('rule,'init) summary_file = 
  {
    summary_rule_map: (int * 'rule)  Mods.StringMap.t ; 
    summary_init_state_map: (int * 'init)  Mods.StringMap.t ;
 (*   summary_rule_set: Mods.StringSet.t ; 
    summary_init_state_set: Mods.StringSet.t ;*)
    } 

type ('a, 'b) summary = ('a,'b) summary_file Mods.StringMap.t 


type diff_elt = {new_elt: int list; removed_elt: int list ; pos_renaming: (Loc.t * Loc.t) list}
type diff = 
 {
    diff_rules: diff_elt ; 
    diff_init: diff_elt ; 
 }

 type new_indexs=
 { 
   next_rule: Ckappa_sig.c_rule_id ; 
   next_init: int; 
   next_nsites: Ckappa_sig.c_site_name;
   next_nr_predicates: Ckappa_sig.c_guard_parameter
 }

 let starting_new_elt = 
   {
    next_rule = Ckappa_sig.rule_id_of_int 0; 
    next_init = 0; 
    next_nsites = Ckappa_sig.site_name_of_int 0 ; 
    next_nr_predicates = Ckappa_sig.guard_parameter_of_int 0 ;
   }
 let empty_summary_file = 
   {
    summary_rule_map =  Mods.StringMap.empty; 
    summary_init_state_map = Mods.StringMap.empty;   
    } 
 
let get_file _parameters errors ~filename summary = 
  match 
    Mods.StringMap.find_option filename summary
  with  
    | None -> errors, empty_summary_file 
    | Some x -> errors, x 

let renaming_of_diff diff = 
Loc.fun_of_list (diff.diff_rules.pos_renaming @ diff.diff_init.pos_renaming)

let dump_summary parameters _error summary = 
  let logger = Remanent_parameters.get_logger parameters in 
  let () = Loggers.fprintf logger "     RULE_MAP" in 
  let () = Loggers.print_newline logger in    
  let () = 
    Mods.StringMap.iter
      (fun s i -> 
        let () = Loggers.fprintf logger "         FILE: %s" s in 
        let () = Loggers.print_newline logger in 
        let () = Loggers.fprintf logger "             Rules:" in 
        let () = Loggers.print_newline logger in 
        let () = 
          Mods.StringMap.iter
            (fun s (i,_) -> 
              let () = Loggers.fprintf logger "                %s -> %i" s i in 
              let () = Loggers.print_newline logger in ())
            i.summary_rule_map
        in 
        let () = Loggers.print_newline logger in 
        let () = Loggers.fprintf logger "             Init:" in 
        let () = Loggers.print_newline logger in 
        let () = 
          Mods.StringMap.iter
            (fun s (i,_) -> 
              let () = Loggers.fprintf logger "                %s -> %i" s i in 
              let () = Loggers.print_newline logger in ())
            i.summary_init_state_map
        in 
        let () = Loggers.print_newline logger in 
         let () = Loggers.print_newline logger in   
         ())
      summary
      in () 

let summarize_gen get_file_name get_string get_map set_map parameters error id  elt summary = 
  let _ = parameters in 
  let file_name = get_file_name elt in 
  let summary_file = 
     match 
       Mods.StringMap.find_option file_name summary 
     with 
      | Some summary_file -> summary_file 
      | None -> empty_summary_file 
   in 
   let error, string = get_string parameters error elt in 
   let map = get_map summary_file in 
   let map = Mods.StringMap.add string (id, elt) map in 
   let summary_file = set_map map summary_file in 
   error, Mods.StringMap.add file_name summary_file summary 

let summarize_rule_from_ast parameters error id (rule:Ast.rule Ast.compil_rule) summary = 
  let get_file_name (_,_,_,(_,loc)) = 
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string _parameters error (rule:Ast.rule Ast.compil_rule) = 
    let (_,_,_,(ast_rule,_)) = rule in 
    let b = Buffer.create 1 in 
    let fmt = Format.formatter_of_buffer b in 
    let () = Ast.print_ast_rule fmt ast_rule in 
    let () = Format.pp_print_flush fmt () in 
    let string = Buffer.contents b in 
    let label = 
      let (_,label_opt,_,_) = rule in 
      match label_opt with 
      | None -> "" 
      | Some (x,_) -> x 
    in 
    error, label^"."^string 
  in 
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_rule_map) 
      (fun summary_rule_map x -> {x with summary_rule_map})
      parameters error id rule summary

let summarize_init_state_from_ast parameters error id (init_state:(Ast.mixture,Ast.mixture,string) Ast.init_statement) summary = 
  let get_file_name (init_state:(Ast.mixture,Ast.mixture,string) Ast.init_statement)  = 
      let (_,_,init) = init_state in 
      let loc = 
        match init with 
      | Ast.INIT_MIX (_,loc) -> loc
      | Ast.INIT_TOK (_) -> assert false 
      in  
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string _parameters error init_state = 
     let (_,_,init) = init_state in 
      let string = 
        match init with 
      | Ast.INIT_MIX (mix,_) -> 
            let b = Buffer.create 1 in 
            let fmt = Format.formatter_of_buffer b in 
            let () = Ast.print_ast_mix fmt mix in 
            let () = Format.pp_print_flush fmt () in 
            let string = Buffer.contents b in 
            string 
      | Ast.INIT_TOK (_) -> assert false 
      in  
      error, string 
    in   
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_init_state_map) 
      (fun summary_init_state_map x -> {x with summary_init_state_map})
      parameters error id init_state summary

let summarize_from_ast parameters error compil = 
  let error, summary = error, Mods.StringMap.empty in 
  let error, _, summary = 
    List.fold_left 
      (fun (error, id, summary) rule ->
         let error, summary = 
          summarize_rule_from_ast parameters error id rule summary in 
          error, id+1, summary)
    (error, 0, summary)        
    compil.Ast.rules
  in   
  let error, _, summary = 
    List.fold_left 
      (fun (error, id, summary) 
          (init_statement:int option * (Ast.mixture,Ast.mixture,string) Ast.init_statement) -> 
        let (_,(a,b,init)) = init_statement in 
        match init with 
          | Ast.INIT_TOK _ -> (error, id+1, summary)
          | Ast.INIT_MIX _ -> 
            let error, summary = 
              summarize_init_state_from_ast parameters error id (a,b,init) summary 
            in 
            error, id+1, summary)
        (error, 0, summary)        
        compil.Ast.init 
  in   
  error, summary  



let summarize_rule_from_ckappa parameters error id (rule:Ckappa_sig.enriched_rule) summary = 
  let get_file_name (rule:Ckappa_sig.enriched_rule) = 
      let loc = rule.Ckappa_sig.e_rule_rule.Ckappa_sig.position in 
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string _parameters error (rule:Ckappa_sig.enriched_rule) = 
    let string = rule.Ckappa_sig.e_rule_rule.Ckappa_sig.ast in 
    let label = 
      match rule.Ckappa_sig.e_rule_label with 
      | None -> "" 
      | Some (x,_) -> x 
    in 
    error, label^"."^string 
  in 
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_rule_map) 
      (fun summary_rule_map x -> {x with summary_rule_map})
      parameters error id rule summary

let summarize_init_state_from_ckappa parameters error id (init_state:Ckappa_sig.enriched_init) summary = 
  let get_file_name (init_state:(Ckappa_sig.enriched_init))  = 
      let loc = init_state.Ckappa_sig.e_init_pos in 
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string parameters error (init_state:Ckappa_sig.enriched_init) = 
    let mix = init_state.Ckappa_sig.e_init_mixture in 
    let b = Buffer.create 1 in 
    let fmt = Format.formatter_of_buffer b in 
    let logger = Loggers.open_logger_from_formatter fmt in 
    let parameters = Remanent_parameters.set_logger parameters logger in 
    let error = Print_ckappa.print_mixture parameters error  mix in 
    let string = Buffer.contents b in 
    error, string 
  in   
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_init_state_map) 
      (fun summary_init_state_map x -> {x with summary_init_state_map})
      parameters error id init_state summary

let summarize_from_ckappa parameters error (compil:Ckappa_sig.c_compil) = 
  let error, summary = error, Mods.StringMap.empty in 
  let error, summary = 
    Int_storage.Nearly_inf_Imperatif.fold 
      parameters 
      error 
      (fun parameters error id rule summary ->
         let error, summary = 
          summarize_rule_from_ckappa 
              parameters error id rule summary 
          in 
          error, summary)
    compil.Ckappa_sig.c_rules
    summary        
  in   
  let error, summary = 
    Int_storage.Nearly_inf_Imperatif.fold 
      parameters error 
      (fun parameters error id init_statement summary -> 
            let error, summary = 
              summarize_init_state_from_ckappa parameters error id init_statement summary 
            in 
            error, summary)
        compil.Ckappa_sig.c_init  
        summary 
  in   
  error, summary  

let summarize_rule_from_cckappa parameters error id (rule:Cckappa_sig.enriched_rule) summary = 
  let get_file_name (rule:Cckappa_sig.enriched_rule) = 
      let loc = rule.Cckappa_sig.e_rule_rule.Ckappa_sig.position in 
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string _parameters error (rule:Cckappa_sig.enriched_rule) = 
    let string = rule.Cckappa_sig.e_rule_rule.Ckappa_sig.ast in 
    let label = 
      match rule.Cckappa_sig.e_rule_label with 
      | None -> "" 
      | Some (x,_) -> x 
    in 
    error, label^"."^string 
  in 
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_rule_map) 
      (fun summary_rule_map x -> {x with summary_rule_map})
      parameters error id rule summary

let summarize_init_state_from_cckappa parameters error id (init_state:Cckappa_sig.enriched_init) summary = 
  let get_file_name (init_state:Cckappa_sig.enriched_init)  = 
      let loc = init_state.Cckappa_sig.e_init_position in       
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string _parameters error (init_state:Cckappa_sig.enriched_init) = 
    let mix = init_state.Cckappa_sig.e_init_mixture in 
    let b = Buffer.create 1 in 
    let fmt = Format.formatter_of_buffer b in 
    let logger = Loggers.open_logger_from_formatter fmt in 
    let parameters = Remanent_parameters.set_logger parameters logger in 
    let error = Print_ckappa.print_mixture parameters error  mix in 
    let () = Format.pp_print_flush fmt () in 
    let string = Buffer.contents b in 
    error, string 
  in   
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_init_state_map) 
      (fun summary_init_state_map x -> {x with summary_init_state_map})
      parameters error id init_state summary

let summarize_from_cckappa parameters error (compil:Cckappa_sig.compil) = 
  let error, summary = error, Mods.StringMap.empty in 
  let error, summary = 
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold 
      parameters 
      error 
      (fun parameters error id rule summary ->
        let id = Ckappa_sig.int_of_rule_id id in 
         let error, summary = 
          summarize_rule_from_cckappa 
              parameters error id rule summary 
          in 
          error, summary)
    compil.Cckappa_sig.rules
    summary        
  in   
  let error, summary = 
    Int_storage.Nearly_inf_Imperatif.fold 
      parameters error 
      (fun parameters error id init_statement summary -> 
            let error, summary = 
              summarize_init_state_from_cckappa parameters error id init_statement summary 
            in 
            error, summary)
        compil.Cckappa_sig.init  
        summary 
  in   
  error, summary  
 
 let diff_gen diff_pos get_id get_obj get_map parameters errors ~before  ~after = 
  let map_before = get_map before in 
  let map_after = get_map after in 
 let errors, (removed_list, created_list, pos_renaming) = 
    Mods.StringMap.monadic_fold2
     parameters errors 
      (fun _parameters errors _ elt elt' (removed_list, added_list, pos_diff) -> 
              errors, (
                        removed_list, 
                        added_list,  
                        diff_pos (get_obj elt) (get_obj elt') pos_diff)
                        )
     (fun _parameters errors  _ elt (removed_list, added_list, pos_diff) -> 
              errors, ((get_id elt)::removed_list, added_list, pos_diff))
     (fun _parameters errors  _ elt (removed_list, added_list, pos_diff) -> 
             errors, (removed_list, (get_id elt)::added_list, pos_diff))
    map_before 
    map_after 
    ([],[],Loc.diff_pos_empty) in 
 errors, {new_elt = created_list ; removed_elt = removed_list ; pos_renaming }

let diff diff_pos_rule diff_init parameters errors ~before ~filename ~after = 
  let before = 
    match 
      Mods.StringMap.find_option filename before 
    with 
      | None -> empty_summary_file 
      | Some x -> x 
  in 
  let errors, diff_rules = 
    diff_gen 
      diff_pos_rule 
      fst snd 
      (fun x -> x.summary_rule_map)
      parameters errors ~before ~after 
  in 
  let errors, diff_init = 
    diff_gen 
      diff_init     
      fst snd 
      (fun x -> x.summary_init_state_map)
      parameters errors ~before ~after 
  in 
  errors, {diff_rules; diff_init}

let is_new_gen get _parameters error summary ~filename string = 
  match 
    Mods.StringMap.find_option filename summary 
  with 
  | None -> error, true 
  | Some s -> 
      error, 
      match Mods.StringMap.find_option string (get s) with 
      | None -> false 
      | Some _ -> true 
  

let is_new_rule parameters errors summary ~filename ~rule = 
  is_new_gen (fun x -> x.summary_rule_map) parameters errors summary ~filename rule  

let is_new_init_state parameters errors summary ~filename ~init_state = 
  is_new_gen (fun x -> x.summary_init_state_map) parameters errors summary ~filename init_state 


let dump_diff parameters errors (diff:diff) = 
    let logger = Remanent_parameters.get_logger parameters in 
    let () = Loggers.fprintf logger "     DIFF" in 
    let () = Loggers.print_newline logger in  
    let () = Loggers.fprintf logger "         REMOVED: " in 
    let () = Loggers.print_newline logger in 
    let () = Loggers.fprintf logger "             Rules:" in 
    let () = Loggers.print_newline logger in 
    let () = 
      List.iter
        (fun i -> 
          let () = Loggers.fprintf logger "                %i" i 
          in 
          let () = Loggers.print_newline logger in ())
        diff.diff_rules.removed_elt 
    in 
    let () = Loggers.print_newline logger in 
    let () = Loggers.fprintf logger "             Init:" in 
    let () = Loggers.print_newline logger in 
    let () = 
       List.iter 
         (fun i -> 
            let () = Loggers.fprintf logger "                %i" i in 
            let () = Loggers.print_newline logger in ())
         diff.diff_init.removed_elt 
    in 
    let () = Loggers.print_newline logger in 
    let () = Loggers.print_newline logger in   
    let () = Loggers.fprintf logger "         NEW: " in 
    let () = Loggers.print_newline logger in 
    let () = Loggers.fprintf logger "             Rules:" in 
    let () = Loggers.print_newline logger in 
    let () = 
      List.iter
        (fun i -> 
          let () = Loggers.fprintf logger "                %i" i in 
          let () = Loggers.print_newline logger in ())
            diff.diff_rules.new_elt 
    in 
    let () = Loggers.print_newline logger in 
    let () = Loggers.fprintf logger "             Init:" in 
    let () = Loggers.print_newline logger in 
    let () = 
      List.iter 
        (fun i -> 
          let () = Loggers.fprintf logger "                %i" i in 
          let () = Loggers.print_newline logger in ())
        diff.diff_init.new_elt 
    in 
    let () = Loggers.print_newline logger in 
    let () = Loggers.print_newline logger in  
    let () = Loggers.fprintf logger "         POS renaming : "  in 
    let () = Loggers.print_newline logger in 
    let () = 
      List.iter
        (fun (pos,pos') -> 
          let () = Loggers.fprintf logger "                %s -> %s" (Loc.to_string pos) (Loc.to_string pos') in 
          let () = Loggers.print_newline logger in ())
        diff.diff_rules.pos_renaming
    in 
    errors 

let extract index_list list = 
  let rec aux index_list i list acc = 
    match index_list, list with 
    | [], _ -> List.rev acc 
    | h::t,a::b when h=i -> aux t (i+1) b (a::acc)
    | _::t,_::b -> aux t (i+1) b acc 
    | _, [] -> assert false 
  in aux index_list 0 ((*List.rev*) list) []

let cut diff (ast:Ast.parsing_compil) = 
  let rules = extract diff.diff_rules.new_elt ast.Ast.rules in 
  let init = extract diff.diff_init.new_elt ast.Ast.init in 
  {ast with Ast.init ; Ast.rules}

let get_new_indexs parameters errors handler c_compil = 
  let n = Handler.nrules parameters errors handler in 
  let errors, n' = Int_storage.Nearly_inf_Imperatif.dimension parameters errors 
  c_compil.Cckappa_sig.init in 
  let next_nsites = Handler.get_nsites handler in 
  let next_nr_predicates = Handler.get_nr_guard_parameters handler in 
  let new_indexs = 
      {
        next_rule = Ckappa_sig.rule_id_of_int n ; 
        next_init = n' ;
        next_nsites; next_nr_predicates
      }
  in 
  errors, new_indexs 
  
let fuse parameters errors handler c_compil c_compil' = 
  let n = Handler.nrules parameters errors handler in 
  let errors, n' = Int_storage.Nearly_inf_Imperatif.dimension parameters errors 
  c_compil.Cckappa_sig.init in 
  let rules_label_map = handler.Cckappa_sig.rules_label_map in 
  let errors, (rules_label_map, rules, nrules_pred)  = 
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold 
      parameters errors 
      (fun parameters errors i rule' (rules_label_map,rules,_) -> 
        let id = (Ckappa_sig.int_of_rule_id i)+n in 
         let errors, rules  = 
      Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.set parameters errors (Ckappa_sig.rule_id_of_int id) rule' rules 
        in 
       let errors, rules_label_map =
         match rule'.Cckappa_sig.e_rule_label with
        | None -> errors, rules_label_map
        | Some (label, _) ->
          let errors, rules_label_map =
            Ckappa_sig.Rule_label_map_and_set.Map.add_or_overwrite parameters
              errors label (Ckappa_sig.rule_id_of_int id) rules_label_map
          in
          errors, rules_label_map
        in 
        errors,(rules_label_map, rules, id ))
      (c_compil'.Cckappa_sig.rules)
      (rules_label_map, c_compil.Cckappa_sig.rules,n)
  in 
  let nrules = nrules_pred +1 in 
  let errors, init = 
    Int_storage.Nearly_inf_Imperatif.fold 
      parameters errors 
      (fun parameters errors i init' inits -> 
          Int_storage.Nearly_inf_Imperatif.set parameters errors (i+n') init' inits)
      c_compil'.Cckappa_sig.init
      c_compil.Cckappa_sig.init
  in 
  errors, {handler with Cckappa_sig.nrules; Cckappa_sig.rules_label_map}, {c_compil with Cckappa_sig.rules ; Cckappa_sig.init}