type summary_file = 
  {
    summary_rule_map: (int * Cckappa_sig.enriched_rule) Mods.StringMap.t ; 
    summary_init_state_map: (int * Cckappa_sig.enriched_init) Mods.StringMap.t ;
    summary_rule_set: Mods.StringSet.t ; 
    summary_init_state_set: Mods.StringSet.t ;
    } 

type summary = summary_file Mods.StringMap.t 

type diff_elt = {new_elt: int list; removed_elt: int list}
type diff = 
 {
    diff_rules: diff_elt ; 
    diff_init: diff_elt ; 
 }

 let empty_summary_file = 
   {
    summary_rule_map =  Mods.StringMap.empty; 
    summary_init_state_map = Mods.StringMap.empty;   
    summary_rule_set = Mods.StringSet.empty ; 
    summary_init_state_set = Mods.StringSet.empty ;
    } 
 
let dump_summary parameters _error summary = 
  let logger = Remanent_parameters.get_logger parameters in 
  let () = Loggers.fprintf logger "RULE_MAP" in 
  let () = 
    Mods.StringMap.iter
      (fun s i -> 
        let () = Loggers.fprintf logger "FILE: %s" s in 
        let () = Loggers.fprintf logger "  Rules:" in 
        let () = 
          Mods.StringMap.iter
            (fun s (i,_) -> Loggers.fprintf logger "    %s -> %i" s i)
            i.summary_rule_map
        in 
        let () = Loggers.fprintf logger "  Init:" in 
        let () = 
          Mods.StringMap.iter
            (fun s (i,_) -> Loggers.fprintf logger "    %s -> %i" s i)
            i.summary_init_state_map
        in 
        ())
      summary
      in () 
(*let string_of_rule rule = *)  
   (*let buffer = Buffer.create 0 in
   let fmt_buffer = Format.formatter_of_buffer buffer in
   let logger = Loggers.open_logger_from_formatter fmt_buffer in 
   let parameter = Remanent_parameters.set_logger parameter logger in 
   let () = Ast.print_ast_rule fmt_buffer rule in 
   Buffer.contents buffer *)

let summarize_gen get_file_name get_string get_map set_map get_set set_set parameters error id  elt summary = 
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
   let set = get_set summary_file in 
   let set = Mods.StringSet.add string set in 
   let summary_file = set_set set summary_file in 
   error, Mods.StringMap.add file_name summary_file summary 

let summarize_rule parameters error id rule summary = 
  let get_file_name rule = 
      let rule = rule.Cckappa_sig.e_rule_rule in 
      let loc = rule.Ckappa_sig.position in 
      let filename = loc.Loc.file in 
      filename
  in 
  let id = Ckappa_sig.int_of_rule_id id in 
  let get_string _parameters error rule = 
    let string = rule.Cckappa_sig.e_rule_rule.Ckappa_sig.ast  in 
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
      (fun x -> x.summary_rule_set) 
      (fun summary_rule_set x -> {x with summary_rule_set})
      parameters error id rule summary

let summarize_init_state parameters error id init_state summary = 
  let get_file_name _init_state  = 
      let loc = Loc.dummy (* to do *) (*init_state.Ckappa_sig.e_init_pos*) in 
      let filename = loc.Loc.file in 
      filename
  in 
  let get_string _ error _init_state = error, "" in (* TO DO *)
  summarize_gen 
      get_file_name get_string 
      (fun x -> x.summary_init_state_map) 
      (fun summary_init_state_map x -> {x with summary_init_state_map})
      (fun x -> x.summary_init_state_set) 
      (fun summary_init_state_set x -> {x with summary_init_state_set})
      parameters error id init_state summary
let summarize parameters error compil = 
  let error, summary = error, Mods.StringMap.empty in 
  let error, summary = 
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold 
      parameters error 
      (fun parameters error id rule summary -> 
        summarize_rule parameters error id rule summary )
      compil.Cckappa_sig.rules summary 
  in   
  let error, summary = 
    Int_storage.Nearly_inf_Imperatif.fold 
      parameters error 
      (fun _parameters error id init summary -> 
          summarize_init_state parameters error id init  summary )
      compil.Cckappa_sig.init summary 
  in      
  error, summary  

let diff_gen get_id get_set get_map _parameters error ~before  ~after = 
  let set_before = get_set before in 
  let set_after = get_set after in 
  let removed = Mods.StringSet.diff set_before set_after in 
  let created = Mods.StringSet.diff set_after set_before in 
  let removed_list = Mods.StringSet.elements removed in 
  let removed_list = 
    List.rev_map 
      (fun s -> 
        match 
          Mods.StringMap.find_option s (get_map before)
        with 
          | None -> assert false 
          | Some elt -> get_id elt) 
          (List.rev removed_list) 
  in
  let created_list = Mods.StringSet.elements created in 
  let created_list = 
    List.rev_map 
      (fun s -> 
        match 
          Mods.StringMap.find_option s (get_map after)
        with 
          | None -> assert false 
          | Some elt -> get_id elt) 
          (List.rev created_list) 
  in 
  error, {new_elt = created_list ; removed_elt = removed_list}

let diff parameters errors ~before ~filename ~after = 
  let before = 
    match 
      Mods.StringMap.find_option filename before 
    with 
      | None -> empty_summary_file 
      | Some x -> x 
  in 
  let errors, diff_rules = 
    diff_gen 
      fst 
      (fun x -> x.summary_rule_set)
      (fun x -> x.summary_rule_map)
      parameters errors ~before ~after 
  in 
  let errors, diff_init = 
    diff_gen 
      fst 
      (fun x -> x.summary_init_state_set)
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
      error, Mods.StringSet.mem string (get s)
  

let is_new_rule parameters errors summary ~filename ~rule = 
  is_new_gen (fun x -> x.summary_rule_set) parameters errors summary ~filename rule  

let is_new_init_state parameters errors summary ~filename ~init_state = 
  is_new_gen (fun x -> x.summary_init_state_set) parameters errors summary ~filename init_state 
