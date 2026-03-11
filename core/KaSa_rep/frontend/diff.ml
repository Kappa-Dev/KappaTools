type summary_file = 
  {
    summary_rules_map: (int * Ckappa_sig.enriched_rule) Mods.StringMap.t ; 
    summary_init_state_map: (int * Ckappa_sig.enriched_init) Mods.StringMap.t ;
    summary_rules_set: Mods.StringSet.t ; 
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
    summary_rules_map =  Mods.StringMap.empty; 
    summary_init_state_map = Mods.StringMap.empty;   summary_rules_set = Mods.StringSet.empty ; 
    summary_init_state_set = Mods.StringSet.empty ;
    } 
 

let summarize _parameters error _compil = error, Mods.StringMap.empty  

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
      (fun x -> x.summary_rules_set)
      (fun x -> x.summary_rules_map)
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
  is_new_gen (fun x -> x.summary_rules_set) parameters errors summary ~filename rule  

let is_new_init_state parameters errors summary ~filename ~init_state = 
  is_new_gen (fun x -> x.summary_init_state_set) parameters errors summary ~filename init_state 
