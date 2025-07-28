type size_sig = {
  threshold_sig_name: string Loc.annoted;
  threshold_sig_value: string option Loc.annoted list;
  threshold_sig_agent_name: string option; 
  threshold: int;
}

type t = size_sig option array array
type previous_threshold_thread = int Array.t
type previous_threshold = previous_threshold_thread * previous_threshold_thread array 

let previous_threshold_init () = Array.make 0 0, Array.make 0 (Array.make 0 0)

let copy_previous_threshold (a,b) = 
  Array.copy a, 
  Array.map Array.copy b 

let print_size_predicate_sig f t = 
  match t.threshold_sig_agent_name with 
   | None -> Format.fprintf f "<=%i" t.threshold
   | Some i -> Format.fprintf f "%s <= %i" i t.threshold 

let print_size_predicate size_predicate_sites_info agent_id site_id f =
  match size_predicate_sites_info.(agent_id).(site_id) with
  | None -> ()
  | Some s -> print_size_predicate_sig f s

let print_kappa ~noSizepredicates sigs f t =
  Format.fprintf f "@[<v>%a@]"
    (Pp.array Pp.space (fun ag f intf ->
         if noSizepredicates then
           Format.fprintf f "@[<hv 2>%%agent:@ %a(@[%a@])@]"
             (Signature.print_agent sigs)
             ag
             (Pp.array Pp.space (fun s f size_sig_opt ->
                  match size_sig_opt with
                  | None -> ()
                  | Some size_sig ->
                    if
                      Signature.site_is_size_predicate_site sigs ag s
                      && not noSizepredicates
                    then
                      Format.fprintf f "@[%a%a@]"
                        (Signature.print_site sigs ag)
                        s print_size_predicate_sig size_sig))
             intf))
    t

let size_sig_name_str = "size_sig_name"
let size_sig_internals_str = "size_sig_internals"
let size_sig_value_str = "size_sig_threshold"
let size_sig_agent_name = "size_sig_agent_name"

let size_sig_option_to_yojson ~filenames =
  JsonUtil.of_option (fun size_sig ->
      `Assoc
        ((fun l -> 
          match size_sig.threshold_sig_agent_name with 
            | None -> l 
            | Some agent_name -> (size_sig_agent_name,JsonUtil.of_string agent_name)::l)
          [( size_sig_name_str,
            Loc.string_annoted_to_json ~filenames size_sig.threshold_sig_name );
          size_sig_value_str, JsonUtil.of_int size_sig.threshold;
          ( size_sig_internals_str,
            JsonUtil.of_list
              (Loc.string_option_annoted_to_json ~filenames)
              size_sig.threshold_sig_value );
      
        ]))

let size_sig_option_of_yojson ~filenames =
  JsonUtil.to_option (function
    | `Assoc l as x ->
      (try
         let threshold_sig_name =
           List.assoc size_sig_name_str l
           |> Loc.string_annoted_of_json ~filenames
         in
         let threshold_sig_agent_name = 
           match List.assoc_opt size_sig_agent_name l with 
            | None -> None 
            | Some s -> Some (JsonUtil.to_string s)
         in 
         let threshold = List.assoc size_sig_value_str l |> JsonUtil.to_int in
         let threshold_sig_value =
           List.assoc size_sig_internals_str l
           |> JsonUtil.to_list (Loc.string_option_annoted_of_json ~filenames)
         in
         { threshold_sig_name; threshold_sig_value; threshold ; threshold_sig_agent_name}
       with _ ->
         raise
           (Yojson.Basic.Util.Type_error
              (JsonUtil.exn_msg_cant_import_from_json "threshold_info", x)))
    | x ->
      raise
        (Yojson.Basic.Util.Type_error
           (JsonUtil.exn_msg_cant_import_from_json "threshold_info", x)))

let to_yojson ~filenames (t : t) : Yojson.Basic.t =
  JsonUtil.of_array (JsonUtil.of_array (size_sig_option_to_yojson ~filenames)) t

let of_yojson ~filenames (json : Yojson.Basic.t) : t =
  JsonUtil.to_array
    (JsonUtil.to_array (size_sig_option_of_yojson ~filenames))
    json

let raise_error ?except except' =
  match except with
  | Some except -> raise except
  | None -> raise except'

let get_size_predicate_sites_sig ?except sigs c agent_id site_id =
  match c.(agent_id).(site_id) with
  | None ->
    raise_error ?except
      (let error =
         Format.asprintf "Site %a of agent %a is not a size predicate site"
           (Signature.print_site sigs agent_id)
           (site_id : int)
           (Signature.print_agent sigs)
           (agent_id : int)
       in
       ExceptionDefn.Internal_Error (Loc.annot_with_dummy error))
  | Some int -> int

let name_of_size_predicate_before_compil id_opt i = 
  match id_opt with 
    | None -> Format.sprintf "__cc_size_leqt_%i" i
    | Some a -> Format.sprintf "__nb_of_%s_leqt_%i" a i 

let name_of_size_predicate sigs id_opt i = 
  let name_opt = 
    match id_opt with 
    | None -> None 
    | Some a -> Some (Signature.agent_of_num a sigs) 
  in name_of_size_predicate_before_compil name_opt i 
    

let get_size_predicate_site agent_id id_opt threshold sigs =
  let site_name = name_of_size_predicate sigs id_opt threshold in
  let s = Signature.get sigs agent_id in
  Signature.num_of_site (Loc.annot_with_dummy site_name) s

let get_internal_state_true agent_id id_opt threshold sigs =
  let site_id = get_size_predicate_site agent_id id_opt threshold sigs in
  let s = Signature.get sigs agent_id in
  Signature.num_of_internal_state site_id (Loc.annot_with_dummy "true") s

let get_internal_state_false agent_id id_opt threshold sigs =
  let site_id = get_size_predicate_site agent_id id_opt threshold sigs in
  let s = Signature.get sigs agent_id in
  Signature.num_of_internal_state site_id (Loc.annot_with_dummy "false") s
  
let compute_threshold ((op,b),i) = 
  match op,b with 
    | Operator.SMALLER, true -> i+1
    | Operator.SMALLER, false -> i  (* < i <=> <= i-1 *)
    | Operator.GREATER, false -> i+1 (* a> j <=> not (a <= j)*)
    | Operator.GREATER, true -> i (* a >= j <=> not (a <= j) *)
    | (Operator.DIFF | Operator.EQUAL), _ -> assert false 