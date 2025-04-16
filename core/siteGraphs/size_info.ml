type size_sig = {
  threshold_sig_name: string Loc.annoted;
  threshold_sig_value: string option Loc.annoted list;
  threshold: int;
}

type t = size_sig option array array
type previous_threshold = int Array.t

(*val print_size_predicate : t -> int -> int -> Format.formatter -> unit

  val print_kappa :
    noSizepredicate:bool -> Signature.s -> Format.formatter -> t -> unit

  val to_yojson : filenames:int Mods.StringMap.t -> t -> Yojson.Basic.t
  val of_yojson : filenames:string array -> Yojson.Basic.t -> t

  val get_counter_sig :
    ?except:exn -> Signature.s -> t -> int -> int -> size_sig*)

(*let print_gen k f name original_name convert =
    match convert with
    | BASIS_MINUS_INPUT b ->
      Format.fprintf f "%s%s = %i - %s%s ; " k (Loc.v name) b k
        (Loc.v original_name)

  let print_x = print_gen ""
  let print_delta = print_gen "D"

  let print_counter_sig f t =
    Format.fprintf f "%a %a, default: %i %a " print_lower_bound t.counter_sig_min
      print_upper_bound t.counter_sig_max t.counter_sig_default print_sig_visible
      t*)

let print_size_predicate_sig f t = Format.fprintf f "<=%i" t.threshold

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

let size_sig_option_to_yojson ~filenames =
  JsonUtil.of_option (fun size_sig ->
      `Assoc
        [
          ( size_sig_name_str,
            Loc.string_annoted_to_json ~filenames size_sig.threshold_sig_name );
          size_sig_value_str, JsonUtil.of_int size_sig.threshold;
          ( size_sig_internals_str,
            JsonUtil.of_list
              (Loc.string_option_annoted_to_json ~filenames)
              size_sig.threshold_sig_value );
        ])

let size_sig_option_of_yojson ~filenames =
  JsonUtil.to_option (function
    | `Assoc l as x ->
      (try
         let threshold_sig_name =
           List.assoc size_sig_name_str l
           |> Loc.string_annoted_of_json ~filenames
         in
         let threshold = List.assoc size_sig_value_str l |> JsonUtil.to_int in
         let threshold_sig_value =
           List.assoc size_sig_internals_str l
           |> JsonUtil.to_list (Loc.string_option_annoted_of_json ~filenames)
         in
         { threshold_sig_name; threshold_sig_value; threshold }
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

let name_of_size_predicate i = Format.sprintf "__cc_size_leqt_%i" i
