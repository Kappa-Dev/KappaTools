type translate_int = BASIS_MINUS_INPUT of int

type conversion_info = {
  from_sig_name: string Loc.annoted;
  convert_value: translate_int;
  convert_delta: translate_int;
}

type origin = From_original_ast | From_clte_elimination of conversion_info

let apply_int t i =
  match t with
  | BASIS_MINUS_INPUT d -> d - i

let apply_origin_to_value t i =
  match t with
  | From_original_ast -> i
  | From_clte_elimination d -> apply_int d.convert_value i

let apply_origin_to_delta t i =
  match t with
  | From_original_ast -> i
  | From_clte_elimination d -> apply_int d.convert_delta i

let reorder_bounds t (i, j) =
  match t with
  | BASIS_MINUS_INPUT _ -> j, i

type counter_sig = {
  counter_sig_name: string Loc.annoted;
  counter_sig_min: int option Loc.annoted option;
  counter_sig_max: int option Loc.annoted option;
  counter_sig_visible: origin;
  counter_sig_default: int;
}

type t = counter_sig option array array

let print_bound where sign f x =
  match x with
  | None -> ()
  | Some x ->
    Format.fprintf f "%s %a" where
      (fun f -> function
        | None -> Format.fprintf f "%soo" sign
        | Some i -> Format.pp_print_int f i)
      (Loc.v x)

let print_lower_bound = print_bound "from" "-"
let print_upper_bound = print_bound "to" "+"

let print_gen k f name original_name convert =
  match convert with
  | BASIS_MINUS_INPUT b ->
    Format.fprintf f "%s%s = %i - %s%s ; " k (Loc.v name) b k
      (Loc.v original_name)

let print_x = print_gen ""
let print_delta = print_gen "D"

let print_sig_visible f x =
  match x.counter_sig_visible with
  | From_original_ast -> Format.fprintf f "ORIGINAL"
  | From_clte_elimination convert ->
    print_x f x.counter_sig_name convert.from_sig_name convert.convert_value;
    print_delta f x.counter_sig_name convert.from_sig_name convert.convert_delta

let print_counter_sig f t =
  Format.fprintf f "%a %a, default: %i %a " print_lower_bound t.counter_sig_min
    print_upper_bound t.counter_sig_max t.counter_sig_default print_sig_visible
    t

let print_counter counters_info agent_id site_id f =
  match counters_info.(agent_id).(site_id) with
  | None -> ()
  | Some c -> print_counter_sig f c

let print_kappa ~noCounters sigs f t =
  Format.fprintf f "@[<v>%a@]"
    (Pp.array Pp.space (fun ag f intf ->
         if (not (Signature.is_counter_agent sigs ag)) || noCounters then
           Format.fprintf f "@[<hv 2>%%agent:@ %a(@[%a@])@]"
             (Signature.print_agent sigs)
             ag
             (Pp.array Pp.space (fun s f counter_sig_opt ->
                  match counter_sig_opt with
                  | None -> ()
                  | Some counter_sig ->
                    if Signature.site_is_counter sigs ag s && not noCounters
                    then
                      Format.fprintf f "@[%a%a@]"
                        (Signature.print_site sigs ag)
                        s print_counter_sig counter_sig))
             intf))
    t

let int_opt_annoted_opt_to_yojson ~filenames =
  JsonUtil.of_int |> JsonUtil.of_option
  |> Loc.yojson_of_annoted ~filenames
  |> JsonUtil.of_option

let int_opt_annoted_opt_of_yojson ~filenames =
  JsonUtil.to_int |> JsonUtil.to_option
  |> Loc.annoted_of_yojson ~filenames
  |> JsonUtil.to_option

let from_original_ast_yojson_str = "from_original_ast"
let from_clte_elimination_yojson_str = "from_clte_elimination"

let translate_int_to_yojson = function
  | BASIS_MINUS_INPUT i -> JsonUtil.of_int i

let translate_int_of_yojson translate_int_json =
  BASIS_MINUS_INPUT (JsonUtil.to_int translate_int_json)

let from_sig_name_str = "from_sig_name"
let convert_value_str = "convert_value"
let convert_delta_str = "convert_delta"

let conversion_info_to_yojson ~filenames conversion_info =
  `Assoc
    [
      ( from_sig_name_str,
        Loc.string_annoted_to_json ~filenames conversion_info.from_sig_name );
      convert_value_str, translate_int_to_yojson conversion_info.convert_value;
      convert_delta_str, translate_int_to_yojson conversion_info.convert_delta;
    ]

let conversion_info_of_yojson ~filenames = function
  | `Assoc l as x ->
    (try
       let from_sig_name =
         List.assoc from_sig_name_str l |> Loc.string_annoted_of_json ~filenames
       in
       let convert_value =
         List.assoc convert_value_str l |> translate_int_of_yojson
       in
       let convert_delta =
         List.assoc convert_delta_str l |> translate_int_of_yojson
       in
       { from_sig_name; convert_value; convert_delta }
     with _ ->
       raise
         (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "conversion_info", x)))
  | x ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "conversion_info", x))

let origin_to_yojson ~filenames origin =
  match origin with
  | From_original_ast -> `Assoc [ from_original_ast_yojson_str, `Null ]
  | From_clte_elimination conversion_info ->
    `Assoc
      [
        ( from_clte_elimination_yojson_str,
          conversion_info_to_yojson ~filenames conversion_info );
      ]

let origin_of_yojson ~filenames origin_json =
  match origin_json with
  | `Assoc [ (s, `Null) ] when s = from_original_ast_yojson_str ->
    From_original_ast
  | `Assoc [ (s, conversion_info_json) ]
    when s = from_clte_elimination_yojson_str ->
    From_clte_elimination
      (conversion_info_of_yojson ~filenames conversion_info_json)
  | _ ->
    raise
      (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "origin", origin_json))

let counter_sig_name_str = "counter_sig_name"
let counter_sig_min_str = "counter_sig_min"
let counter_sig_max_str = "counter_sig_max"
let counter_sig_visible_str = "counter_sig_visible"
let counter_sig_default_str = "counter_sig_default"

let counter_sig_option_to_yojson ~filenames =
  JsonUtil.of_option (fun counter_sig ->
      `Assoc
        [
          ( counter_sig_name_str,
            Loc.string_annoted_to_json ~filenames counter_sig.counter_sig_name );
          ( counter_sig_min_str,
            int_opt_annoted_opt_to_yojson ~filenames counter_sig.counter_sig_min
          );
          ( counter_sig_max_str,
            int_opt_annoted_opt_to_yojson ~filenames counter_sig.counter_sig_max
          );
          ( counter_sig_visible_str,
            origin_to_yojson ~filenames counter_sig.counter_sig_visible );
          ( counter_sig_default_str,
            JsonUtil.of_int counter_sig.counter_sig_default );
        ])

let counter_sig_option_of_yojson ~filenames =
  JsonUtil.to_option (function
    | `Assoc l as x ->
      (try
         let counter_sig_name =
           List.assoc counter_sig_name_str l
           |> Loc.string_annoted_of_json ~filenames
         in
         let counter_sig_min =
           List.assoc counter_sig_min_str l
           |> int_opt_annoted_opt_of_yojson ~filenames
         in
         let counter_sig_max =
           List.assoc counter_sig_max_str l
           |> int_opt_annoted_opt_of_yojson ~filenames
         in
         let counter_sig_visible =
           List.assoc counter_sig_visible_str l |> origin_of_yojson ~filenames
         in
         let counter_sig_default =
           List.assoc counter_sig_default_str l |> JsonUtil.to_int
         in
         {
           counter_sig_name;
           counter_sig_min;
           counter_sig_max;
           counter_sig_visible;
           counter_sig_default;
         }
       with _ ->
         raise
           (Yojson.Basic.Util.Type_error
              (JsonUtil.build_msg "conversion_info", x)))
    | x ->
      raise
        (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "conversion_info", x)))

let to_yojson ~filenames (t : t) : Yojson.Basic.t =
  JsonUtil.of_array
    (JsonUtil.of_array (counter_sig_option_to_yojson ~filenames))
    t

let of_yojson ~filenames (json : Yojson.Basic.t) : t =
  JsonUtil.to_array
    (JsonUtil.to_array (counter_sig_option_of_yojson ~filenames))
    json

let raise_error ?except except' =
  match except with
  | Some except -> raise except
  | None -> raise except'

let get_counter_sig ?except sigs c agent_id site_id =
  match c.(agent_id).(site_id) with
  | None ->
    raise_error ?except
      (let error =
         Format.asprintf "Site %a of agent %a is not an counter"
           (Signature.print_site sigs agent_id)
           (site_id : int)
           (Signature.print_agent sigs)
           (agent_id : int)
       in
       ExceptionDefn.Internal_Error (Loc.annot_with_dummy error))
  | Some counter_sig -> counter_sig

let get_conversion_info ?except c =
  match c.counter_sig_visible with
  | From_original_ast ->
    raise_error ?except
      (ExceptionDefn.Internal_Error
         (Loc.annot_with_dummy
            "internal error: conversion applyied to an original counter"))
  | From_clte_elimination counter_conversion -> counter_conversion
