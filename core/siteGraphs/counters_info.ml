type translate_int = BASIS_MINUS_INPUT of int

type conversion_info =
  {
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

let reorder_bounds t (i,j) =
  match t with
    | BASIS_MINUS_INPUT _ -> j,i

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
        (fun f ->
          function
          | None -> Format.fprintf f "%soo" sign
          | Some i -> Format.pp_print_int f i)
        (Loc.v x)

let print_lower_bound = print_bound "from" "-"
let print_upper_bound = print_bound "to" "+"

let print_gen k f name original_name convert =
    match convert with
      | BASIS_MINUS_INPUT b -> Format.fprintf f "%s%s = %i - %s%s ; " k (Loc.v name) b k (Loc.v original_name)

let print_x = print_gen ""
let print_delta = print_gen "D"

let print_sig_visible f x =
      match x.counter_sig_visible with
        | From_original_ast -> (Format.fprintf f "ORIGINAL")
        | From_clte_elimination convert ->
            print_x f x.counter_sig_name convert.from_sig_name convert.convert_value;
            print_delta f x.counter_sig_name convert.from_sig_name convert.convert_delta




let print_counter_sig f t =
    Format.fprintf f "%a %a, default: %i %a "
                     print_lower_bound t.counter_sig_min
                     print_upper_bound t.counter_sig_max
                     t.counter_sig_default
                     print_sig_visible t

let print_counter counters_info agent_id site_id f =
    match (counters_info.(agent_id).(site_id)) with
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
                  if Signature.site_is_counter sigs ag s && not noCounters then
                    Format.fprintf f "@[%a%a@]"
                      (Signature.print_site sigs ag)
                      s
                      print_counter_sig counter_sig
                      )
                  )
             intf))
    t

let to_yojson _t = `Null (*TO DO*)
let of_yojson _t = [||]  (* TO DO *)


let raise_error ?except except' =
  match except with
    | Some except -> raise except
    | None -> raise except'

let get_counter_sig ?except sigs c agent_id site_id =
 match c.(agent_id).(site_id) with
    | None ->  raise_error ?except
                (let error =
                    Format.asprintf
                        "Site %a of agent %a is not an counter"
                        (Signature.print_site sigs agent_id) (site_id:int)
                        (Signature.print_agent sigs) (agent_id:int)
                  in
                  (ExceptionDefn.Internal_Error (Loc.annot_with_dummy error)))
    | Some counter_sig -> counter_sig

let get_conversion_info ?except c =
  match c.counter_sig_visible with
      | From_original_ast -> raise_error ?except (ExceptionDefn.Internal_Error (Loc.annot_with_dummy "internal error: conversion applyied to an original counter"))
      | From_clte_elimination counter_conversion -> counter_conversion
