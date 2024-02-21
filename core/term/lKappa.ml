(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type ('a, 'annot) link =
  | ANY_FREE
  | LNK_VALUE of int * 'annot
  | LNK_FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a * 'a  (** port * agent_type *)

type switching = Linked of int | Freed | Maintained | Erased

type rule_internal =
  | I_ANY
  | I_ANY_CHANGED of int
  | I_ANY_ERASED
  | I_VAL_CHANGED of int * int
  | I_VAL_ERASED of int

type rule_agent = {
  ra_type: int;
  ra_erased: bool;
  ra_ports: ((int, int * int) link Loc.annoted * switching) array;
  ra_ints: rule_internal array;
  ra_syntax:
    (((int, int * int) link Loc.annoted * switching) array
    * rule_internal array)
    option;
}

type rule_mixture = rule_agent list

type rule = {
  r_mix: rule_mixture;
  r_created: Raw_mixture.t;
  r_delta_tokens: ((rule_mixture, int) Alg_expr.e Loc.annoted * int) list;
  r_rate: (rule_mixture, int) Alg_expr.e Loc.annoted;
  r_un_rate:
    ((rule_mixture, int) Alg_expr.e Loc.annoted
    * (rule_mixture, int) Alg_expr.e Loc.annoted option)
    option;
  r_edit_style: bool;
}

let print_link pr_port pr_type pr_annot f = function
  | ANY_FREE -> Format.pp_print_string f "#"
  | LNK_TYPE (p, a) -> Format.fprintf f "%a.%a" (pr_port a) p pr_type a
  | LNK_ANY -> Format.pp_print_string f "#"
  | LNK_FREE -> Format.pp_print_string f "."
  | LNK_SOME -> Format.pp_print_string f "_"
  | LNK_VALUE (i, a) -> Format.fprintf f "%i%a" i pr_annot a

let link_to_json port_to_json type_to_json annot_to_json = function
  | ANY_FREE -> `String "ANY_FREE"
  | LNK_FREE -> `String "FREE"
  | LNK_TYPE (p, a) -> `List [ port_to_json a p; type_to_json a ]
  | LNK_ANY -> `Null
  | LNK_SOME -> `String "SOME"
  | LNK_VALUE (i, a) -> `List (`Int i :: annot_to_json a)

let link_of_json port_of_json type_of_json annoted_of_json = function
  | `String "ANY_FREE" -> ANY_FREE
  | `String "FREE" -> LNK_FREE
  | `List [ p; a ] ->
    let x = type_of_json a in
    LNK_TYPE (port_of_json x p, x)
  | `Null -> LNK_ANY
  | `String "SOME" -> LNK_SOME
  | `List (`Int i :: (([] | _ :: _ :: _) as a)) ->
    LNK_VALUE (i, annoted_of_json a)
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect link", x))

let print_link_annot ~ltypes sigs f (s, a) =
  if ltypes then
    Format.fprintf f "/*%a.%a*/"
      (Signature.print_site sigs a)
      s
      (Signature.print_agent sigs)
      a

let print_rule_internal sigs ~show_erased ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED j ->
    Format.fprintf f "{#/%a}" (Signature.print_internal_state sigs ag_ty site) j
  | I_ANY_ERASED ->
    Format.fprintf f "{#%t}" (fun f ->
        if show_erased then Format.pp_print_string f "--")
  | I_VAL_CHANGED (i, j) ->
    if i <> j then
      Format.fprintf f "{%a/%a}"
        (Signature.print_internal_state sigs ag_ty site)
        i
        (Signature.print_internal_state sigs ag_ty site)
        j
    else
      Format.fprintf f "{%a}" (Signature.print_internal_state sigs ag_ty site) i
  | I_VAL_ERASED i ->
    Format.fprintf f "{%a%t}" (Signature.print_internal_state sigs ag_ty site) i
      (fun f -> if show_erased then Format.pp_print_string f "--")

let rule_internal_to_json = function
  | I_ANY -> `Null
  | I_ANY_CHANGED j -> `List [ `String "ANY"; `Int j ]
  | I_ANY_ERASED -> `String "ERASED"
  | I_VAL_CHANGED (i, j) -> `List [ `Int i; `Int j ]
  | I_VAL_ERASED i -> `List [ `Int i; `String "ERASED" ]

let rule_internal_of_json = function
  | `Null -> I_ANY
  | `List [ `String "ANY"; `Int j ] -> I_ANY_CHANGED j
  | `String "ERASED" -> I_ANY_ERASED
  | `List [ `Int i; `Int j ] -> I_VAL_CHANGED (i, j)
  | `List [ `Int i; `String "ERASED" ] -> I_VAL_ERASED i
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect rule_internal", x))

let print_switching ~show_erased f = function
  | Linked i -> Format.fprintf f "/%i" i
  | Freed -> Format.pp_print_string f "/."
  | Maintained -> ()
  | Erased -> if show_erased then Format.pp_print_string f "--"

let switching_to_json = function
  | Freed -> `String "Freed"
  | Maintained -> `String "Maintained"
  | Erased -> `String "Erased"
  | Linked i -> JsonUtil.of_int i

let switching_of_json = function
  | `String "Freed" -> Freed
  | `String "Maintained" -> Maintained
  | `String "Erased" -> Erased
  | x -> Linked (JsonUtil.to_int ~error_msg:"Invalid Switching" x)

let print_rule_link sigs ~show_erased ~ltypes f ((e, _), s) =
  Format.fprintf f "[%a%a]"
    (print_link
       (Signature.print_site sigs)
       (Signature.print_agent sigs)
       (print_link_annot ~ltypes sigs))
    e
    (print_switching ~show_erased)
    s

let print_counter_test f = function
  | c, true -> Format.fprintf f "=%i" c
  | c, false -> Format.fprintf f ">=%i" c

let print_counter_delta counters j f switch =
  match switch with
  | Linked i ->
    let root = Raw_mixture.find counters i in
    let s, (_, is_counter) = Mods.DynArray.get counters.Raw_mixture.rank root in
    let delta =
      if is_counter then
        s - 1
      else
        j - i
    in
    Format.fprintf f "/+=%d" delta
  | Freed ->
    raise
      (ExceptionDefn.Internal_Error
         (Loc.annot_with_dummy "Cannot erase all increment agents"))
  | Maintained -> ()
  | Erased -> ()

let print_rule_intf ~noCounters sigs ~show_erased ~ltypes ag_ty f
    (ports, ints, counters, created_counters) =
  let rec aux empty i =
    if i < Array.length ports then
      if
        match ports.(i) with
        | (LNK_ANY, _), Maintained -> ints.(i) <> I_ANY
        | (LNK_ANY, _), (Erased | Freed | Linked _)
        | ((LNK_SOME | ANY_FREE | LNK_FREE | LNK_TYPE _ | LNK_VALUE _), _), _ ->
          true
      then (
        let (e, _), switch = ports.(i) in
        let is_counter =
          match e with
          | ANY_FREE | LNK_FREE | LNK_ANY | LNK_TYPE _ | LNK_SOME -> false
          | LNK_VALUE (j, _) ->
            (try
               let root = Raw_mixture.find counters j in
               let c, (eq, is_counter') =
                 Mods.DynArray.get counters.Raw_mixture.rank root
               in
               if is_counter' && not noCounters then (
                 Format.fprintf f "%t%a{%a%a}"
                   (if empty then
                      Pp.empty
                    else
                      Pp.space)
                   (Signature.print_site sigs ag_ty)
                   i print_counter_test
                   (c - 1, eq)
                   (print_counter_delta created_counters j)
                   switch;
                 true
               ) else
                 false
             with Invalid_argument _ -> false)
        in
        let () =
          if not is_counter then
            Format.fprintf f "%t%a%a%a"
              (if empty then
                 Pp.empty
               else
                 Pp.space)
              (Signature.print_site sigs ag_ty)
              i
              (print_rule_internal sigs ~show_erased ag_ty i)
              ints.(i)
              (print_rule_link sigs ~show_erased ~ltypes)
              ports.(i)
          else
            ()
        in
        aux false (succ i)
      ) else
        aux empty (succ i)
  in
  aux true 0

let union_find_counters sigs mix =
  let t = Raw_mixture.create 1 in
  let () =
    match sigs with
    | None -> ()
    | Some sigs ->
      List.iter
        (fun ag ->
          match Signature.ports_if_counter_agent sigs ag.ra_type with
          | None -> ()
          | Some (before, after) ->
            let (a, _), _ = ag.ra_ports.(after) in
            let (b, _), _ = ag.ra_ports.(before) in
            (match b with
            | ANY_FREE | LNK_FREE | LNK_ANY | LNK_TYPE _ | LNK_SOME -> ()
            | LNK_VALUE (lnk_b, _) ->
              (match a with
              | LNK_VALUE (lnk_a, _) -> Raw_mixture.union t lnk_b lnk_a
              | ANY_FREE | LNK_FREE ->
                let root = Raw_mixture.find t lnk_b in
                let s, _ = Mods.DynArray.get t.Raw_mixture.rank root in
                Mods.DynArray.set t.Raw_mixture.rank root (s, (true, true))
              | LNK_ANY ->
                let root = Raw_mixture.find t lnk_b in
                let s, _ = Mods.DynArray.get t.Raw_mixture.rank root in
                Mods.DynArray.set t.Raw_mixture.rank root (s, (false, true))
              | LNK_TYPE _ | LNK_SOME ->
                raise
                  (ExceptionDefn.Internal_Error
                     (Loc.annot_with_dummy
                        "Port a of __counter_agent agent not well specified")))))
        mix
  in
  t

let print_rule_agent ~noCounters sigs ~ltypes counters created_counters f ag =
  Format.fprintf f "%a(@[<h>%a@])%t" (Signature.print_agent sigs) ag.ra_type
    (print_rule_intf ~noCounters sigs ~show_erased:false ~ltypes ag.ra_type)
    (ag.ra_ports, ag.ra_ints, counters, created_counters) (fun f ->
      if ag.ra_erased then Format.pp_print_string f "-")

let print_rule_mixture ~noCounters sigs ~ltypes created f mix =
  let counter_agents = union_find_counters (Some sigs) mix in
  let created_incr = Raw_mixture.union_find_counters (Some sigs) created in
  let rec aux_print some = function
    | [] -> ()
    | h :: t ->
      if Signature.is_counter_agent sigs h.ra_type && not noCounters then
        aux_print some t
      else (
        let () = if some then Pp.comma f in
        let () =
          print_rule_agent ~noCounters sigs ~ltypes counter_agents created_incr
            f h
        in
        aux_print true t
      )
  in
  aux_print false mix

let print_internal_lhs sigs ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED _ | I_ANY_ERASED -> Format.pp_print_string f "{#}"
  | I_VAL_CHANGED (i, _) | I_VAL_ERASED i ->
    Format.fprintf f "{%a}" (Signature.print_internal_state sigs ag_ty site) i

let print_internal_rhs sigs ag_ty site f = function
  | I_ANY -> ()
  | I_ANY_CHANGED j | I_VAL_CHANGED (_, j) ->
    Format.fprintf f "{%a}" (Signature.print_internal_state sigs ag_ty site) j
  | I_ANY_ERASED | I_VAL_ERASED _ -> assert false

let print_link_lhs ~ltypes sigs f ((e, _), _) =
  print_link
    (Signature.print_site sigs)
    (Signature.print_agent sigs)
    (print_link_annot ~ltypes sigs)
    f e

let print_link_rhs ~ltypes sigs f ((e, _), s) =
  match s with
  | Linked i ->
    print_link
      (Signature.print_site sigs)
      (Signature.print_agent sigs)
      (fun _ () -> ())
      f
      (LNK_VALUE (i, ()))
  | Freed -> Format.pp_print_string f "."
  | Maintained ->
    print_link
      (Signature.print_site sigs)
      (Signature.print_agent sigs)
      (print_link_annot ~ltypes sigs)
      f e
  | Erased -> assert false

let print_intf_lhs ~ltypes sigs ag_ty f (ports, ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if
        match ports.(i) with
        | ((LNK_SOME | LNK_FREE | ANY_FREE | LNK_TYPE _ | LNK_VALUE _), _), _ ->
          true
        | (LNK_ANY, _), _ ->
          (match ints.(i) with
          | I_ANY | I_ANY_ERASED | I_ANY_CHANGED _ -> false
          | I_VAL_CHANGED _ | I_VAL_ERASED _ -> true)
      then (
        let () =
          Format.fprintf f "%t%a%a[%a]"
            (if empty then
               Pp.empty
             else
               Pp.space)
            (Signature.print_site sigs ag_ty)
            i
            (print_internal_lhs sigs ag_ty i)
            ints.(i)
            (print_link_lhs ~ltypes sigs)
            ports.(i)
        in
        aux false (succ i)
      ) else
        aux empty (succ i)
  in
  aux true 0

let print_intf_rhs ~ltypes sigs ag_ty f (ports, ints) =
  let rec aux empty i =
    if i < Array.length ports then
      if
        match ports.(i) with
        | ((LNK_SOME | LNK_FREE | ANY_FREE | LNK_TYPE _ | LNK_VALUE _), _), _ ->
          true
        | (LNK_ANY, _), (Erased | Freed | Linked _) -> true
        | (LNK_ANY, _), Maintained ->
          (match ints.(i) with
          | I_ANY -> false
          | I_VAL_CHANGED (i, j) -> i <> j
          | I_ANY_ERASED | I_ANY_CHANGED _ | I_VAL_ERASED _ -> true)
      then (
        let () =
          Format.fprintf f "%t%a%a[%a]"
            (if empty then
               Pp.empty
             else
               Pp.space)
            (Signature.print_site sigs ag_ty)
            i
            (print_internal_rhs sigs ag_ty i)
            ints.(i)
            (print_link_rhs ~ltypes sigs)
            ports.(i)
        in
        aux false (succ i)
      ) else
        aux empty (succ i)
  in
  aux true 0

let print_agent_lhs ~ltypes sigs f ag =
  Format.fprintf f "%a(@[<h>%a@])"
    (Signature.print_agent sigs)
    ag.ra_type
    (print_intf_lhs ~ltypes sigs ag.ra_type)
    (ag.ra_ports, ag.ra_ints)

let print_agent_rhs ~ltypes sigs f ag =
  if not ag.ra_erased then
    Format.fprintf f "%a(@[<h>%a@])"
      (Signature.print_agent sigs)
      ag.ra_type
      (print_intf_rhs ~ltypes sigs ag.ra_type)
      (ag.ra_ports, ag.ra_ints)

let print_rhs ~noCounters ~ltypes sigs created f mix =
  let rec aux empty = function
    | [] ->
      Raw_mixture.print ~noCounters ~initial_comma:(not empty) ~created:false
        ~sigs f created
    | h :: t ->
      if h.ra_erased then (
        let () =
          Format.fprintf f "%t."
            (if empty then
               Pp.empty
             else
               Pp.comma)
        in
        aux false t
      ) else (
        let () =
          Format.fprintf f "%t%a"
            (if empty then
               Pp.empty
             else
               Pp.comma)
            (print_agent_rhs ~ltypes sigs)
            h
        in
        aux false t
      )
  in
  aux true mix

let print_rates ~noCounters sigs pr_tok pr_var f r =
  let ltypes = false in
  Format.fprintf f " @@ %a%t"
    (Alg_expr.print
       (fun f m ->
         Format.fprintf f "|%a|"
           (print_rule_mixture ~noCounters sigs ~ltypes [])
           m)
       pr_tok pr_var)
    (fst r.r_rate)
    (fun f ->
      match r.r_un_rate with
      | None -> ()
      | Some ((ra, _), max_dist) ->
        Format.fprintf f " {%a%a}"
          (Alg_expr.print
             (fun f m ->
               Format.fprintf f "|%a|"
                 (print_rule_mixture ~noCounters sigs ~ltypes [])
                 m)
             pr_tok pr_var)
          ra
          (Pp.option (fun f (md, _) ->
               Format.fprintf f ":%a"
                 (Alg_expr.print
                    (fun f m ->
                      Format.fprintf f "|%a|"
                        (print_rule_mixture ~noCounters sigs ~ltypes [])
                        m)
                    pr_tok pr_var)
                 md))
          max_dist)

let print_rule ~noCounters ~full sigs pr_tok pr_var f r =
  Format.fprintf f "@[<h>%t%t%a%t@]"
    (fun f ->
      if full || r.r_edit_style then
        Format.fprintf f "%a%a"
          (print_rule_mixture ~noCounters sigs ~ltypes:false r.r_created)
          r.r_mix
          (Raw_mixture.print ~noCounters ~created:true
             ~initial_comma:(r.r_mix <> []) ~sigs)
          r.r_created
      else
        Format.fprintf f "%a%t%a -> %a"
          (Pp.list Pp.comma (print_agent_lhs ~ltypes:false sigs))
          r.r_mix
          (fun f -> if r.r_mix <> [] && r.r_created <> [] then Pp.comma f)
          (Pp.list Pp.comma (fun f _ -> Format.pp_print_string f "."))
          r.r_created
          (print_rhs ~noCounters ~ltypes:false sigs r.r_created)
          r.r_mix)
    (fun f ->
      match r.r_delta_tokens with
      | [] -> ()
      | _ :: _ -> Format.pp_print_string f " | ")
    (Pp.list Pp.comma (fun f ((nb, _), tk) ->
         Format.fprintf f "%a %a"
           (Alg_expr.print
              (fun f m ->
                Format.fprintf f "|%a|"
                  (print_rule_mixture ~noCounters sigs ~ltypes:false [])
                  m)
              pr_tok pr_var)
           nb pr_tok tk))
    r.r_delta_tokens
    (fun f -> if full then print_rates ~noCounters sigs pr_tok pr_var f r)

let rule_agent_to_json filenames a =
  `Assoc
    [
      "type", `Int a.ra_type;
      ( "bindings",
        `List
          (Array.fold_right
             (fun (e, s) c ->
               `List
                 [
                   Loc.yojson_of_annoted ~filenames
                     (link_to_json
                        (fun _ i -> `Int i)
                        (fun i -> `Int i)
                        (fun (s, a) -> [ `Int s; `Int a ]))
                     e;
                   switching_to_json s;
                 ]
               :: c)
             a.ra_ports []) );
      ( "states",
        `List
          (Array.fold_right
             (fun x c -> rule_internal_to_json x :: c)
             a.ra_ints []) );
      "erased", `Bool a.ra_erased;
    ]

let rule_agent_of_json filenames = function
  | `Assoc l as x when List.length l = 4 ->
    (try
       let ports =
         match List.assoc "bindings" l with
         | `List s ->
           Tools.array_map_of_list
             (function
               | `List [ e; s ] ->
                 ( Loc.annoted_of_yojson ~filenames
                     (link_of_json
                        (fun _ -> Yojson.Basic.Util.to_int)
                        Yojson.Basic.Util.to_int
                        (function
                          | [ `Int s; `Int a ] -> s, a
                          | _ -> raise Not_found))
                     e,
                   switching_of_json s )
               | _ -> raise Not_found)
             s
         | _ -> raise Not_found
       in
       let ints =
         match List.assoc "states" l with
         | `List s -> Tools.array_map_of_list rule_internal_of_json s
         | _ -> raise Not_found
       in
       {
         ra_type = Yojson.Basic.Util.to_int (List.assoc "type" l);
         ra_ports = ports;
         ra_ints = ints;
         ra_erased = Yojson.Basic.Util.to_bool (List.assoc "erased" l);
         ra_syntax = Some (ports, ints);
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Invalid rule_agent", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid rule_agent", x))

let rule_mixture_to_json filenames =
  JsonUtil.of_list (rule_agent_to_json filenames)

let rule_mixture_of_json filenames =
  JsonUtil.to_list (rule_agent_of_json filenames)

let lalg_expr_to_json filenames =
  Alg_expr.e_to_yojson ~filenames
    (rule_mixture_to_json filenames)
    JsonUtil.of_int

let lalg_expr_of_json filenames =
  Alg_expr.e_of_yojson ~filenames
    (rule_mixture_of_json filenames)
    (JsonUtil.to_int ?error_msg:None)

let rule_to_json ~filenames r =
  `Assoc
    [
      "mixture", rule_mixture_to_json filenames r.r_mix;
      "created", Raw_mixture.to_json r.r_created;
      ( "delta_tokens",
        JsonUtil.of_list
          (JsonUtil.of_pair ~lab1:"val" ~lab2:"tok"
             (Loc.yojson_of_annoted ~filenames (lalg_expr_to_json filenames))
             JsonUtil.of_int)
          r.r_delta_tokens );
      ( "rate",
        Loc.yojson_of_annoted ~filenames (lalg_expr_to_json filenames) r.r_rate
      );
      ( "unary_rate",
        JsonUtil.of_option
          (JsonUtil.of_pair
             (Loc.yojson_of_annoted ~filenames (lalg_expr_to_json filenames))
             (JsonUtil.of_option
                (Loc.yojson_of_annoted ~filenames (lalg_expr_to_json filenames))))
          r.r_un_rate );
      "edit_style", `Bool r.r_edit_style;
    ]

let rule_of_json ~filenames = function
  | `Assoc l as x when List.length l < 7 ->
    (try
       {
         r_mix = rule_mixture_of_json filenames (List.assoc "mixture" l);
         r_created = Raw_mixture.of_json (List.assoc "created" l);
         r_delta_tokens =
           JsonUtil.to_list
             (JsonUtil.to_pair ~lab1:"val" ~lab2:"tok"
                (Loc.annoted_of_yojson ~filenames (lalg_expr_of_json filenames))
                (JsonUtil.to_int ?error_msg:None))
             (List.assoc "delta_tokens" l);
         r_rate =
           Loc.annoted_of_yojson ~filenames
             (lalg_expr_of_json filenames)
             (List.assoc "rate" l);
         r_un_rate =
           (try
              JsonUtil.to_option
                (JsonUtil.to_pair
                   (Loc.annoted_of_yojson ~filenames
                      (lalg_expr_of_json filenames))
                   (JsonUtil.to_option
                      (Loc.annoted_of_yojson (lalg_expr_of_json filenames))))
                (List.assoc "unary_rate" l)
            with Not_found -> None);
         r_edit_style = Yojson.Basic.Util.to_bool (List.assoc "edit_style" l);
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Incorrect rule", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect rule", x))

let raise_if_modification pos = function
  | None -> ()
  | _ ->
    raise
      (ExceptionDefn.Malformed_Decl ("A modification is forbidden here.", pos))

let raise_several_internal_states pos =
  raise
    (ExceptionDefn.Malformed_Decl
       ("In a pattern, a site cannot have several internal states.", pos))

let raise_not_enough_specified ~status ~side agent_name (na, pos) =
  raise
    (ExceptionDefn.Malformed_Decl
       ( "The " ^ status ^ " state of agent '" ^ agent_name ^ "', site '" ^ na
         ^ "' on the " ^ side ^ " hand side is underspecified",
         pos ))

let raise_several_occurence_of_site agent_name (na, pos) =
  raise
    (ExceptionDefn.Malformed_Decl
       ( "Site '" ^ na ^ "' occurs more than once in this agent '" ^ agent_name
         ^ "'",
         pos ))

let raise_counter_misused agent_name (na, pos) =
  raise
    (ExceptionDefn.Malformed_Decl
       ( "Site '" ^ na ^ "' occurs both as port and as counter in '"
         ^ agent_name ^ "'",
         pos ))

let raise_link_only_one_occurence i pos =
  raise
    (ExceptionDefn.Malformed_Decl
       ( "The link '" ^ string_of_int i
         ^ "' occurs only one time in the mixture.",
         pos ))

let raise_link_should_be_removed i agent_name (na, pos) =
  raise
    (ExceptionDefn.Malformed_Decl
       ( "The link '" ^ string_of_int i ^ "' should be made free in the site '"
         ^ na ^ "' of agent '" ^ agent_name
         ^ "', since it will made be free by side-effect.",
         pos ))

let copy_rule_agent a =
  let p = Array.copy a.ra_ports in
  let i = Array.copy a.ra_ints in
  {
    ra_type = a.ra_type;
    ra_erased = a.ra_erased;
    ra_ports = p;
    ra_ints = i;
    ra_syntax =
      Option_util.map (fun _ -> Array.copy p, Array.copy i) a.ra_syntax;
  }

let agent_to_erased sigs r =
  let ra_ports = Array.map (fun (a, _) -> a, Erased) r.ra_ports in
  let ra_ints =
    Array.mapi
      (fun j -> function
        | I_VAL_CHANGED (i, _) | I_VAL_ERASED i -> I_VAL_ERASED i
        | I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED ->
          (match Signature.default_internal_state r.ra_type j sigs with
          | Some _ -> I_ANY_ERASED
          | None -> I_ANY))
      r.ra_ints
  in
  {
    ra_type = r.ra_type;
    ra_erased = true;
    ra_ports;
    ra_ints;
    ra_syntax =
      (match r.ra_syntax with
      | None -> None
      | Some _ -> Some (Array.copy ra_ports, Array.copy ra_ints));
  }

let to_erased (sigs : Signature.s) (x : rule_mixture) : rule_mixture =
  List.map (agent_to_erased sigs) x

let to_maintained (x : rule_mixture) : rule_mixture =
  List.map
    (fun r ->
      let ports = Array.map (fun (a, _) -> a, Maintained) r.ra_ports in
      let ints =
        Array.map
          (function
            | I_VAL_CHANGED (i, _) | I_VAL_ERASED i -> I_VAL_CHANGED (i, i)
            | I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED -> I_ANY)
          r.ra_ints
      in
      {
        ra_type = r.ra_type;
        ra_erased = false;
        ra_ports = ports;
        ra_ints = ints;
        ra_syntax =
          (match r.ra_syntax with
          | None -> None
          | Some _ -> Some (ports, ints));
      })
    x

let to_raw_mixture sigs x =
  List.map
    (fun r ->
      let internals =
        Array.mapi
          (fun j -> function
            | I_VAL_CHANGED (i, _) | I_VAL_ERASED i -> Some i
            | I_ANY | I_ANY_CHANGED _ | I_ANY_ERASED ->
              Signature.default_internal_state r.ra_type j sigs)
          r.ra_ints
      in
      let ports =
        Array.mapi
          (fun j -> function
            | (LNK_SOME, pos | LNK_TYPE _, pos), _ ->
              let agent_name =
                Format.asprintf "%a" (Signature.print_agent sigs) r.ra_type
              in
              let port_name =
                Format.asprintf "%a" (Signature.print_site sigs r.ra_type) j
              in
              raise_not_enough_specified ~status:"linking" ~side:"left"
                agent_name (port_name, pos)
            | (LNK_VALUE (i, _), _), _ -> Raw_mixture.VAL i
            | ((LNK_ANY | ANY_FREE | LNK_FREE), _), _ -> Raw_mixture.FREE)
          r.ra_ports
      in
      {
        Raw_mixture.a_type = r.ra_type;
        Raw_mixture.a_ports = ports;
        Raw_mixture.a_ints = internals;
      })
    x

let max_link_id r =
  let max_s m = function
    | Linked i -> max i m
    | Freed | Maintained | Erased -> m
  in
  let max_link_id_sites max_id ag =
    Array.fold_left
      (fun max_id -> function
        | (LNK_VALUE (j, _), _), s -> max_s (max j max_id) s
        | ((LNK_TYPE _ | LNK_SOME | LNK_FREE | LNK_ANY | ANY_FREE), _), s ->
          max_s max_id s)
      max_id ag.ra_ports
  in
  List.fold_left max_link_id_sites 0 r
