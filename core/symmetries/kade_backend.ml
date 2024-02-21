module Utils = struct
  let print_link_to_any_symbol f symbol_table =
    Format.fprintf f "%s" symbol_table.Symbol_table.link_to_any

  let print_free_symbol f symbol_table =
    Format.fprintf f "%s" symbol_table.Symbol_table.free

  let print_bound_to_unknown_symbol f symbol_table =
    Format.fprintf f "%s" symbol_table.Symbol_table.link_to_some

  let print_bound_symbol symbol_table pr_bound f bound =
    Format.fprintf f "%s%a" symbol_table.Symbol_table.bound pr_bound bound

  let print_binding_type_symbol symbol_table pr_port pr_type p f a =
    print_bound_symbol symbol_table
      (fun f () ->
        Format.fprintf f "%a%s%a" (pr_port a) p
          symbol_table.Symbol_table.btype_sep pr_type p)
      f ()

  let print_binding_state symbol_table pr_binding_state f binding_state =
    Format.fprintf f "%s%a%s" symbol_table.Symbol_table.open_binding_state
      pr_binding_state binding_state
      symbol_table.Symbol_table.close_binding_state

  let print_bound symbol_table pr_bound f bound =
    print_binding_state symbol_table
      (fun f () -> print_bound_symbol symbol_table pr_bound f bound)
      f ()

  let print_binding_state_and_switch_symbol _symbol_table pr_binding_state
      binding_state pr_switch f switch =
    Format.fprintf f "%a%a" pr_binding_state binding_state pr_switch switch

  let print_binding_state_and_switch symbol_table pr_binding_state binding_state
      pr_switch f switch =
    print_binding_state symbol_table
      (fun f () ->
        print_binding_state_and_switch_symbol symbol_table pr_binding_state
          binding_state pr_switch f switch)
      f ()

  let print_binding_type symbol_table pr_port pr_type p f a =
    print_binding_state symbol_table
      (fun f () -> print_binding_type_symbol symbol_table pr_port pr_type p f a)
      f ()

  let print_free_site f symbol_table =
    print_binding_state symbol_table print_free_symbol f symbol_table

  let print_internal_state symbol_table pr_st f st =
    Format.fprintf f "%s%s%a%s" symbol_table.Symbol_table.open_internal_state
      symbol_table.Symbol_table.internal_state_symbol pr_st st
      symbol_table.Symbol_table.close_internal_state

  let print_internal_state_symbol_any f symbol_table =
    Format.fprintf f "%s" symbol_table.Symbol_table.internal_state_any

  let print_internal_state_any f symbol_table =
    Format.fprintf f "%s%a%s" symbol_table.Symbol_table.open_internal_state
      print_internal_state_symbol_any symbol_table
      symbol_table.Symbol_table.close_internal_state

  let print_space symbol_table f space =
    Format.fprintf f
      (match space, symbol_table.Symbol_table.breakable with
      | Symbol_table.Space, true -> "@ "
      | Symbol_table.Space, false -> " "
      | Symbol_table.No_space, true -> "@,"
      | Symbol_table.No_space, false -> "")

  let print_separator symbol_table f (string, space) =
    Format.fprintf f "%s%a" string (print_space symbol_table) space

  let print_agent_sep_comma symbol_table f =
    print_separator symbol_table f symbol_table.Symbol_table.agent_sep_comma

  let print_agent_sep_dot symbol_table f =
    print_separator symbol_table f symbol_table.Symbol_table.agent_sep_dot

  let print_agent_sep_plus symbol_table f =
    print_separator symbol_table f symbol_table.Symbol_table.agent_sep_plus

  let print_site_sep symbol_table f =
    print_separator symbol_table f symbol_table.Symbol_table.site_sep
end

module Pattern = struct
  type id = Pattern.id

  let print_free_site = Utils.print_free_site

  let print_internal ?sigs (_, agent) site f id =
    match sigs with
    | Some sigs -> Signature.print_internal_state sigs agent site f id
    | None -> Format.fprintf f "%i" id

  let print_internal_state symbol_table ?sigs ag p fmt st =
    Utils.print_internal_state symbol_table (print_internal ?sigs ag p) fmt st

  let print_cc ?(full_species = false) ?sigs ?cc_id ~noCounters ~with_id
      ?(symbol_table = Symbol_table.symbol_table_V4) f cc =
    let print_intf ((ag_i, _) as ag) link_ids neigh =
      snd
        (Tools.array_fold_lefti
           (fun p (not_empty, ((free, link_ids) as out)) (el, st) ->
             let () =
               if st >= 0 then
                 Format.fprintf f "%t%a%a"
                   (if not_empty then
                      Utils.print_site_sep symbol_table
                    else
                      Pp.empty)
                   (Agent.print_site ?sigs ag)
                   p
                   (print_internal_state symbol_table ?sigs ag p)
                   st
               else if el <> Pattern.UnSpec then
                 Format.fprintf f "%t%a"
                   (if not_empty then
                      Utils.print_site_sep symbol_table
                    else
                      Pp.empty)
                   (Agent.print_site ?sigs ag)
                   p
             in
             match el with
             | Pattern.UnSpec ->
               if st >= 0 then (
                 let () = if full_species then print_free_site f symbol_table in
                 true, out
               ) else
                 not_empty, out
             | Pattern.Free ->
               let () = print_free_site f symbol_table in
               true, out
             | Pattern.Link (dst_a, dst_p) ->
               let dst_ty = Pattern.find_ty cc dst_a in
               if
                 match sigs with
                 | None -> false
                 | Some sigs ->
                   Signature.is_counter_agent sigs dst_ty && not noCounters
               then (
                 let counter = Pattern.counter_value_cc cc (dst_a, dst_p) 0 in
                 let () = Format.fprintf f "{=%d}" counter in
                 (* to do: add symbols in symbol table for counters *)
                 true, out
               ) else (
                 let i, out' =
                   match Mods.Int2Map.find_option (dst_a, dst_p) link_ids with
                   | Some x -> x, out
                   | None ->
                     free, (succ free, Mods.Int2Map.add (ag_i, p) free link_ids)
                 in
                 let () =
                   Utils.print_bound symbol_table Format.pp_print_int f i
                 in
                 true, out'
               ))
           (false, link_ids) neigh)
    in
    let () =
      match cc_id with
      | None -> ()
      | Some cc_id -> Format.fprintf f "/*cc%a*/@ " Pattern.debug_print_id cc_id
    in
    let _, _ =
      Pattern.fold
        (fun x el (not_empty, link_ids) ->
          let ag_x = x, Pattern.find_ty cc x in
          if
            match sigs with
            | None -> true
            | Some sigs ->
              (not (Signature.is_counter_agent sigs (snd ag_x))) || noCounters
          then (
            let () =
              Format.fprintf f "%t@[<h>%a%s"
                (if not_empty then
                   Utils.print_agent_sep_dot symbol_table
                 else
                   Pp.empty)
                (Agent.print ?sigs ~with_id)
                ag_x symbol_table.Symbol_table.agent_open
            in
            let out = print_intf ag_x link_ids el in
            let () =
              Format.fprintf f "%s@]" symbol_table.Symbol_table.agent_close
            in
            true, out
          ) else
            not_empty, link_ids)
        cc
        (false, (1, Mods.Int2Map.empty))
    in
    ()

  let print ?domain ~noCounters ~with_id
      ?(symbol_table = Symbol_table.symbol_table_V4) f id =
    match domain with
    | None -> Pattern.debug_print_id f id
    | Some env ->
      let cc_id =
        if with_id then
          Some id
        else
          None
      in
      print_cc
        ~sigs:(Pattern.Env.signatures env)
        ?cc_id ~noCounters ~with_id ~symbol_table f
        (Pattern.Env.content (Pattern.Env.get env id))
end

module Ast = struct
  include Ast

  let print_link pr_port pr_type pr_annot symbol_table f = function
    | LKappa.ANY_FREE | LKappa.LNK_ANY ->
      Utils.print_link_to_any_symbol f symbol_table
    | LKappa.LNK_TYPE (p, a) ->
      Utils.print_binding_type_symbol symbol_table pr_port pr_type p f a
    | LKappa.LNK_FREE -> Utils.print_free_symbol f symbol_table
    | LKappa.LNK_SOME -> Utils.print_bound_to_unknown_symbol f symbol_table
    | LKappa.LNK_VALUE (i, a) ->
      Utils.print_bound_symbol symbol_table
        (fun fmt a -> Format.fprintf fmt "%i%a" i pr_annot a)
        f a
end

module Raw_mixture = struct
  include Raw_mixture

  let print_link ~noCounters symbol_table counter_agents f = function
    | Raw_mixture.FREE -> Utils.print_free_site f symbol_table
    | Raw_mixture.VAL i ->
      (try
         let root = Raw_mixture.find counter_agents i in
         let counter, (_, is_counter) =
           Mods.DynArray.get counter_agents.Raw_mixture.rank root
         in
         if is_counter && not noCounters then
           Format.fprintf f "{=%d}" counter
         (* to do: add symbols in symbol table for counters *)
         else
           Utils.print_bound symbol_table Format.pp_print_int f i
       with Invalid_argument _ ->
         Utils.print_bound symbol_table Format.pp_print_int f i)

  let aux_pp_si sigs symbol_table a s f i =
    match sigs with
    | Some sigs ->
      Format.fprintf f "%a%a"
        (Signature.print_site sigs a)
        s
        (fun fmt id_opt ->
          match id_opt with
          | None -> ()
          | Some i ->
            Utils.print_internal_state symbol_table
              (Signature.print_internal_state sigs a s)
              fmt i)
        i
    | None ->
      (match i with
      | Some i ->
        Format.fprintf f "%i%a" s
          (Utils.print_internal_state symbol_table Format.pp_print_int)
          i
      | None -> Format.pp_print_int f s)

  let print_intf ~noCounters with_link ?sigs
      ?(symbol_table = Symbol_table.symbol_table_V4) counter_agents
      (ag_ty : int) f (ports, ints) =
    let rec aux empty i =
      if i < Array.length ports then (
        let () =
          Format.fprintf f "%t%a%a"
            (if empty then
               Pp.empty
             else
               Utils.print_site_sep symbol_table)
            (aux_pp_si sigs symbol_table ag_ty i)
            ints.(i)
            (if with_link then
               print_link ~noCounters symbol_table counter_agents
             else
               fun _ _ ->
             ())
            ports.(i)
        in
        aux false (succ i)
      )
    in
    aux true 0

  let aux_pp_ag sigs f a =
    match sigs with
    | Some sigs -> Signature.print_agent sigs f a
    | None -> Format.pp_print_int f a

  let print_agent ~noCounters created link ?sigs
      ?(symbol_table = Symbol_table.symbol_table_V4) counter_agents f ag =
    Format.fprintf f "%a%s@[<h>%a@]%s%t" (aux_pp_ag sigs) ag.Raw_mixture.a_type
      symbol_table.Symbol_table.agent_open
      (print_intf ~noCounters link ?sigs ~symbol_table counter_agents
         ag.Raw_mixture.a_type) (ag.Raw_mixture.a_ports, ag.Raw_mixture.a_ints)
      symbol_table.Symbol_table.agent_close (fun f ->
        if created then Format.pp_print_string f "+")
  (* to do: add symbols for agent creation/degradation *)

  let print ~noCounters ~created ?sigs
      ?(symbol_table = Symbol_table.symbol_table_V4) f mix =
    let counter_agents = Raw_mixture.union_find_counters sigs mix in
    let rec aux_print some = function
      | [] -> ()
      | h :: t ->
        if
          match sigs with
          | None -> false
          | Some sigs ->
            Signature.is_counter_agent sigs h.Raw_mixture.a_type
            && not noCounters
        then
          aux_print some t
        else (
          let () = if some then Utils.print_agent_sep_comma symbol_table f in
          let () =
            print_agent ~noCounters created true ?sigs ~symbol_table
              counter_agents f h
          in
          aux_print true t
        )
    in
    aux_print false mix
end

module LKappa = struct
  let print_link_annot ~ltypes sigs symbol_table f (s, a) =
    if ltypes then
      Format.fprintf f "/*%a%s%a*/"
        (Signature.print_site sigs a)
        s symbol_table.Symbol_table.btype_sep
        (Signature.print_agent sigs)
        a

  let print_switching ~show_erased f = function
    (* to do: add symbols in symbol table for counters *)
    | LKappa.Linked i -> Format.fprintf f "/%i" i
    | LKappa.Freed -> Format.pp_print_string f "/."
    | LKappa.Maintained -> ()
    | LKappa.Erased -> if show_erased then Format.pp_print_string f "--"

  let print_rule_link sigs symbol_table ~show_erased ~ltypes f ((e, _), s) =
    Utils.print_binding_state_and_switch symbol_table
      (Ast.print_link
         (Signature.print_site sigs)
         (Signature.print_agent sigs)
         (print_link_annot ~ltypes sigs symbol_table)
         symbol_table)
      e
      (print_switching ~show_erased)
      f s

  let print_rule_internal sigs symbol_table ~show_erased ag_ty site f = function
    (* to do: add symbols for mods *)
    | LKappa.I_ANY -> ()
    | LKappa.I_ANY_CHANGED j ->
      Format.fprintf f "{#/%a}"
        (Signature.print_internal_state sigs ag_ty site)
        j
    | LKappa.I_ANY_ERASED -> if show_erased then Format.fprintf f "~--"
    | LKappa.I_VAL_CHANGED (i, j) ->
      if i <> j then
        Format.fprintf f "{%a/%a}"
          (Signature.print_internal_state sigs ag_ty site)
          i
          (Signature.print_internal_state sigs ag_ty site)
          j
      else
        Pattern.print_internal_state symbol_table ~sigs ((), ag_ty) site f i
    | LKappa.I_VAL_ERASED i ->
      Format.fprintf f "{%a%t}" (Signature.print_internal_state sigs ag_ty site)
        i (fun f -> if show_erased then Format.pp_print_string f "--")

  let print_counter_test f = function
    (* to do: add symbols for counters *)
    | c, true -> Format.fprintf f "=%i" c
    | c, false -> Format.fprintf f ">=%i" c

  let print_counter_delta counters j f switch =
    match switch with
    | LKappa.Linked i ->
      let root = Raw_mixture.find counters i in
      let s, (_, is_counter) =
        Mods.DynArray.get counters.Raw_mixture.rank root
      in
      let delta =
        if is_counter then
          s - 1
        else
          j - i
      in
      (* to do: add symbols for counters *)
      Format.fprintf f "/+=%d" delta
    | LKappa.Freed ->
      raise
        (ExceptionDefn.Internal_Error
           (Loc.annot_with_dummy "Cannot erase all increment agents"))
    | LKappa.Maintained -> ()
    | LKappa.Erased -> ()

  let print_rule_intf sigs ~show_erased ~ltypes symbol_table ag_ty ?counters f
      (ports, ints) =
    let rec aux empty i =
      if i < Array.length ports then
        if
          match ports.(i) with
          | (LKappa.LNK_ANY, _), LKappa.Maintained -> ints.(i) <> LKappa.I_ANY
          | (LKappa.LNK_ANY, _), (LKappa.Erased | LKappa.Freed | LKappa.Linked _)
          | ( ( ( LKappa.LNK_SOME | LKappa.ANY_FREE | LKappa.LNK_FREE
                | LKappa.LNK_TYPE _ | LKappa.LNK_VALUE _ ),
                _ ),
              _ ) ->
            true
        then (
          let (e, _), switch = ports.(i) in
          let is_counter =
            match e with
            | LKappa.ANY_FREE | LKappa.LNK_FREE | LKappa.LNK_ANY
            | LKappa.LNK_TYPE _ | LKappa.LNK_SOME ->
              false
            | LKappa.LNK_VALUE (j, _) ->
              (match counters with
              | None -> false
              | Some (counters, created_counters) ->
                (try
                   let root = Raw_mixture.find counters j in
                   let c, (eq, is_counter') =
                     Mods.DynArray.get counters.Raw_mixture.rank root
                   in
                   if is_counter' then (
                     (* to do: add symbols for counters *)
                     let () =
                       Format.fprintf f "%t%a{%a%a}"
                         (if empty then
                            Pp.empty
                          else
                            Pp.space)
                         (Signature.print_site sigs ag_ty)
                         i print_counter_test
                         (c - 1, eq)
                         (print_counter_delta created_counters j)
                         switch
                     in
                     true
                   ) else
                     false
                 with Invalid_argument _ -> false))
          in
          let () =
            if not is_counter then
              Format.fprintf f "%t%a%a%a"
                (if empty then
                   Pp.empty
                 else
                   Utils.print_site_sep symbol_table)
                (Signature.print_site sigs ag_ty)
                i
                (print_rule_internal sigs symbol_table ~show_erased ag_ty i)
                ints.(i)
                (print_rule_link sigs symbol_table ~show_erased ~ltypes)
                ports.(i)
            else
              ()
          in
          aux false (succ i)
        ) else
          aux empty (succ i)
    in
    aux true 0

  let print_rule_agent sigs ~ltypes
      ?(symbol_table = Symbol_table.symbol_table_V4) ?counters f ag =
    Format.fprintf f "%a%s@[<h>%a@]%s%t" (Signature.print_agent sigs)
      ag.LKappa.ra_type symbol_table.Symbol_table.agent_open
      (print_rule_intf sigs symbol_table ~show_erased:false ~ltypes
         ag.LKappa.ra_type ?counters) (ag.LKappa.ra_ports, ag.LKappa.ra_ints)
      symbol_table.Symbol_table.agent_close (fun f ->
        if ag.LKappa.ra_erased then Format.pp_print_string f "-")

  let union_find_counters sigs mix =
    let t = Raw_mixture.create 1 in
    let () =
      match sigs with
      | None -> ()
      | Some sigs ->
        List.iter
          (fun ag ->
            match Signature.ports_if_counter_agent sigs ag.LKappa.ra_type with
            | None -> ()
            | Some (before, after) ->
              let (a, _), _ = ag.LKappa.ra_ports.(after) in
              let (b, _), _ = ag.LKappa.ra_ports.(before) in
              (match b with
              | LKappa.ANY_FREE | LKappa.LNK_FREE | LKappa.LNK_ANY
              | LKappa.LNK_TYPE _ | LKappa.LNK_SOME ->
                ()
              | LKappa.LNK_VALUE (lnk_b, _) ->
                (match a with
                | LKappa.LNK_VALUE (lnk_a, _) -> Raw_mixture.union t lnk_b lnk_a
                | LKappa.ANY_FREE | LKappa.LNK_FREE ->
                  let root = Raw_mixture.find t lnk_b in
                  let s, _ = Mods.DynArray.get t.Raw_mixture.rank root in
                  Mods.DynArray.set t.Raw_mixture.rank root (s, (true, true))
                | LKappa.LNK_ANY ->
                  let root = Raw_mixture.find t lnk_b in
                  let s, _ = Mods.DynArray.get t.Raw_mixture.rank root in
                  Mods.DynArray.set t.Raw_mixture.rank root (s, (false, true))
                | LKappa.LNK_TYPE _ | LKappa.LNK_SOME ->
                  raise
                    (ExceptionDefn.Internal_Error
                       (Loc.annot_with_dummy
                          "Port a of __counter_agent agent not well specified")))))
          mix
    in
    t

  let print_rule_mixture ~noCounters sigs ~ltypes
      ?(symbol_table = Symbol_table.symbol_table_V4) created f mix =
    let counters =
      if noCounters then
        None
      else
        Some
          ( union_find_counters (Some sigs) mix,
            Raw_mixture.union_find_counters (Some sigs) created )
    in
    let rec aux_print some = function
      | [] -> ()
      | h :: t ->
        if Signature.is_counter_agent sigs h.LKappa.ra_type && not noCounters
        then
          aux_print some t
        else (
          let () = if some then Utils.print_agent_sep_comma symbol_table f in
          let () = print_rule_agent sigs ~ltypes ~symbol_table ?counters f h in
          aux_print true t
        )
    in
    aux_print false mix

  let print_internal_lhs sigs symbol_table ag_ty site f = function
    | LKappa.I_ANY -> ()
    | LKappa.I_ANY_CHANGED _ | LKappa.I_ANY_ERASED ->
      Utils.print_internal_state_any f symbol_table
    | LKappa.I_VAL_CHANGED (i, _) | LKappa.I_VAL_ERASED i ->
      Pattern.print_internal_state symbol_table ~sigs ((), ag_ty) site f i

  let print_internal_rhs sigs symbol_table ag_ty site f = function
    | LKappa.I_ANY -> ()
    | LKappa.I_ANY_CHANGED j | LKappa.I_VAL_CHANGED (_, j) ->
      Pattern.print_internal_state symbol_table ~sigs ((), ag_ty) site f j
    | LKappa.I_ANY_ERASED | LKappa.I_VAL_ERASED _ -> assert false

  let print_link_lhs ~ltypes sigs symbol_table f ((e, _), _) =
    Utils.print_binding_state symbol_table
      (Ast.print_link
         (Signature.print_site sigs)
         (Signature.print_agent sigs)
         (print_link_annot ~ltypes sigs symbol_table)
         symbol_table)
      f e

  let print_link_rhs ~ltypes sigs symbol_table f ((e, _), s) =
    Utils.print_binding_state symbol_table
      (fun f -> function
        | LKappa.Linked i ->
          Ast.print_link
            (Signature.print_site sigs)
            (Signature.print_agent sigs)
            (fun _ () -> ())
            symbol_table f
            (LKappa.LNK_VALUE (i, ()))
        | LKappa.Freed ->
          Ast.print_link
            (Signature.print_site sigs)
            (Signature.print_agent sigs)
            (fun _ () -> ())
            symbol_table f LKappa.LNK_FREE
        | LKappa.Maintained ->
          Ast.print_link
            (Signature.print_site sigs)
            (Signature.print_agent sigs)
            (print_link_annot ~ltypes sigs symbol_table)
            symbol_table f e
        | LKappa.Erased -> assert false)
      f s

  let print_intf_lhs ~ltypes sigs symbol_table ag_ty f (ports, ints) =
    let rec aux empty i =
      if i < Array.length ports then
        if
          match ports.(i) with
          | ( ( ( LKappa.LNK_SOME | LKappa.LNK_FREE | LKappa.ANY_FREE
                | LKappa.LNK_TYPE _ | LKappa.LNK_VALUE _ ),
                _ ),
              _ ) ->
            true
          | (LKappa.LNK_ANY, _), _ ->
            (match ints.(i) with
            | LKappa.I_ANY | LKappa.I_ANY_ERASED | LKappa.I_ANY_CHANGED _ ->
              false
            | LKappa.I_VAL_CHANGED _ | LKappa.I_VAL_ERASED _ -> true)
        then (
          let () =
            Format.fprintf f "%t%a%a%a"
              (if empty then
                 Pp.empty
               else
                 Utils.print_site_sep symbol_table)
              (Signature.print_site sigs ag_ty)
              i
              (print_internal_lhs sigs symbol_table ag_ty i)
              ints.(i)
              (print_link_lhs ~ltypes sigs symbol_table)
              ports.(i)
          in
          aux false (succ i)
        ) else
          aux empty (succ i)
    in
    aux true 0

  let print_intf_rhs ~ltypes sigs symbol_table ag_ty f (ports, ints) =
    let rec aux empty i =
      if i < Array.length ports then
        if
          match ports.(i) with
          | ( ( ( LKappa.LNK_SOME | LKappa.LNK_FREE | LKappa.ANY_FREE
                | LKappa.LNK_TYPE _ | LKappa.LNK_VALUE _ ),
                _ ),
              _ ) ->
            true
          | (LKappa.LNK_ANY, _), (LKappa.Erased | LKappa.Freed | LKappa.Linked _)
            ->
            true
          | (LKappa.LNK_ANY, _), LKappa.Maintained ->
            (match ints.(i) with
            | LKappa.I_ANY -> false
            | LKappa.I_VAL_CHANGED (i, j) -> i <> j
            | LKappa.I_ANY_ERASED | LKappa.I_ANY_CHANGED _
            | LKappa.I_VAL_ERASED _ ->
              true)
        then (
          let () =
            Format.fprintf f "%t%a%a%a"
              (if empty then
                 Pp.empty
               else
                 Utils.print_site_sep symbol_table)
              (Signature.print_site sigs ag_ty)
              i
              (print_internal_rhs sigs symbol_table ag_ty i)
              ints.(i)
              (print_link_rhs ~ltypes sigs symbol_table)
              ports.(i)
          in
          aux false (succ i)
        ) else
          aux empty (succ i)
    in
    aux true 0

  let print_agent_lhs ~ltypes sigs symbol_table f ag =
    Format.fprintf f "%a%s@[<h>%a@]%s"
      (Signature.print_agent sigs)
      ag.LKappa.ra_type symbol_table.Symbol_table.agent_open
      (print_intf_lhs ~ltypes sigs symbol_table ag.LKappa.ra_type)
      (ag.LKappa.ra_ports, ag.LKappa.ra_ints)
      symbol_table.Symbol_table.agent_close

  let print_agent_rhs ~ltypes sigs symbol_table f ag =
    if not ag.LKappa.ra_erased then
      Format.fprintf f "%a%s@[<h>%a@]%s"
        (Signature.print_agent sigs)
        ag.LKappa.ra_type symbol_table.Symbol_table.agent_open
        (print_intf_rhs ~ltypes sigs symbol_table ag.LKappa.ra_type)
        (ag.LKappa.ra_ports, ag.LKappa.ra_ints)
        symbol_table.Symbol_table.agent_close

  let print_rhs ~noCounters ~ltypes sigs symbol_table created f mix =
    let rec aux empty = function
      | [] ->
        Format.fprintf f "%t%a"
          (if empty || created = [] then
             Pp.empty
           else
             Utils.print_agent_sep_comma symbol_table)
          (Raw_mixture.print ~noCounters ~created:false ~sigs ~symbol_table)
          created
      | h :: t ->
        if h.LKappa.ra_erased then
          if symbol_table.Symbol_table.show_ghost then (
            let () =
              Format.fprintf f "%t%s"
                (if empty then
                   Pp.empty
                 else
                   Utils.print_agent_sep_comma symbol_table)
                symbol_table.Symbol_table.ghost_agent
            in
            aux false t
          ) else
            aux false t
        else (
          let () =
            Format.fprintf f "%t%a"
              (if empty then
                 Pp.empty
               else
                 Utils.print_agent_sep_comma symbol_table)
              (print_agent_rhs ~ltypes sigs symbol_table)
              h
          in
          aux false t
        )
    in
    aux true mix

  let print_rates ~noCounters sigs ?symbol_table pr_tok pr_var f r =
    let ltypes = false in
    Format.fprintf f " @@ %a%t"
      (Alg_expr.print
         (fun f m ->
           Format.fprintf f "|%a|"
             (print_rule_mixture ~noCounters sigs ?symbol_table ~ltypes [])
             m)
         pr_tok pr_var)
      (fst r.LKappa.r_rate)
      (fun f ->
        match r.LKappa.r_un_rate with
        | None -> ()
        | Some ((ra, _), max_dist) ->
          Format.fprintf f " {%a%a}"
            (Alg_expr.print
               (fun f m ->
                 Format.fprintf f "|%a|"
                   (print_rule_mixture ~noCounters sigs ?symbol_table ~ltypes [])
                   m)
               pr_tok pr_var)
            ra
            (Pp.option (fun f (md, _) ->
                 Format.fprintf f ":%a"
                   (Alg_expr.print
                      (fun f m ->
                        Format.fprintf f "|%a|"
                          (print_rule_mixture ~noCounters sigs ?symbol_table
                             ~ltypes [])
                          m)
                      pr_tok pr_var)
                   md))
            max_dist)

  let print_rule ~noCounters ~full sigs
      ?(symbol_table = Symbol_table.symbol_table_V4) pr_tok pr_var f r =
    Format.fprintf f "@[<h>%t%t%a%t@]"
      (fun f ->
        if full || r.LKappa.r_edit_style then
          Format.fprintf f "%a%t%a"
            (print_rule_mixture ~noCounters sigs ~ltypes:false ~symbol_table
               r.LKappa.r_created)
            r.LKappa.r_mix
            (fun f ->
              if r.LKappa.r_mix <> [] && r.LKappa.r_created <> [] then
                (Utils.print_agent_sep_comma symbol_table) f)
            (Raw_mixture.print ~noCounters ~created:true ~sigs ~symbol_table)
            r.LKappa.r_created
        else
          Format.fprintf f "%a%t%a -> %a"
            (Pp.list
               (Utils.print_agent_sep_comma symbol_table)
               (print_agent_lhs ~ltypes:false sigs symbol_table))
            r.LKappa.r_mix
            (fun f ->
              if r.LKappa.r_mix <> [] && r.LKappa.r_created <> [] then
                (Utils.print_agent_sep_comma symbol_table) f)
            (if symbol_table.Symbol_table.show_ghost then
               Pp.list (Utils.print_agent_sep_comma symbol_table) (fun f _ ->
                   Format.pp_print_string f
                     symbol_table.Symbol_table.ghost_agent)
             else
               fun f _ ->
             Pp.empty f)
            r.LKappa.r_created
            (print_rhs ~noCounters ~ltypes:false sigs symbol_table
               r.LKappa.r_created)
            r.LKappa.r_mix)
      (fun f ->
        match r.LKappa.r_delta_tokens with
        | [] -> ()
        | _ :: _ -> Format.pp_print_string f " | ")
      (Pp.list Pp.comma (fun f ((nb, _), tk) ->
           Format.fprintf f "%a %a"
             (Alg_expr.print
                (fun f m ->
                  Format.fprintf f "|%a|"
                    (print_rule_mixture ~noCounters sigs ~symbol_table
                       ~ltypes:false [])
                    m)
                pr_tok pr_var)
             nb pr_tok tk))
      r.LKappa.r_delta_tokens
      (fun f ->
        if full then
          print_rates ~noCounters sigs ~symbol_table pr_tok pr_var f r)
end

module Kappa_printer = struct
  let cc_mix ~noCounters ?env ?symbol_table =
    let domain =
      match env with
      | None -> None
      | Some e -> Some (Model.domain e)
    in
    Pp.list
      (fun f -> Format.fprintf f " +@ ")
      (fun f ccs ->
        Pp.array
          (fun f -> Format.fprintf f "*")
          (fun _ f cc ->
            Format.fprintf f "|%a|"
              (Pattern.print ~noCounters ?domain ~with_id:false ?symbol_table)
              cc)
          f ccs)

  let alg_expr ~noCounters ?env ?symbol_table =
    Alg_expr.print
      (cc_mix ~noCounters ?env ?symbol_table)
      (Model.print_token ?env) (Model.print_alg ?env)

  let decompiled_rule ~noCounters ~full
      ?(symbol_table = Symbol_table.symbol_table_V4) env f r =
    let sigs = Model.signatures env in
    let r_mix, r_created =
      Pattern_compiler.lkappa_of_elementary_rule sigs (Model.domain env) r
    in
    let pr_alg f (a, _) = alg_expr ~noCounters ~env ~symbol_table f a in
    let pr_tok f (va, tok) =
      Format.fprintf f "%a %a" pr_alg va (Model.print_token ~env) tok
    in
    Format.fprintf f "%a%t%a%t%a%t"
      (LKappa.print_rule_mixture ~noCounters sigs ~symbol_table ~ltypes:false
         r_created)
      r_mix
      (if r_mix <> [] && r_created <> [] then
         fun fmt ->
       Utils.print_agent_sep_dot symbol_table fmt
       else
         Pp.empty)
      (Raw_mixture.print ~noCounters ~created:true ~sigs ~symbol_table)
      r_created
      (if r.Primitives.delta_tokens <> [] then
         fun f ->
       Format.fprintf f "|@ "
       else
         Pp.empty)
      (Pp.list Pp.comma pr_tok) r.Primitives.delta_tokens
      (fun f ->
        if full then
          Format.fprintf f " @@@ %a%t" pr_alg r.Primitives.rate (fun f ->
              match r.Primitives.unary_rate with
              | None -> ()
              | Some (rate, dist) ->
                Format.fprintf f " {%a%a}" pr_alg rate
                  (Pp.option (fun f md ->
                       Format.fprintf f ":%a"
                         (alg_expr ~noCounters ~env ~symbol_table)
                         md))
                  dist))

  let elementary_rule ~noCounters ?env ?symbol_table f r =
    let domain, sigs =
      match env with
      | None -> None, None
      | Some e -> Some (Model.domain e), Some (Model.signatures e)
    in
    let pr_alg f (a, _) = alg_expr ~noCounters ?env ?symbol_table f a in
    let pr_tok f (va, tok) =
      Format.fprintf f "%a %a" pr_alg va (Model.print_token ?env) tok
    in
    let pr_trans f t = Primitives.Transformation.print ?sigs f t in
    let boxed_cc i f cc =
      let () = Format.pp_open_box f 2 in
      let () = Format.pp_print_int f i in
      let () = Format.pp_print_string f ": " in
      let () =
        Pattern.print ~noCounters ?domain ~with_id:true ?symbol_table f cc
      in
      Format.pp_close_box f ()
    in
    Format.fprintf f
      "(ast: %i)@ @[@[%a@]%t@[%a@]@]@ -- @[%a@]@ ++ @[%a@]@ @@%a%t"
      r.Primitives.syntactic_rule
      (Pp.array Pp.comma boxed_cc)
      r.Primitives.connected_components
      (if r.Primitives.delta_tokens <> [] then
         fun f ->
       Format.fprintf f "|@ "
       else
         Pp.empty)
      (Pp.list Pp.comma pr_tok) r.Primitives.delta_tokens
      (Pp.list Pp.comma pr_trans)
      r.Primitives.removed
      (Pp.list Pp.comma pr_trans)
      r.Primitives.inserted pr_alg r.Primitives.rate
      (fun f ->
        match r.Primitives.unary_rate with
        | None -> ()
        | Some (rate, dist) ->
          Format.fprintf f " {%a%a}" pr_alg rate
            (Pp.option (fun f md ->
                 Format.fprintf f ":%a"
                   (alg_expr ~noCounters ?env ?symbol_table)
                   md))
            dist)
end

module Model = struct
  let print_ast_rule ~noCounters ?env ?symbol_table f i =
    match env with
    | None -> Format.fprintf f "__ast_rule_%i" i
    | Some env ->
      let sigs = Model.signatures env in
      if i = 0 then
        Format.pp_print_string f "Interventions"
      else (
        match Model.get_ast_rule_with_label env i with
        | Some (na, _), _ -> Format.pp_print_string f na
        | None, (r, _) ->
          LKappa.print_rule ~noCounters ~full:false sigs ?symbol_table
            (Model.print_token ~env) (Model.print_alg ~env) f r
      )
end
