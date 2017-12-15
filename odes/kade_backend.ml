module Pattern =
struct

  let print_free_site symbol_table fmt =
    Format.fprintf fmt "%s%s%s"
      symbol_table.Symbol_table.open_binding_state
      symbol_table.Symbol_table.free
      symbol_table.Symbol_table.close_binding_state
  let print_internal ?sigs (_,agent) site f id =
    match sigs with
    | Some sigs ->
      Signature.print_internal_state sigs agent site f id
    | None -> Format.fprintf f "%i" id

  let print_internal_state symbol_table ?sigs ag p fmt st =
    Format.fprintf fmt "%s%s%a%s"
      symbol_table.Symbol_table.open_internal_state
      symbol_table.Symbol_table.internal_state_symbol
      (print_internal ?sigs ag p) st
      symbol_table.Symbol_table.close_internal_state

  let print_cc
      ?dotnet:(dotnet=false)
      ?full_species:(full_species=false) ?sigs ?cc_id ~with_id ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4) f cc =
        let print_intf (ag_i, _ as ag) link_ids neigh =
          snd
            (Tools.array_fold_lefti
               (fun p (not_empty, (free, link_ids as out)) (el, st) ->
                  let () =
                    if st >= 0
                    then Format.fprintf
                        f "%t%a%a"
                        (if not_empty then
                           (fun fmt -> Format.fprintf fmt "%s"
                               (if dotnet then
                                  symbol_table.Symbol_table.dotnet_site_sep
                                else symbol_table.Symbol_table.site_sep_comma))
                         else Pp.empty)
                        (Agent.print_site ?sigs ag) p
                        (print_internal_state symbol_table ?sigs ag p)
                        st
                    else if  el <> Pattern.UnSpec then
                      Format.fprintf
                        f "%t%a"
                        (if not_empty then
                          (fun fmt -> Format.fprintf f "%s"
                            (if dotnet then symbol_table.Symbol_table.dotnet_site_sep
                             else symbol_table.Symbol_table.site_sep_comma))
                         else Pp.empty)
                        (Agent.print_site ?sigs ag) p in
                  match el with
                  | Pattern.UnSpec ->
                    if st >= 0 then
                      let () =
                        if full_species then
                          print_free_site symbol_table f
                      in
                      (true,out)
                    else (not_empty,out)
                  | Pattern.Free ->
                    let () = print_free_site symbol_table f in
                    (true,out)
                  | Pattern.Link (dst_a,dst_p) ->
                    let dst_ty = Pattern.find_ty cc dst_a in
                    if Signature.is_counter_agent sigs dst_ty
                    && not(!Parameter.debugModeOn) then
                      let counter = Pattern.counter_value_cc cc (dst_a,dst_p) 0 in
                      let () = Format.fprintf f "{=%d}" counter in
                      true,out
                    else
                      let i,out' =
                        match
                          Mods.Int2Map.find_option (dst_a,dst_p) link_ids
                        with
                        | Some x -> (x, out)
                        | None ->
                          (free, (succ free,
                                  Mods.Int2Map.add (ag_i,p) free link_ids))
                      in
                      let () = Format.fprintf f "%s%s%i%s"
                          symbol_table.Symbol_table.open_binding_state
                          symbol_table.Symbol_table.bound
                          i
                          symbol_table.Symbol_table.close_binding_state
                      in
                      true, out')
               (false, link_ids) neigh)
        in
        let () = match cc_id with
          | None -> ()
          | Some cc_id -> Format.fprintf f "/*cc%a*/@ "
                            Pattern.debug_print_id cc_id in
        let (_, _) =
          Pattern.fold
            (fun x el (not_empty,link_ids) ->
               let ag_x = (x,Pattern.find_ty cc x) in
               if (not (Signature.is_counter_agent sigs (snd ag_x)))
               || (!Parameter.debugModeOn) then
                 let () =
                   Format.fprintf
                     f "%t@[<h>%a%s"
                     (if not_empty
                      then
                        begin
                          if dotnet then
                            (fun fmt -> Format.fprintf fmt ".")
                          else Pp.comma
                        end
                      else Pp.empty)
                     (Agent.print ?sigs ~with_id) ag_x
                     symbol_table.Symbol_table.agent_open
                 in
                 let out = print_intf ag_x link_ids el in
                 let () = Format.fprintf f "%s@]" symbol_table.Symbol_table.agent_close in
                 true, out
               else not_empty,link_ids)
            cc (false, (1, Mods.Int2Map.empty))
        in
        ()


  let print
      ?domain ~with_id ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
      f id =
    match domain with
    | None -> Pattern.debug_print_id f id
    | Some env ->
      let cc_id = if with_id then Some id else None in
      print_cc
        ~sigs:(Pattern.Env.signatures env) ?cc_id ~with_id
        ~symbol_table
        f (Pattern.Env.content (Pattern.Env.get env id))

end

module Ast =
struct
  include Ast

  let print_link pr_port pr_type pr_annot symbol_table f = function
    | Ast.ANY_FREE | Ast.LNK_ANY ->
      Format.pp_print_string f symbol_table.Symbol_table.link_to_any
    | Ast.LNK_TYPE (p, a) ->
      Format.fprintf f "%a%s%a" (pr_port a) p
        symbol_table.Symbol_table.btype_sep pr_type a
    | Ast.LNK_FREE ->
      Format.pp_print_string f symbol_table.Symbol_table.free
    | Ast.LNK_SOME ->
      Format.pp_print_string f symbol_table.Symbol_table.link_to_some

    | Ast.LNK_VALUE (i,a) -> Format.fprintf f "%s%i%a"
                               symbol_table.Symbol_table.bound i
                               pr_annot a

end

module LKappa =
struct


  let print_link_annot ~ltypes sigs symbol_table f (s,a) =
    if ltypes then
      Format.fprintf f "/*%a%s%a*/"
        (Signature.print_site sigs a) s
        symbol_table.Symbol_table.btype_sep
        (Signature.print_agent sigs) a

  let print_switching ~show_erased f = function
    | LKappa.Linked i -> Format.fprintf f "/%i" i
    | LKappa.Freed -> Format.pp_print_string f "/."
    | LKappa.Maintained -> ()
    | LKappa.Erased -> if show_erased then Format.pp_print_string f "--"

  let print_rule_link sigs symbol_table ~show_erased ~ltypes f ((e,_),s) =
    Format.fprintf
      f "%s%a%a%s"
      symbol_table.Symbol_table.open_binding_state
      (Ast.print_link
         (Signature.print_site sigs)
         (Signature.print_agent sigs) (print_link_annot ~ltypes sigs symbol_table) symbol_table)
      e
      (print_switching ~show_erased) s
      symbol_table.Symbol_table.close_binding_state


  let print_rule_internal sigs symbol_table ~show_erased ag_ty site f = function
    | LKappa.I_ANY -> ()
    | LKappa.I_ANY_CHANGED j ->
      Format.fprintf f "{#/%a}" (Signature.print_internal_state sigs ag_ty site) j
    | LKappa.I_ANY_ERASED -> if show_erased then Format.fprintf f "~--"
    | LKappa.I_VAL_CHANGED (i,j) ->
      if i <> j then
        Format.fprintf
          f "{%a/%a}" (Signature.print_internal_state sigs ag_ty site) i
          (Signature.print_internal_state sigs ag_ty site) j
      else
        Format.fprintf f "%s%s%a%s"
          symbol_table.Symbol_table.open_internal_state
          symbol_table.Symbol_table.internal_state_symbol (Signature.print_internal_state sigs ag_ty site) i
          symbol_table.Symbol_table.close_internal_state
    | LKappa.I_VAL_ERASED i ->
      Format.fprintf
        f "{%a%t}" (Signature.print_internal_state sigs ag_ty site) i
        (fun f -> if show_erased then Format.pp_print_string f "--")

  let print_counter_test f = function
    | (c,true) -> Format.fprintf f "=%i" c
    | (c,false) -> Format.fprintf f ">=%i" c

  let print_counter_delta counters j f switch = match switch with
    | LKappa.Linked i ->
      begin
        let root = Raw_mixture.find counters i in
        let (s,(_,is_counter)) =
          Mods.DynArray.get counters.Raw_mixture.rank root in
        let delta = if (is_counter) then s-1 else (j-i) in
        Format.fprintf f "/+=%d" delta
      end
    | LKappa.Freed ->
      raise (ExceptionDefn.Internal_Error
               (Locality.dummy_annot("Cannot erase all increment agents")))
    | LKappa.Maintained -> ()
    | LKappa.Erased -> ()


  let print_rule_intf
      sigs ~show_erased ~ltypes symbol_table
      ag_ty f (ports,ints,counters,created_counters) =
    let rec aux empty i =
      if i < Array.length ports then
        if (match ports.(i) with
            | (Ast.LNK_ANY, _), LKappa.Maintained ->  ints.(i) <> LKappa.I_ANY
            | ((Ast.LNK_ANY, _), (LKappa.Erased | LKappa.Freed | LKappa.Linked _) |
               ((Ast.LNK_SOME | Ast.ANY_FREE | Ast.LNK_FREE |
                 Ast.LNK_TYPE _ | Ast.LNK_VALUE _),_), _) -> true) then

          let ((e,_),switch) = ports.(i) in
          let is_counter = match e with
            | Ast.ANY_FREE | Ast.LNK_FREE | Ast.LNK_ANY
            | Ast.LNK_TYPE _ | Ast.LNK_SOME -> false
            | Ast.LNK_VALUE (j,_) ->
              try
                let root = Raw_mixture.find counters j in
                let (c,(eq,is_counter')) =
                  Mods.DynArray.get counters.Raw_mixture.rank root in
                if (is_counter')&&not(!Parameter.debugModeOn) then
                  let () = Format.fprintf f "%t%a{%a%a}"
                      (if empty then Pp.empty else Pp.space)
                      (Signature.print_site sigs ag_ty) i
                      print_counter_test (c-1,eq)
                      (print_counter_delta created_counters j) switch
                  in true else false
              with Invalid_argument _ -> false in
          let () = if not(is_counter) then
              Format.fprintf
                f "%t%a%a%a" (if empty then Pp.empty else Pp.space)
                (Signature.print_site sigs ag_ty) i
                (print_rule_internal sigs symbol_table ~show_erased ag_ty i) ints.(i)
                (print_rule_link sigs symbol_table ~show_erased ~ltypes)
                ports.(i) else () in
          aux false (succ i)
        else aux empty (succ i) in
    aux true 0


  let print_rule_agent
      sigs
      ~ltypes
      ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
      counters created_counters f ag =
    Format.fprintf f "%a(@[<h>%a@])%t"
      (Signature.print_agent sigs) ag.LKappa.ra_type
      (print_rule_intf sigs symbol_table ~show_erased:false ~ltypes  ag.LKappa.ra_type)
      (ag.LKappa.ra_ports,ag.LKappa.ra_ints,counters,created_counters)
      (fun f -> if ag.LKappa.ra_erased then Format.pp_print_string f "-")


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
           | Some (before,after) ->
             let ((a,_),_) = ag.LKappa.ra_ports.(after) in
             let ((b,_),_) = ag.LKappa.ra_ports.(before) in
             match b with
             | Ast.ANY_FREE | Ast.LNK_FREE | Ast.LNK_ANY
             | Ast.LNK_TYPE _ | Ast.LNK_SOME -> ()
             | Ast.LNK_VALUE (lnk_b,_) ->
               match a with
               | Ast.LNK_VALUE (lnk_a,_) -> Raw_mixture.union t lnk_b lnk_a
               | Ast.ANY_FREE | Ast.LNK_FREE ->
                 let root = Raw_mixture.find t lnk_b in
                 let (s,_) = Mods.DynArray.get t.Raw_mixture.rank root in
                 Mods.DynArray.set t.Raw_mixture.rank root (s,(true,true))
               | Ast.LNK_ANY ->
                 let root = Raw_mixture.find t lnk_b in
                 let (s,_) = Mods.DynArray.get t.Raw_mixture.rank root in
                 Mods.DynArray.set t.Raw_mixture.rank root (s,(false,true))
               | Ast.LNK_TYPE _ | Ast.LNK_SOME ->
                 raise (ExceptionDefn.Internal_Error
                          (Locality.dummy_annot
                             ("Port a of __incr agent not well specified"))))
        mix in
  t

let print_rule_mixture
    sigs ~ltypes ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
    created f mix =
    let incr_agents = union_find_counters (Some sigs) mix in
    let created_incr = Raw_mixture.union_find_counters (Some sigs) created in
    let rec aux_print some = function
      | [] -> ()
      | h::t ->
        if Signature.is_counter_agent (Some sigs) h.LKappa.ra_type
        && not !Parameter.debugModeOn
        then aux_print some t
        else
          let () = if some then Pp.comma f in
          let () = print_rule_agent sigs ~ltypes ~symbol_table
              incr_agents created_incr f h in
          aux_print true t in
    aux_print false mix
end

module Raw_mixture =
struct


  let print_link symbol_table incr_agents f = function
    | Raw_mixture.FREE ->
      Pattern.print_free_site symbol_table f
    | Raw_mixture.VAL i ->
      try
        let root = Raw_mixture.find incr_agents i in
        let (counter,(_,is_counter)) = Mods.DynArray.get incr_agents.Raw_mixture.rank root
        in
        if (is_counter)&&not(!Parameter.debugModeOn) then
          Format.fprintf f "{=%d}" counter
        else
          Format.fprintf f "%s%s%i%s"
            symbol_table.Symbol_table.open_binding_state
            symbol_table.Symbol_table.bound
            i
            symbol_table.Symbol_table.close_binding_state
      with Invalid_argument _ ->
        Format.fprintf f "%s%s%i%s"
          symbol_table.Symbol_table.open_binding_state
          symbol_table.Symbol_table.bound
          i
          symbol_table.Symbol_table.close_binding_state

  let aux_pp_si sigs symbol_table a s f i =
    match sigs with
    | Some sigs ->
      Format.fprintf f
        "%a%a"
        (Signature.print_site sigs a) s
        (fun fmt id_opt ->
           match id_opt with
           | None -> ()
           | Some i ->
             Signature.print_internal_state sigs a s fmt i)
        i
    | None ->
      match i with
      | Some i -> Format.fprintf f
                    "%i%s%s%i%s"
                    s
                    symbol_table.Symbol_table.open_internal_state
                    symbol_table.Symbol_table.internal_state_symbol
                    i
                    symbol_table.Symbol_table.close_internal_state
      | None -> Format.pp_print_int f s

  let print_intf with_link ?sigs
      ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
      incr_agents (ag_ty:int) f (ports,ints) =
    let rec aux empty i =
      if i < Array.length ports then
        let () = Format.fprintf
            f "%t%a%a"
            (if empty then Pp.empty else Pp.space)
            (aux_pp_si sigs symbol_table ag_ty i) ints.(i)
            (if with_link
             then print_link symbol_table incr_agents
             else (fun _ _ -> ()))
            ports.(i) in
        aux false (succ i) in
    aux true 0

  let aux_pp_ag sigs f a =
    match sigs with
    | Some sigs -> Signature.print_agent sigs f a
    | None -> Format.pp_print_int f a

  let print_agent created link
      ?sigs ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
      incr_agents f ag =
    Format.fprintf f "%a%s@[<h>%a@]%s%t"
      (aux_pp_ag sigs) ag.Raw_mixture.a_type
      symbol_table.Symbol_table.agent_open
      (print_intf
         link
         ?sigs
         ~symbol_table
         incr_agents
         ag.Raw_mixture.a_type)
      (ag.Raw_mixture.a_ports, ag.Raw_mixture.a_ints)
      symbol_table.Symbol_table.agent_close
      (fun f -> if created then Format.pp_print_string f "+")

  let print ~created ?sigs
      ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
      f mix =
    let incr_agents = Raw_mixture.union_find_counters sigs mix in
    let rec aux_print some = function
      | [] -> ()
      | h::t ->
        if Signature.is_counter_agent sigs h.Raw_mixture.a_type && not
             !Parameter.debugModeOn
        then aux_print some t
        else
          let () =
            if some then
              Format.pp_print_string f symbol_table.Symbol_table.agent_sep_comma
          in
          let () = print_agent created true ?sigs ~symbol_table incr_agents f h
        in
        aux_print true t in
  aux_print false mix
end

module Kappa_printer =
struct

  let cc_mix ?env ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4) =
    let domain = match env with
      | None -> None
      | Some e -> Some (Model.domain e) in
    Pp.list
      (fun f -> Format.fprintf f " +@ ")
      (fun f ccs ->
         Pp.array
           (fun f -> Format.fprintf f "*")
           (fun _ f cc ->
              Format.fprintf
                f "|%a|" (Pattern.print ?domain ~with_id:false ~symbol_table)
                cc)
           f ccs)

  let alg_expr ?env ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4) =
    Alg_expr.print
      (cc_mix ?env ~symbol_table)
      (Model.print_token ?env)
      (Model.print_alg ?env)

  let bool_expr ?env ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4) =
    Alg_expr.print_bool
      (cc_mix ?env ~symbol_table)
      (fun f i -> Format.fprintf f "|%a|" (Model.print_token ?env) i)
      (Model.print_alg ?env)

  let print_expr ?env ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4) f =
    let aux f = function
      | Primitives.Str_pexpr (str,_) -> Format.fprintf f "\"%s\"" str
      | Primitives.Alg_pexpr (alg,_) -> alg_expr ?env ~symbol_table f alg
    in function
      | [] -> ()
      | [ Primitives.Str_pexpr (str,_) ] -> Format.fprintf f "\"%s\"" str
      | ([ Primitives.Alg_pexpr  _ ] | _::_::_) as e ->
        Format.fprintf f "(%a)" (Pp.list (fun f -> Format.fprintf f ".") aux) e

  let print_expr_val alg_val f e =
    let aux f = function
      | Primitives.Str_pexpr (str,_) -> Format.pp_print_string f str
      | Primitives.Alg_pexpr (alg,_) ->
        Nbr.print f (alg_val alg)
    in Pp.list (fun f -> Format.pp_print_cut f ()) aux f e

  let decompiled_rule
      ~full ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4) env f r =
    let sigs = Model.signatures env in
    let (r_mix,r_created) =
      Snip.lkappa_of_elementary_rule sigs (Model.domain env) r in
    let pr_alg f (a,_) = alg_expr ~env ~symbol_table f a in
    let pr_tok f (va,tok) =
      Format.fprintf f "%a %a" pr_alg va (Model.print_token ~env) tok in
    Format.fprintf f "%a%t%a%t%a%t"
      (LKappa.print_rule_mixture sigs ~ltypes:false r_created) r_mix
      (if r_mix <> [] && r_created <> [] then Pp.comma else Pp.empty)
      (Raw_mixture.print ~created:true ~sigs ~symbol_table) r_created

      (if r.Primitives.delta_tokens <> []
       then (fun f -> Format.fprintf f "|@ ") else Pp.empty)
      (Pp.list Pp.comma pr_tok) r.Primitives.delta_tokens
      (fun f -> if full then
          Format.fprintf f " @@@ %a%t"
            pr_alg r.Primitives.rate
            (fun f ->
               match r.Primitives.unary_rate with
               | None -> ()
               | Some (rate, dist) ->
                 Format.fprintf
                   f " {%a%a}" pr_alg rate
                   (Pp.option (fun f md ->
                        Format.fprintf f ":%a" (alg_expr ~env ~symbol_table) md))
                   dist))

  let elementary_rule
      ?env ?symbol_table:(symbol_table=Symbol_table.symbol_table_V4)
      f r =
    let domain,sigs = match env with
      | None -> None,None
      | Some e -> Some (Model.domain e), Some (Model.signatures e) in
    let pr_alg f (a,_) = alg_expr ?env ~symbol_table f a in
    let pr_tok f (va,tok) =
      Format.fprintf f "%a %a" pr_alg va (Model.print_token ?env) tok in
    let pr_trans f t = Primitives.Transformation.print ?sigs f t in
    let boxed_cc i f cc =
      let () = Format.pp_open_box f 2 in
      let () = Format.pp_print_int f i in
      let () = Format.pp_print_string f ": " in
      let () = Pattern.print ?domain ~with_id:true ~symbol_table f cc in
      Format.pp_close_box f ()
    in
    Format.fprintf
      f "(ast: %i)@ @[@[%a@]%t@[%a@]@]@ -- @[%a@]@ ++ @[%a@]@ @@%a%t"
      r.Primitives.syntactic_rule

      (Pp.array Pp.comma boxed_cc) r.Primitives.connected_components
      (if r.Primitives.delta_tokens <> []
       then (fun f -> Format.fprintf f "|@ ") else Pp.empty)
      (Pp.list Pp.comma pr_tok)
      r.Primitives.delta_tokens

      (Pp.list Pp.comma pr_trans) r.Primitives.removed
      (Pp.list Pp.comma pr_trans) r.Primitives.inserted

      pr_alg r.Primitives.rate
      (fun f ->
         match r.Primitives.unary_rate with
         | None -> ()
         | Some (rate, dist) ->
           Format.fprintf
             f " {%a%a}" pr_alg rate
             (Pp.option (fun f md ->
                  Format.fprintf f ":%a" (alg_expr ?env ~symbol_table) md))
             dist)


end
