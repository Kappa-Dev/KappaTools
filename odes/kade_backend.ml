module Pattern =
struct

  let print_free_site symbol_table fmt =
    Format.fprintf fmt "%s%s%s"
      symbol_table.Symbol_table.open_binding_state
      symbol_table.Symbol_table.free
      symbol_table.Symbol_table.close_binding_state
  let print_internal ?sigs (i,agent) site f id =
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
                    let () =
                    Format.fprintf f "%s%s%s"
                      symbol_table.Symbol_table.open_binding_state
                      symbol_table.Symbol_table.free
                      symbol_table.Symbol_table.close_binding_state  in
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
          | Some cc_id -> Format.fprintf f "/*cc%i*/@ " cc_id in
        let (_, _) =
          Pattern.fold
            (fun x el (not_empty,link_ids) ->
               let ag_x = (x,Pattern.find_ty cc x) in
               if (not (Signature.is_counter_agent sigs (snd ag_x)))
               || (!Parameter.debugModeOn) then
                 let () =
                   Format.fprintf
                     f "%t@[<h>%a("
                     (if not_empty
                      then
                        begin
                          if dotnet then
                            (fun fmt -> Format.fprintf fmt ".")
                          else Pp.comma
                        end
                      else Pp.empty)
                     (Agent.print ?sigs ~with_id) ag_x in
                 let out = print_intf ag_x link_ids el in
                 let () = Format.fprintf f ")@]" in
                 true, out
               else not_empty,link_ids)
            cc (false, (1, Mods.Int2Map.empty))
        in
        ()

end
