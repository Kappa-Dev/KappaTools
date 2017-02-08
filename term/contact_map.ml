type t = ((int list) * (int*int) list) array array

let print_kappa sigs f c =
  Format.fprintf f "@[<v>%a@]"
    (Pp.array Pp.space
       (fun ag f intf -> Format.fprintf f "@[<h>%%agent: %a(%a)@]"
           (Signature.print_agent sigs) ag
           (Pp.array Pp.comma
              (fun s f (is,ls) -> Format.fprintf f "%a%a%a"
                  (Signature.print_site sigs ag) s
                  (Pp.list Pp.empty
                     (fun f i -> Format.fprintf f "~%a"
                         (Signature.print_internal_state sigs ag s) i)) is
                  (Pp.list Pp.empty (fun f (ad,sd) -> Format.fprintf f "!%a.%a"
                                        (Signature.print_site sigs ad) sd
                                        (Signature.print_agent sigs) ad)) ls))
           intf))
    c

let cut_at i s' l =
  let rec aux_cut_at o = function
    | [] -> None
    | ((j,s),_ as h) :: t -> if i = j then
        if s = s' then None else Some (h::o)
      else aux_cut_at (h::o) t
  in aux_cut_at [] l

let get_cycles contact_map =
  let rec dfs (known,out as acc) path i last_s =
    if Mods.IntSet.mem i known
    then match cut_at i last_s path with
      | None -> acc
      | Some x -> known, x :: out
    else
      let known' = Mods.IntSet.add i known in
      Tools.array_fold_lefti
        (fun s acc (_,l)->
           if s = last_s then acc else
             List.fold_left
               (fun acc (ty,s' as x) -> dfs acc (((i,s),x)::path) ty s')
               acc l)
        (known',out) contact_map.(i) in
  let rec scan (known,out as acc) i =
    if i < 0 then out else
      scan
        (if Mods.IntSet.mem i known then acc else dfs acc [] i (-1))
        (pred i) in
  scan (Mods.IntSet.empty,[]) (Array.length contact_map - 1)

let print_cycles sigs form contact_map =
  let o = get_cycles contact_map in
  Pp.list Pp.space
    (Pp.list Pp.empty
       (fun f ((ag,s),(ag',s')) ->
          Format.fprintf f "%a.%a-%a."
            (Signature.print_agent sigs) ag
            (Signature.print_site sigs ag) s
            (Signature.print_site sigs ag') s'
       )
    ) form o
