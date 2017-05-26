(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let cc_mix ?env =
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
              f "|%a|"
              (Pattern.print ~new_syntax:true ?domain ~with_id:false) cc) f ccs)

let alg_expr ?env =
  Alg_expr.print (cc_mix ?env) (Model.print_token ?env) (Model.print_alg ?env)

let bool_expr ?env =
  Alg_expr.print_bool
    (cc_mix ?env)
    (fun f i -> Format.fprintf f "|%a|" (Model.print_token ?env) i)
    (Model.print_alg ?env)

let print_expr ?env f e =
  let aux f = function
    | Primitives.Str_pexpr (str,_) -> Format.fprintf f "\"%s\"" str
    | Primitives.Alg_pexpr (alg,_) -> alg_expr ?env f alg
  in Pp.list (fun f -> Format.fprintf f ".") aux f e

let print_expr_val alg_val f e =
  let aux f = function
    | Primitives.Str_pexpr (str,_) -> Format.pp_print_string f str
    | Primitives.Alg_pexpr (alg,_) ->
      Nbr.print f (alg_val alg)
  in Pp.list (fun f -> Format.pp_print_cut f ()) aux f e

let elementary_rule ?env f r =
  let domain,sigs = match env with
    | None -> None,None
    | Some e -> Some (Model.domain e), Some (Model.signatures e) in
  let pr_alg f (a,_) = alg_expr ?env f a in
  let pr_tok f (va,tok) =
    Format.fprintf f "%a <- %a" (Model.print_token ?env) tok pr_alg va in
  let pr_trans f t = Primitives.Transformation.print ?sigs f t in
  let pr_mixte f (a,s,i) =
    Format.fprintf f "@[%a.%a!%i@]"
      (Matching.Agent.print ?sigs) a (Matching.Agent.print_site ?sigs a) s i in
  let boxed_cc i f cc =
    let () = Format.pp_open_box f 2 in
    let () = Format.pp_print_int f i in
    let () = Format.pp_print_string f ": " in
    let () = Pattern.print ~new_syntax:true ?domain ~with_id:true f cc in
    Format.pp_close_box f ()
  in
  let ins_fresh,ins_mixte,ins_existing =
    match sigs with
    | None -> [],[],r.Primitives.inserted
    | Some sigs ->
      Primitives.Transformation.raw_mixture_of_fresh
        sigs r.Primitives.inserted
  in
  Format.fprintf
    f "(ast: %i)@ @[@[%a%t%a@]%t@[%a@]@]@ -- @[%a@]@ ++ @[%a%a@]@ @@%a%t"
    r.Primitives.syntactic_rule
    (Pp.array Pp.comma boxed_cc) r.Primitives.connected_components
    (if r.Primitives.connected_components <> [||] && ins_fresh <> []
     then Pp.comma else Pp.empty)
    (Raw_mixture.print ~new_syntax:false ~compact:true ~created:true ?sigs)
    (List.map snd ins_fresh)
    (if r.Primitives.delta_tokens <> []
     then (fun f -> Format.fprintf f "|@ ") else Pp.empty)
    (Pp.list (fun f -> Format.fprintf f "@ + ") pr_tok)
    r.Primitives.delta_tokens

    (Pp.list Pp.comma pr_trans) r.Primitives.removed

    (Pp.list ~trailing:Pp.space Pp.comma pr_mixte) ins_mixte
    (Pp.list Pp.comma pr_trans) ins_existing

    pr_alg r.Primitives.rate
    (fun f ->
       match r.Primitives.unary_rate with
       | None -> ()
       | Some (rate, dist) ->
         Format.fprintf
           f " {%a%a}" pr_alg rate
           (Pp.option (fun f md ->
                Format.fprintf f ":%a" (alg_expr ?env) md))
           dist)

let modification ?env f m =
  let domain = match env with
    | None -> None
    | Some e -> Some (Model.domain e) in
  match m with
  | Primitives.PRINT (nme,va) ->
    Format.fprintf f "$PRINTF %a <%a>"
      (print_expr ?env) nme (print_expr ?env) va
  | Primitives.PLOTENTRY -> Format.pp_print_string f "$PLOTENTRY"
  | Primitives.ITER_RULE ((n,_),rule) ->
    if rule.Primitives.inserted = [] then
      if rule.Primitives.connected_components = [||] then
        match rule.Primitives.delta_tokens with
        | [ va, id ] ->
          Format.fprintf f "%a <- %a + |%a|"
            (Model.print_token ?env) id
            (fun f (a,_) -> alg_expr ?env f a) va
            (Model.print_token ?env) id
        | _ -> assert false
      else
        let boxed_cc _ =
          Pattern.print ~new_syntax:true ?domain ~with_id:false in
        Format.fprintf f "$DEL %a %a" (alg_expr ?env) n
          (Pp.array Pp.comma boxed_cc)
          rule.Primitives.connected_components
    else
      begin
        match env with
        | None ->
          Format.fprintf f "$APPLY %a %a" (alg_expr ?env) n
            (elementary_rule ?env) rule
        | Some env ->
          let sigs = Model.signatures env in
          let ins_fresh,_,ins_existing =
            Primitives.Transformation.raw_mixture_of_fresh
              sigs rule.Primitives.inserted in
          if ins_existing = [] then
            Format.fprintf f "$ADD %a %a" (alg_expr ~env) n
              (Raw_mixture.print
                 ~new_syntax:false ~compact:false ~created:false ~sigs)
              (List.map snd ins_fresh)
          else
            Format.fprintf f "$APPLY %a %a" (alg_expr ~env) n
              (elementary_rule ~env) rule
      end
  | Primitives.UPDATE (id,(va,_)) ->
    Format.fprintf f "$UPDATE %a %a"
      (Model.print_alg ?env) id (alg_expr ?env) va
  | Primitives.SNAPSHOT fn ->
    Format.fprintf f "$SNAPSHOT %a" (print_expr ?env) fn
  | Primitives.STOP fn ->
    Format.fprintf f "$STOP %a" (print_expr ?env) fn
  | Primitives.FLUX (kind,fn) ->
    Format.fprintf
      f "$FLUX %a %t[true]" (print_expr ?env) fn
      (fun f -> match kind with
         | Primitives.ABSOLUTE -> Format.fprintf f "\"absolute\" "
         | Primitives.RELATIVE -> ()
         | Primitives.PROBABILITY -> Format.fprintf f "\"probability\" ")
  | Primitives.FLUXOFF fn ->
    Format.fprintf f "$FLUX %a [false]" (print_expr ?env) fn
  | Primitives.CFLOW (_name,cc,_) ->
    Format.fprintf
      f "$TRACK @[%a@] [true]"
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ~new_syntax:true ?domain ~with_id:false)) cc
  | Primitives.CFLOWOFF cc ->
    Format.fprintf
      f "$TRACK %a [false]"
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ~new_syntax:true ?domain ~with_id:false)) cc
  | Primitives.SPECIES (fn,cc,_) ->
    Format.fprintf
      f "$SPECIES %a @[%a@] [true]"
      (print_expr ?env) fn
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ~new_syntax:true ?domain ~with_id:false)) cc
  | Primitives.SPECIES_OFF cc ->
    Format.fprintf
      f "$SPECIES_OFF %a [false]"
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ~new_syntax:true ?domain ~with_id:false)) cc

let perturbation ?env f pert =
  let aux f =
    Format.fprintf
      f "%a do %a"
      (bool_expr ?env) (fst pert.Primitives.precondition)
      (Pp.list Pp.colon (modification ?env)) pert.Primitives.effect
  in
  match pert.Primitives.abort with
  | None -> Format.fprintf f "%%mod: %t" aux
  | Some (ab,_) ->
    Format.fprintf f "%%mod: repeat %t until %a" aux (bool_expr ?env) ab

let env f env =
  Model.print (fun env -> alg_expr ~env) (fun env -> elementary_rule ~env)
    (fun env -> perturbation ~env) f env

let env_kappa contact_map f env =
  Format.fprintf f "@[<v>%a@,%a@]"
    (Contact_map.print_kappa (Model.signatures env)) contact_map
    (Model.print_kappa
       (fun env -> alg_expr ~env) (fun env -> perturbation ~env)) env
