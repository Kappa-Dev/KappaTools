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
              f "|%a|" (Pattern.print ?domain ~with_id:false) cc) f ccs)

let alg_expr ?env =
  Alg_expr.print (cc_mix ?env) (Model.print_token ?env) (Model.print_alg ?env)

let bool_expr ?env =
  Alg_expr.print_bool
    (cc_mix ?env)
    (fun f i -> Format.fprintf f "|%a|" (Model.print_token ?env) i)
    (Model.print_alg ?env)

let print_expr ?env f =
  let aux f = function
    | Primitives.Str_pexpr (str,_) -> Format.fprintf f "\"%s\"" str
    | Primitives.Alg_pexpr (alg,_) -> alg_expr ?env f alg
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

let decompiled_rule ~full env f r =
  let sigs = Model.signatures env in
  let (r_mix,r_created) =
    Snip.lkappa_of_elementary_rule sigs (Model.domain env) r in
  let pr_alg f (a,_) = alg_expr ~env f a in
  let pr_tok f (va,tok) =
    Format.fprintf f "%a %a" pr_alg va (Model.print_token ~env) tok in
  Format.fprintf f "%a%t%a%t%a%t"
    (LKappa.print_rule_mixture sigs ~ltypes:false r_created) r_mix
    (if r_mix <> [] && r_created <> [] then Pp.comma else Pp.empty)
    (Raw_mixture.print ~created:true ~sigs) r_created

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
                      Format.fprintf f ":%a" (alg_expr ~env) md))
                 dist))

let elementary_rule ?env f r =
  let domain,sigs = match env with
    | None -> None,None
    | Some e -> Some (Model.domain e), Some (Model.signatures e) in
  let pr_alg f (a,_) = alg_expr ?env f a in
  let pr_tok f (va,tok) =
    Format.fprintf f "%a %a" pr_alg va (Model.print_token ?env) tok in
  let pr_trans f t = Primitives.Transformation.print ?sigs f t in
  let boxed_cc i f cc =
    let () = Format.pp_open_box f 2 in
    let () = Format.pp_print_int f i in
    let () = Format.pp_print_string f ": " in
    let () = Pattern.print ?domain ~with_id:true f cc in
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
                Format.fprintf f ":%a" (alg_expr ?env) md))
           dist)

let modification ?env f m =
  let domain = match env with
    | None -> None
    | Some e -> Some (Model.domain e) in
  match m with
  | Primitives.PRINT (nme,va) ->
    if nme <> [] then
      Format.fprintf f "$PRINTF %a > %a"
        (print_expr ?env) va (print_expr ?env) nme
    else
      Format.fprintf f "$PRINTF %a" (print_expr ?env) va
  | Primitives.PLOTENTRY -> Format.pp_print_string f "$PLOTENTRY"
  | Primitives.ITER_RULE ((n,_),rule) ->
    Format.fprintf f "$APPLY %a %a" (alg_expr ?env) n
      (match env with
       | None -> elementary_rule ?env
       | Some env -> decompiled_rule ~full:false env)
      rule
  | Primitives.UPDATE (id,(va,_)) ->
    Format.fprintf f "$UPDATE %a %a"
      (Model.print_alg ?env) id (alg_expr ?env) va
  | Primitives.SNAPSHOT fn ->
    Format.fprintf f "$SNAPSHOT %a" (print_expr ?env) fn
  | Primitives.STOP fn ->
    Format.fprintf f "$STOP %a" (print_expr ?env) fn
  | Primitives.FLUX (kind,fn) ->
    Format.fprintf
      f "$DIN %a %t[true]" (print_expr ?env) fn
      (fun f -> match kind with
         | Primitives.ABSOLUTE -> Format.fprintf f "\"absolute\" "
         | Primitives.RELATIVE -> ()
         | Primitives.PROBABILITY -> Format.fprintf f "\"probability\" ")
  | Primitives.FLUXOFF fn ->
    Format.fprintf f "$DIN %a [false]" (print_expr ?env) fn
  | Primitives.CFLOW (_name,cc,_) ->
    Format.fprintf
      f "$TRACK @[%a@] [true]"
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ?domain ~with_id:false)) cc
  | Primitives.CFLOWOFF (_,cc) ->
    Format.fprintf
      f "$TRACK %a [false]"
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ?domain ~with_id:false)) cc
  | Primitives.SPECIES (fn,cc,_) ->
    Format.fprintf
      f "$SPECIES_OF @[%a@] [true] > %a"
      (Pp.array
         Pp.comma
         (fun _ -> Pattern.print ?domain ~with_id:false)) cc
      (print_expr ?env) fn
  | Primitives.SPECIES_OFF fn ->
    Format.fprintf
      f "$SPECIES_OF [false] > %a"
      (print_expr ?env) fn

let perturbation ?env f pert =
  let aux_alarm f =
    match pert.Primitives.alarm with
    | None -> ()
    | Some n -> Format.fprintf f "alarm %a " Nbr.print n
  in
  Format.fprintf f "%%mod: %t%a do %arepeat %a"
    aux_alarm
    (bool_expr ?env) (fst pert.Primitives.precondition)
    (Pp.list ~trailing:Pp.colon Pp.colon (modification ?env)) pert.Primitives.effect
    (bool_expr ?env) (fst pert.Primitives.repeat)

let env f env =
  Model.print (fun env -> alg_expr ~env) (fun env -> elementary_rule ~env)
    (fun env -> perturbation ~env) f env

let env_kappa contact_map f env =
  Format.fprintf f "@[<v>%a@,%a@]"
    (Contact_map.print_kappa (Model.signatures env)) contact_map
    (Model.print_kappa
       (fun env -> alg_expr ~env) (fun env -> perturbation ~env)) env
