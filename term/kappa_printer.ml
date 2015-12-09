let alg_expr ?env f alg =
  let sigs = match env with
    | None -> None
    | Some e -> Some (Environment.signatures e) in
  let rec aux f = function
    | Alg_expr.BIN_ALG_OP (op, (a,_), (b,_)) ->
       Format.fprintf f "(%a %a %a)" aux a Operator.print_bin_alg_op op aux b
    | Alg_expr.UN_ALG_OP (op, (a,_)) ->
       Format.fprintf f "(%a %a)" Operator.print_un_alg_op op aux a
    | Alg_expr.STATE_ALG_OP op -> Operator.print_state_alg_op f op
    | Alg_expr.CONST n -> Nbr.print f n
    | Alg_expr.ALG_VAR i ->
       Environment.print_alg ?env f i
    | Alg_expr.KAPPA_INSTANCE ccs ->
       Pp.list
	 (fun f -> Format.fprintf f " +@ ")
	 (fun f ccs ->
	  Pp.array
	    (fun f -> Format.fprintf f "*")
	    (fun _ f cc ->
	     Format.fprintf
	       f "|%a|"
	       (Connected_component.print ?sigs false) cc) f ccs)
	 f ccs
    | Alg_expr.TOKEN_ID i ->
       Format.fprintf f "|%a|" (Environment.print_token ?env) i
  in aux f alg

let print_expr ?env f e =
  let aux f = function
    | Ast.Str_pexpr (str,_) -> Format.fprintf f "\"%s\"" str
    | Ast.Alg_pexpr (alg,_) -> alg_expr ?env f alg
  in Pp.list (fun f -> Format.fprintf f ".") aux f e

let print_expr_val alg_val f e =
  let aux f = function
    | Ast.Str_pexpr (str,_) -> Format.pp_print_string f str
    | Ast.Alg_pexpr (alg,_) ->
       Nbr.print f (alg_val alg)
  in Pp.list (fun f -> Format.pp_print_cut f ()) aux f e

let elementary_rule ?env f r =
  let sigs = match env with
    | None -> None
    | Some e -> Some (Environment.signatures e) in
  let pr_alg f a = alg_expr ?env f a in
  let pr_tok f (va,tok) =
    Format.fprintf
      f "%a <- %a"
      (Environment.print_token ?env) tok
      pr_alg va in
  let pr_trans f t =
    Primitives.Transformation.print ?sigs f t in
  let boxed_cc i f cc =
    let () = Format.pp_open_box f 2 in
    let () = Format.pp_print_int f i in
    let () = Format.pp_print_string f ": " in
    let () = Connected_component.print ?sigs true f cc in
    Format.pp_close_box f () in
  Format.fprintf
    f "@[%a@]@ -- @[@[%a@]%t@[%a@]@]@ ++ @[@[%a@]%t@[%a@]@]@ @@%a%t"
    (Pp.array Pp.comma boxed_cc) r.Primitives.connected_components
    (Pp.list Pp.comma pr_trans) r.Primitives.removed
    (if r.Primitives.removed <> [] && r.Primitives.consumed_tokens <> []
     then Pp.space else Pp.empty)
    (Pp.list Pp.space pr_tok) r.Primitives.consumed_tokens
    (Pp.list Pp.comma pr_trans) r.Primitives.inserted
    (if r.Primitives.inserted <> [] && r.Primitives.injected_tokens <> []
     then Pp.space else Pp.empty)
    (Pp.list Pp.space pr_tok) r.Primitives.injected_tokens
    (alg_expr ?env) r.Primitives.rate
    (fun f -> match r.Primitives.unary_rate with
	      | None -> ()
	      | Some rate -> Format.fprintf f " (%a)" (alg_expr ?env) rate)

let modification ?env f m =
  let sigs = match env with
    | None -> None
    | Some e -> Some (Environment.signatures e) in
  match m with
  | Primitives.PRINT (nme,va) ->
     Format.fprintf f "$PRINTF %a <%a>"
		    (print_expr ?env) nme (print_expr ?env) va
  | Primitives.PLOTENTRY -> Format.pp_print_string f "$PLOTENTRY"
  | Primitives.ITER_RULE ((n,_),rule) ->
     if rule.Primitives.inserted = [] then
       if rule.Primitives.connected_components = [||] then
	 match rule.Primitives.injected_tokens with
	 | [ va, id ] ->
	    Format.fprintf f "%a <- %a"
			   (Environment.print_token ?env) id
			   (alg_expr ?env) va
	 | _ -> assert false
       else
	 let boxed_cc i f cc =
	   let () = Format.pp_open_box f 2 in
	   let () = Format.pp_print_int f i in
	   let () = Format.pp_print_string f ": " in
	   let () = Connected_component.print ?sigs false f cc in
	   Format.pp_close_box f () in
	 Format.fprintf f "$DEL %a %a" (alg_expr ?env) n
			(Pp.array Pp.comma boxed_cc)
			rule.Primitives.connected_components
     else
       Format.fprintf f "$APPLY %a %a" (alg_expr ?env) n
		      (elementary_rule ?env) rule (* TODO Later *)
  | Primitives.UPDATE (id,(va,_)) ->
     Format.fprintf f "$UPDATE %a %a"
		    (Environment.print_alg ?env) id (alg_expr ?env) va
  | Primitives.SNAPSHOT fn ->
     Format.fprintf f "SNAPSHOT %a" (print_expr ?env) fn
  | Primitives.STOP fn ->
     Format.fprintf f "STOP %a" (print_expr ?env) fn
  | Primitives.FLUX fn ->
     Format.fprintf f "$FLUX %a [true]" (print_expr ?env) fn
  | Primitives.FLUXOFF fn ->
     Format.fprintf f "$FLUX %a [false]" (print_expr ?env) fn
  | Primitives.CFLOW (_name,cc,_) ->
     Format.fprintf
       f "$TRACK @[%a@] [true]"
       (Pp.array Pp.comma (fun _ -> Connected_component.print ?sigs false)) cc
  | Primitives.CFLOWOFF cc ->
     Format.fprintf
       f "$TRACK %a [false]"
       (Pp.array Pp.comma (fun _ -> Connected_component.print ?sigs false)) cc

let perturbation ?env f pert =
  let aux f =
    Format.fprintf f "%a do %a"
		   (Ast.print_bool (alg_expr ?env)) pert.Primitives.precondition
		   (Pp.list Pp.colon (modification ?env)) pert.Primitives.effect
  in
  match pert.Primitives.abort with
  | None -> Format.fprintf f "%%mod: %t" aux
  | Some ab ->
     Format.fprintf f "%%mod: repeat %t until %a" aux
		    (Ast.print_bool (alg_expr ?env)) ab

let env f env =
  Environment.print (fun env -> alg_expr ~env) (fun env -> elementary_rule ~env)
		    (fun env -> perturbation ~env) f env
