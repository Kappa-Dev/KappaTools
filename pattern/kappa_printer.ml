let lnk_t env f = function
  | Mixture.WLD -> Format.fprintf f "?"
  | Mixture.BND -> Format.fprintf f "!"
  | Mixture.FREE -> Format.fprintf f ""
  | Mixture.TYPE (site_id,sig_id) ->
     Format.fprintf f "!%a.%a" (Environment.print_site env sig_id) site_id
		    (Environment.print_agent env) sig_id

let follower_string (bnd,fresh) mix uid = function
  | Mixture.BND ->
     let opt = Mixture.follow uid mix in
     begin
       match opt with
       | Some (agent_id',site_id') ->
	  begin
	    let lnk =
	      try Hashtbl.find bnd uid with
	      | Not_found ->
		 (Hashtbl.replace bnd (agent_id',site_id') !fresh ;
		  let i = !fresh in
		  fresh := !fresh+1 ;
		  i)
	    in
	    Hashtbl.replace bnd uid lnk ;
	    (string_of_int lnk)
	  end
       | None ->
	  try string_of_int (Hashtbl.find bnd uid)
	  with Not_found -> "_"
     end
  | _ -> ""

let intf_item env (bnd,fresh) mix sig_id agent_id
		    f (site_id,(opt_v,opt_l)) =
  let s_s f = Environment.print_site_state env sig_id site_id f opt_v in
  let s_lnk f =
    Format.fprintf f "%a%s" (lnk_t env) opt_l
       (follower_string (bnd,fresh) mix (agent_id,site_id) opt_l)
  in
  Format.fprintf f "%t%t" s_s s_lnk

let intf env mix sig_id agent_id (bnd,fresh) f interface =
  Pp.set Mods.IntMap.bindings (fun f -> Format.fprintf f ",")
	 (intf_item env (bnd,fresh) mix sig_id agent_id)
	 f (Mods.IntMap.remove 0 interface)
(* Beware: removes "_" the hackish way *)

let agent with_number env mix (bnd,fresh) f (id,ag) =
  let sig_id = Mixture.name ag in
  let name =
    if with_number then "#"^(string_of_int id) else "" in
  Format.fprintf f "%a%s(%a)" (Environment.print_agent env) sig_id name
		 (intf env mix sig_id id (bnd,fresh))
		 (Mixture.interface ag)

let mixture with_number env f mix =
  let bnd = Hashtbl.create 7 in
  let fresh = ref 0 in
  Pp.set Mods.IntMap.bindings (fun f -> Format.fprintf f ",")
	 (agent with_number env mix (bnd,fresh))
	 f (Mixture.agents mix)

let alg_expr env f alg =
  let rec aux f = function
    | Expr.BIN_ALG_OP (op, (a,_), (b,_)) ->
       Format.fprintf f "(%a %a %a)" aux a Term.print_bin_alg_op op aux b
    | Expr.UN_ALG_OP (op, (a,_)) ->
       Format.fprintf f "(%a %a)" Term.print_un_alg_op op aux a
    | Expr.STATE_ALG_OP op -> Term.print_state_alg_op f op
    | Expr.CONST n -> Nbr.print f n
    | Expr.ALG_VAR i ->
       Format.fprintf f "'%a'" (Environment.print_alg env) i
    | Expr.KAPPA_INSTANCE i ->
       Format.fprintf f "|#secret#|"
		     (* (mixture false env) (Environment.kappa_of_num i env) *)
    | Expr.TOKEN_ID i ->
       Format.fprintf f "|%a|" (Environment.print_token env) i
  in aux f alg

let print_expr env f e =
  let aux f = function
    | Ast.Str_pexpr str,_ -> Format.fprintf f "\"%s\"" str
    | Ast.Alg_pexpr alg,_ -> alg_expr env f alg
  in Pp.list (fun f -> Format.fprintf f ".") aux f e

let print_expr_val env alg_val f e =
  let aux f = function
    | Ast.Str_pexpr str,_ -> Format.pp_print_string f str
    | Ast.Alg_pexpr alg,_ ->
       Nbr.print f (alg_val env alg)
  in Pp.list (fun f -> Format.pp_print_cut f ()) aux f e

let modification env f = function
  | Primitives.PRINT (nme,va) ->
     Format.fprintf f "$PRINTF %a <%a>" (print_expr env) nme (print_expr env) va
  | Primitives.PLOTENTRY -> Format.pp_print_string f "$PLOTENTRY"
  | Primitives.ITER_RULE ((n,_),rule) ->
     if Mixture.is_empty rule.Primitives.lhs then
       Format.fprintf f "$ADD %a %a" (alg_expr env) n
		      (mixture false env) rule.Primitives.rhs
     else
       let () = assert (Mixture.is_empty rule.Primitives.rhs) in
       Format.fprintf f "$DEL %a %a" (alg_expr env) n
		      (mixture false env) rule.Primitives.lhs
  | Primitives.UPDATE (d_id,(va,_)) ->
     begin
       match d_id with
       | Term.TOK id ->
	  Format.fprintf f "%s <- %a"
			 (NamedDecls.elt_name env.Environment.tokens id)
       | Term.ALG id ->
	  Format.fprintf f "$UPDATE '%s' %a"
			 (NamedDecls.elt_name env.Environment.algs id)
       | Term.RULE id ->
	  Format.fprintf f "$UPDATE '%s' %a" (Environment.rule_of_num id env)
       | _ -> Format.fprintf f "$UPDATE '%a' %a" Term.print_dep_type d_id
     end (alg_expr env) va
  | Primitives.SNAPSHOT fn ->
     Format.fprintf f "SNAPSHOT %a" (print_expr env) fn
  | Primitives.STOP fn ->
     Format.fprintf f "STOP %a" (print_expr env) fn
  | Primitives.FLUX fn ->
     Format.fprintf f "$FLUX %a [true]" (print_expr env) fn
  | Primitives.FLUXOFF fn ->
     Format.fprintf f "$FLUX %a [false]" (print_expr env) fn
  | Primitives.CFLOW id ->
     let nme = try Environment.rule_of_num id env
	       with Not_found -> Environment.kappa_of_num id env
     in Format.fprintf f "$TRACK '%s' [true]" nme
  | Primitives.CFLOWOFF id ->
     let nme = try Environment.rule_of_num id env
	       with Not_found -> Environment.kappa_of_num id env
     in Format.fprintf f "$TRACK '%s' [false]" nme

let perturbation env f pert =
  let aux f =
    Format.fprintf f "%a do %a"
		   (Expr.print_bool (alg_expr env)) pert.Primitives.precondition
		   (Pp.list Pp.colon (modification env)) pert.Primitives.effect
  in
  match pert.Primitives.abort with
  | None -> Format.fprintf f "%%mod: %t@." aux
  | Some ab ->
     Format.fprintf f "%%mod: repeat %t until %a@." aux
		    (Expr.print_bool (alg_expr env)) ab
