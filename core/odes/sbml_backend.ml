let print_comment logger_fmt logger_buf (s : string) : unit =
  match Ode_loggers_sig.get_encoding_format logger_fmt with
  | Loggers.DOTNET ->
    let () = Loggers.fprintf logger_buf "# %s" s in
    let () = Loggers.print_newline logger_buf in
    ()
  | Loggers.SBML ->
    let () = Loggers.fprintf logger_buf "<!-- %s -->" s in
    let () = Loggers.print_newline logger_buf in
    ()
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.GEPHI | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
    ()

let warn_with_pos __POS__ loc m logger logger_err =
  let a, b, c, d = __POS__ in
  let s = Printf.sprintf "%s%s %s %i %i %i" loc m a b c d in
  let () = Printf.printf "%s\n" s in
  let () = print_comment logger logger_err s in
  ()

let warn_without_pos loc m logger logger_err =
  let s = Printf.sprintf "%s%s" loc m in
  let () = Printf.printf "%s\n" s in
  let () = print_comment logger logger_err s in
  ()

let warn ?pos loc m logger logger_err =
  match pos with
  | None -> warn_without_pos loc m logger logger_err
  | Some pos -> warn_with_pos pos loc m logger logger_err

let warn_expr ?pos expr m logger logger_err =
  let loc = Loc.to_string (snd expr) in
  warn ?pos loc m logger logger_err

let do_sbml logger logger_err f =
  match Ode_loggers_sig.get_encoding_format logger with
  | Loggers.SBML -> f logger logger_err
  | Loggers.DOTNET | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.GEPHI | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
    ()

let do_dotnet logger logger_err f =
  match Ode_loggers_sig.get_encoding_format logger with
  | Loggers.DOTNET -> f logger logger_err
  | Loggers.SBML | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.GEPHI | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
    ()

let is_dotnet logger =
  match Ode_loggers_sig.get_encoding_format logger with
  | Loggers.DOTNET -> true
  | Loggers.SBML | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.GEPHI | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
    false

let is_sbml logger =
  match Ode_loggers_sig.get_encoding_format logger with
  | Loggers.SBML -> true
  | Loggers.DOTNET | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph
  | Loggers.HTML | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.GEPHI | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
    false

let is_dotnet_or_sbml logger =
  match Ode_loggers_sig.get_encoding_format logger with
  | Loggers.DOTNET | Loggers.SBML -> true
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.Octave | Loggers.Mathematica | Loggers.GEPHI
  | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
    false

let do_dotnet_or_sbml logger logger_err f =
  if is_dotnet_or_sbml logger then f logger logger_err

let do_not_sbml logger logger_err f =
  if not (is_sbml logger) then f logger logger_err

let do_not_dotnet logger logger_err f =
  if not (is_dotnet logger) then f logger logger_err

let do_neither_dotnet_nor_sbml logger logger_err f =
  if not (is_dotnet_or_sbml logger) then f logger logger_err

let lift0 f logger _ = f logger
let lift1 f logger _ s = f logger s

let print_sbml logger logger_err s =
  do_sbml logger logger_err
    (lift0 (fun logger -> Ode_loggers_sig.fprintf logger "%s" s))

let print_dotnet logger logger_err s =
  do_dotnet logger logger_err
    (lift0 (fun logger -> Ode_loggers_sig.fprintf logger "%s" s))

(*break sbml or dotnet*)
let break_dotnet_or_sbml logger logger_err =
  do_dotnet_or_sbml logger logger_err
    (lift0 Ode_loggers_sig.print_breakable_hint)

let line_dotnet_or_sbml logger logger_err =
  do_dotnet_or_sbml logger logger_err (lift0 Ode_loggers_sig.print_newline)

let line_dotnet logger logger_err =
  do_dotnet logger logger_err (lift0 Ode_loggers_sig.print_newline)

let line_sbml logger logger_err =
  do_sbml logger logger_err (lift0 Ode_loggers_sig.print_newline)

let extend s =
  if s = "" || String.sub s 0 1 = " " then
    s
  else
    " " ^ s

let open_box ?(options = fun () -> "") logger logger_err label =
  print_sbml logger logger_err ("<" ^ label ^ extend (options ()) ^ ">");
  break_dotnet_or_sbml logger logger_err

let open_box_dotnet ?(options_dotnet = fun () -> "") logger logger_err label =
  print_dotnet logger logger_err (label ^ extend (options_dotnet ()));
  break_dotnet_or_sbml logger logger_err

let close_box logger logger_err label =
  print_sbml logger logger_err ("</" ^ label ^ ">");
  line_sbml logger logger_err

let close_box_dotnet logger logger_err label =
  print_dotnet logger logger_err label;
  line_dotnet logger logger_err

let single_box ?(options = fun () -> "") logger logger_err label =
  let () =
    do_sbml logger logger_err (fun logger logger_err ->
        let () =
          print_sbml logger logger_err ("<" ^ label ^ extend (options ()) ^ "/>")
        in
        line_dotnet_or_sbml logger logger_err)
  in
  let () =
    do_dotnet logger logger_err (fun logger logger_err ->
        let () = print_dotnet logger logger_err (label ^ extend (options ())) in
        line_dotnet_or_sbml logger logger_err)
  in
  ()

let potential_break break logger logger_err =
  if break then
    line_dotnet_or_sbml logger logger_err
  else
    break_dotnet_or_sbml logger logger_err

let add_box ?(break = false) ?(options = fun () -> "")
    ?(options_dotnet = fun () -> "") logger logger_err label_sbml label_dotnet
    cont =
  let () =
    do_sbml logger logger_err (fun logger logger_err ->
        let () = open_box ~options logger logger_err label_sbml in
        let () = potential_break break logger logger_err in
        let () = do_sbml logger logger_err cont in
        let () = close_box logger logger_err label_sbml in
        ())
  in
  do_dotnet logger logger_err (fun logger logger_err ->
      let () = open_box_dotnet ~options_dotnet logger logger_err label_dotnet in
      let () = do_dotnet logger logger_err cont in
      let () = close_box_dotnet logger logger_err label_dotnet in
      ())

let add_box_in_sbml_only ?(break = false) ?(options = fun () -> "") logger
    logger_err label cont =
  let () =
    do_sbml logger logger_err (fun logger logger_err ->
        add_box ~break ~options logger logger_err label "" cont)
  in
  let () = do_dotnet logger logger_err cont in
  ()

let clean_variable_id =
  String.map (function
    | '(' -> '_'
    | ')' -> ' '
    | x -> x)

let string_in_comment s =
  let s =
    String.map
      (fun x ->
        match x with
        | '\'' -> ' '
        | _ -> x)
      s
  in
  let size = String.length s in
  let rec aux k s =
    if k + 1 = size then
      Bytes.to_string s
    else (
      let () =
        if Bytes.get s k = '-' && Bytes.get s (k + 1) = '-' then
          Bytes.set s k ' '
      in
      aux (k + 1) s
    )
  in
  aux 0 (Bytes.of_string s)

let string_of_variable logger string_of_var_id variable =
  match variable with
  | Ode_loggers_sig.Expr i -> string_of_var_id logger i
  | Ode_loggers_sig.Concentration i -> "s" ^ string_of_int i
  | Ode_loggers_sig.Init _ | Ode_loggers_sig.Initbis _ | Ode_loggers_sig.Deriv _
  | Ode_loggers_sig.Obs _ | Ode_loggers_sig.Jacobian _
  | Ode_loggers_sig.Jacobian_var _ | Ode_loggers_sig.Tinit
  | Ode_loggers_sig.Tend | Ode_loggers_sig.Period_t_points
  | Ode_loggers_sig.N_rules | Ode_loggers_sig.N_ode_var | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.N_max_stoc_coef | Ode_loggers_sig.InitialStep
  | Ode_loggers_sig.NonNegative | Ode_loggers_sig.MaxStep
  | Ode_loggers_sig.RelTol | Ode_loggers_sig.AbsTol | Ode_loggers_sig.Tmp ->
    Ode_loggers_sig.string_of_array_name variable
  | Ode_loggers_sig.Current_time -> "t"
  | Ode_loggers_sig.Time_scale_factor -> "t_scale_factor"
  | Ode_loggers_sig.Rate int ->
    let s = Printf.sprintf "k%i" int in
    if is_dotnet logger then
      Ode_loggers_sig.allocate_fresh_name logger s "_"
    else
      s
  | Ode_loggers_sig.Rated int ->
    let s = Printf.sprintf "kd%i" int in
    if is_dotnet logger then
      Ode_loggers_sig.allocate_fresh_name logger s "_"
    else
      s
  | Ode_loggers_sig.Rateun int ->
    let s = Printf.sprintf "kun%i" int in
    if is_dotnet logger then
      Ode_loggers_sig.allocate_fresh_name logger s "_"
    else
      s
  | Ode_loggers_sig.Rateund int ->
    let s = Printf.sprintf "kdun%i" int in
    if is_dotnet logger then
      Ode_loggers_sig.allocate_fresh_name logger s "_"
    else
      s
  | Ode_loggers_sig.Stochiometric_coef (int1, int2) ->
    Printf.sprintf "stoc%i.%i" int1 int2
  | Ode_loggers_sig.Jacobian_stochiometric_coef _
  | Ode_loggers_sig.Jacobian_rate _ | Ode_loggers_sig.Jacobian_rated _
  | Ode_loggers_sig.Jacobian_rateun _ | Ode_loggers_sig.Jacobian_rateund _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation", Loc.dummy))

let unit_of_variable variable =
  match variable with
  | Ode_loggers_sig.Period_t_points | Ode_loggers_sig.Tinit
  | Ode_loggers_sig.InitialStep | Ode_loggers_sig.MaxStep | Ode_loggers_sig.Tend
    ->
    Some "time"
  | Ode_loggers_sig.Current_time | Ode_loggers_sig.RelTol
  | Ode_loggers_sig.AbsTol | Ode_loggers_sig.Obs _ | Ode_loggers_sig.Init _
  | Ode_loggers_sig.Concentration _ | Ode_loggers_sig.Stochiometric_coef _
  | Ode_loggers_sig.Initbis _ ->
    Some "substance"
  | Ode_loggers_sig.Time_scale_factor -> Some "time_per_substance"
  | Ode_loggers_sig.NonNegative | Ode_loggers_sig.Expr _
  | Ode_loggers_sig.Deriv _ | Ode_loggers_sig.Jacobian _
  | Ode_loggers_sig.Jacobian_var _ | Ode_loggers_sig.Rate _
  | Ode_loggers_sig.Rated _ | Ode_loggers_sig.Rateun _
  | Ode_loggers_sig.Rateund _ | Ode_loggers_sig.Jacobian_stochiometric_coef _
  | Ode_loggers_sig.Jacobian_rate _ | Ode_loggers_sig.Jacobian_rated _
  | Ode_loggers_sig.Jacobian_rateun _ | Ode_loggers_sig.Jacobian_rateund _
  | Ode_loggers_sig.N_rules | Ode_loggers_sig.N_ode_var | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.N_max_stoc_coef | Ode_loggers_sig.Tmp ->
    None

let meta_id_of_logger logger =
  "CMD" ^ string_of_int (Ode_loggers_sig.get_fresh_meta_id logger)

let dotnet_id_of_logger logger =
  string_of_int (Ode_loggers_sig.get_fresh_meta_id logger)

let print_parameters string_of_var_id logger logger_buffer logger_err variable
    expr =
  let unit_string =
    match unit_of_variable variable with
    | None -> ""
    | Some x -> " units=\"" ^ x ^ "\""
  in
  let id = string_of_variable logger string_of_var_id variable in
  let () = Ode_loggers_sig.set_id_of_global_parameter logger variable id in
  let () =
    do_sbml logger logger_err (fun logger logger_err ->
        single_box logger_buffer logger_err "parameter" ~options:(fun () ->
            Format.sprintf "metaid=\"%s\" id=\"%s\" value=\"%s\"%s"
              (meta_id_of_logger logger) id (Nbr.to_string expr) unit_string))
  in
  let () =
    do_dotnet logger logger_err (fun logger logger_err ->
        single_box logger_buffer logger_err "" ~options:(fun () ->
            Format.sprintf "%s %s %s"
              (dotnet_id_of_logger logger)
              id (Nbr.to_string expr)))
  in
  ()

let of_bool_op op =
  match op with
  | Operator.AND -> ( && )
  | Operator.OR -> ( || )

let unsome expr_opt =
  match expr_opt with
  | None -> Alg_expr.const Nbr.zero
  | Some expr -> expr

let rec eval_init_alg_expr logger network_handler alg_expr =
  match fst alg_expr with
  | Alg_expr.CONST x -> x
  | Alg_expr.ALG_VAR x ->
    let id = network_handler.Network_handler.int_of_obs x in
    let expr_opt = Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id) in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.KAPPA_INSTANCE x ->
    let id = network_handler.Network_handler.int_of_kappa_instance x in
    let expr_opt = Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Init id) in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.TOKEN_ID x ->
    let id = network_handler.Network_handler.int_of_token_id x in
    let expr_opt = Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Init id) in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP Operator.TMAX_VAR ->
    let expr_opt = Ode_loggers_sig.get_expr logger Ode_loggers_sig.Tend in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP
      ( Operator.CPUTIME | Operator.TIME_VAR | Operator.EVENT_VAR
      | Operator.EMAX_VAR | Operator.NULL_EVENT_VAR ) ->
    Nbr.zero
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    Nbr.of_bin_alg_op op
      (eval_init_alg_expr logger network_handler a)
      (eval_init_alg_expr logger network_handler b)
  | Alg_expr.UN_ALG_OP (op, a) ->
    Nbr.of_un_alg_op op (eval_init_alg_expr logger network_handler a)
  | Alg_expr.IF (cond, yes, no) ->
    if eval_init_bool_expr logger network_handler cond then
      eval_init_alg_expr logger network_handler yes
    else
      eval_init_alg_expr logger network_handler no
  | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation", snd alg_expr))

and eval_init_bool_expr logger network_handler expr =
  match fst expr with
  | Alg_expr.TRUE -> true
  | Alg_expr.FALSE -> false
  | Alg_expr.COMPARE_OP (op, a, b) ->
    Nbr.of_compare_op op
      (eval_init_alg_expr logger network_handler a)
      (eval_init_alg_expr logger network_handler b)
  | Alg_expr.UN_BOOL_OP (Operator.NOT, a) ->
    not (eval_init_bool_expr logger network_handler a)
  | Alg_expr.BIN_BOOL_OP (op, a, b) ->
    of_bool_op op
      (eval_init_bool_expr logger network_handler a)
      (eval_init_bool_expr logger network_handler b)

let rec propagate_def_in_alg_expr_p p logger network_handler alg_expr =
  match alg_expr with
  | Alg_expr.CONST _, _ | Alg_expr.TOKEN_ID _, _ | Alg_expr.KAPPA_INSTANCE _, _
    ->
    alg_expr
  | Alg_expr.ALG_VAR x, loc ->
    let id = network_handler.Network_handler.int_of_obs x in
    let expr_opt = Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id) in
    let expr = unsome expr_opt in
    if p logger id x expr then
      fst (propagate_def_in_alg_expr_p p logger network_handler expr), loc
    else
      alg_expr
  | Alg_expr.STATE_ALG_OP Operator.TMAX_VAR, loc ->
    let expr_opt = Ode_loggers_sig.get_expr logger Ode_loggers_sig.Tend in
    ( fst
        (propagate_def_in_alg_expr_p p logger network_handler (unsome expr_opt)),
      loc )
  | ( Alg_expr.STATE_ALG_OP
        ( Operator.CPUTIME | Operator.TIME_VAR | Operator.EVENT_VAR
        | Operator.EMAX_VAR | Operator.NULL_EVENT_VAR ),
      loc ) ->
    Alg_expr.CONST Nbr.zero, loc
  | Alg_expr.BIN_ALG_OP (op, a, b), loc ->
    ( Alg_expr.BIN_ALG_OP
        ( op,
          propagate_def_in_alg_expr_p p logger network_handler a,
          propagate_def_in_alg_expr_p p logger network_handler b ),
      loc )
  | Alg_expr.UN_ALG_OP (op, a), loc ->
    ( Alg_expr.UN_ALG_OP
        (op, propagate_def_in_alg_expr_p p logger network_handler a),
      loc )
  | Alg_expr.IF (cond, yes, no), loc ->
    ( Alg_expr.IF
        ( propagate_def_in_bool_expr_p p logger network_handler cond,
          propagate_def_in_alg_expr_p p logger network_handler yes,
          propagate_def_in_alg_expr_p p logger network_handler no ),
      loc )
  | (Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _), pos ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation", pos))

and propagate_def_in_bool_expr_p p logger network_handler expr =
  match expr with
  | Alg_expr.TRUE, _ | Alg_expr.FALSE, _ -> expr
  | Alg_expr.COMPARE_OP (op, a, b), loc ->
    ( Alg_expr.COMPARE_OP
        ( op,
          propagate_def_in_alg_expr_p p logger network_handler a,
          propagate_def_in_alg_expr_p p logger network_handler b ),
      loc )
  | Alg_expr.BIN_BOOL_OP (op, a, b), loc ->
    ( Alg_expr.BIN_BOOL_OP
        ( op,
          propagate_def_in_bool_expr_p p logger network_handler a,
          propagate_def_in_bool_expr_p p logger network_handler b ),
      loc )
  | Alg_expr.UN_BOOL_OP (op, a), loc ->
    ( Alg_expr.UN_BOOL_OP
        (op, propagate_def_in_bool_expr_p p logger network_handler a),
      loc )

let propagate_dep_in_alg_expr a b c =
  propagate_def_in_alg_expr_p (fun _ _ _ _ -> true) a b c

let propagate_dangerous_var_names_in_alg_expr a b c =
  if is_dotnet a then
    propagate_def_in_alg_expr_p
      (fun logger id _ _ ->
        Ode_loggers_sig.is_dangerous_ode_variable logger
          (Ode_loggers_sig.Expr id))
      a b c
  else
    c

let rec eval_const_alg_expr logger network_handler alg_expr =
  match fst alg_expr with
  | Alg_expr.CONST x -> Some x
  | Alg_expr.TOKEN_ID _ | Alg_expr.KAPPA_INSTANCE _ -> None
  | Alg_expr.ALG_VAR x ->
    let id = network_handler.Network_handler.int_of_obs x in
    let expr_opt = Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id) in
    eval_const_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP Operator.TMAX_VAR ->
    let expr_opt = Ode_loggers_sig.get_expr logger Ode_loggers_sig.Tend in
    eval_const_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP
      ( Operator.CPUTIME | Operator.TIME_VAR | Operator.EVENT_VAR
      | Operator.EMAX_VAR | Operator.NULL_EVENT_VAR ) ->
    Some Nbr.zero
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    let const_a_opt = eval_const_alg_expr logger network_handler a in
    let const_b_opt = eval_const_alg_expr logger network_handler b in
    (match const_a_opt, const_b_opt with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (Nbr.of_bin_alg_op op a b))
  | Alg_expr.UN_ALG_OP (op, a) ->
    let const_a_opt = eval_const_alg_expr logger network_handler a in
    (match const_a_opt with
    | None -> None
    | Some a -> Some (Nbr.of_un_alg_op op a))
  | Alg_expr.IF (cond, yes, no) ->
    let const_cond_opt = eval_const_bool_expr logger network_handler cond in
    (match const_cond_opt with
    | None -> None
    | Some true -> eval_const_alg_expr logger network_handler yes
    | Some false -> eval_const_alg_expr logger network_handler no)
  | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation", snd alg_expr))

and eval_const_bool_expr logger network_handler expr =
  match fst expr with
  | Alg_expr.TRUE -> Some true
  | Alg_expr.FALSE -> Some false
  | Alg_expr.COMPARE_OP (op, a, b) ->
    let const_a_opt = eval_const_alg_expr logger network_handler a in
    let const_b_opt = eval_const_alg_expr logger network_handler b in
    (match const_a_opt, const_b_opt with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (Nbr.of_compare_op op a b))
  | Alg_expr.UN_BOOL_OP (Operator.NOT, a) ->
    Option_util.map not (eval_const_bool_expr logger network_handler a)
  | Alg_expr.BIN_BOOL_OP (op, a, b) ->
    let const_a_opt = eval_const_bool_expr logger network_handler a in
    let const_b_opt = eval_const_bool_expr logger network_handler b in
    (match const_a_opt, const_b_opt with
    | None, _ | _, None -> None
    | Some a, Some b ->
      (match op with
      | Operator.AND -> Some (a && b)
      | Operator.OR -> Some (a || b)))

let rec get_last_alias logger network_handler x =
  let id = network_handler.Network_handler.int_of_obs x in
  let expr_opt = Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id) in
  match fst (unsome expr_opt) with
  | Alg_expr.ALG_VAR x'
    when x <> x'
         && not
              (Ode_loggers_sig.is_dangerous_ode_variable logger
                 (Ode_loggers_sig.Expr
                    (network_handler.Network_handler.int_of_obs x'))) ->
    get_last_alias logger network_handler x'
  | Alg_expr.ALG_VAR _ | Alg_expr.CONST _ | Alg_expr.TOKEN_ID _
  | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.STATE_ALG_OP _ | Alg_expr.BIN_ALG_OP _
  | Alg_expr.UN_ALG_OP _ | Alg_expr.IF _ | Alg_expr.DIFF_KAPPA_INSTANCE _
  | Alg_expr.DIFF_TOKEN _ ->
    x

let print_int logger logger_err n =
  let () =
    do_sbml logger logger_err
      (lift0 (fun logger ->
           Ode_loggers_sig.fprintf logger "<cn type=\"integer\"> %i </cn>" n))
  in
  do_dotnet logger logger_err
    (lift0 (fun logger -> Ode_loggers_sig.fprintf logger "%i" n))

let print_nbr logger logger_err nbr =
  match nbr with
  | Nbr.I n -> print_int logger logger_err n
  | Nbr.I64 n -> print_int logger logger_err (Int64.to_int n)
  | Nbr.F f ->
    let () =
      do_sbml logger logger_err
        (lift0 (fun logger ->
             Ode_loggers_sig.fprintf logger "<cn type=\"real\"> %g </cn>" f))
    in
    do_dotnet logger logger_err
      (lift0 (fun logger -> Ode_loggers_sig.fprintf logger "%g" f))

let print_ci logger logger_err ci =
  let () =
    do_sbml logger logger_err (fun logger _ ->
        Ode_loggers_sig.fprintf logger "<ci>%s</ci>" ci)
  in
  do_dotnet logger logger_err (fun logger _ ->
      Ode_loggers_sig.fprintf logger "%s" ci)

let print_ci_with_id logger logger_err ci id =
  let () =
    do_sbml logger logger_err (fun logger _ ->
        Ode_loggers_sig.fprintf logger "<ci>%s%i</ci>" ci id)
  in
  do_dotnet logger logger_err (fun logger _ ->
      Ode_loggers_sig.fprintf logger "%i" id)

let rec print_alg_expr_in_sbml string_of_var_id logger logger_err
    (alg_expr :
      (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
      Loc.annoted)
    (network :
      (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
    =
  match fst alg_expr with
  | Alg_expr.CONST nbr -> print_nbr logger logger_err nbr
  | Alg_expr.ALG_VAR x ->
    if Alg_expr.is_constant alg_expr then (
      (* TODO Jérome: This is dead code
         ([alg_expr] = [ALG_VAR _, _] and [is_constant (ALG_VAR _,_)] = false)
         please have a look *)
      let () =
        print_ci logger logger_err
          (Ode_loggers_sig.get_id_of_global_parameter logger
             (Ode_loggers_sig.Expr (network.Network_handler.int_of_obs x)))
      in
      do_dotnet logger logger_err (fun _ logger_err ->
          let pos = Some __POS__ in
          warn_expr ?pos alg_expr "not handled yet, todo" logger logger_err)
    ) else (
      let id = network.Network_handler.int_of_obs x in
      match Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id) with
      | Some expr ->
        if Alg_expr.is_constant expr then (
          let () =
            do_sbml logger logger_err
              (lift0 (fun logger ->
                   Ode_loggers_sig.fprintf logger "<ci> %s </ci>"
                     (string_of_var_id id)))
          in
          do_dotnet logger logger_err
            (lift0 (fun logger ->
                 Ode_loggers_sig.fprintf logger "%s" (string_of_var_id id)))
        ) else
          (*if expr is not a constant in case of
            DOTNET gives warning*)
          (* let () =
             do_sbml logger logger_err
               (fun logger logger_err ->*)
          print_alg_expr_in_sbml string_of_var_id logger logger_err expr network
        (*
              in
              let () =
                do_dotnet logger logger_err
                  (fun _logger logger_err ->
                    warn_expr
                      alg_expr
                      ("DOTNET backend does not support non-constant rates for rules: cowardly replacing it with 1 "^(string_of_var_id id))
                      logger
                      logger_err)
              in
              do_dotnet logger logger_err
                (fun logger logger_err ->
                   print_nbr logger logger_err Nbr.one
                               )*)
      | None -> Ode_loggers_sig.fprintf logger "<ci>TODO:v%i</ci>" id
    )
  | Alg_expr.KAPPA_INSTANCE x ->
    let () =
      do_sbml logger logger_err (fun logger _logger_err ->
          Ode_loggers_sig.fprintf logger "<ci>s%i</ci>"
            (network.Network_handler.int_of_kappa_instance x))
    in
    do_dotnet logger logger_err (fun _logger logger_err ->
        let () =
          warn_expr alg_expr
            "DOTNET backend does not support kappa expression in rates for \
             rules: cowardly replacing it with 0"
            logger logger_err
        in
        print_nbr logger logger_err Nbr.zero)
  | Alg_expr.TOKEN_ID x ->
    let () =
      do_sbml logger logger_err (fun logger logger_err ->
          print_ci_with_id logger logger_err "t"
            (network.Network_handler.int_of_token_id x))
    in
    do_dotnet logger logger_err (fun _logger logger_err ->
        let () =
          warn_expr alg_expr
            "DOTNET backend does not support token values in rates for rules: \
             cowardly replacing it with 0"
            logger logger_err
        in
        print_nbr logger logger_err Nbr.zero)
  | Alg_expr.STATE_ALG_OP Operator.TMAX_VAR -> print_ci logger logger_err "tend"
  | Alg_expr.STATE_ALG_OP Operator.CPUTIME ->
    print_nbr logger logger_err Nbr.zero
  | Alg_expr.STATE_ALG_OP Operator.TIME_VAR ->
    let () =
      do_sbml logger logger_err (fun logger _ ->
          Ode_loggers_sig.fprintf logger
            "<apply>%s<ci>time</ci><ci>t_scale_factor</ci></apply>"
            (Ode_loggers_sig.string_of_bin_op logger Operator.MULT))
    in
    let () =
      do_dotnet logger logger_err (fun _logger logger_err ->
          let pos = Some __POS__ in
          warn_expr ?pos alg_expr "Internal error" logger logger_err)
    in
    ()
  | Alg_expr.STATE_ALG_OP Operator.EVENT_VAR ->
    print_nbr logger logger_err Nbr.zero
  | Alg_expr.STATE_ALG_OP Operator.EMAX_VAR ->
    print_ci logger logger_err "event_max"
  | Alg_expr.STATE_ALG_OP Operator.NULL_EVENT_VAR ->
    print_nbr logger logger_err Nbr.zero
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    let string_op = Ode_loggers_sig.string_of_bin_op logger op in
    let () =
      do_sbml logger logger_err (fun logger logger_err ->
          let () = Ode_loggers_sig.fprintf logger "<apply>" in
          let () = Ode_loggers_sig.fprintf logger "%s" string_op in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err a network
          in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err b network
          in
          let () = Ode_loggers_sig.fprintf logger "</apply>" in
          ())
    in
    let () =
      do_dotnet logger logger_err (fun logger logger_err ->
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err a network
          in
          let () = Ode_loggers_sig.fprintf logger "%s" string_op in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err b network
          in
          ())
    in
    ()
  | Alg_expr.UN_ALG_OP (op, a) ->
    let string_op = Ode_loggers_sig.string_of_un_op logger op in
    let () =
      do_sbml logger logger_err (fun logger logger_err ->
          let () = Ode_loggers_sig.fprintf logger "<apply>" in
          let () = Ode_loggers_sig.fprintf logger "%s" string_op in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err a network
          in
          let () = Ode_loggers_sig.fprintf logger "</apply>" in
          ())
    in
    let () =
      do_dotnet logger logger_err (fun logger logger_err ->
          let () = Ode_loggers_sig.fprintf logger "%s" string_op in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err a network
          in
          ())
    in
    ()
  | Alg_expr.IF (cond, yes, no) ->
    let () =
      do_sbml logger logger_err (fun logger logger_err ->
          let () = Ode_loggers_sig.fprintf logger "<apply>" in
          let () = Ode_loggers_sig.fprintf logger "<if-then-else>" in
          let () =
            print_bool_expr_in_sbml string_of_var_id logger logger_err cond
              network
          in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err yes
              network
          in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err no network
          in
          let () = Ode_loggers_sig.fprintf logger "</apply>" in
          ())
    in
    do_dotnet logger logger_err (fun _logger logger_err ->
        let pos = Some __POS__ in
        warn_expr ?pos alg_expr "Conditionals are not allowed in DOTNET backend"
          logger logger_err)
  | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation", snd alg_expr))

and print_bool_expr_in_sbml string_of_var_id logger logger_err cond network =
  let () =
    do_sbml logger logger_err (fun logger logger_err ->
        match fst cond with
        | Alg_expr.TRUE -> Ode_loggers_sig.fprintf logger "<true/>"
        | Alg_expr.FALSE -> Ode_loggers_sig.fprintf logger "<false/>"
        | Alg_expr.COMPARE_OP (op, a, b) ->
          let () = Ode_loggers_sig.fprintf logger "<apply>" in
          let () =
            Ode_loggers_sig.fprintf logger "%s"
              (Ode_loggers_sig.string_of_compare_op logger op)
          in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err a network
          in
          let () =
            print_alg_expr_in_sbml string_of_var_id logger logger_err b network
          in
          let () = Ode_loggers_sig.fprintf logger "</apply>" in
          ()
        | Alg_expr.BIN_BOOL_OP (op, a, b) ->
          let () = Ode_loggers_sig.fprintf logger "<apply>" in
          let () =
            Ode_loggers_sig.fprintf logger "%s"
              (Ode_loggers_sig.string_of_bin_bool_op logger op)
          in
          let () =
            print_bool_expr_in_sbml string_of_var_id logger logger_err a network
          in
          let () =
            print_bool_expr_in_sbml string_of_var_id logger logger_err b network
          in
          let () = Ode_loggers_sig.fprintf logger "</apply>" in
          ()
        | Alg_expr.UN_BOOL_OP (op, a) ->
          let () = Ode_loggers_sig.fprintf logger "<apply>" in
          let () =
            Ode_loggers_sig.fprintf logger "%s"
              (Ode_loggers_sig.string_of_un_bool_op logger op)
          in
          let () =
            print_bool_expr_in_sbml string_of_var_id logger logger_err a network
          in
          let () = Ode_loggers_sig.fprintf logger "</apply>" in
          ())
  in
  do_dotnet logger logger_err (fun _logger logger_err ->
      let pos = Some __POS__ in
      warn_expr ?pos cond
        "Boolean expressions are not allowed in DOTNET backend" logger
        logger_err)

let rec substance_expr_in_sbml logger
    (alg_expr :
      (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
      Loc.annoted)
    (network :
      (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
    =
  match fst alg_expr with
  | Alg_expr.CONST _
  | Alg_expr.STATE_ALG_OP Operator.CPUTIME
  | Alg_expr.STATE_ALG_OP Operator.EVENT_VAR
  | Alg_expr.STATE_ALG_OP Operator.NULL_EVENT_VAR ->
    Mods.StringSet.empty
  | Alg_expr.ALG_VAR x ->
    if Alg_expr.is_constant alg_expr then
      (* TODO Jérome: This is dead code, please have a look *)
      Mods.StringSet.singleton
        (Ode_loggers_sig.get_id_of_global_parameter logger
           (Ode_loggers_sig.Expr (network.Network_handler.int_of_obs x)))
    else (
      let id = network.Network_handler.int_of_obs x in
      match Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id) with
      | Some expr -> substance_expr_in_sbml logger expr network
      | None ->
        (* TO DO *)
        Mods.StringSet.empty
    )
  | Alg_expr.KAPPA_INSTANCE x ->
    Mods.StringSet.singleton
      ("s" ^ string_of_int (network.Network_handler.int_of_kappa_instance x))
  | Alg_expr.TOKEN_ID x ->
    Mods.StringSet.singleton
      ("t" ^ string_of_int (network.Network_handler.int_of_token_id x))
  | Alg_expr.STATE_ALG_OP Operator.TMAX_VAR -> Mods.StringSet.singleton "tend"
  | Alg_expr.STATE_ALG_OP Operator.TIME_VAR -> Mods.StringSet.singleton "time"
  | Alg_expr.STATE_ALG_OP Operator.EMAX_VAR ->
    Mods.StringSet.singleton "event_max"
  | Alg_expr.BIN_ALG_OP (_op, a, b) ->
    Mods.StringSet.union
      (substance_expr_in_sbml logger a network)
      (substance_expr_in_sbml logger b network)
  | Alg_expr.UN_ALG_OP (_op, a) -> substance_expr_in_sbml logger a network
  | Alg_expr.IF (cond, yes, no) ->
    Mods.StringSet.union
      (substance_bool_expr_in_sbml logger cond network)
      (Mods.StringSet.union
         (substance_expr_in_sbml logger yes network)
         (substance_expr_in_sbml logger no network))
  | Alg_expr.DIFF_KAPPA_INSTANCE _ | Alg_expr.DIFF_TOKEN _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation", snd alg_expr))

and substance_bool_expr_in_sbml logger cond network =
  match fst cond with
  | Alg_expr.TRUE | Alg_expr.FALSE -> Mods.StringSet.empty
  | Alg_expr.COMPARE_OP (_, a, b) ->
    Mods.StringSet.union
      (substance_expr_in_sbml logger a network)
      (substance_expr_in_sbml logger b network)
  | Alg_expr.BIN_BOOL_OP (_, a, b) ->
    Mods.StringSet.union
      (substance_bool_expr_in_sbml logger a network)
      (substance_bool_expr_in_sbml logger b network)
  | Alg_expr.UN_BOOL_OP (_, a) -> substance_bool_expr_in_sbml logger a network

let rec maybe_time_dependent_alg_expr_in_sbml logger
    (alg_expr :
      (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Alg_expr.e
      Loc.annoted)
    (network :
      (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
    =
  match fst alg_expr with
  | Alg_expr.CONST (Nbr.I _)
  | Alg_expr.CONST (Nbr.I64 _)
  | Alg_expr.CONST (Nbr.F _) ->
    false
  | Alg_expr.ALG_VAR _x -> false
  (* TODO Jérome: please check that! All this commented code was dead code
     but it is very suspicious... (signed pirbo) *)
  (* begin
      let id =
        network.Network_handler.int_of_obs x
      in
      match
        Ode_loggers_sig.get_expr logger (Ode_loggers_sig.Expr id)
      with
      | Some expr ->
        maybe_time_dependent_alg_expr_in_sbml
          logger
          expr
          network
      | None -> false
       end*)
  | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.TOKEN_ID _
  | Alg_expr.STATE_ALG_OP Operator.TMAX_VAR
  | Alg_expr.STATE_ALG_OP Operator.CPUTIME ->
    false
  | Alg_expr.STATE_ALG_OP Operator.TIME_VAR -> true
  | Alg_expr.STATE_ALG_OP Operator.EVENT_VAR
  | Alg_expr.STATE_ALG_OP Operator.EMAX_VAR
  | Alg_expr.STATE_ALG_OP Operator.NULL_EVENT_VAR ->
    false
  | Alg_expr.BIN_ALG_OP (_, a, b) ->
    maybe_time_dependent_alg_expr_in_sbml logger a network
    || maybe_time_dependent_alg_expr_in_sbml logger b network
  | Alg_expr.UN_ALG_OP (_, a)
  | Alg_expr.DIFF_KAPPA_INSTANCE (a, _)
  | Alg_expr.DIFF_TOKEN (a, _) ->
    maybe_time_dependent_alg_expr_in_sbml logger a network
  | Alg_expr.IF (cond, yes, no) ->
    maybe_time_dependent_bool_expr_in_sbml logger cond network
    || maybe_time_dependent_alg_expr_in_sbml logger yes network
    || maybe_time_dependent_alg_expr_in_sbml logger no network

and maybe_time_dependent_bool_expr_in_sbml logger cond network =
  match fst cond with
  | Alg_expr.TRUE | Alg_expr.FALSE -> false
  | Alg_expr.COMPARE_OP (_op, a, b) ->
    maybe_time_dependent_alg_expr_in_sbml logger a network
    || maybe_time_dependent_alg_expr_in_sbml logger b network
  | Alg_expr.BIN_BOOL_OP (_op, a, b) ->
    maybe_time_dependent_bool_expr_in_sbml logger a network
    || maybe_time_dependent_bool_expr_in_sbml logger b network
  | Alg_expr.UN_BOOL_OP (_op, a) ->
    maybe_time_dependent_bool_expr_in_sbml logger a network

let break = true

let replace_space_with_underscore =
  String.map (fun c ->
      if c = ' ' then
        '_'
      else
        c)

let dump_initial_species ?units loggers logger_err network_handler k name
    species =
  let expr =
    match Ode_loggers_sig.get_expr loggers (Ode_loggers_sig.Init k) with
    | Some a -> a
    | None -> Alg_expr.const Nbr.zero
  in
  let units =
    match units with
    | None -> "substance"
    | Some units -> units
  in
  let concentration = eval_init_alg_expr loggers network_handler expr in
  let s =
    Format.sprintf
      "metaid=\"%s\" id=\"%s\" name=\"%s\" compartment=\"default\" \
       initialAmount=\"%s\" substanceUnits=\"%s\""
      (meta_id_of_logger loggers)
      species name
      (Nbr.to_string concentration)
      units
  in
  let s_dotnet =
    Format.sprintf "%s %s %s" species name (Nbr.to_string concentration)
  in
  let () =
    do_sbml loggers logger_err (fun loggers logger_err ->
        single_box ~options:(fun () -> s) loggers logger_err "species")
  in
  let () =
    do_dotnet loggers logger_err (fun loggers logger_err ->
        single_box ~options:(fun () -> s_dotnet) loggers logger_err "")
  in
  ()

let dump_species_reference ?(dotnet_sep = "") loggers logger_err species i =
  let s =
    Format.sprintf "metaid=\"%s\" species=\"s%i\"%s"
      (meta_id_of_logger loggers)
      species
      (if i = 1 then
         ""
       else
         " stoichiometry=\"" ^ string_of_int i ^ "\"")
  in
  let () =
    do_sbml loggers logger_err (fun loggers logger_err ->
        single_box ~options:(fun () -> s) loggers logger_err "speciesReference")
  in
  let () =
    do_dotnet loggers logger_err (fun loggers _ ->
        let rec aux k =
          let () = Ode_loggers_sig.fprintf loggers "%i" species in
          if k >= i then
            ()
          else (
            let () = Ode_loggers_sig.fprintf loggers "%s" dotnet_sep in
            aux (k + 1)
          )
        in
        aux 1)
  in
  ()

let add map (id, sym) =
  let old =
    match Mods.IntMap.find_option id map with
    | Some (i, _) -> i
    | None -> 0
  in
  Mods.IntMap.add id (succ old, sym) map

let dump_list_of_species_reference loggers logger_err ?(dotnet_sep = "") list =
  let map = List.fold_left add Mods.IntMap.empty list in
  let _ =
    List.fold_left
      (fun bool (s, (i, j)) ->
        let () =
          if bool then
            do_dotnet loggers logger_err (fun logger _ ->
                Ode_loggers_sig.fprintf logger "%s" dotnet_sep)
        in
        let () =
          dump_species_reference ~dotnet_sep loggers logger_err s (i * j)
        in
        (* check what to do when stochiometric coefficients are bigger than 1*)
        true)
      false (Mods.IntMap.bindings map)
  in
  ()

let dump_pair logger logger_err (t, i) =
  if i = 1 then (
    let () = Ode_loggers_sig.fprintf logger "<ci> s%i </ci>" t in
    ()
  ) else (
    let () =
      add_box ~break logger logger_err "apply" "" (fun logger _ ->
          let () = Ode_loggers_sig.fprintf logger "<divide/>" in
          Ode_loggers_sig.fprintf logger
            "<ci> s%i </ci><cn type=\"integer\"> %i </cn>" t i)
    in
    ()
  )

let maybe_time_dependent logger network var_rule =
  match Ode_loggers_sig.get_encoding_format logger with
  | Loggers.SBML ->
    let expr_opt = Ode_loggers_sig.get_expr logger var_rule in
    let expr = unsome expr_opt in
    maybe_time_dependent_alg_expr_in_sbml logger expr network
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.Octave | Loggers.Mathematica | Loggers.GEPHI
  | Loggers.Matlab | Loggers.Maple | Loggers.Json | Loggers.DOTNET ->
    false

let can_be_cast f =
  let s = Nbr.to_string (Nbr.F f) in
  s.[String.length s - 1] = '.'

let promote nbr =
  match nbr with
  | Nbr.F f when can_be_cast f -> Nbr.I (int_of_float f)
  | Nbr.I _ | Nbr.F _ | Nbr.I64 _ -> nbr

let dump_kinetic_law ~propagate_constants string_of_var_id logger logger_err
    network reactants var_rule correct nocc =
  let () =
    do_dotnet logger logger_err (fun logger logger_err ->
        let expr_opt = Ode_loggers_sig.get_expr logger var_rule in
        let expr = unsome expr_opt in
        let f logger =
          let dump_constant =
            match eval_const_alg_expr logger network expr with
            | None ->
              let () =
                warn_expr expr
                  ("DOTNET backend does not support non-constant rates for \
                    rules: cowardly replacing it with "
                  ^ string_of_variable logger
                      (fun _logger var ->
                        string_of_int
                          (network.Network_handler.int_of_kappa_instance var))
                      var_rule)
                  logger logger_err
              in
              fun logger ->
                Ode_loggers_sig.fprintf logger "%s"
                  (string_of_variable logger
                     (fun _logger var ->
                       string_of_int
                         (network.Network_handler.int_of_kappa_instance var))
                     var_rule)
            | Some cst ->
              let cst = promote cst in
              if propagate_constants then
                fun logger ->
              Ode_loggers_sig.fprintf logger "%s" (Nbr.to_string cst)
              else (
                match expr with
                | Alg_expr.ALG_VAR var_id, _ ->
                  let var_id = get_last_alias logger network var_id in
                  let s =
                    string_of_var_id (network.Network_handler.int_of_obs var_id)
                  in
                  if Ode_loggers_sig.has_forbidden_char logger s then
                    (* MAKE A CLEANER TEST *)
                    fun logger ->
                  Ode_loggers_sig.fprintf logger "%s"
                    (string_of_variable logger
                       (fun _logger var ->
                         string_of_int
                           (network.Network_handler.int_of_kappa_instance var))
                       var_rule)
                  else
                    fun logger ->
                  Ode_loggers_sig.fprintf logger "%s"
                    (string_of_var_id
                       (network.Network_handler.int_of_obs var_id))
                | ( ( Alg_expr.BIN_ALG_OP _ | Alg_expr.UN_ALG_OP _
                    | Alg_expr.IF _ | Alg_expr.STATE_ALG_OP _
                    | Alg_expr.KAPPA_INSTANCE _ | Alg_expr.DIFF_KAPPA_INSTANCE _
                    | Alg_expr.TOKEN_ID _ | Alg_expr.DIFF_TOKEN _
                    | Alg_expr.CONST _ ),
                    _ ) ->
                  fun logger ->
                    Ode_loggers_sig.fprintf logger "%s"
                      (string_of_variable logger
                         (fun _logger var ->
                           string_of_int
                             (network.Network_handler.int_of_kappa_instance var))
                         var_rule)
              )
          in
          if correct = nocc then
            dump_constant logger
          else if correct = 1 then (
            let () = Ode_loggers_sig.fprintf logger "%i*" nocc in
            let () = dump_constant logger in
            let () = Ode_loggers_sig.print_newline logger in
            ()
          ) else if nocc = 1 then (
            let () =
              Ode_loggers_sig.fprintf logger "%g*" (1. /. float_of_int correct)
            in
            let () = dump_constant logger in
            let () = Ode_loggers_sig.print_newline logger in
            ()
          ) else (
            let () =
              Ode_loggers_sig.fprintf logger "%g*"
                (float_of_int nocc /. float_of_int correct)
            in
            let () = dump_constant logger in
            let () = Ode_loggers_sig.print_newline logger in
            ()
          )
        in
        f logger
        (*match reactants with
          | [] ->
            f logger
          | _::_ ->
            add_box ~break logger "apply" ""
              (fun logger ->
                 let () = Ode_loggers_sig.fprintf logger "<times/>" in
                 let () = f logger in
                 let rec aux list =
                   match list with
                     [] -> ()
                   | [t] ->
                     dump_pair logger t
                   | t::q ->
                     add_box ~break logger "apply" ""
                       (fun logger ->
                          let () = Ode_loggers_sig.fprintf logger "<times/>" in
                          let () = dump_pair logger t in
                          aux q)
                 in aux reactants
              )*))
  in
  do_sbml logger logger_err (fun logger _ ->
      let expr_opt = Ode_loggers_sig.get_expr logger var_rule in
      let expr = unsome expr_opt in
      let f logger =
        if Alg_expr.is_constant expr then
          if correct = nocc then
            Ode_loggers_sig.fprintf logger "<ci> %s </ci>"
              (string_of_variable logger
                 (fun _logger var ->
                   string_of_int
                     (network.Network_handler.int_of_kappa_instance var))
                 var_rule)
          else if nocc = 1 then
            add_box ~break logger logger_err "apply" "" (fun logger _ ->
                let () = Ode_loggers_sig.fprintf logger "<divide/>" in
                let () =
                  Ode_loggers_sig.fprintf logger
                    "<ci> %s </ci><cn type=\"integer\"> %i </cn>"
                    (string_of_variable logger
                       (fun _logger var ->
                         string_of_int
                           (network.Network_handler.int_of_kappa_instance var))
                       var_rule)
                    correct
                in
                let () = Ode_loggers_sig.print_newline logger in
                ())
          else if correct = 1 then
            add_box ~break logger logger_err "apply" "" (fun logger _ ->
                let () = Ode_loggers_sig.fprintf logger "<times/>" in
                let () =
                  Ode_loggers_sig.fprintf logger
                    "<cn type=\"integer\"> %i </cn><ci> %s </ci>" nocc
                    (string_of_variable logger
                       (fun _logger var ->
                         string_of_int
                           (network.Network_handler.int_of_kappa_instance var))
                       var_rule)
                in
                let () = Ode_loggers_sig.print_newline logger in
                ())
          else
            add_box ~break logger logger_err "apply" "" (fun logger _ ->
                let () = Ode_loggers_sig.fprintf logger "<times/>" in
                let () =
                  Ode_loggers_sig.fprintf logger
                    "<cn type=\"integer\"> %i </cn><divide/><ci> %s </ci><cn \
                     type=\"integer\"> %i </cn>"
                    nocc
                    (string_of_variable logger
                       (fun _logger var ->
                         string_of_int
                           (network.Network_handler.int_of_kappa_instance var))
                       var_rule)
                    correct
                in
                let () = Ode_loggers_sig.print_newline logger in
                ())
        else (
          let expr =
            if correct = nocc then
              expr
            else if nocc = 1 then
              Alg_expr.div expr (Alg_expr.int correct)
            else if correct = 1 then
              Alg_expr.mult (Alg_expr.int nocc) expr
            else
              Alg_expr.div
                (Alg_expr.mult (Alg_expr.int nocc) expr)
                (Alg_expr.int correct)
          in
          print_alg_expr_in_sbml string_of_var_id logger logger_err expr network
        )
      in
      match reactants with
      | [] -> f logger
      | _ :: _ ->
        add_box ~break logger logger_err "apply" "" (fun logger _ ->
            let () = Ode_loggers_sig.fprintf logger "<times/>" in
            let () = f logger in
            let rec aux list =
              match list with
              | [] -> ()
              | [ t ] -> dump_pair logger logger_err t
              | t :: q ->
                add_box ~break logger logger_err "apply" ""
                  (fun logger logger_err ->
                    let () = Ode_loggers_sig.fprintf logger "<times/>" in
                    let () = dump_pair logger logger_err t in
                    aux q)
            in
            aux reactants))

let negative_part expr =
  Loc.annot_with_dummy
    (Alg_expr.UN_ALG_OP
       ( Operator.UMINUS,
         Loc.annot_with_dummy
           (Alg_expr.BIN_ALG_OP
              ( Operator.MIN,
                Loc.annot_with_dummy (Alg_expr.CONST Nbr.zero),
                expr )) ))

let positive_part expr =
  Loc.annot_with_dummy
    (Alg_expr.BIN_ALG_OP
       (Operator.MAX, Loc.annot_with_dummy (Alg_expr.CONST Nbr.zero), expr))

let dump_token_vector convert logger logger_err network_handler rule_id
    token_vector =
  let () =
    do_sbml logger logger_err (fun logger _ ->
        let _ =
          List.fold_left
            (fun n (id, _) ->
              let expr_opt =
                Ode_loggers_sig.get_expr logger
                  (Ode_loggers_sig.Stochiometric_coef (rule_id, n))
              in
              let expr = unsome expr_opt in
              let stochiometry_opt =
                eval_const_alg_expr logger network_handler (convert expr)
              in
              let () =
                match stochiometry_opt with
                | None ->
                  let () =
                    warn_expr expr
                      "Expressions for token consumption/production should be \
                       constants: cowardly replace it with 0\n"
                      logger logger_err
                  in
                  ()
                | Some x when Nbr.is_zero x -> ()
                | Some x ->
                  let s =
                    Format.sprintf "metaid=\"%s\" species=\"t%i\"%s"
                      (meta_id_of_logger logger) id
                      (if Nbr.is_equal x (Nbr.I 1) then
                         ""
                       else
                         " stoichiometry=\"" ^ Nbr.to_string x ^ "\"")
                  in
                  single_box
                    ~options:(fun () -> s)
                    logger logger_err "speciesReference"
              in
              n + 1)
            1 token_vector
        in
        ())
  in
  let () =
    do_dotnet logger logger_err (fun logger logger_err ->
        match token_vector with
        | [] -> ()
        | _ :: _ ->
          let expr_opt =
            Ode_loggers_sig.get_expr logger
              (Ode_loggers_sig.Stochiometric_coef (rule_id, 1))
          in
          let expr = unsome expr_opt in
          warn_expr expr
            "Rules with tokens are not allowed in DOTNET output: cowardly \
             ignoring them"
            logger logger_err)
  in
  ()

let has_good_token_token_vector convert logger network_handler rule_id
    token_vector =
  let rec aux n l =
    match l with
    | [] -> false
    | _ :: tail ->
      let expr_opt =
        Ode_loggers_sig.get_expr logger
          (Ode_loggers_sig.Stochiometric_coef (rule_id, n))
      in
      let stochiometry_opt =
        eval_const_alg_expr logger network_handler (convert (unsome expr_opt))
      in
      (match stochiometry_opt with
      | None -> aux (n + 1) tail
      | Some x when Nbr.is_zero x -> aux (n + 1) tail
      | Some _ -> true)
  in
  aux 1 token_vector

let has_reactants_in_token_vector logger network_handler rule_id token_vector =
  has_good_token_token_vector negative_part logger network_handler rule_id
    token_vector

let has_products_in_token_vector logger network_handler rule_id token_vector =
  has_good_token_token_vector positive_part logger network_handler rule_id
    token_vector

let dump_products_of_token_vector logger logger_err network_handler rule_id
    token_vector =
  do_dotnet_or_sbml logger logger_err (fun logger logger_err ->
      dump_token_vector positive_part logger logger_err network_handler rule_id
        token_vector)

let dump_reactants_of_token_vector logger logger_err network_handler rule_id
    token_vector =
  do_dotnet_or_sbml logger logger_err (fun logger logger_err ->
      dump_token_vector negative_part logger logger_err network_handler rule_id
        token_vector)

let dump_sbml_reaction ~propagate_constants _print_expr string_of_var_id
    get_rule get_rule_id print_rule_name compil logger logger_err network
    reactants products token_vector enriched_rule var_rule correct nocc
    fictitious =
  let dotnet_sep = "," in
  let reaction_id = Ode_loggers_sig.get_fresh_reaction_id logger in
  let label_reaction = "reaction" in
  let label = "" in
  let label_list_of_reactants = "listOfReactants" in
  let label_list_of_products = "listOfProducts" in
  let label_list_of_mods = "listOfModifiers" in
  let rule_id = get_rule_id enriched_rule in
  let reactants, products =
    if reactants = [] && is_dotnet logger then (
      match fictitious with
      | None -> failwith "Internal error"
      | Some id -> (id, 1) :: reactants, List.rev ((id, 1) :: List.rev products)
    ) else
      reactants, products
  in

  let token_vector =
    if is_dotnet logger then (
      match token_vector with
      | [] -> token_vector
      | _ :: _ ->
        let expr_opt =
          Ode_loggers_sig.get_expr logger
            (Ode_loggers_sig.Stochiometric_coef (rule_id, 1))
        in
        let expr = unsome expr_opt in
        let () =
          warn_expr expr
            "Rules with tokens are not allowed in DOTNET output: cowardl \
             ignoring them"
            logger logger_err
        in
        []
    ) else
      token_vector
  in
  let options () =
    Format.asprintf
      "id=\"re%i\" name=\"%a\" reversible=\"false\" fast=\"false\"" reaction_id
      (print_rule_name ?compil) (get_rule enriched_rule)
  in
  let () =
    add_box_in_sbml_only ~options ~break logger logger_err label_reaction
      (fun logger logger_err ->
        let () =
          do_dotnet logger logger_err (fun logger _ ->
              Ode_loggers_sig.fprintf logger "%i " reaction_id)
        in
        let () =
          if
            reactants = []
            && not
                 (has_reactants_in_token_vector logger network rule_id
                    token_vector)
          then
            ()
          else (
            let () =
              add_box_in_sbml_only ~break logger logger_err
                label_list_of_reactants (fun logger logger_err ->
                  let () =
                    dump_list_of_species_reference ~dotnet_sep logger logger_err
                      reactants
                  in
                  let () =
                    dump_reactants_of_token_vector logger logger_err network
                      rule_id token_vector
                  in
                  ())
            in
            let () =
              do_dotnet logger logger_err (fun logger _ ->
                  Ode_loggers_sig.fprintf logger " ")
            in
            ()
          )
        in
        let () =
          if
            products = []
            && not
                 (has_products_in_token_vector logger network rule_id
                    token_vector)
          then
            ()
          else (
            let () =
              add_box_in_sbml_only ~break logger logger_err
                label_list_of_products (fun logger logger_err ->
                  let () =
                    dump_list_of_species_reference ~dotnet_sep logger logger_err
                      products
                  in
                  let () =
                    dump_products_of_token_vector logger logger_err network
                      rule_id token_vector
                  in
                  ())
            in
            let () =
              do_dotnet logger logger_err (fun logger _ ->
                  Ode_loggers_sig.fprintf logger " ")
            in
            ()
          )
        in
        let expr_opt = Ode_loggers_sig.get_expr logger var_rule in
        let expr = unsome expr_opt in
        let modifiers = substance_expr_in_sbml logger expr network in
        let modifiers =
          List.fold_left
            (fun set (a, _) ->
              Mods.StringSet.remove ("s" ^ string_of_int a) set)
            modifiers reactants
        in
        (*
         let () =
           if maybe_time_dependent logger network var_rule
           then
             add_box ~break logger label_list_of_mods
               (fun
                 logger ->
                 let s =
                   Format.sprintf
                   "metaid=\"%s\" species=\"time\""
                   (meta_id_of_logger logger)
                 in
                 let () = single_box ~options:(fun () -> s) logger "modifierSpeciesReference" in
                 ()
                )
         in*)
        let () =
          if Mods.StringSet.is_empty modifiers then
            ()
          else
            add_box_in_sbml_only ~break logger logger_err label_list_of_mods
              (fun logger logger_err ->
                Mods.StringSet.iter
                  (fun string ->
                    let () =
                      do_sbml logger logger_err (fun logger logger_err ->
                          let s =
                            Format.sprintf "metaid=\"%s\" species=\"%s\""
                              (meta_id_of_logger logger) string
                          in
                          let () =
                            single_box
                              ~options:(fun () -> s)
                              logger logger_err "modifierSpeciesReference"
                          in
                          ())
                    in
                    ())
                  modifiers)
        in
        let () =
          add_box_in_sbml_only ~break logger logger_err "kineticLaw"
            (fun logger logger_err ->
              add_box ~break
                ~options:(fun () ->
                  " xmlns=\"http://www.w3.org/1998/Math/MathML\"")
                logger logger_err "math" label
                (fun logger logger_err ->
                  dump_kinetic_law ~propagate_constants string_of_var_id logger
                    logger_err network reactants var_rule correct nocc))
        in
        ())
  in
  ()

let time_advance logger logger_err =
  let reaction_id = Ode_loggers_sig.get_fresh_reaction_id logger in
  let label_reaction = "reaction" in
  let label_dotnet = "" in
  let label_list_of_products = "listOfProducts" in
  let options () =
    Format.asprintf
      "id=\"re%i\" name=\"time advance\" reversible=\"false\" fast=\"false\""
      reaction_id
  in
  let () =
    add_box ~options ~break logger logger_err label_reaction label_dotnet
      (fun logger logger_err ->
        let () =
          add_box ~break logger logger_err label_list_of_products label_dotnet
            (fun logger logger_err ->
              let () =
                do_sbml logger logger_err (fun logger logger_err ->
                    let s =
                      Format.sprintf "metaid=\"%s\" species=\"time\""
                        (meta_id_of_logger logger)
                    in
                    let () =
                      single_box
                        ~options:(fun () -> s)
                        logger logger_err "speciesReference"
                    in
                    ())
              in
              ())
        in
        let () =
          add_box ~break logger logger_err "kineticLaw" label_dotnet
            (fun logger logger_err ->
              add_box ~break
                ~options:(fun () ->
                  " xmlns=\"http://www.w3.org/1998/Math/MathML\"")
                logger logger_err "math" label_dotnet
                (fun logger logger_err ->
                  print_sbml logger logger_err "<cn type=\"integer\"> 1 </cn>"))
        in
        ())
  in
  ()
