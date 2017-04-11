let do_sbml logger f = (*TODO: do_dotnet?*)
match
  Loggers.get_encoding_format logger
with
| Loggers.SBML ->
  let () =
    f logger
  in
  ()
| Loggers.DOTNET
| Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
| Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
| Loggers.XLS | Loggers.Octave | Loggers.Mathematica
| Loggers.Matlab | Loggers.Maple | Loggers.Json -> ()

let do_not_sbml logger f =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.SBML -> ()
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.Matlab | Loggers.Maple | Loggers.Json | Loggers.DOTNET ->
    let () =
      f logger
    in
    ()

let print_sbml logger s =
  do_sbml logger
    (fun logger ->
       Loggers.fprintf logger "%s" s
    )

let break_sbml logger =
  do_sbml logger
    Loggers.print_breakable_hint

let line_sbml  logger =
  do_sbml logger
    Loggers.print_newline

let extend s =
  if s="" || String.sub s 0 1 = " "  then s else " "^s

let open_box ?options:(options=fun () -> "") logger label =
  let () = print_sbml logger ("<"^label^(extend (options ()))^">") in
  break_sbml logger

let close_box logger label =
  let () = print_sbml logger ("</"^label^">") in
  line_sbml logger

let single_box ?options:(options=fun () -> "") logger label =
  let () = print_sbml logger ("<"^label^(extend (options ()))^"/>") in
  line_sbml logger

let potential_break break logger =
  if break
  then
    line_sbml logger
  else
    break_sbml logger

let add_box ?break:(break=false) ?options:(options=fun () -> "") logger label cont =
  let () = open_box ~options logger label in
  let () = potential_break break logger in
  let () = do_sbml logger cont in
  let () = close_box logger label in
  ()

let clean_variable_id =
    String.map
      (function
        | '(' -> '_'
        | ')' -> ' '
        | x -> x )

let string_in_comment s =
  let s =
    String.map
    (fun x ->
       match x with
       | '\'' -> ' '
       | _ -> x )
    s
  in
  let size = String.length s in
  let rec aux k s =
    if k+1=size
    then Bytes.to_string s
    else
      let () =
        if Bytes.get s k ='-' && Bytes.get s (k+1) = '-'
        then
          Bytes.set s k ' '
      in
      aux (k+1) s
  in aux 0 (Bytes.of_string s)

let string_of_variable logger string_of_var_id variable =
  match variable with
  | Ode_loggers_sig.Expr i ->
    string_of_var_id logger i
  | Ode_loggers_sig.Concentration i -> "s"^(string_of_int i)
  | Ode_loggers_sig.Init _
  | Ode_loggers_sig.Initbis _
  | Ode_loggers_sig.Deriv _
  | Ode_loggers_sig.Obs _
  | Ode_loggers_sig.Jacobian _
  | Ode_loggers_sig.Jacobian_var _
  | Ode_loggers_sig.Tinit
  | Ode_loggers_sig.Tend
  | Ode_loggers_sig.Period_t_points
  | Ode_loggers_sig.N_rules
  | Ode_loggers_sig.N_ode_var
  | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs
  | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.N_max_stoc_coef
  | Ode_loggers_sig.InitialStep
  | Ode_loggers_sig.NonNegative
  | Ode_loggers_sig.MaxStep
  | Ode_loggers_sig.RelTol
  | Ode_loggers_sig.AbsTol
  | Ode_loggers_sig.Tmp -> Ode_loggers_sig.string_of_array_name variable
  | Ode_loggers_sig.Current_time -> "t"
  | Ode_loggers_sig.Time_scale_factor -> "t_scale_factor"
  | Ode_loggers_sig.Rate int -> Printf.sprintf "k%i" int
  | Ode_loggers_sig.Rated int -> Printf.sprintf "kd%i" int
  | Ode_loggers_sig.Rateun int -> Printf.sprintf "kun%i" int
  | Ode_loggers_sig.Rateund int -> Printf.sprintf "kdun%i" int
  | Ode_loggers_sig.Stochiometric_coef (int1,int2) ->
    Printf.sprintf "stoc%i.%i" int1 int2
  | Ode_loggers_sig.Jacobian_stochiometric_coef _
  | Ode_loggers_sig.Jacobian_rate _
  | Ode_loggers_sig.Jacobian_rated _
  | Ode_loggers_sig.Jacobian_rateun _
  | Ode_loggers_sig.Jacobian_rateund _ ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation",Locality.dummy))

let unit_of_variable variable =
  match variable with
  | Ode_loggers_sig.Period_t_points
  | Ode_loggers_sig.Tinit
  | Ode_loggers_sig.InitialStep
  | Ode_loggers_sig.MaxStep
  | Ode_loggers_sig.Tend -> Some "time"
  | Ode_loggers_sig.Current_time
  | Ode_loggers_sig.RelTol
  | Ode_loggers_sig.AbsTol
  | Ode_loggers_sig.Obs _
  | Ode_loggers_sig.Init _
  | Ode_loggers_sig.Concentration _
  | Ode_loggers_sig.Stochiometric_coef _
  | Ode_loggers_sig.Initbis _ -> Some "substance"
  | Ode_loggers_sig.Time_scale_factor -> Some "time_per_substance"
  | Ode_loggers_sig.NonNegative
  | Ode_loggers_sig.Expr _
  | Ode_loggers_sig.Deriv _
  | Ode_loggers_sig.Jacobian _
  | Ode_loggers_sig.Jacobian_var _
  | Ode_loggers_sig.Rate _
  | Ode_loggers_sig.Rated _
  | Ode_loggers_sig.Rateun _
  | Ode_loggers_sig.Rateund _
  | Ode_loggers_sig.Jacobian_stochiometric_coef _
  | Ode_loggers_sig.Jacobian_rate _
  | Ode_loggers_sig.Jacobian_rated _
  | Ode_loggers_sig.Jacobian_rateun _
  | Ode_loggers_sig.Jacobian_rateund _
  | Ode_loggers_sig.N_rules
  | Ode_loggers_sig.N_ode_var
  | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs
  | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.N_max_stoc_coef
  | Ode_loggers_sig.Tmp -> None

let meta_id_of_logger logger =
  "CMD"^(string_of_int (Loggers.get_fresh_meta_id logger))

let print_parameters string_of_var_id logger logger_buffer variable expr =
  let unit_string =
    match
      unit_of_variable variable
    with
    | None -> ""
    | Some x -> " units=\""^x^"\""
  in
  let id = string_of_variable logger string_of_var_id variable in
  let () = Loggers.set_id_of_global_parameter logger variable id  in
  single_box
    logger_buffer
    "parameter"
    ~options:(fun () ->
        Format.sprintf
          "metaid=\"%s\" id=\"%s\" value=\"%s\"%s"
          (meta_id_of_logger logger)
          id
          (Nbr.to_string expr)
          unit_string)

let of_bool_op op =
  match op with
  | Operator.AND -> ( && )
  | Operator.OR -> ( || )

let unsome expr_opt =
  match expr_opt
  with
  | None -> Alg_expr.const Nbr.zero
  | Some expr -> expr

let rec eval_init_alg_expr logger network_handler alg_expr =
  match fst alg_expr with
  | Alg_expr.CONST x  -> x
  | Alg_expr.ALG_VAR x ->
    let id = network_handler.Network_handler.int_of_obs x in
    let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Expr id) in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.KAPPA_INSTANCE x ->
      let id = network_handler.Network_handler.int_of_kappa_instance x in
    let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Init id) in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.TOKEN_ID x ->
    let id = network_handler.Network_handler.int_of_token_id x in
    let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Init id) in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) ->
    let expr_opt = Loggers.get_expr logger Ode_loggers_sig.Tend in
    eval_init_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP
      ( Operator.CPUTIME
      | Operator.TIME_VAR
      | Operator.EVENT_VAR
      | Operator.EMAX_VAR
      | Operator.NULL_EVENT_VAR ) -> Nbr.zero
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    Nbr.of_bin_alg_op
      op
      (eval_init_alg_expr logger network_handler a)
      (eval_init_alg_expr logger network_handler b)
  | Alg_expr.UN_ALG_OP (op, a) ->
    Nbr.of_un_alg_op
      op
      (eval_init_alg_expr logger network_handler a)
  | Alg_expr.IF (cond, yes, no) ->
    if eval_init_bool_expr logger network_handler cond
    then
      eval_init_alg_expr logger network_handler yes
    else
      eval_init_alg_expr logger network_handler no
  | (Alg_expr.DIFF_KAPPA_INSTANCE _
    | Alg_expr.DIFF_TOKEN _) ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation",snd alg_expr))
and eval_init_bool_expr logger network_handler expr =
  match fst expr with
  | Alg_expr.TRUE -> true
  | Alg_expr.FALSE -> false
  | Alg_expr.COMPARE_OP (op,a,b) ->
    Nbr.of_compare_op op
      (eval_init_alg_expr logger network_handler a)
      (eval_init_alg_expr logger network_handler b)
  | Alg_expr.BOOL_OP (op,a,b) ->
    of_bool_op op
      (eval_init_bool_expr logger network_handler a)
      (eval_init_bool_expr logger network_handler b)

let rec propagate_def_in_alg_expr logger network_handler alg_expr =
  match alg_expr with
  | Alg_expr.CONST _,_
  | Alg_expr.TOKEN_ID _,_
  | Alg_expr.KAPPA_INSTANCE _,_ -> alg_expr
  | Alg_expr.ALG_VAR x,loc ->
    let id = network_handler.Network_handler.int_of_obs x in
    let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Expr id) in
    fst (propagate_def_in_alg_expr logger network_handler (unsome expr_opt)),loc
  | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR),loc ->
    let expr_opt = Loggers.get_expr logger Ode_loggers_sig.Tend in
    fst (propagate_def_in_alg_expr logger network_handler (unsome expr_opt)),loc
  | Alg_expr.STATE_ALG_OP
      ( Operator.CPUTIME
      | Operator.TIME_VAR
      | Operator.EVENT_VAR
      | Operator.EMAX_VAR
      | Operator.NULL_EVENT_VAR ),loc ->
    Alg_expr.CONST Nbr.zero,loc
  | Alg_expr.BIN_ALG_OP (op, a, b),loc ->
    Alg_expr.BIN_ALG_OP (
      op,
      (propagate_def_in_alg_expr logger network_handler a),
      (propagate_def_in_alg_expr logger network_handler b)),loc
  | Alg_expr.UN_ALG_OP (op, a),loc ->
    Alg_expr.UN_ALG_OP (
      op,
      (propagate_def_in_alg_expr logger network_handler a)),loc
  | Alg_expr.IF (cond, yes, no),loc ->
    Alg_expr.IF
      (propagate_def_in_bool_expr logger network_handler cond,
       propagate_def_in_alg_expr logger network_handler yes,
       propagate_def_in_alg_expr logger network_handler no), loc
  | (Alg_expr.DIFF_KAPPA_INSTANCE _
    | Alg_expr.DIFF_TOKEN _),pos ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation",pos))
and propagate_def_in_bool_expr logger network_handler expr =
  match expr with
  | Alg_expr.TRUE,_
  | Alg_expr.FALSE,_ -> expr
  | Alg_expr.COMPARE_OP (op,a,b),loc ->
    Alg_expr.COMPARE_OP
      (op,
       (propagate_def_in_alg_expr logger network_handler a),
       (propagate_def_in_alg_expr  logger network_handler b)),
    loc
  | Alg_expr.BOOL_OP (op,a,b),loc ->
    Alg_expr.BOOL_OP
      (op,
       (propagate_def_in_bool_expr logger network_handler a),
       (propagate_def_in_bool_expr logger network_handler b)),
    loc

let rec eval_const_alg_expr logger network_handler alg_expr =
  match fst alg_expr with
  | Alg_expr.CONST x -> Some x
  | Alg_expr.TOKEN_ID _
  | Alg_expr.KAPPA_INSTANCE _ -> None
  | Alg_expr.ALG_VAR x ->
    let id = network_handler.Network_handler.int_of_obs x in
    let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Expr id) in
    eval_const_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) ->
    let expr_opt = Loggers.get_expr logger Ode_loggers_sig.Tend in
    eval_const_alg_expr logger network_handler (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP
      ( Operator.CPUTIME
      | Operator.TIME_VAR
      | Operator.EVENT_VAR
      | Operator.EMAX_VAR
      | Operator.NULL_EVENT_VAR ) -> Some (Nbr.zero)
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    let const_a_opt =
      eval_const_alg_expr logger network_handler a
    in
    let const_b_opt =
      eval_const_alg_expr logger network_handler b
    in
    begin
      match const_a_opt, const_b_opt with
      | None, _ | _,None -> None
      | Some a, Some b ->
        Some (Nbr.of_bin_alg_op op a b)
    end
  | Alg_expr.UN_ALG_OP (op, a) ->
    let const_a_opt =
      eval_const_alg_expr logger network_handler a
    in
    begin
      match const_a_opt with
      | None -> None
      | Some a ->
        Some (Nbr.of_un_alg_op op a )
    end
  | Alg_expr.IF (cond, yes, no) ->
    let const_cond_opt =
      eval_const_bool_expr logger network_handler cond
    in
    begin
      match const_cond_opt with
      | None -> None
      | Some true ->
        eval_const_alg_expr logger network_handler yes
      | Some false ->
        eval_const_alg_expr logger network_handler no
    end
  | (Alg_expr.DIFF_KAPPA_INSTANCE _
    | Alg_expr.DIFF_TOKEN _) ->
    raise
      (ExceptionDefn.Internal_Error
         ("SBML does not support differentiation",snd alg_expr))
and eval_const_bool_expr logger network_handler expr =
  match fst expr with
  | Alg_expr.TRUE -> Some true
  | Alg_expr.FALSE -> Some false
  | Alg_expr.COMPARE_OP (op,a,b) ->
    let const_a_opt =
      eval_const_alg_expr logger network_handler a
    in
    let const_b_opt =
      eval_const_alg_expr logger network_handler b
    in
    begin
      match const_a_opt, const_b_opt with
      | None, _ | _,None -> None
      | Some a, Some b ->
        Some (Nbr.of_compare_op op a b)
    end
  | Alg_expr.BOOL_OP (op,a,b) ->
    let const_a_opt =
      eval_const_bool_expr logger network_handler a
    in
    let const_b_opt =
      eval_const_bool_expr logger network_handler b
    in
    begin
      match const_a_opt, const_b_opt with
      | None, _ | _,None -> None
      | Some a, Some b ->
        begin
          match op with
          | Operator.AND -> Some (a && b)
          | Operator.OR -> Some (a || b)
        end
    end

let rec print_alg_expr_in_sbml string_of_var_id logger
    (alg_expr:
       (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id)
         Alg_expr.e Locality.annot
    ) (network:
         (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
  =
  match
    Ode_loggers_sig.is_expr_alias alg_expr,
    Ode_loggers_sig.is_expr_const alg_expr
  with
  | Some x, true ->
    Loggers.fprintf logger "<ci> %s </ci>"
      (Loggers.get_id_of_global_parameter
         logger (Ode_loggers_sig.Expr (network.Network_handler.int_of_obs x)))
  | None, _ | Some _, false ->
    begin
      match fst alg_expr with
      | Alg_expr.CONST (Nbr.I n)  ->
        Loggers.fprintf logger "<cn type=\"integer\"> %i </cn>" n
      | Alg_expr.CONST (Nbr.I64 n) ->
        Loggers.fprintf logger "<cn type=\"integer\"> %i </cn>" (Int64.to_int n)
      | Alg_expr.CONST (Nbr.F f) ->
        Loggers.fprintf logger "<cn type=\"real\"> %g </cn>" f
      | Alg_expr.ALG_VAR x ->
        begin
          let id =
            network.Network_handler.int_of_obs x
          in
          match
            Loggers.get_expr logger (Ode_loggers_sig.Expr id)
          with
          | Some expr ->
            if Ode_loggers_sig.is_expr_const expr
            then
              Loggers.fprintf logger "<ci> %s </ci>"
                (string_of_var_id id)
            else
              print_alg_expr_in_sbml
                string_of_var_id
                logger
                expr
                network
          | None ->
            Loggers.fprintf logger "<ci>TODO:v%i</ci>" id
        end
      | Alg_expr.KAPPA_INSTANCE x ->
        Loggers.fprintf logger "<ci>s%i</ci>"
          (network.Network_handler.int_of_kappa_instance x)
      | Alg_expr.TOKEN_ID x ->
        Loggers.fprintf logger "<ci>t%i</ci>"
          (network.Network_handler.int_of_token_id x)
      | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) ->
        Loggers.fprintf logger "<ci>tend</ci>"
      | Alg_expr.STATE_ALG_OP (Operator.CPUTIME) ->
        Loggers.fprintf logger "<ci>0</ci>"
      | Alg_expr.STATE_ALG_OP (Operator.TIME_VAR) ->
        Loggers.fprintf logger "<apply>%s<ci>time</ci><ci>t_scale_factor</ci></apply>"
          (Loggers_string_of_op.string_of_bin_op logger Operator.MULT)
      | Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR) ->
        Loggers.fprintf logger "<ci>0</ci>"
      | Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR) ->
        Loggers.fprintf logger "<ci>event_max</ci>"
      | Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR) ->
        Loggers.fprintf logger "<ci>0</ci>"
      | Alg_expr.BIN_ALG_OP (op, a, b) ->
        let string_op = Loggers_string_of_op.string_of_bin_op logger op in
        let () = Loggers.fprintf logger "<apply>" in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = print_alg_expr_in_sbml string_of_var_id logger a network in
        let () = print_alg_expr_in_sbml string_of_var_id logger b network in
        let () = Loggers.fprintf logger "</apply>" in
        ()
      | Alg_expr.UN_ALG_OP (op, a) ->
        let string_op = Loggers_string_of_op.string_of_un_op logger op in
        let () = Loggers.fprintf logger "<apply>" in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = print_alg_expr_in_sbml string_of_var_id logger a network in
        let () = Loggers.fprintf logger "</apply>" in
        ()
      | Alg_expr.IF (cond, yes, no) ->
        let () = Loggers.fprintf logger "<apply>" in
        let () = Loggers.fprintf logger "<if-then-else>"  in
        let () = print_bool_expr_in_sbml string_of_var_id logger cond network in
        let () = print_alg_expr_in_sbml string_of_var_id logger yes network in
        let () = print_alg_expr_in_sbml string_of_var_id logger no network in
        let () = Loggers.fprintf logger "</apply>" in
        ()
      | (Alg_expr.DIFF_KAPPA_INSTANCE _
        | Alg_expr.DIFF_TOKEN _) ->
        raise
          (ExceptionDefn.Internal_Error
             ("SBML does not support differentiation",snd alg_expr))
    end
and
  print_bool_expr_in_sbml string_of_var_id logger cond network =
  match fst cond with
  | Alg_expr.TRUE -> Loggers.fprintf logger "<true/>"
  | Alg_expr.FALSE -> Loggers.fprintf logger "<false/>"
  | Alg_expr.COMPARE_OP (op,a,b) ->
    let () = Loggers.fprintf logger "<apply>" in
    let () =
      Loggers.fprintf logger "%s"
        (Loggers_string_of_op.string_of_compare_op logger op) in
    let () = print_alg_expr_in_sbml string_of_var_id logger a network in
    let () = print_alg_expr_in_sbml string_of_var_id logger b network in
    let () = Loggers.fprintf logger "</apply>" in
    ()
  | Alg_expr.BOOL_OP (op,a,b) ->
    let () = Loggers.fprintf logger "<apply>" in
    let () = Loggers.fprintf logger "%s"
        (Loggers_string_of_op.string_of_bool_op logger op) in
    let () = print_bool_expr_in_sbml string_of_var_id logger a network in
    let () = print_bool_expr_in_sbml string_of_var_id logger b network in
    let () = Loggers.fprintf logger "</apply>" in
    ()

let rec substance_expr_in_sbml logger
    (alg_expr:
       (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id)
         Alg_expr.e Locality.annot
    ) (network:
         (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
  =
  match
    Ode_loggers_sig.is_expr_alias alg_expr,
    Ode_loggers_sig.is_expr_const alg_expr
  with
  | Some x, true ->
    Mods.StringSet.singleton
      (Loggers.get_id_of_global_parameter
         logger (Ode_loggers_sig.Expr (network.Network_handler.int_of_obs x)))
  | None, _ | Some _, false ->
    begin
      match fst alg_expr with
      | Alg_expr.CONST _
      | Alg_expr.STATE_ALG_OP (Operator.CPUTIME)
      | Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR)
      | Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR)
        ->
        Mods.StringSet.empty
      | Alg_expr.ALG_VAR x ->
        begin
          let id =
            network.Network_handler.int_of_obs x
          in
          match
            Loggers.get_expr logger (Ode_loggers_sig.Expr id)
          with
          | Some expr ->
            substance_expr_in_sbml
              logger
              expr
              network
          | None ->(* TO DO *)
            Mods.StringSet.empty
        end
      | Alg_expr.KAPPA_INSTANCE x ->
        Mods.StringSet.singleton
          ("s"^(string_of_int (network.Network_handler.int_of_kappa_instance x)))
      | Alg_expr.TOKEN_ID x ->
        Mods.StringSet.singleton
          ("t"^(string_of_int (network.Network_handler.int_of_token_id x)))
      | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) ->
        Mods.StringSet.singleton "tend"
      | Alg_expr.STATE_ALG_OP (Operator.TIME_VAR) ->
        Mods.StringSet.singleton "time"
      | Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR) ->
        Mods.StringSet.singleton "event_max"
      | Alg_expr.BIN_ALG_OP (_op, a, b) ->
        Mods.StringSet.union
          (substance_expr_in_sbml logger a network)
          (substance_expr_in_sbml logger b network)
      | Alg_expr.UN_ALG_OP (_op, a) ->
        substance_expr_in_sbml logger a network
      | Alg_expr.IF (cond, yes, no) ->
        Mods.StringSet.union
          (substance_bool_expr_in_sbml logger cond network)
          (Mods.StringSet.union
             (substance_expr_in_sbml logger yes network)
             (substance_expr_in_sbml logger no network))
      | (Alg_expr.DIFF_KAPPA_INSTANCE _
        | Alg_expr.DIFF_TOKEN _) ->
        raise
          (ExceptionDefn.Internal_Error
             ("SBML does not support differentiation",snd alg_expr))
    end
and
  substance_bool_expr_in_sbml logger cond network =
  match fst cond with
  | Alg_expr.TRUE
  | Alg_expr.FALSE -> Mods.StringSet.empty
  | Alg_expr.COMPARE_OP (_,a,b) ->
    Mods.StringSet.union
      (substance_expr_in_sbml logger a network)
      (substance_expr_in_sbml logger b network)
  | Alg_expr.BOOL_OP (_,a,b) ->
  Mods.StringSet.union
    (substance_bool_expr_in_sbml logger a network)
    (substance_bool_expr_in_sbml logger b network)

let rec maybe_time_dependent_alg_expr_in_sbml logger
    (alg_expr:
       (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id)
         Alg_expr.e Locality.annot
    ) (network:
         (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
  =
  match
    Ode_loggers_sig.is_expr_alias alg_expr
  with
  | Some _ -> false
  | None ->
    begin
      match fst alg_expr with
      | Alg_expr.CONST (Nbr.I _)
      | Alg_expr.CONST (Nbr.I64 _)
      | Alg_expr.CONST (Nbr.F _) -> false
      | Alg_expr.ALG_VAR x ->
        begin
          let id =
            network.Network_handler.int_of_obs x
          in
          match
            Loggers.get_expr logger (Ode_loggers_sig.Expr id)
          with
          | Some expr ->
            maybe_time_dependent_alg_expr_in_sbml
              logger
              expr
              network
          | None -> false
        end
      | Alg_expr.KAPPA_INSTANCE _
      | Alg_expr.TOKEN_ID _
      | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR)
      | Alg_expr.STATE_ALG_OP (Operator.CPUTIME) -> false
      | Alg_expr.STATE_ALG_OP (Operator.TIME_VAR) -> true
      | Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR)
      | Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR)
      | Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR) -> false
      | Alg_expr.BIN_ALG_OP (_, a, b) ->
        maybe_time_dependent_alg_expr_in_sbml logger a network
        || maybe_time_dependent_alg_expr_in_sbml logger b network
      | Alg_expr.UN_ALG_OP (_, a)
      | Alg_expr.DIFF_KAPPA_INSTANCE (a,_)
      | Alg_expr.DIFF_TOKEN (a,_) ->
        maybe_time_dependent_alg_expr_in_sbml logger a network
      | Alg_expr.IF (cond, yes, no) ->
        maybe_time_dependent_bool_expr_in_sbml logger cond network
        ||
        maybe_time_dependent_alg_expr_in_sbml logger yes network
        ||
        maybe_time_dependent_alg_expr_in_sbml logger no network
    end
and
  maybe_time_dependent_bool_expr_in_sbml logger cond network =
  match fst cond with
  | Alg_expr.TRUE
  | Alg_expr.FALSE -> false
  | Alg_expr.COMPARE_OP (_op,a,b) ->
    maybe_time_dependent_alg_expr_in_sbml logger a network
    || maybe_time_dependent_alg_expr_in_sbml logger b network
  | Alg_expr.BOOL_OP (_op,a,b) ->
    maybe_time_dependent_bool_expr_in_sbml logger a network
    || maybe_time_dependent_bool_expr_in_sbml logger b network

let break = true
let replace_space_with_underscore =
  String.map (fun c -> if c=' ' then '_' else c)

let dump_initial_species ?units loggers network_handler k name species =
  let expr =
    match Loggers.get_expr loggers (Ode_loggers_sig.Init k) with
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
      "metaid=\"%s\" id=\"%s\" name=\"%s\" compartment=\"default\" initialAmount=\"%s\" substanceUnits=\"%s\""
      (meta_id_of_logger loggers)
      species
      name
      (Nbr.to_string concentration)
      units
  in
  let () = single_box ~options:(fun () -> s) loggers "species" in
  ()


let dump_species_reference loggers species i =
  let s =
    Format.sprintf
      "metaid=\"%s\" species=\"s%i\"%s"
      (meta_id_of_logger loggers)
      species
      (if i=1 then "" else " stoichiometry=\""^(string_of_int i)^"\"")
  in
  let () = single_box ~options:(fun () -> s) loggers "speciesReference" in
  ()

let add map (id,sym) =
  let old =
    match
      Mods.IntMap.find_option id map
    with
    | Some (i,_) -> i
    | None -> 0
  in
  Mods.IntMap.add id ((succ old),sym) map

let dump_list_of_species_reference
    loggers
    list
  =
  let map =
    List.fold_left
      add
      Mods.IntMap.empty
      list
  in
  List.iter
    (fun (s,(i,j)) ->
       dump_species_reference loggers s (i*j))
    (Mods.IntMap.bindings map)

let dump_pair logger (t,i) =
  if i = 1 then
    Loggers.fprintf logger "<ci> s%i </ci>" t
  else
    add_box ~break logger "apply"
      (fun logger ->
         let () = Loggers.fprintf logger "<divide/>" in
         Loggers.fprintf logger "<ci> s%i </ci><cn type=\"integer\"> %i </cn>" t i)

let maybe_time_dependent logger network var_rule =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.SBML ->
    let expr_opt =
      Loggers.get_expr logger var_rule in
    let expr = unsome expr_opt in
    maybe_time_dependent_alg_expr_in_sbml logger expr network
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.Octave | Loggers.Mathematica
  | Loggers.Matlab | Loggers.Maple | Loggers.Json | Loggers.DOTNET -> false



let dump_kinetic_law
    string_of_var_id logger network reactants var_rule correct =
  do_sbml logger
    (fun logger  ->
       begin
  let expr_opt =
    Loggers.get_expr logger var_rule in
  let expr = unsome expr_opt in
  let f logger =
    if Ode_loggers_sig.is_expr_const expr
    then
      if correct = 1
      then
        Loggers.fprintf logger "<ci> %s </ci>"
          (string_of_variable
             logger
             (fun _logger var -> string_of_int
                 (* this line is error prone, check*)
                 (network.Network_handler.int_of_kappa_instance var))
             var_rule)
      else
        add_box ~break logger "apply"
          (fun logger ->
            let () = Loggers.fprintf logger "<divide/>" in
            let () =
              Loggers.fprintf logger "<ci> %s </ci><cn type=\"integer\"> %i </cn>"
                (string_of_variable
                   logger
                   (fun _logger var -> string_of_int
                      (* this line is error prone, check*)
                       (network.Network_handler.int_of_kappa_instance var))
                   var_rule)
                correct
            in
            let () = Loggers.print_newline logger in
            ()
          )

    else
      let expr =
        if correct = 1
        then expr
        else
          Alg_expr.div expr (Alg_expr.int correct)
      in
      print_alg_expr_in_sbml string_of_var_id logger expr network
  in
  match reactants with
  | [] ->
    f logger
  | _::_ ->
      add_box ~break logger "apply"
        (fun logger ->
           let () = Loggers.fprintf logger "<times/>" in
           let () = f logger in
           let rec aux list =
             match list with
               [] -> ()
             | [t] ->
               dump_pair logger t
             | t::q ->
               add_box ~break logger "apply"
                 (fun logger ->
                    let () = Loggers.fprintf logger "<times/>" in
                    let () = dump_pair logger t in
                    aux q)
           in aux reactants
        )
       end
    )


let negative_part expr =
  Locality.dummy_annot
    (Alg_expr.UN_ALG_OP
    (Operator.UMINUS,
     Locality.dummy_annot
       (Alg_expr.BIN_ALG_OP(Operator.MIN,
                            Locality.dummy_annot(
                              Alg_expr.CONST (Nbr.zero)),expr))))

let positive_part expr =
  Locality.dummy_annot
    (Alg_expr.BIN_ALG_OP(Operator.MAX,
                         Locality.dummy_annot(
                           Alg_expr.CONST (Nbr.zero)),expr))

let dump_token_vector convert logger network_handler rule_id token_vector =
  let _ =
    List.fold_left
      (fun n (id,_) ->
         let expr_opt =
           Loggers.get_expr logger
             (Ode_loggers_sig.Stochiometric_coef (rule_id,n))
         in
         let expr = unsome expr_opt in
         let stochiometry_opt =
           eval_const_alg_expr logger network_handler (convert expr)
         in
         let () =
           match stochiometry_opt with
           | None ->
             Printf.printf
               "%s: Expressions for token consumption/production  should be constants \n Cowardly replace it with 0"
               (Locality.to_string (snd expr))
           | Some x when Nbr.is_zero x -> ()
           | Some x ->
             let s =
               Format.sprintf
                 "metaid=\"%s\" species=\"t%i\"%s"
                 (meta_id_of_logger logger)
                 id
                 (if Nbr.is_equal x (Nbr.I 1) then "" else " stoichiometry=\""^(Nbr.to_string x)^"\"")
             in
             single_box ~options:(fun () -> s) logger "speciesReference"
         in
         n+1)
      1
      token_vector
  in
  ()

let has_good_token_token_vector
    convert logger network_handler rule_id token_vector =
  let rec aux n l =
    match l with
    | [] -> false
    | _::tail ->
      begin
        let expr_opt =
          Loggers.get_expr logger
            (Ode_loggers_sig.Stochiometric_coef (rule_id,n))
        in
        let stochiometry_opt =
          eval_const_alg_expr logger network_handler (convert (unsome expr_opt))
        in
        match stochiometry_opt with
        | None -> aux (n+1) tail
        | Some x when Nbr.is_zero x -> aux (n+1) tail
        | Some _ -> true
      end
  in aux 1 token_vector

let has_reactants_in_token_vector logger network_handler rule_id token_vector =
  has_good_token_token_vector
    negative_part logger network_handler rule_id token_vector

let has_products_in_token_vector logger network_handler rule_id token_vector =
  has_good_token_token_vector
    positive_part logger network_handler rule_id token_vector

let dump_products_of_token_vector
    logger network_handler rule_id token_vector =
  dump_token_vector
    positive_part logger network_handler rule_id token_vector

let dump_reactants_of_token_vector logger network_handler rule_id token_vector =
  dump_token_vector
    negative_part logger network_handler rule_id token_vector

let dump_sbml_reaction
    string_of_var_id
    get_rule
    get_rule_id
    print_rule_name
    compil
    logger
    network
    reactants
    products
    token_vector
    enriched_rule
    var_rule
    correct
  =
  let reaction_id = Loggers.get_fresh_reaction_id logger in
  let label_reaction  = "reaction" in
  let label_list_of_reactants = "listOfReactants" in
  let label_list_of_products = "listOfProducts" in
  let label_list_of_mods = "listOfModifiers" in
  let rule_id = get_rule_id enriched_rule in
  let options =
    (fun () -> Format.asprintf
        "id=\"re%i\" name=\"%a\" reversible=\"false\" fast=\"false\"" reaction_id (print_rule_name ?compil) (get_rule enriched_rule))
  in
  let () =
    add_box ~options ~break logger label_reaction
      (fun logger ->
         let () =
           if reactants = [] &&
            not (has_reactants_in_token_vector logger network rule_id token_vector)
           then ()
           else
             add_box ~break logger label_list_of_reactants
               (fun logger ->
                  let () =
                    dump_list_of_species_reference
                      logger reactants
                  in
                  let () =
                    dump_reactants_of_token_vector
                      logger network rule_id token_vector
                  in
                  ()
               )
         in
         let () =
           if products = [] &&
              not (has_products_in_token_vector logger network  rule_id token_vector)
           then ()
           else
             add_box ~break logger label_list_of_products
               (fun logger ->
                  let () =
                    dump_list_of_species_reference
                      logger products in
                  let () =
                    dump_products_of_token_vector
                      logger network rule_id token_vector
                  in
                  ())
         in
         let expr_opt =
           Loggers.get_expr logger var_rule in
         let expr = unsome expr_opt in
         let modifiers =
           substance_expr_in_sbml
             logger expr network
         in
         let modifiers =
           List.fold_left
             (fun set (a,_) ->
                Mods.StringSet.remove ("s"^(string_of_int a)) set)
             modifiers
             reactants
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
           if Mods.StringSet.is_empty modifiers
           then
             ()
           else
           add_box ~break logger label_list_of_mods
             (fun
               logger ->
               Mods.StringSet.iter
                 (fun string ->
                    let s =
                 Format.sprintf
                   "metaid=\"%s\" species=\"%s\""
                 (meta_id_of_logger logger) string
               in
               let () = single_box ~options:(fun () -> s) logger "modifierSpeciesReference" in
               ()) modifiers)
         in

         let () =
           add_box ~break logger "kineticLaw"
             (fun logger ->
                add_box
                  ~break
                  ~options:(fun () ->
                      " xmlns=\"http://www.w3.org/1998/Math/MathML\"")
                  logger "math"
                  (fun logger ->
                     dump_kinetic_law
                       string_of_var_id
                       logger network
                       reactants var_rule correct)
             )
         in
         ()
      )
  in
  ()

let time_advance logger  =
  let reaction_id = Loggers.get_fresh_reaction_id logger in
  let label_reaction  = "reaction" in
  let label_list_of_products = "listOfProducts" in
  let options =
    (fun () -> Format.asprintf
        "id=\"re%i\" name=\"time advance\" reversible=\"false\" fast=\"false\"" reaction_id)
  in
  let () =
    add_box ~options ~break logger label_reaction
      (fun logger ->
         let () =
           add_box ~break logger label_list_of_products
             (fun logger ->
                let s =
                  Format.sprintf
                    "metaid=\"%s\" species=\"time\""
                    (meta_id_of_logger logger)
                in
                let () =
                  single_box ~options:(fun () -> s) logger "speciesReference" in
                ()
             )
         in
         let () =
           add_box ~break logger "kineticLaw"
             (fun logger ->
                add_box
                  ~break
                  ~options:(fun () ->
                      " xmlns=\"http://www.w3.org/1998/Math/MathML\"")
                  logger "math"
                  (fun logger ->
                     print_sbml logger ("<cn type=\"integer\"> 1 </cn>"                    ))

             )
         in
         ()
      )
  in
  ()
