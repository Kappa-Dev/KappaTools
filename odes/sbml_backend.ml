let do_sbml logger f =
match
  Loggers.get_encoding_format logger
with
| Loggers.SBML ->
  let () =
    f logger
  in
  ()
| Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
| Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
| Loggers.XLS | Loggers.Octave
| Loggers.Matlab | Loggers.Maple | Loggers.Json -> ()

let do_not_sbml logger f =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.SBML -> ()
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.Octave
  | Loggers.Matlab | Loggers.Maple | Loggers.Json ->
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

let string_of_variable string_of_var_id variable =
  match variable with
  | Ode_loggers_sig.Expr i ->
    string_of_var_id i
  | Ode_loggers_sig.Concentration i -> "s"^(string_of_int i)
  | Ode_loggers_sig.Init _
  | Ode_loggers_sig.Initbis _
  | Ode_loggers_sig.Deriv _
  | Ode_loggers_sig.Obs _
  | Ode_loggers_sig.Jacobian _
  | Ode_loggers_sig.Tinit
  | Ode_loggers_sig.Tend
  | Ode_loggers_sig.InitialStep
  | Ode_loggers_sig.Period_t_points
  | Ode_loggers_sig.N_rules
  | Ode_loggers_sig.N_ode_var
  | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs
  | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.Tmp -> Ode_loggers_sig.string_of_variable variable
  | Ode_loggers_sig.Current_time -> "t"
  | Ode_loggers_sig.Rate int -> Printf.sprintf "k%i" int
  | Ode_loggers_sig.Rated int -> Printf.sprintf "kd%i" int
  | Ode_loggers_sig.Rateun int -> Printf.sprintf "kun%i" int
  | Ode_loggers_sig.Rateund int -> Printf.sprintf "kdun%i" int



let unit_of_variable variable =
  match variable with
  | Ode_loggers_sig.Current_time
  | Ode_loggers_sig.Period_t_points
  | Ode_loggers_sig.Tinit
  | Ode_loggers_sig.Tend -> Some "time"
  | Ode_loggers_sig.Obs _
  | Ode_loggers_sig.Init _
  | Ode_loggers_sig.Concentration _
  | Ode_loggers_sig.Initbis _ -> Some "substance"
  | Ode_loggers_sig.Expr _
  | Ode_loggers_sig.Deriv _
  | Ode_loggers_sig.Jacobian _
  | Ode_loggers_sig.InitialStep
  | Ode_loggers_sig.Rate _
  | Ode_loggers_sig.Rated _
  | Ode_loggers_sig.Rateun _
  | Ode_loggers_sig.Rateund _
  | Ode_loggers_sig.N_rules
  | Ode_loggers_sig.N_ode_var
  | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs
  | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.Tmp -> None

let meta_id_of_logger logger =
  "CMD"^(string_of_int (Loggers.get_fresh_meta_id logger))

let print_parameters string_of_var_id logger variable expr =
  let unit_string =
    match
      unit_of_variable variable
    with
    | None -> ""
    | Some x -> " units=\""^x^"\""
  in
  let id = string_of_variable string_of_var_id variable in
  let () = Loggers.set_id_of_global_parameter logger variable id  in
  single_box
    logger
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
  | None -> Location.dummy_annot (Alg_expr.CONST Nbr.zero)
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

let rec print_alg_expr_in_sbml logger
    (alg_expr:
       (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id)
         Alg_expr.e Location.annot
    ) (network:
         (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
  =
  match
    Ode_loggers_sig.is_expr_alias alg_expr
  with
  | Some x ->
    Loggers.fprintf logger "<ci> %s </ci>"
      (Loggers.get_id_of_global_parameter
         logger (Ode_loggers_sig.Expr (network.Network_handler.int_of_obs x)))
  | None ->
    begin
      match fst alg_expr with
      | Alg_expr.CONST (Nbr.I n)  ->
        Loggers.fprintf logger "<cn type=\"integer\"> %i </cn>" n
      | Alg_expr.CONST (Nbr.I64 n) ->
        Loggers.fprintf logger "<cn type=\"integer\"> %i </cn>" (Int64.to_int n)
      | Alg_expr.CONST (Nbr.F f) ->
        Loggers.fprintf logger "<cn type=\"real\"> %f </cn>" f
      | Alg_expr.ALG_VAR x ->
        begin
          let id =
            network.Network_handler.int_of_obs x
          in
          match
            Loggers.get_expr logger (Ode_loggers_sig.Expr id)
          with
          | Some expr ->
            print_alg_expr_in_sbml
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
        Loggers.fprintf logger "<ci>t</ci>"
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
        let () = print_alg_expr_in_sbml logger a network in
        let () = print_alg_expr_in_sbml logger b network in
        let () = Loggers.fprintf logger "</apply>" in
        ()
      | Alg_expr.UN_ALG_OP (op, a) ->
        let string_op = Loggers_string_of_op.string_of_un_op logger op in
        let () = Loggers.fprintf logger "<apply>" in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = print_alg_expr_in_sbml logger a network in
        let () = Loggers.fprintf logger "</apply>" in
        ()
      | Alg_expr.IF (cond, yes, no) ->
        let () = Loggers.fprintf logger "<apply>" in
        let () = Loggers.fprintf logger "<if-then-else>"  in
        let () = print_bool_expr_in_sbml logger cond network in
        let () = print_alg_expr_in_sbml logger yes network in
        let () = print_alg_expr_in_sbml logger no network in
        let () = Loggers.fprintf logger "</apply>" in
        ()
    end
and
  print_bool_expr_in_sbml logger cond network =
  match fst cond with
  | Alg_expr.TRUE -> Loggers.fprintf logger "<true/>"
  | Alg_expr.FALSE -> Loggers.fprintf logger "<false/>"
  | Alg_expr.COMPARE_OP (op,a,b) ->
    let () = Loggers.fprintf logger "<apply>" in
    let () =
      Loggers.fprintf logger "%s"
        (Loggers_string_of_op.string_of_compare_op logger op) in
    let () = print_alg_expr_in_sbml logger a network in
    let () = print_alg_expr_in_sbml logger b network in
    let () = Loggers.fprintf logger "</apply>" in
    ()
  | Alg_expr.BOOL_OP (op,a,b) ->
    let () = Loggers.fprintf logger "<apply>" in
    let () = Loggers.fprintf logger "%s"
        (Loggers_string_of_op.string_of_bool_op logger op) in
    let () = print_bool_expr_in_sbml logger a network in
    let () = print_bool_expr_in_sbml logger b network in
    let () = Loggers.fprintf logger "</apply>" in
    ()

let break = true
let replace_space_with_underscore =
  String.map (fun c -> if c=' ' then '_' else c)

let dump_initial_species ?units loggers network_handler k name species =
  let expr =
    match Loggers.get_expr loggers (Ode_loggers_sig.Init k) with
    | Some a -> a
    | None -> Location.dummy_annot (Alg_expr.CONST Nbr.zero)
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

let dump_kinetic_law
    logger network reactants var_rule correct =
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
             (fun var -> string_of_int
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
                   (fun var -> string_of_int
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
          Location.dummy_annot
            (Alg_expr.BIN_ALG_OP
               (Operator.DIV,
                expr,
                Location.dummy_annot (Alg_expr.CONST (Nbr.I correct))))
      in
      print_alg_expr_in_sbml logger expr network
  in
  match reactants with
  | [] ->
    print_alg_expr_in_sbml logger expr network
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
  Location.dummy_annot
    (Alg_expr.UN_ALG_OP
    (Operator.UMINUS,
     Location.dummy_annot
       (Alg_expr.BIN_ALG_OP(Operator.MIN,
                            Location.dummy_annot(
                              Alg_expr.CONST (Nbr.zero)),expr))))

let positive_part expr =
  Location.dummy_annot
    (Alg_expr.BIN_ALG_OP(Operator.MAX,
                         Location.dummy_annot(
                           Alg_expr.CONST (Nbr.zero)),expr))

let dump_token_vector convert logger network_handler token_vector =
  List.iter
    (fun (expr,(id,_)) ->
       let stochiometry_opt =
         eval_const_alg_expr logger network_handler (convert expr)
       in
       match stochiometry_opt with
       | None ->
         let () =
           Printf.printf "%s: Expressions for token consumption/production should be constants \n Cowardly replace it with 0" (Location.to_string (snd expr))
         in
         ()
       | Some x when Nbr.is_zero x -> ()
       | Some x ->
         let s =
           Format.sprintf
             "metaid=\"%s\" species=\"t%i\"%s"
             (meta_id_of_logger logger)
             id
             (if Nbr.is_equal x (Nbr.I 1) then "" else " stoichiometry=\""^(Nbr.to_string x)^"\"")
         in
         let () = single_box ~options:(fun () -> s) logger "speciesReference" in
         ())
    token_vector



let dump_products_of_token_vector logger network_handler token_vector =
  dump_token_vector positive_part logger network_handler token_vector
let dump_reactants_of_token_vector logger network_handler token_vector =
  dump_token_vector negative_part logger network_handler token_vector



let dump_sbml_reaction
    get_rule
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
  let options =
    (fun () -> Format.asprintf
        "id=\"re%i\" name=\"%a\" reversible=\"false\" fast=\"false\"" reaction_id (print_rule_name ?compil) (get_rule enriched_rule))
  in
  let () =
    add_box ~options ~break logger label_reaction
      (fun logger ->
         let () =
           add_box ~break logger label_list_of_reactants
             (fun logger ->
                let () =
                  dump_list_of_species_reference
                    logger reactants
                in
                let () =
                  dump_reactants_of_token_vector
                    logger network token_vector
                in
                ()
             )
         in
         let () =
           add_box ~break logger label_list_of_products
             (fun logger ->
                let () =
                  dump_list_of_species_reference
                    logger products in
                let () =
                  dump_products_of_token_vector
                    logger network token_vector
                in
                ())
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
