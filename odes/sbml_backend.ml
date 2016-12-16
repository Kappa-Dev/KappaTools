let of_bool_op op =
  match op with
  | Operator.AND -> ( && )
  | Operator.OR -> ( || )

let unsome expr_opt =
  match expr_opt
  with
  | None -> Location.dummy_annot (Alg_expr.CONST Nbr.zero)
  | Some expr -> expr

let rec eval_init_alg_expr logger alg_expr =
  match fst alg_expr with
  | Alg_expr.CONST x  -> x
  | Alg_expr.ALG_VAR x ->
    let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Expr x) in
    eval_init_alg_expr logger (unsome expr_opt)
  | Alg_expr.KAPPA_INSTANCE x ->
  let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Init x) in
  eval_init_alg_expr logger (unsome expr_opt)
  | Alg_expr.TOKEN_ID x ->
  let expr_opt = Loggers.get_expr logger (Ode_loggers_sig.Init x) in
  eval_init_alg_expr logger (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) ->
    let expr_opt = Loggers.get_expr logger Ode_loggers_sig.Tend in
    eval_init_alg_expr logger (unsome expr_opt)
  | Alg_expr.STATE_ALG_OP
      ( Operator.CPUTIME
      | Operator.TIME_VAR
      | Operator.EVENT_VAR
      | Operator.EMAX_VAR
      | Operator.NULL_EVENT_VAR ) -> Nbr.zero
  | Alg_expr.BIN_ALG_OP (op, a, b) ->
    Nbr.of_bin_alg_op
      op
      (eval_init_alg_expr logger a)
      (eval_init_alg_expr logger b)
  | Alg_expr.UN_ALG_OP (op, a) ->
    Nbr.of_un_alg_op
      op
      (eval_init_alg_expr logger a)
  | Alg_expr.IF (cond, yes, no) ->
    if eval_init_bool_expr logger cond
    then
      eval_init_alg_expr logger yes
    else
      eval_init_alg_expr logger no
and eval_init_bool_expr logger expr =
  match fst expr with
  | Alg_expr.TRUE -> true
  | Alg_expr.FALSE -> false
  | Alg_expr.COMPARE_OP (op,a,b) ->
    Nbr.of_compare_op op
      (eval_init_alg_expr logger a) (eval_init_alg_expr logger b)
  | Alg_expr.BOOL_OP (op,a,b) ->
    of_bool_op op
      (eval_init_bool_expr logger a)
      (eval_init_bool_expr logger b)

let rec print_alg_expr_in_sbml logger
    (alg_expr:
       (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id)
         Alg_expr.e Location.annot
    ) (network:
         (Ode_loggers_sig.ode_var_id, Ode_loggers_sig.ode_var_id) Network_handler.t)
  =
  match fst alg_expr with
  | Alg_expr.CONST (Nbr.I n)  ->
    Loggers.fprintf logger "<cn type=\"integer\"> %i </cn>" n
  | Alg_expr.CONST (Nbr.I64 n) ->
    Loggers.fprintf logger "<cn type=\"integer\"> %i </cn>" (Int64.to_int n)
  | Alg_expr.CONST (Nbr.F f) ->
    Loggers.fprintf logger "<cn type=\"real\"> %f </cn>" f
  | Alg_expr.ALG_VAR x ->
    begin
      match
        Loggers.get_expr logger (Ode_loggers_sig.Expr x)
      with
      | Some expr ->
        print_alg_expr_in_sbml
          logger
          expr
          network
      | None ->
        Loggers.fprintf logger "<ci>v%i</ci>" (network.Network_handler.int_of_obs x)
    end
  | Alg_expr.KAPPA_INSTANCE x ->
    Loggers.fprintf logger "<ci>y%i</ci>"
      (network.Network_handler.int_of_kappa_instance x)
  | Alg_expr.TOKEN_ID x ->
    Loggers.fprintf logger "<ci>y%i</ci>" (network.Network_handler.int_of_token_id x)
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
and
  print_bool_expr_in_sbml logger cond network =
  match fst cond with
  | Alg_expr.TRUE -> Loggers.fprintf logger "<true/>"
  | Alg_expr.FALSE -> Loggers.fprintf logger "<false/>"
  | Alg_expr.COMPARE_OP (op,a,b) ->
    let () = Loggers.fprintf logger "<apply>" in
    let () = Loggers.fprintf logger "%s"
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


let open_box ?options:(options=fun () -> "") logger label =
  let () = print_sbml logger ("<"^label^(options ())^">") in
  break_sbml logger

let close_box logger label =
  let () = print_sbml logger ("</"^label^">") in
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
  let () = cont logger in
  let () = close_box logger label in
  ()

let break = true

let dump_initial_species loggers name species =
  let expr =
    match Loggers.get_expr loggers (Ode_loggers_sig.Init species) with
    | Some a -> a
    | None -> Location.dummy_annot (Alg_expr.CONST Nbr.zero)
  in
  let concentration = eval_init_alg_expr loggers expr in
  let s =
    Format.sprintf
      " metaid=\"s%i\" id=\"%s\" name=\"%s\" compartment=\"default\" initialAmount=\"%s\""
      species
      name
      name
      (Nbr.to_string concentration)
  in
  let () = open_box ~options:(fun () -> s) loggers "species" in
  let () = line_sbml loggers in
  ()


let dump_species_reference print_chemical_species species_of_species_id compil loggers species i =
  let s =
    Format.asprintf
      " species=\"%a\"%s"
      (fun log id ->
         print_chemical_species ?compil log
           (fst (species_of_species_id id)))
      species
      (if i=1 then "" else "stoichiometry=\""^(string_of_int i))
  in
  let () = open_box ~options:(fun () -> s) loggers "speciesReference" in
  let () = line_sbml loggers in
  ()

let add map id =
  let old =
    match
      Mods.IntMap.find_option id map
    with
    | Some i -> i
    | None -> 0
  in
  Mods.IntMap.add id (succ old) map

let dump_list_of_species_reference
    print_chemical_species
    species_of_species_id
    compil
    loggers
    list
  =
  let map = List.fold_left add Mods.IntMap.empty list in
  List.iter
    (fun (s,i) ->
       dump_species_reference
         print_chemical_species species_of_species_id compil loggers s i)
    (Mods.IntMap.bindings map)

let dump_kinetic_law
    logger compil network print_species species_of_id reactants var_rule correct =
  do_sbml logger
    (fun logger  ->
       begin
  let expr_opt =
    Loggers.get_expr logger var_rule in
  let expr = unsome expr_opt in
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
  match reactants with
  | [] ->
    print_alg_expr_in_sbml logger expr network
  | _::_ ->
      add_box ~break logger "apply"
        (fun logger ->
           let () = Loggers.fprintf logger "<times/>" in
           let () = print_alg_expr_in_sbml logger expr network in
           let rec aux list =
             match list with
               [] -> ()
             | [t] ->
               Loggers.fprintf logger "<ci>%a</ci>"
                 (fun log id ->
                    print_species ?compil log
                      (fst (species_of_id id))) t
             | t::q ->
               add_box ~break logger "apply"
                 (fun logger ->
                    let () = Loggers.fprintf logger "<times/>" in
                    let () = Loggers.fprintf logger "<ci>%a</ci>"
                        (fun log id ->
                           print_species ?compil log
                             (fst (species_of_id id))) t
                    in
                    aux q)
           in aux reactants
        )
       end
    )

let dump_reactants_of_token_vector
    print_chemical_species
    species_of_id
    compil logger token_vector =
  ()

let dump_products_of_token_vector
    print_chemical_species
    species_of_id
    compil logger token_vector =
  ()


let dump_sbml_reaction
    get_rule
    print_rule_name
    print_chemical_species
    species_of_species_id
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
                    print_chemical_species
                    species_of_species_id
                    compil logger reactants in
                let () =
                  dump_reactants_of_token_vector
                    print_chemical_species
                    species_of_species_id
                    compil logger token_vector
                in
                ()
             )
         in
         let () =
           add_box ~break logger label_list_of_products
             (fun logger ->
                let () =
                  dump_list_of_species_reference
                print_chemical_species
                species_of_species_id
                compil logger products in
                let () =
                  dump_products_of_token_vector
                    print_chemical_species
                    species_of_species_id
                    compil logger token_vector
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
                  logger compil network
                  print_chemical_species
                  species_of_species_id reactants var_rule correct)
             )
         in
         ()
      )
  in
  ()
