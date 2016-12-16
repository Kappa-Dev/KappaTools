(**
  * graph_loggers.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 22/07/2016
  * Last modification: Time-stamp: <Jul 25 2016>
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type correct = Div of int | Mul of int | Nil

type options =
  | Comment of string

let shall_I_do_it format filter_in filter_out =
  let b1 =
    match
      filter_in
    with
    | None -> true
    | Some l -> List.mem format l
  in
  b1 && (not (List.mem format filter_out))


let print_list logger l =
  List.iter
    (fun s ->
       let () = Loggers.fprintf logger "%s" s in Loggers.print_newline logger)
    l

let print_ode_preamble
    logger
    ~count
    ~rate_convention
    ?filter_in:(filter_in=None) ?filter_out:(filter_out=[])
    ()
  =
  let format = Loggers.get_encoding_format logger in
  if shall_I_do_it format filter_in filter_out
  then
    match
      format
    with
    | Loggers.Matlab  | Loggers.Octave ->
      begin
        let () = print_list logger
            [
              "%% THINGS THAT ARE KNOWN FROM KAPPA FILE AND KaSim OPTIONS;";
              "%% ";
              "%% init - the initial abundances of each species and token";
              "%% tinit - the initial simulation time (likely 0)";
              "%% tend - the final simulation time ";
              "%% initialstep - initial time step at the beginning of numerical integration";
              "%% period_t_point - the time period between points to return";
              "%%" ;
              "%% "^
              (match
                 count
               with
               | Ode_args.Embeddings -> "variables (init(i),y(i)) denote numbers of embeddings "
               | Ode_args.Occurrences -> "variables (init(i),y(i)) denote numbers occurrences");
              "%% "^
              (match
                 rate_convention
               with
               | Ode_args.Biochemist ->
                 "rule rates are corrected by the number of automorphisms that induce an automorphism in the rhs as well"
               | Ode_args.Divide_by_nbr_of_autos_in_lhs ->
                 "rule rates are corrected by the number of automorphisms in the lhs of rules"
               | Ode_args.KaSim ->
                 "no correcion is applied on rule rates")]
            in
        let () = Loggers.print_newline logger in
        ()
      end
    | Loggers.SBML ->
      begin
        let () = print_list logger
            [
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";
              "<sbml xmlns=\"http://www.sbml.org/sbml/level2/version4\" xmlns:celldesigner=\"http://www.sbml.org/2001/ns/celldesigner\" level=\"2\" version=\"4\">";
              "<model metaid=\"untitled\" id=\"\">";
            ]
        in ()
      end
    | Loggers.Maple -> ()
    | Loggers.Json
    | Loggers.DOT
    | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT
    | Loggers.TXT_Tabular | Loggers.XLS -> ()

let declare_global logger string =
  let format = Loggers.get_encoding_format logger in
  let string = Ode_loggers_sig.string_of_array_name string in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () = Loggers.fprintf logger "global %s" string in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.SBML | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let initialize logger variable =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () =
        match variable with
        | Ode_loggers_sig.Rate _ ->
          Loggers.fprintf logger "k=zeros(nrules,1)"
        | Ode_loggers_sig.Rated _ ->
          Loggers.fprintf logger "kd=sparse(nrules,1)"
        | Ode_loggers_sig.Rateun _ ->
          Loggers.fprintf logger "kun=sparse(nrules,1)"
        | Ode_loggers_sig.Rateund _ ->
          Loggers.fprintf logger "kdun=sparse(nrules,1)"
        | Ode_loggers_sig.Expr _ ->
          Loggers.fprintf logger "var=zeros(nvar,1);"
        | Ode_loggers_sig.Init _ ->
          Loggers.fprintf logger "init=sparse(nodevar,1);"
        | Ode_loggers_sig.Initbis _ ->
          Loggers.fprintf logger "Init=zeros(nodevar,1);"
        | Ode_loggers_sig.Concentration _ ->
          Loggers.fprintf logger "y=zeros(nodevar,1)"
        | Ode_loggers_sig.Deriv _ ->
          Loggers.fprintf logger "dydt=zeros(nodevar,1);"
        | Ode_loggers_sig.Jacobian _ ->
          Loggers.fprintf logger "Jac = sparse(nodevar,nodevar);"
        | Ode_loggers_sig.Obs _ ->
          Loggers.fprintf logger "obs = zeros(nobs,1);"
        | Ode_loggers_sig.Tinit
        | Ode_loggers_sig.Tend
        | Ode_loggers_sig.InitialStep
        | Ode_loggers_sig.Period_t_points
        | Ode_loggers_sig.N_ode_var
        | Ode_loggers_sig.N_var
        | Ode_loggers_sig.N_rows
        | Ode_loggers_sig.N_obs
        | Ode_loggers_sig.Current_time
        | Ode_loggers_sig.N_rules -> ()
        | Ode_loggers_sig.Tmp -> Loggers.fprintf logger "tmp = zeros(nodevar,1);"

      in
      let () =
        match variable with
        | Ode_loggers_sig.Tmp
        | Ode_loggers_sig.Rate _
        | Ode_loggers_sig.Rateun _
        | Ode_loggers_sig.Rated _
        | Ode_loggers_sig.Rateund _
        | Ode_loggers_sig.Expr _
        | Ode_loggers_sig.Init _
        | Ode_loggers_sig.Initbis _
        | Ode_loggers_sig.Concentration _
        | Ode_loggers_sig.Deriv _
        | Ode_loggers_sig.Obs _
        | Ode_loggers_sig.Jacobian _ -> Loggers.print_newline logger
        | Ode_loggers_sig.Tinit
        | Ode_loggers_sig.Tend
        | Ode_loggers_sig.InitialStep
        | Ode_loggers_sig.N_ode_var
        | Ode_loggers_sig.N_rows
        | Ode_loggers_sig.N_var
        | Ode_loggers_sig.N_obs
        | Ode_loggers_sig.N_rules
        | Ode_loggers_sig.Current_time
        | Ode_loggers_sig.Period_t_points -> ()
      in
      ()
    end
  | Loggers.Json
  | Loggers.Maple | Loggers.SBML -> ()
  | Loggers.DOT | Loggers.HTML_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS -> ()


let print_newline logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab | Loggers.Octave ->
    Loggers.print_newline logger
  | Loggers.Json
  | Loggers.Maple | Loggers.SBML -> ()
  | Loggers.DOT | Loggers.HTML_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS -> ()

type bin_op_pos = PREFIX | INFIX | POSTFIX

let _ = POSTFIX

let bin_op_pos _logger op =
  match op with
  | Operator.MULT | Operator.POW | Operator.MINUS | Operator.SUM | Operator.DIV  -> INFIX
  | Operator.MIN | Operator.MODULO | Operator.MAX -> PREFIX

let string_of_bin_op _logger op =
  match op with
  | Operator.MULT -> "*"
  | Operator.POW -> "**"
  | Operator.MINUS -> "-"
  | Operator.SUM -> "+"
  | Operator.DIV -> "/"
  | Operator.MIN -> "min"
  | Operator.MODULO -> "mod"
  | Operator.MAX -> "max"

let is_fun _logger op =
  match op with
  | Operator.UMINUS -> false
  | Operator.LOG | Operator.SQRT | Operator.EXP
  | Operator.SINUS | Operator.COSINUS | Operator.TAN
  | Operator.INT-> true


let rec print_alg_expr ?init_mode logger alg_expr network_handler  =
  let var = match init_mode with
    | None -> "y"
    | Some init_mode -> if init_mode then "init" else "y" in
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      match fst alg_expr with
      | Alg_expr.CONST (Nbr.I n)  -> Loggers.fprintf logger "%i" n
      | Alg_expr.CONST (Nbr.I64 n) -> Loggers.fprintf logger "%i" (Int64.to_int n)
      | Alg_expr.CONST (Nbr.F f) -> Loggers.fprintf logger "%f" f
      | Alg_expr.ALG_VAR x ->
        Loggers.fprintf
          logger "var(%i)"
          (network_handler.Network_handler.int_of_obs x)
      | Alg_expr.KAPPA_INSTANCE x ->
        Loggers.fprintf
          logger "%s(%i)" var
          (network_handler.Network_handler.int_of_kappa_instance x)
      | Alg_expr.TOKEN_ID x ->
        Loggers.fprintf
          logger "%s(%i)" var
          (network_handler.Network_handler.int_of_token_id x)
      | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) -> Loggers.fprintf logger "tend"
      | Alg_expr.STATE_ALG_OP (Operator.CPUTIME) -> Loggers.fprintf logger "0"
      | Alg_expr.STATE_ALG_OP (Operator.TIME_VAR) -> Loggers.fprintf logger "t"
      | Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR) -> Loggers.fprintf logger "0"
      | Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR) -> Loggers.fprintf logger "event_max"
      | Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR) -> Loggers.fprintf logger "0"
      | Alg_expr.BIN_ALG_OP (op, a, b) ->
        begin
          let string_op = string_of_bin_op logger op in
          match bin_op_pos logger op
          with
          | INFIX ->
            let () = Loggers.fprintf logger "(" in
            let () = print_alg_expr ?init_mode logger a network_handler in
            let () = Loggers.fprintf logger "%s" string_op in
            let () = print_alg_expr ?init_mode logger b network_handler in
            let () = Loggers.fprintf logger ")" in
            ()
          | PREFIX ->
            let () = Loggers.fprintf logger "%s" string_op in
            let () = Loggers.fprintf logger "(" in
            let () = print_alg_expr ?init_mode logger a network_handler in
            let () = Loggers.fprintf logger "," in
            let () = print_alg_expr ?init_mode logger b network_handler in
            let () = Loggers.fprintf logger ")" in
            ()
          | POSTFIX ->
            let () = Loggers.fprintf logger "(" in
            let () = print_alg_expr ?init_mode logger a network_handler in
            let () = Loggers.fprintf logger "," in
            let () = print_alg_expr ?init_mode logger b network_handler in
            let () = Loggers.fprintf logger ")" in
            let () = Loggers.fprintf logger "%s" string_op in
            ()
        end

      | Alg_expr.UN_ALG_OP (op, a) ->
        let () = Loggers.fprintf logger "(" in
        let string_op = Loggers_string_of_op.string_of_un_op logger op in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = if is_fun logger op then Loggers.fprintf logger "(" in
        let () = print_alg_expr ?init_mode logger a network_handler in
        let () = if is_fun logger op then Loggers.fprintf logger ")" in
        let () = Loggers.fprintf logger ")" in
        ()
      | Alg_expr.IF (cond, yes, no) ->
        let () = Loggers.fprintf logger "merge(" in
        let () = print_bool_expr ?init_mode logger cond network_handler in
        let () = Loggers.fprintf logger "," in
        let () = print_alg_expr ?init_mode logger yes network_handler in
        let () = Loggers.fprintf logger "," in
        let () = print_alg_expr ?init_mode logger no network_handler in
        let () = Loggers.fprintf logger ")" in
            ()
    end
  | Loggers.SBML ->
    let () = Loggers.fprintf logger "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" in
    let () =
      Sbml_backend.print_alg_expr_in_sbml logger alg_expr
        network_handler in
    let () = Loggers.fprintf logger "</math>" in
    ()
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
and print_bool_expr ?init_mode logger expr network_handler =
 match Loggers.get_encoding_format logger with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      match fst expr with
      | Alg_expr.TRUE -> Loggers.fprintf logger "true"
      | Alg_expr.FALSE -> Loggers.fprintf logger "false"
      | Alg_expr.COMPARE_OP (op,a,b) ->
        let () = Loggers.fprintf logger "(" in
        let () =
          print_alg_expr ?init_mode logger a network_handler in
        let () = Loggers.fprintf logger "%s" (Loggers_string_of_op.string_of_compare_op logger op) in
        let () = print_alg_expr ?init_mode logger b network_handler in
        let () = Loggers.fprintf logger ")" in
        ()
      | Alg_expr.BOOL_OP (op,a,b) ->
        let () = Loggers.fprintf logger "(" in
        let () = print_bool_expr ?init_mode logger a network_handler in
        let () = Loggers.fprintf logger "%s" (Loggers_string_of_op.string_of_bool_op logger op) in
        let () = print_bool_expr ?init_mode logger b network_handler in
        let () = Loggers.fprintf logger ")" in
        ()
    end
  | Loggers.SBML ->
    let () = Loggers.fprintf logger "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" in
    let () = Sbml_backend.print_bool_expr_in_sbml logger expr network_handler in
    let () = Loggers.fprintf logger "</math>" in ()
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let string_of_variable_sbml string_of_var_id variable =
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
  | Ode_loggers_sig.Rate _
  | Ode_loggers_sig.Rated _
  | Ode_loggers_sig.Rateun _
  | Ode_loggers_sig.Rateund _
  | Ode_loggers_sig.N_rules
  | Ode_loggers_sig.N_ode_var
  | Ode_loggers_sig.N_var
  | Ode_loggers_sig.N_obs
  | Ode_loggers_sig.N_rows
  | Ode_loggers_sig.Tmp -> Ode_loggers_sig.string_of_variable variable
  | Ode_loggers_sig.Current_time -> "t"

let print_sbml_parameters string_of_var_id logger variable expr =
  Sbml_backend.add_box
    logger
    "parameter"
    (fun logger ->
       Loggers.fprintf
         logger "<id =\"%s\" value=\"%s\">"
         (string_of_variable_sbml string_of_var_id variable)
         (Nbr.to_string expr))

let print_comment
    logger
    ?filter_in:(filter_in=None) ?filter_out:(filter_out=[])
    string
  =
  if string = ""
  then
    ()
  else
    let format = Loggers.get_encoding_format logger in
    if shall_I_do_it format filter_in filter_out
    then
      match
        format
      with
      | Loggers.Matlab
      | Loggers.Octave ->
        Loggers.fprintf logger "%%%s" string
      | Loggers.Maple
      | Loggers.SBML
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph
      | Loggers.HTML
      | Loggers.HTML_Tabular
      | Loggers.TXT
      | Loggers.TXT_Tabular
      | Loggers.XLS -> ()

let associate ?init_mode:(init_mode=false) ?comment:(comment="") string_of_var_id logger variable alg_expr network_handler =
  let () = Loggers.set_expr logger variable alg_expr in
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () = Loggers.fprintf logger "%s=" (Ode_loggers_sig.string_of_variable variable) in
      let () = print_alg_expr ~init_mode logger alg_expr network_handler in
      let () = Loggers.fprintf logger ";" in
      let () = if comment = "" then () else Loggers.fprintf logger " " in
      let () = print_comment logger comment in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.SBML ->
    begin
      match variable, init_mode with
      | Ode_loggers_sig.Expr _ , true
      | (Ode_loggers_sig.Tinit |
         Ode_loggers_sig.Tend |
         Ode_loggers_sig.Period_t_points
        ) ,_ ->
        print_sbml_parameters
          string_of_var_id
          logger
          variable
          (Sbml_backend.eval_init_alg_expr logger alg_expr)
      | Ode_loggers_sig.Expr _ , false
      | Ode_loggers_sig.Init _, _
      | Ode_loggers_sig.Initbis _, _
      | Ode_loggers_sig.Concentration _,_
      | Ode_loggers_sig.Deriv _,_
      | Ode_loggers_sig.Obs _,_
      | Ode_loggers_sig.Jacobian _,_
      | Ode_loggers_sig.InitialStep,_
      | Ode_loggers_sig.Rate _,_
      | Ode_loggers_sig.Rated _,_
      | Ode_loggers_sig.Rateun _,_
      | Ode_loggers_sig.Rateund _,_
      | Ode_loggers_sig.N_rules,_
      | Ode_loggers_sig.N_ode_var,_
      | Ode_loggers_sig.N_var,_
      | Ode_loggers_sig.N_obs,_
      | Ode_loggers_sig.N_rows,_
      | Ode_loggers_sig.Tmp,_
      | Ode_loggers_sig.Current_time,_ -> ()
    end
  | Loggers.Maple
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let associate_nrows logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab | Loggers.Octave ->
    let () = Loggers.fprintf logger "nrows = length(soln.x);" in
    Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let associate_t logger n =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab | Loggers.Octave ->
    let () = Loggers.fprintf logger "t = y(%i);" n in
    Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let init_time logger n =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab | Loggers.Octave ->
    let () = Loggers.fprintf logger "y(%i) = t;" n in
    Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let increment ?init_mode:(init_mode=false) ?comment:(comment="") logger variable alg_expr network =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let var = Ode_loggers_sig.string_of_variable variable in
      let () = Loggers.fprintf logger "%s=%s+(" var var in
      let () = print_alg_expr ~init_mode logger alg_expr network in
      let () = Loggers.fprintf logger ");" in
      let () = if comment = "" then () else Loggers.fprintf logger " " in
      let () = print_comment logger comment in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let apply_correct correct var  =
  let var_string = Ode_loggers_sig.string_of_variable var in
  match
    correct
  with
  | Nil | Div 1 | Mul 1 -> var_string
  | Div i -> var_string^"/"^(string_of_int i)
  | Mul i -> (string_of_int i)^"*"^var_string

let gen string logger var_species ~nauto_in_species ~nauto_in_lhs var_rate var_list =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let var = Ode_loggers_sig.string_of_variable var_species in
      let () = Loggers.fprintf logger "%s=%s%s" var var string in
      let bool =
        if nauto_in_species = 1
        then false
        else
          let () = Loggers.fprintf logger "%i" nauto_in_species in
          true
      in
      let bool =
        if nauto_in_lhs =1
        then
          bool
        else
          let () =
            if bool
            then
              Loggers.fprintf logger "/%i" nauto_in_lhs
            else
              Loggers.fprintf logger "1/%i" nauto_in_lhs
          in
          true
      in
      let () =
        if bool
        then
          Loggers.fprintf logger "*"
      in
      let () = Loggers.fprintf logger "%s" (Ode_loggers_sig.string_of_variable var_rate) in
      let () =
        List.iter
          (fun (var,correct) ->
             Loggers.fprintf logger "*%s"
               (apply_correct correct var))
          var_list
      in
      let () = Loggers.fprintf logger ";" in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()


let consume = gen "-"
let produce = gen "+"

let update_token logger var_token ~nauto_in_lhs var_rate expr var_list handler =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let var = Ode_loggers_sig.string_of_variable var_token in
      let () = Loggers.fprintf logger "%s=%s+" var var in
      let bool =
        if nauto_in_lhs =1
        then
          false
        else
          let () =
            Loggers.fprintf logger "1/%i" nauto_in_lhs
          in
          true
      in
      let () =
        if bool
        then
          Loggers.fprintf logger "*"
      in
      let () = Loggers.fprintf logger "%s" (Ode_loggers_sig.string_of_variable var_rate) in
      let () =
        List.iter
          (fun (var,correct) ->
             Loggers.fprintf logger "*%s" (apply_correct correct var))
          var_list
      in
      let () = Loggers.fprintf logger "*(" in
      let () = print_alg_expr logger expr handler in
      let () = Loggers.fprintf logger ");" in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_options logger =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      print_list logger
        ["options = odeset('RelTol', 1e-3, ...";
         "                 'AbsTol', 1e-3, ...";
         "                 'InitialStep', initialstep, ...";
         (*     "                 'MaxStep', tend, ...";
                "                 'Jacobian', @ode_jacobian);"] *)
         "                 'MaxStep', tend);"]
    in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let start_time logger float =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () = Loggers.fprintf logger "t = %f;" float in
    Loggers.print_newline logger
  | Loggers.SBML
  | Loggers.Maple
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let declare_init ?comment:(comment="") logger i =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      Loggers.fprintf logger
        "Init(%i) = init(%i); " i i
    in
    let () = print_comment logger comment in
    Loggers.print_newline logger
  | Loggers.SBML
  | Loggers.Maple
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let print_license_check logger =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () = print_list logger
        [
          "uiIsOctave = false;";
          "uiIsMatlab = false;";
          "LIC = license('inuse');";
          "for elem = 1:numel(LIC)";
          "    envStr = LIC(elem).feature";
          "    if strcmpi(envStr,'octave')";
          "       LICname=envStr;";
          "       uiIsOctave = true;";
          "       break";
          "    end";
          "    if strcmpi(envStr,'matlab')";
          "       LICname=envStr";
          "       uiIsMatlab = true;";
          "       break";
          "    end";
          "end";
        ]
    in Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let print_integrate logger =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      print_list logger
        [
          "if uiIsMatlab";
          "   soln =  ode15s(@ode_aux,[tinit tend],ode_init(),options);";
          "   soln.y=soln.y';";
          "elseif uiIsOctave";
          "   soln = ode2r(@ode_aux,[tinit tend],ode_init(),options);";
          "end";]
    in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let print_interpolate logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      print_list logger
        [
          "n_points = floor ((tend-tinit)/period_t_point)+1;";
          "t = linspace(tinit, tend, n_points);";
          "obs = zeros(nrows,nobs);";
          "";
          "for j=1:nrows";
          "    for i=1:nodevar";
          "        z(i)=soln.y(j,i);";
          "    end";
          "    h=ode_obs(z);";
          "    for i=1:nobs";
          "        obs(j,i)=h(i);";
          "    end";
          "end";
          "if nobs==1";
          "   y = interp1(soln.x, obs, t, 'pchip')';";
          "else";
          "   y = interp1(soln.x, obs, t, 'pchip');";
          "end"
        ]
    in Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let print_dump_plots ~data_file ~command_line ~titles logger  =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      print_list logger
        [
          "filename = '"^data_file^"';";
          "fid = fopen (filename,'w');";
          "fprintf(fid,'# "^command_line^"\\n')";
          "fprintf(fid,'# t')"]
    in
    let () =
      print_list logger
        (List.rev_map
           (fun x -> "fprintf(fid,',"^x^"')")
           (List.rev titles))
    in
    let () =
      print_list logger
        [
          "fprintf(fid,'\\n')";
          "for j=1:n_points";
          "    fprintf(fid,'%f',t(j));";
          "    for i=1:nobs";
          "        fprintf(fid,',%f',y(j,i));";
          "    end";
          "    fprintf(fid,'\\n');";
          "end";
          "fclose(fid);"]
    in Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()


let open_procedure logger name name' arg =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () = Loggers.fprintf logger "function %s=%s(" name name' in
    let _ =
      List.fold_left
        (fun bool s ->
           let () =
             Loggers.fprintf logger "%s%s" (if bool then "," else "") s
           in true)
        false
        arg
    in
    let () = Loggers.fprintf logger ")" in
    Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let return _ _  = ()
let close_procedure logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () = Loggers.fprintf logger "end" in
    Loggers.print_newline logger
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let launch_main logger =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Octave ->
    let () = Loggers.fprintf logger "main();" in
    Loggers.print_newline logger
  | Loggers.SBML ->
    let () = Loggers.fprintf logger "</model>" in
    let () = Loggers.print_newline logger in
    let () = Loggers.fprintf logger "</sbml>" in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Matlab
  | Loggers.Maple
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()
