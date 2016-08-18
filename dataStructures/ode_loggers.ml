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

type variable =
  | Expr of int
  | Init of int
  | Initbis of int
  | Concentration of int
  | Deriv of int
  | Obs of int
  | Jacobian of int * int
  | Tinit
  | Tend
  | InitialStep
  | Num_t_points
  | Rate of int
  | Rated of int
  | Rateun of int
  | Rateund of int
  | N_rules
  | N_ode_var
  | N_var
  | N_obs
  | N_rows
  | Tmp
  | Current_time

type correct = Div of int | Mul of int | Nil

type ('a,'b) network_handler =
  {
    int_of_obs: 'b -> int;
    int_of_kappa_instance:  'a -> int;
    int_of_token_id: 'b -> int;
  }
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
              "%% num_t_point - the number of time points to return"] in
        let () = Loggers.print_newline logger in
        ()
      end
    | Loggers.Maple -> ()
    | Loggers.Json
    | Loggers.DOT
    | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT
    | Loggers.TXT_Tabular | Loggers.XLS -> ()

let string_of_variable var =
  match var with
  | Rate int -> Printf.sprintf "k(%i)" int
  | Rated int -> Printf.sprintf "kd(%i)" int
  | Rateun int -> Printf.sprintf "kun(%i)" int
  | Rateund int -> Printf.sprintf "kdun(%i)" int
  | Expr int -> Printf.sprintf "var(%i)" int
  | Obs int -> Printf.sprintf "obs(%i)" int
  | Init int -> Printf.sprintf "init(%i)" int
  | Initbis int -> Printf.sprintf "Init(%i)" int
  | Concentration int -> Printf.sprintf "y(%i)" int
  | Deriv int -> Printf.sprintf "dydt(%i)" int
  | Jacobian (int1,int2) -> Printf.sprintf "Jac(%i,%i)" int1 int2
  | Tinit -> "tinit"
  | Tend -> "tend"
  | InitialStep -> "initialstep"
  | Num_t_points -> "num_t_point"
  | N_ode_var -> "nodevar"
  | N_var -> "nvar"
  | N_obs -> "nobs"
  | N_rules -> "nrules"
  | N_rows -> "nrows"
  | Tmp -> "tmp"
  | Current_time -> "t"

let string_of_array_name var =
  match var with
  | Rate _ -> "k"
  | Rated _ -> "kd"
  | Rateun _ -> "kun"
  | Rateund _ -> "kdun"
  | Expr _ -> "var"
  | Obs _ -> "obs"
  | Init _ -> "init"
  | Initbis _ -> "Init"
  | Concentration _ -> "y"
  | Deriv _ -> "dydt"
  | Jacobian _ -> "Jac"
  | Tinit -> "tinit"
  | Tend -> "tend"
  | InitialStep -> "initialstep"
  | Num_t_points -> "num_t_point"
  | N_ode_var -> "nodevar"
  | N_var -> "nvar"
  | N_obs -> "nobs"
  | N_rows -> "nrows"
  | N_rules -> "nrules"
  | Tmp -> "tmp"
  | Current_time -> "t"

let declare_global logger string =
  let format = Loggers.get_encoding_format logger in
  let string = string_of_array_name string in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () = Loggers.fprintf logger "global %s" string in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple -> ()
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
        | Rate _ -> Loggers.fprintf logger "k=zeros(nrules,1)"
        | Rated _ -> Loggers.fprintf logger "kd=sparse(nrules,1)"
        | Rateun _ -> Loggers.fprintf logger "kun=sparse(nrules,1)"
        | Rateund _ -> Loggers.fprintf logger "kdun=sparse(nrules,1)"
        | Expr _ -> Loggers.fprintf logger "var=zeros(nvar,1);"
        | Init _ -> Loggers.fprintf logger "init=sparse(nodevar,1);"
        | Initbis _ -> Loggers.fprintf logger "Init=zeros(nodevar,1);"
        | Concentration _ -> Loggers.fprintf logger "y=zeros(nodevar,1)"
        | Deriv _ -> Loggers.fprintf logger "dydt=zeros(nodevar,1);"
        | Jacobian _ -> Loggers.fprintf logger "Jac = sparse(nodevar,nodevar);"
        | Obs _ -> Loggers.fprintf logger "obs = zeros(nobs,1);"
        | Tinit
        | Tend
        | InitialStep
        | Num_t_points
        | N_ode_var
        | N_var
        | N_rows
        | N_obs
        | Current_time
        | N_rules -> ()
        | Tmp -> Loggers.fprintf logger "tmp = zeros(nodevar,1);"

      in
      let () =
        match variable with
        | Tmp | Rate _ | Rateun _ | Rated _ | Rateund _
        | Expr _
        | Init _
        | Initbis _
        | Concentration _
        | Deriv _
        | Obs _
        | Jacobian _ -> Loggers.print_newline logger
        | Tinit
        | Tend
        | InitialStep
        | N_ode_var
        | N_rows
        | N_var
        | N_obs
        | N_rules
        | Current_time
        | Num_t_points -> ()
      in
      ()
    end
  | Loggers.Json
  | Loggers.Maple -> ()
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

let string_of_un_op _logger op =
  match op with
  | Operator.UMINUS-> "-"
  | Operator.LOG -> "log"
  | Operator.SQRT -> "sqrt"
  | Operator.EXP -> "exp"
  | Operator.SINUS -> "sin"
  | Operator.COSINUS -> "cos"
  | Operator.TAN -> "tan"
  | Operator.INT -> "floor"

let rec print_alg_expr init_mode logger  alg_expr network
  =
  let var = if init_mode then "init" else "y" in
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
        Loggers.fprintf logger "var(%i)" (network.int_of_obs x)
      | Alg_expr.KAPPA_INSTANCE x ->
        Loggers.fprintf logger "%s(%i)" var (network.int_of_kappa_instance x)
      | Alg_expr.TOKEN_ID x -> Loggers.fprintf logger "%s(%i)" var (network.int_of_token_id x)
      | Alg_expr.STATE_ALG_OP (Operator.TMAX_VAR) -> Loggers.fprintf logger "tend"
      | Alg_expr.STATE_ALG_OP (Operator.CPUTIME) -> Loggers.fprintf logger "0"
      | Alg_expr.STATE_ALG_OP (Operator.TIME_VAR) -> Loggers.fprintf logger "t"
      | Alg_expr.STATE_ALG_OP (Operator.EVENT_VAR) -> Loggers.fprintf logger "0"
      | Alg_expr.STATE_ALG_OP (Operator.EMAX_VAR) -> Loggers.fprintf logger "event_max"
      | Alg_expr.STATE_ALG_OP (Operator.PLOTNUM) -> Loggers.fprintf logger "num_t_point"
      | Alg_expr.STATE_ALG_OP (Operator.NULL_EVENT_VAR) -> Loggers.fprintf logger "0"
      | Alg_expr.BIN_ALG_OP (op, a, b) ->
        begin
          let string_op = string_of_bin_op logger op in
          match bin_op_pos logger op
          with
          | INFIX ->
            let () = Loggers.fprintf logger "(" in
            let () = print_alg_expr init_mode logger a network in
            let () = Loggers.fprintf logger "%s" string_op in
            let () = print_alg_expr init_mode logger b network in
            let () = Loggers.fprintf logger ")" in
            ()
          | PREFIX ->
            let () = Loggers.fprintf logger "%s" string_op in
            let () = Loggers.fprintf logger "(" in
            let () = print_alg_expr init_mode logger a network in
            let () = Loggers.fprintf logger "," in
            let () = print_alg_expr init_mode logger b network in
            let () = Loggers.fprintf logger ")" in
            ()
          | POSTFIX ->
            let () = Loggers.fprintf logger "(" in
            let () = print_alg_expr init_mode logger a network in
            let () = Loggers.fprintf logger "," in
            let () = print_alg_expr init_mode logger b network in
            let () = Loggers.fprintf logger ")" in
            let () = Loggers.fprintf logger "%s" string_op in
            ()
        end

      | Alg_expr.UN_ALG_OP (op, a) ->
        let () = Loggers.fprintf logger "(" in
        let string_op = string_of_un_op logger op in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = if is_fun logger op then Loggers.fprintf logger "(" in
        let () = print_alg_expr init_mode logger a network in
        let () = if is_fun logger op then Loggers.fprintf logger ")" in
        let () = Loggers.fprintf logger ")" in
        ()
    end
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_alg_expr ?init_mode:(init_mode=false) logger  alg_expr network
  = print_alg_expr init_mode logger alg_expr network

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
      | Loggers.Octave -> Loggers.fprintf logger "%%%s" string
      | Loggers.Maple -> ()
      | Loggers.Json
      | Loggers.DOT
      | Loggers.HTML_Graph
      | Loggers.HTML
      | Loggers.HTML_Tabular
      | Loggers.TXT
      | Loggers.TXT_Tabular
      | Loggers.XLS -> ()

let associate ?init_mode:(init_mode=false) ?comment:(comment="") logger variable alg_expr network =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () = Loggers.fprintf logger "%s=" (string_of_variable variable) in
      let () = print_alg_expr ~init_mode logger alg_expr network in
      let () = Loggers.fprintf logger ";" in
      let () = if comment = "" then () else Loggers.fprintf logger " " in
      let () = print_comment logger comment in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
      let var = string_of_variable variable in
      let () = Loggers.fprintf logger "%s=%s+(" var var in
      let () = print_alg_expr ~init_mode logger alg_expr network in
      let () = Loggers.fprintf logger ");" in
      let () = if comment = "" then () else Loggers.fprintf logger " " in
      let () = print_comment logger comment in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let apply_correct correct var  =
  let var_string = string_of_variable var in
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
      let var = string_of_variable var_species in
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
      let () = Loggers.fprintf logger "%s" (string_of_variable var_rate) in
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
  | Loggers.Maple -> ()
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
      let var = string_of_variable var_token in
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
      let () = Loggers.fprintf logger "%s" (string_of_variable var_rate) in
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
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let start_time logger float =
  let () = Loggers.fprintf logger "t = %f;" float in
  Loggers.print_newline logger

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
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
          "t = linspace(tinit, tend, num_t_point+1);";
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
  | Loggers.Maple -> ()
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
           (fun x -> "fprintf(fid,' "^x^"')")
           (List.rev titles))
    in
    let () =
      print_list logger
        [
          "fprintf(fid,'\\n')";
          "for j=1:num_t_point+1";
          "    fprintf(fid,'%f',t(j));";
          "    for i=1:nobs";
          "        fprintf(fid,' %f',y(j,i));";
          "    end";
          "    fprintf(fid,'\\n');";
          "end";
          "fclose(fid);"]
    in Loggers.print_newline logger
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
  | Loggers.Maple -> ()
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
