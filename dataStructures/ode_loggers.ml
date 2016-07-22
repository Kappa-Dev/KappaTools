(**
  * graph_loggers.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 23/05/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type variable =
  | Expr of int
  | Init of int
  | Deriv of int
  | Jacobian of int * int
  | Tinit
  | Tend
  | InitialStep
  | Num_t_points
  | Rate of int
  | Unary_rate of int

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
              "function main=()";
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
    | Loggers.DOT
    | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT
    | Loggers.TXT_Tabular | Loggers.XLS -> ()

let declare_global logger string =
  let format = Loggers.get_encoding_format logger in

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
        | Rate int -> Loggers.fprintf logger "k= sparse(%i,2)" int
        | Unary_rate int -> Loggers.fprintf logger "k= sparse(%i,2)" int
        | Expr int -> Loggers.fprintf logger "var=zeros(%i,1);" int
        | Init int -> Loggers.fprintf logger "init=zeros(%i,1);" int
        | Deriv int -> Loggers.fprintf logger "dydt=zeros(%i,1);" int
        | Jacobian (int1,int2) -> Loggers.fprintf logger "Jac = sparse(%i,%i);" int1 int2
        | Tinit
        | Tend
        | InitialStep
        | Num_t_points -> ()
      in
      let () =
        match variable with
        | Rate _ | Unary_rate _
        | Expr _
        | Init _
        | Deriv _
        | Jacobian _ -> Loggers.print_newline logger
        | Tinit
        | Tend
        | InitialStep
        | Num_t_points -> ()
      in
      ()
    end
  | Loggers.Maple -> ()
  | Loggers.DOT | Loggers.HTML_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.TXT
  | Loggers.TXT_Tabular | Loggers.XLS -> ()

let rec print_alg_expr logger alg_expr network =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      match alg_expr with
      | Ast.CONST (Nbr.I n)  -> Loggers.fprintf logger "%i" n
      | Ast.CONST (Nbr.I64 n) -> Loggers.fprintf logger "%i" (Int64.to_int n)
      | Ast.CONST (Nbr.F f) -> Loggers.fprintf logger "%f" f
      | Ast.OBS_VAR x -> Loggers.fprintf logger "k(%i)" (network.int_of_obs x)
      | Ast.KAPPA_INSTANCE x -> Loggers.fprintf logger "y(%i)" (network.int_of_kappa_instance x)
      | Ast.TOKEN_ID x -> Loggers.fprintf logger "y(%i)" (network.int_of_token_id x)
      | Ast.STATE_ALG_OP (Operator.TMAX_VAR) -> Loggers.fprintf logger "tend"
      | Ast.STATE_ALG_OP (Operator.CPUTIME) -> Loggers.fprintf logger "0"
      | Ast.STATE_ALG_OP (Operator.TIME_VAR) -> Loggers.fprintf logger "t"
      | Ast.STATE_ALG_OP (Operator.EVENT_VAR) -> Loggers.fprintf logger "0"
      | Ast.STATE_ALG_OP (Operator.EMAX_VAR) -> Loggers.fprintf logger "event_max"
      | Ast.STATE_ALG_OP (Operator.PLOTNUM) -> Loggers.fprintf logger "num_t_point"
      | Ast.STATE_ALG_OP (Operator.NULL_EVENT_VAR) -> Loggers.fprintf logger "0"
      | Ast.BIN_ALG_OP (op, (a,_), (b,_)) ->
        let () = Loggers.fprintf logger "(" in
        let () = print_alg_expr logger a network in
        let string_op =
          match op with
            _ -> "todo"
        in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = print_alg_expr logger b network in
        let () = Loggers.fprintf logger ")" in
        ()
      | Ast.UN_ALG_OP (op, (a,_)) ->
        let () = Loggers.fprintf logger "(" in
        let string_op =
          match op with
            _ -> "todo"
        in
        let () = Loggers.fprintf logger "%s" string_op in
        let () = print_alg_expr logger a network in
        let () = Loggers.fprintf logger ")" in
        ()
    end
  | Loggers.Maple -> ()
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()


let string_of_variable var =
  match var with
  | Rate int -> Printf.sprintf "k(%i,1)" int
  | Unary_rate int -> Printf.sprintf "k(%i,2)" int
  | Expr int -> Printf.sprintf "var(%i)" int
  | Init int -> Printf.sprintf "init(%i)" int
  | Deriv int -> Printf.sprintf "dydt(%i)" int
  | Jacobian (int1,int2) -> Printf.sprintf "Jac(%i,%i)" int1 int2
  | Tinit -> Printf.sprintf "tinit"
  | Tend -> Printf.sprintf "tend"
  | InitialStep -> Printf.sprintf "initialstep"
  | Num_t_points -> Printf.sprintf "num_t_point"

let associate logger variable alg_expr network =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () = Loggers.fprintf logger "%s=" (string_of_variable variable) in
      let () = print_alg_expr logger alg_expr network in
      let () = Loggers.fprintf logger ";" in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple -> ()
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let increment logger variable alg_expr network =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let var = string_of_variable variable in
      let () = Loggers.fprintf logger "%s=%s+(" var var in
      let () = print_alg_expr logger alg_expr network in
      let () = Loggers.fprintf logger ");" in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple -> ()
  | Loggers.DOT
  | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_comment
    logger
    ?filter_in:(filter_in=None) ?filter_out:(filter_out=[])
    string
  =
  let format = Loggers.get_encoding_format logger in
  if shall_I_do_it format filter_in filter_out
  then
    match
      format
    with
    | Loggers.Matlab
    | Loggers.Octave -> Loggers.fprintf logger "%%%s" string
    | Loggers.Maple -> ()
    | Loggers.DOT
    | Loggers.HTML_Graph
    | Loggers.HTML
    | Loggers.HTML_Tabular
    | Loggers.TXT
    | Loggers.TXT_Tabular
    | Loggers.XLS -> ()



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
         "                 'MaxStep', tend, ...";
         "                 'Jacobian', @ode_jacobian);"] in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Maple -> ()
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
          "end";]
    in Loggers.print_newline logger
  | Loggers.Maple -> ()
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
          "   soln = ode2r(@mode_aux,[tinit tend],ode_init(),options);";
          "end";]
    in
    let () = Loggers.print_newline logger in
    ()
  | Loggers.Maple -> ()
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let print_interpolate logger nobs nvar =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      print_list logger
        [
          "t = linspace(tinit, tend, num_t_point+1);";
          "nrows = length(soln.x);";
          "nobs = "^(string_of_int nobs)^";";
          "nfragments ="^(string_of_int nvar)^";";
          "tmp = zeros(nvar,1)";
          "obs = zeros (nrows,nobs);";
          "";
          "for j=1:nrows";
          "    for i=1:nvar";
          "        z(i)=soln.y(j,i);";
          "    end";
          "    h=ode_obs(z);";
          "    for i=1:nobs";
          "        obs(j,i)=h(i);";
          "    end";
          "end";
          "if nobs==1";
          "   y = interp1(soln.x, obs, t, 'pchip')';";
          "else   y = interp1(soln.x, obs, t, 'pchip');";
          "end"
        ]
    in Loggers.print_newline logger
  | Loggers.Maple -> ()
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let print_dump_plots logger  =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () =
      print_list logger
        [
          "filename = 'matlab_kappa.data';";
          "fid = fopen (filename,'w');";
          "for j=1:num_t_point+1";
          "    fprintf(fid,'%f',t(j));";
          "    for i=1:nobs";
          "        fprintf(fid,' %f',y(j,i));";
          "    end";
          "    fprintf(fid,'\n');";
          "end";
          "fclose(fid);"]
    in Loggers.print_newline logger
  | Loggers.Maple -> ()
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
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()

let return _ _  = ()
let close_procedure logger =
  let format = Loggers.get_encoding_format logger in
  match
    format
  with
  | Loggers.Matlab
  | Loggers.Octave ->
    let () = Loggers.fprintf logger "end" in
    Loggers.print_newline logger
  | Loggers.Maple -> ()
  | Loggers.DOT
  | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()
