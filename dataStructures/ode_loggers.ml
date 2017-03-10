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
    command_line
    ~may_be_not_time_homogeneous
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
        let () = command_line logger in
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
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>";] in
        let () = command_line logger in
        let () = print_list logger
            ([
              "<sbml xmlns=\"http://www.sbml.org/sbml/level2/version4\" xmlns:celldesigner=\"http://www.sbml.org/2001/ns/celldesigner\" level=\"2\" version=\"4\">";
              "<model name=\"KaDe output:\">";
              "<!--";
              "THINGS THAT ARE KNOWN FROM KAPPA FILE AND KaSim OPTIONS;";
              "";
              "init - the initial abundances of each species and token";
              "tinit - the initial simulation time (likely 0)";
              "tend - the final simulation time ";
              "initialstep - initial time step at the beginning of numerical integration";
              "period_t_point - the time period between points to return";
              "" ;
              ""^
              (match
                 count
               with
               | Ode_args.Embeddings -> "variables denote number of embeddings "
               | Ode_args.Occurrences -> "variables denote numbers occurrences");
              ""^
              (match
                 rate_convention
               with
               | Ode_args.Biochemist ->
                 "rule rates are corrected by the number of automorphisms that induce an automorphism in the rhs as well"
               | Ode_args.Divide_by_nbr_of_autos_in_lhs ->
                 "rule rates are corrected by the number of automorphisms in the lhs of rules"
               | Ode_args.KaSim ->
                 "no correcion is applied on rule rates");
              "-->";
              "<listOfUnitDefinitions>";
              "<unitDefinition metaid=\"substance\" id=\"substance\" name=\"substance\">";
              "<listOfUnits>";
              "<unit metaid=\""^(Sbml_backend.meta_id_of_logger logger)^"\"  kind=\"mole\"/>";
              "</listOfUnits>";
              "</unitDefinition>";
              "<unitDefinition metaid=\"volume\" id=\"volume\" name=\"volume\">";
              "<listOfUnits>";
              "<unit metaid=\""^(Sbml_backend.meta_id_of_logger logger)^"\" kind=\"litre\"/>";
              "</listOfUnits>";
              "</unitDefinition>";]@(if may_be_not_time_homogeneous then
                                       [
              "<unitDefinition metaid=\"time\" id=\"time\" name=\"time\">";
              "<listOfUnits>";
              "<unit metaid=\""^(Sbml_backend.meta_id_of_logger logger)^"\" kind=\"second\"/>";
              "</listOfUnits>";
              "</unitDefinition>";
              "<unitDefinition metaid=\"time_per_substance\" id=\"time_per_substance\" name=\"time_per_substance\">";
              "<listOfUnits>";
              "<unit metaid=\""^(Sbml_backend.meta_id_of_logger logger)^"\" kind=\"second\"/>";
              "<unit metaid=\""^(Sbml_backend.meta_id_of_logger logger)^"\" kind=\"mole\" exponent=\"-1\"/>";
              "</listOfUnits>";
              "</unitDefinition>";] else [])@[
              "</listOfUnitDefinitions>";
              "<listOfCompartments>";
              "<compartment metaid=\"default\" id=\"default\" size=\"1\" units=\"volume\"/>";
              "</listOfCompartments>";

              ])
        in ()
      end
    | Loggers.Maple -> ()
    | Loggers.Json
    | Loggers.DOT
    | Loggers.Matrix
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
  | Loggers.Matrix
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
        | Ode_loggers_sig.Rate _
        | Ode_loggers_sig.Rated _
        | Ode_loggers_sig.Rateun _
        | Ode_loggers_sig.Rateund _ ->
          Loggers.fprintf logger "%s=zeros(nrules,1)"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Jacobian_rate (_,_)
        | Ode_loggers_sig.Jacobian_rateun (_,_)
        | Ode_loggers_sig.Jacobian_rated (_,_)
        | Ode_loggers_sig.Jacobian_rateund (_,_) ->
          Loggers.fprintf logger "%s=zeros(nrules,nodevar)"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Expr _ ->
          Loggers.fprintf logger "%s=zeros(nvar,1);"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Init _ ->
          Loggers.fprintf logger "%s=sparse(nodevar,1);"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Initbis _ ->
          Loggers.fprintf logger "%s=zeros(nodevar,1);"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Concentration _ ->
          Loggers.fprintf logger "%s=zeros(nodevar,1)"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Deriv _ ->
          Loggers.fprintf logger "%s=zeros(nodevar,1);"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Jacobian _ ->
          Loggers.fprintf logger "%s=sparse(nodevar,nodevar);"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Jacobian_var _ ->
              Loggers.fprintf logger "%s=sparse(nvar,nodevar);"
                (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Obs _ ->
          Loggers.fprintf logger "%s=zeros(nobs,1);"
            (Ode_loggers_sig.string_of_array_name variable)
        | Ode_loggers_sig.Tinit
        | Ode_loggers_sig.Tend
        | Ode_loggers_sig.InitialStep
        | Ode_loggers_sig.Period_t_points
        | Ode_loggers_sig.N_ode_var
        | Ode_loggers_sig.N_var
        | Ode_loggers_sig.N_rows
        | Ode_loggers_sig.N_obs
        | Ode_loggers_sig.Current_time
        | Ode_loggers_sig.Time_scale_factor
        | Ode_loggers_sig.N_rules -> ()
        | Ode_loggers_sig.Tmp ->
          Loggers.fprintf logger "%s = zeros(nodevar,1);"
            (Ode_loggers_sig.string_of_array_name variable)
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
        | Ode_loggers_sig.Jacobian _
        | Ode_loggers_sig.Jacobian_var _
        | Ode_loggers_sig.Jacobian_rate (_,_)
        | Ode_loggers_sig.Jacobian_rateun (_,_)
        | Ode_loggers_sig.Jacobian_rated (_,_)
        | Ode_loggers_sig.Jacobian_rateund (_,_)
          -> Loggers.print_newline logger
        | Ode_loggers_sig.Tinit
        | Ode_loggers_sig.Tend
        | Ode_loggers_sig.InitialStep
        | Ode_loggers_sig.N_ode_var
        | Ode_loggers_sig.N_rows
        | Ode_loggers_sig.N_var
        | Ode_loggers_sig.N_obs
        | Ode_loggers_sig.N_rules
        | Ode_loggers_sig.Time_scale_factor
        | Ode_loggers_sig.Current_time
        | Ode_loggers_sig.Period_t_points -> ()
      in
      ()
    end
  | Loggers.Json
  | Loggers.Maple | Loggers.SBML -> ()
  | Loggers.Matrix  | Loggers.DOT | Loggers.HTML_Graph | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML_Tabular | Loggers.TXT
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


type parenthesis_mode =
    Always
  | Never
  | In_sum
  | In_product
  | In_power

let parenthesis_needed ?parenthesis_mode () =
  match parenthesis_mode with
  | Some Never -> false
  | None | Some (Always | In_sum | In_product | In_power) -> true

let parenthesis_needed_in_sum ?parenthesis_mode () =
  match parenthesis_mode with
  | Some (Never | In_sum) -> false
  | None | Some Always | Some In_product | Some In_power-> true

let parenthesis_needed_in_product ?parenthesis_mode () =
  match parenthesis_mode with
  | Some (Never | In_product | In_sum) -> false
  | None | Some (Always | In_power) -> true

let parenthesis_needed_in_power ?parenthesis_mode () =
    match parenthesis_mode with
      | Some (Never | In_product | In_sum | In_power) -> false
      | None | Some Always -> true

let keep_none a b =
  match a with None -> None
             | Some _ -> b
let parenthesis_needed_in_bin_op ?parenthesis_mode bin_op =
  let keep_none a = keep_none parenthesis_mode (Some a) in
  match bin_op with
  | Operator.MULT ->
    parenthesis_needed_in_product ?parenthesis_mode (),
    keep_none In_product,
    keep_none In_product
  | Operator.POW ->
    parenthesis_needed_in_power ?parenthesis_mode (),
    keep_none In_power,
    keep_none In_power
  | Operator.MINUS ->
    parenthesis_needed_in_sum ?parenthesis_mode (),
    keep_none In_sum,
    keep_none Always
  | Operator.SUM ->
    parenthesis_needed_in_sum ?parenthesis_mode (),
    keep_none In_sum,
    keep_none In_sum
  | Operator.DIV ->
    parenthesis_needed_in_product ?parenthesis_mode (),
    keep_none In_product,
    keep_none Always
  | Operator.MIN ->
    parenthesis_needed ?parenthesis_mode (),
    keep_none Never,
    keep_none Never
  | Operator.MODULO ->
    true,
    keep_none Never,
    keep_none Never
  | Operator.MAX ->
    parenthesis_needed ?parenthesis_mode (),
    keep_none Never,
    keep_none Never

let parenthesis_needed_in_un_op ?parenthesis_mode logger op =
  let keep_none a = keep_none parenthesis_mode (Some a) in
  if is_fun logger op then
    false, true, keep_none Never
  else
    true, false, keep_none Always

let parenthesis_needed_in_bool_op ?parenthesis_mode () =
  match parenthesis_mode with
  | None -> true, None
  | Some Never -> false, Some Always
  | Some (In_sum | In_product | In_power | Always) -> true, Some Always

let rec print_alg_expr ?init_mode ?parenthesis_mode string_of_var_id logger alg_expr network_handler  =
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
      | Alg_expr.CONST (Nbr.F f) -> Loggers.fprintf logger "%g" f
      | Alg_expr.ALG_VAR x ->
        Loggers.fprintf
          logger "var(%i)"
          (network_handler.Network_handler.int_of_obs x)
      | Alg_expr.DIFF(x,Alg_expr.Tok id) ->
        Loggers.fprintf
          logger "jac_var(%i,%i)"
          (network_handler.Network_handler.int_of_obs x)
          (network_handler.Network_handler.int_of_token_id id)
      | Alg_expr.DIFF(x,Alg_expr.Mix id) ->
        Loggers.fprintf
          logger "jac_var(%i,%i)"
          (network_handler.Network_handler.int_of_obs x)
          (network_handler.Network_handler.int_of_kappa_instance id)
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
          let parenthesis_needed, mode1, mode2 =
            parenthesis_needed_in_bin_op
              ?parenthesis_mode
              op
          in
          let string_op = string_of_bin_op logger op in
          match bin_op_pos logger op
          with
          | INFIX ->
            let () =
              if parenthesis_needed
              then
                Loggers.fprintf logger "("
            in
            let () =
              print_alg_expr
                ?parenthesis_mode:mode1
                ?init_mode
                string_of_var_id logger a network_handler
            in
            let () = Loggers.fprintf logger "%s" string_op in
            let () =
              print_alg_expr
                ?parenthesis_mode:mode2
                ?init_mode
                string_of_var_id logger b network_handler
            in
            let () =
              if parenthesis_needed
              then
                Loggers.fprintf logger ")" in
            ()
          | PREFIX ->
            let () = Loggers.fprintf logger "%s" string_op in
            let () = Loggers.fprintf logger "(" in
            let () =
              print_alg_expr
                ?parenthesis_mode:mode1 ?init_mode
                string_of_var_id logger a network_handler
            in
            let () = Loggers.fprintf logger "," in
            let () =
              print_alg_expr
                ?parenthesis_mode:mode2 ?init_mode
                string_of_var_id logger b network_handler
            in
            let () = Loggers.fprintf logger ")" in
            ()
          | POSTFIX ->
            let () = Loggers.fprintf logger "(" in
            let () =
              print_alg_expr
                ?parenthesis_mode:mode1 ?init_mode
                string_of_var_id logger a network_handler
            in
            let () = Loggers.fprintf logger "," in
            let () =
              print_alg_expr
                ?parenthesis_mode:mode2 ?init_mode
                string_of_var_id logger b network_handler
            in
            let () = Loggers.fprintf logger ")" in
            let () = Loggers.fprintf logger "%s" string_op in
            ()
        end

      | Alg_expr.UN_ALG_OP (op, a) ->
        let parenthesis_needed_outside,
            parenthesis_needed_inside,
            mode
          =
          parenthesis_needed_in_un_op
            ?parenthesis_mode logger
            op
        in
        let () =
          if parenthesis_needed_outside
          then
            Loggers.fprintf logger "("
        in
        let string_op = Loggers_string_of_op.string_of_un_op logger op in
        let () = Loggers.fprintf logger "%s" string_op in
        let () =
          if parenthesis_needed_inside
          then Loggers.fprintf logger "("
        in
        let () = print_alg_expr ?parenthesis_mode:mode ?init_mode string_of_var_id logger a network_handler in
        let () =
          if parenthesis_needed_inside
          then Loggers.fprintf logger ")"
        in
        let () =
          if parenthesis_needed_outside
          then
            Loggers.fprintf logger ")"
        in
        ()
      | Alg_expr.IF (cond, yes, no) ->
        let mode = keep_none parenthesis_mode (Some Never) in
        let () = Loggers.fprintf logger "merge(" in
        let () = print_bool_expr ?parenthesis_mode:mode ?init_mode string_of_var_id logger cond network_handler in
        let () = Loggers.fprintf logger "," in
        let () = print_alg_expr ?parenthesis_mode:mode ?init_mode string_of_var_id logger yes network_handler in
        let () = Loggers.fprintf logger "," in
        let () = print_alg_expr ?parenthesis_mode:mode ?init_mode string_of_var_id logger no network_handler in
        let () = Loggers.fprintf logger ")" in
            ()
    end
  | Loggers.SBML ->
    let () = Loggers.fprintf logger "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" in
    let () =
      Sbml_backend.print_alg_expr_in_sbml string_of_var_id logger alg_expr
        network_handler in
    let () = Loggers.fprintf logger "</math>" in
    ()
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()
and print_bool_expr ?parenthesis_mode ?init_mode string_of_var_id logger expr network_handler =
 match Loggers.get_encoding_format logger with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      match fst expr with
      | Alg_expr.TRUE -> Loggers.fprintf logger "true"
      | Alg_expr.FALSE -> Loggers.fprintf logger "false"
      | Alg_expr.COMPARE_OP (op,a,b) ->
        let mode = keep_none parenthesis_mode (Some Never) in
        let () =
          print_alg_expr ?parenthesis_mode:mode ?init_mode string_of_var_id logger a network_handler in
        let () = Loggers.fprintf logger "%s" (Loggers_string_of_op.string_of_compare_op logger op) in
        let () = print_alg_expr ?parenthesis_mode:mode ?init_mode string_of_var_id logger b network_handler in
      let () = Loggers.fprintf logger ")" in
        ()
      | Alg_expr.BOOL_OP (op,a,b) ->
        let do_paren, mode =
          parenthesis_needed_in_bool_op ?parenthesis_mode ()
        in
        let () =
          if do_paren then
            Loggers.fprintf logger "("
        in
        let () =
          print_bool_expr
            ?parenthesis_mode:mode ?init_mode string_of_var_id logger a network_handler
        in
        let () = Loggers.fprintf logger "%s" (Loggers_string_of_op.string_of_bool_op logger op) in
        let () = print_bool_expr ?init_mode string_of_var_id logger b network_handler in
        let () =
          if do_paren then
            Loggers.fprintf logger ")"
        in
        ()
    end
  | Loggers.SBML ->
    let () = Loggers.fprintf logger "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" in
    let () = Sbml_backend.print_bool_expr_in_sbml string_of_var_id logger expr network_handler in
    let () = Loggers.fprintf logger "</math>" in ()
  | Loggers.Maple -> ()
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let print_alg_expr_few_parenthesis ?init_mode string_of_var_id logger alg_expr network_handler =
  print_alg_expr
    ?init_mode ?parenthesis_mode:(Some Never)
    string_of_var_id logger
    (Alg_expr_extra.simplify alg_expr)
    network_handler

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
  | Ode_loggers_sig.Jacobian_var _
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
  | Ode_loggers_sig.Time_scale_factor -> "t_correct_dimmension"
  | Ode_loggers_sig.Current_time -> "t"
  | Ode_loggers_sig.Rate int -> Printf.sprintf "k%i" int
  | Ode_loggers_sig.Rated int -> Printf.sprintf "kd%i" int
  | Ode_loggers_sig.Rateun int -> Printf.sprintf "kun%i" int
  | Ode_loggers_sig.Rateund int -> Printf.sprintf "kdun%i" int
  | Ode_loggers_sig.Jacobian_rate (_,_)
  | Ode_loggers_sig.Jacobian_rateun (_,_)
  | Ode_loggers_sig.Jacobian_rated (_,_)
  | Ode_loggers_sig.Jacobian_rateund (_,_) -> ""



  let unit_of_variable_sbml variable =
    match variable with
    | Ode_loggers_sig.Current_time
    | Ode_loggers_sig.Period_t_points
    | Ode_loggers_sig.Tinit
    | Ode_loggers_sig.Tend -> Some "time"
    | Ode_loggers_sig.Time_scale_factor -> Some "time-per-substance"
    | Ode_loggers_sig.Obs _
    | Ode_loggers_sig.Init _
    | Ode_loggers_sig.Concentration _
    | Ode_loggers_sig.Initbis _ -> Some "substance"
    | Ode_loggers_sig.Expr _
    | Ode_loggers_sig.Deriv _
    | Ode_loggers_sig.Jacobian _
    | Ode_loggers_sig.Jacobian_var _
    | Ode_loggers_sig.InitialStep
    | Ode_loggers_sig.Jacobian_rate (_,_)
    | Ode_loggers_sig.Jacobian_rated (_,_)
    | Ode_loggers_sig.Jacobian_rateun (_,_)
    | Ode_loggers_sig.Jacobian_rateund (_,_)
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

let print_sbml_parameters string_of_var_id logger logger_buffer variable expr =
  let unit_string =
    match
      unit_of_variable_sbml variable
    with
    | None -> ""
    | Some x -> " units=\""^x^"\""
  in
  let id = string_of_variable_sbml string_of_var_id variable in
  let () = Loggers.set_id_of_global_parameter logger variable id  in
  Sbml_backend.single_box
    logger_buffer
    "parameter"
    ~options:(fun () ->
        Format.sprintf
          "metaid=\"%s\" id=\"%s\" value=\"%s\"%s"
          (Sbml_backend.meta_id_of_logger logger)
          id
          (Nbr.to_string expr)
          unit_string)


let print_comment
    ?breakline:(breakline=false)
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
        let () = Loggers.fprintf logger "%% %s" string in
        if breakline then Loggers.print_newline logger
      | Loggers.SBML ->
        let () = Loggers.fprintf logger "<!-- %s -->" (Sbml_backend.string_in_comment string) in
        if breakline then
          Loggers.print_newline logger
        else Loggers.print_breakable_hint logger
      | Loggers.Maple
      | Loggers.Json
      | Loggers.DOT
      | Loggers.Matrix | Loggers.HTML_Graph
      | Loggers.HTML
      | Loggers.HTML_Tabular
      | Loggers.TXT
      | Loggers.TXT_Tabular
      | Loggers.XLS -> ()

let associate ?init_mode:(init_mode=false) ?comment:(comment="")
    string_of_var_id logger logger_buffer variable alg_expr network_handler =
  let () = Loggers.set_expr logger variable alg_expr in
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () =
        Loggers.fprintf logger "%s="
          (Ode_loggers_sig.string_of_variable  variable)
      in
      let () = print_alg_expr_few_parenthesis ~init_mode string_of_var_id logger alg_expr network_handler in
      let () = Loggers.fprintf logger ";" in
      let () = if comment = "" then () else Loggers.fprintf logger " " in
      let () = print_comment logger comment in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.SBML ->
    begin
      match variable, init_mode with
      | Ode_loggers_sig.Expr _ , true ->
        begin
          match
            Ode_loggers_sig.is_expr_alias alg_expr,
            Ode_loggers_sig.is_expr_const alg_expr
          with
          | None, true  ->
            print_sbml_parameters
              string_of_var_id
              logger
              logger_buffer
              variable
              (Sbml_backend.eval_init_alg_expr
                 logger
                 network_handler
                 alg_expr)
          | Some _, _ | _, false -> ()
        end
        | (Ode_loggers_sig.Tinit |
         Ode_loggers_sig.Tend |
         Ode_loggers_sig.Period_t_points
        ) ,_ ->
        print_sbml_parameters
          string_of_var_id
          logger
          logger_buffer
          variable
          (Sbml_backend.eval_init_alg_expr logger network_handler alg_expr)
        | Ode_loggers_sig.Rate _,_
        | Ode_loggers_sig.Rated _,_
        | Ode_loggers_sig.Rateun _,_
        | Ode_loggers_sig.Rateund _,_ ->
          if Ode_loggers_sig.is_expr_const alg_expr then
            print_sbml_parameters
              string_of_var_id
              logger
              logger_buffer
              variable
              (Sbml_backend.eval_init_alg_expr logger network_handler alg_expr)
        | Ode_loggers_sig.Jacobian_rate (_,_),_
        | Ode_loggers_sig.Jacobian_rateun (_,_),_
        | Ode_loggers_sig.Jacobian_rated (_,_),_
        | Ode_loggers_sig.Jacobian_rateund (_,_),_
        | Ode_loggers_sig.Expr _ , _
        | Ode_loggers_sig.Init _, _
        | Ode_loggers_sig.Initbis _, _
        | Ode_loggers_sig.Concentration _,_
        | Ode_loggers_sig.Deriv _,_
        | Ode_loggers_sig.Obs _,_
        | Ode_loggers_sig.Jacobian _,_
        | Ode_loggers_sig.Jacobian_var _,_
        | Ode_loggers_sig.InitialStep,_
        | Ode_loggers_sig.N_rules,_
        | Ode_loggers_sig.N_ode_var,_
        | Ode_loggers_sig.N_var,_
        | Ode_loggers_sig.N_obs,_
        | Ode_loggers_sig.N_rows,_
        | Ode_loggers_sig.Tmp,_
        | Ode_loggers_sig.Time_scale_factor,_
        | Ode_loggers_sig.Current_time,_ -> ()
    end
  | Loggers.Maple
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

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
  | Loggers.Matrix | Loggers.HTML_Graph
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
  | Loggers.Matrix | Loggers.HTML_Graph
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
  | Loggers.Matrix | Loggers.HTML_Graph
  | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let increment ?init_mode:(init_mode=false) ?comment:(comment="") string_of_var_id logger variable alg_expr network =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let var = Ode_loggers_sig.string_of_variable variable in
      let () = Loggers.fprintf logger "%s=%s+" var var in
      let () = print_alg_expr ~parenthesis_mode:In_sum ~init_mode string_of_var_id logger alg_expr network in
      let () = Loggers.fprintf logger ";" in
      let () = if comment = "" then () else Loggers.fprintf logger " " in
      let () = print_comment logger comment in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let apply_correct correct var  =
  let var_string = Ode_loggers_sig.string_of_variable var in
  match
    correct
  with
  | Nil | Div 1 | Mul 1 -> var_string
  | Div i -> var_string^"/"^(string_of_int i)
  | Mul i -> (string_of_int i)^"*"^var_string

let apply_empty correct =
  match
    correct
  with
  | Nil | Div 1 | Mul 1 -> ""
  | Div i -> "/"^(string_of_int i)
  | Mul i -> "*"^(string_of_int i)

let apply_correct_expr correct expr =
    match
      correct
    with
    | Nil | Div 1 | Mul 1 -> expr
    | Div i -> Alg_expr.div expr (Alg_expr.int i)
    | Mul i -> Alg_expr.mult (Alg_expr.int i) expr

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
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()


let consume = gen "-"
let produce = gen "+"

let gen_deriv
    string logger var_species ~nauto_in_species ~nauto_in_lhs var_rate var_list dep =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      let () =
        Mods.IntSet.iter
          (fun dt ->
             let var_dt =
               Ode_loggers_sig.string_of_variable
                 (Ode_loggers_sig.variable_of_derived_variable
                    var_species dt)
             in
             let () = Loggers.fprintf logger "%s=%s%s" var_dt var_dt string in
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
             let () =
               Loggers.fprintf logger "%s"
                 (Ode_loggers_sig.string_of_variable
                    (Ode_loggers_sig.variable_of_derived_variable var_rate dt ))
             in
             let () =
               List.iter
                 (fun (var,correct) ->
                    Loggers.fprintf logger "*%s"
                      (apply_correct
                         correct
                         (Ode_loggers_sig.Concentration var)))
                 var_list
             in
             let () = Loggers.fprintf logger ";" in
             let () = Loggers.print_newline logger in
             ()
          )
          dep
      in
      let rec aux tail suffix =
        match tail with
        | [] -> ()
        | (h,correct)::t ->
          begin
            let var_dt =
              Ode_loggers_sig.string_of_variable
                (Ode_loggers_sig.variable_of_derived_variable
                   var_species h)
            in
            let () = Loggers.fprintf logger "%s=%s%s" var_dt var_dt string in
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
            let () =
              Loggers.fprintf logger "%s"
                (Ode_loggers_sig.string_of_variable var_rate)
            in
            let () =
              List.iter
                (fun (var,correct) ->
                   Loggers.fprintf logger "*%s"
                     (apply_correct correct
                        (Ode_loggers_sig.Concentration var)))
                (List.rev suffix)
            in
            let () =
              Loggers.fprintf logger "%s" (apply_empty correct)
            in
            let () =
              List.iter
                (fun (var,correct) ->
                   Loggers.fprintf logger "*%s"
                     (apply_correct correct (Ode_loggers_sig.Concentration var)))
                (List.rev t)
            in
            let () = Loggers.fprintf logger ";" in
            let () = Loggers.print_newline logger in
            aux t ((h,correct)::suffix)
          end
      in aux var_list []
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular
  | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()


let consume_jac = gen_deriv "-"
let produce_jac = gen_deriv "+"
let update_token string_of_var_id logger var_token ~nauto_in_lhs var_rate expr var_list handler =
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
      let () = Loggers.fprintf logger "*" in
      let () =
        print_alg_expr
          ~parenthesis_mode:In_product
          string_of_var_id logger expr handler
      in
      let () = Loggers.fprintf logger ";" in
      let () = Loggers.print_newline logger in
      ()
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()

let update_token_jac ?time_var string_of_var_id logger var_token ~nauto_in_lhs var_rate expr var_list handler dep_rate dep_expr =
  match
    Loggers.get_encoding_format logger
  with
  | Loggers.Matlab  | Loggers.Octave ->
    begin
      (* We differentiate according to the rule rate *)
      let () =
        Mods.IntSet.iter
        (fun dt ->
           let var_dt =
             Ode_loggers_sig.string_of_variable
               (Ode_loggers_sig.variable_of_derived_variable
                  var_token dt)
           in
           let () = Loggers.fprintf logger "%s=%s+" var_dt var_dt in
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
           let () =
             Loggers.fprintf logger "%s"
               (Ode_loggers_sig.string_of_variable
                  (Ode_loggers_sig.variable_of_derived_variable
                     var_rate dt))
           in
           let () =
             List.iter
               (fun (var,correct) ->
                  Loggers.fprintf logger "*%s"
                    (apply_correct correct
                       (Ode_loggers_sig.Concentration var)))
               var_list
           in
           let () = Loggers.fprintf logger "*" in
           let () =
             print_alg_expr
               ~parenthesis_mode:In_product
               string_of_var_id logger expr handler
           in
           let () = Loggers.fprintf logger ";" in
           let () = Loggers.print_newline logger in
           ())
        dep_rate
      in
      let () = (* we differentiate according to the token coefficient *)
        Mods.IntMap.iter
          (fun dt_id dt ->
           let var_dt =
               Ode_loggers_sig.string_of_variable
                 (Ode_loggers_sig.variable_of_derived_variable
                    var_token dt_id)
             in
             let () = Loggers.fprintf logger "%s=%s+" var_dt var_dt in
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
             let () =
               Loggers.fprintf logger "%s"
                 (Ode_loggers_sig.string_of_variable
                    var_rate)
             in
             let () =
               List.iter
                 (fun (var,correct) ->
                    Loggers.fprintf logger "*%s"
                      (apply_correct correct
                         (Ode_loggers_sig.Concentration var)))
                 var_list
             in
             let () = Loggers.fprintf logger "*" in
             let () =
               print_alg_expr
                 ~parenthesis_mode:In_product
                 string_of_var_id logger
                 (Alg_expr_extra.simplify
                    (Alg_expr_extra.diff ?time_var expr dt))
                 handler
             in
             let () = Loggers.fprintf logger ";" in
           let () = Loggers.print_newline logger in
           ())
        dep_expr
      in
      let () =  (* we differentiate according to the species *)
        let rec aux tail suffix =
          match tail with
          | [] -> ()
          | (h,correct)::t ->
            begin
              let var_dt =
                Ode_loggers_sig.string_of_variable
                  (Ode_loggers_sig.variable_of_derived_variable
                     var_token h)
              in
              let () = Loggers.fprintf logger "%s=%s+" var_dt var_dt in
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
              let () =
                Loggers.fprintf logger "%s"
                  (Ode_loggers_sig.string_of_variable
                     var_rate)
              in
              let () =
                List.iter
                  (fun (var,correct) ->
                     Loggers.fprintf logger "*%s"
                       (apply_correct correct
                          (Ode_loggers_sig.Concentration var)))
                  (List.rev suffix)
              in
              let () =
                List.iter
                  (fun (var,correct) ->
                     Loggers.fprintf logger "*%s"
                       (apply_correct correct
                          (Ode_loggers_sig.Concentration var)))
                  (List.rev tail)
              in
              let () = Loggers.fprintf logger "*" in
              let () =
                print_alg_expr
                  ~parenthesis_mode:In_product
                  string_of_var_id logger
                  (Alg_expr_extra.simplify
                     expr)
                  handler
              in
              let () = Loggers.fprintf logger ";" in
              let () = Loggers.print_newline logger in
              aux t ((h,correct)::suffix)
            end
        in aux var_list []
      in
      ()
    end
  | Loggers.Maple
  | Loggers.SBML
  | Loggers.Json
  | Loggers.DOT
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.HTML | Loggers.HTML_Tabular | Loggers.TXT | Loggers.TXT_Tabular | Loggers.XLS -> ()


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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
          "fprintf(fid,'# ')"]
    in
    let () =
      print_list logger
        (List.rev_map
           (fun x -> "fprintf(fid,'"^x^",')")
           (List.rev titles))
    in
    let () =
      print_list logger
        [
          "fprintf(fid,'\\n')";
          "for j=1:n_points";
          "    for i=1:nobs";
          "        fprintf(fid,'%f,',y(j,i));";
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML
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
  | Loggers.Matrix | Loggers.HTML_Graph
  | Loggers.HTML
  | Loggers.HTML_Tabular
  | Loggers.TXT
  | Loggers.TXT_Tabular
  | Loggers.XLS -> ()
