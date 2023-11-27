type ode_var_id = int

let int_of_ode_var_id i = i

type variable =
  | Expr of int
  | Init of int
  | Initbis of int
  | Concentration of int
  | Deriv of int
  | Obs of int
  | Jacobian of int * int
  | Jacobian_var of int * int
  | Tinit
  | Tend
  | InitialStep
  | MaxStep
  | RelTol
  | AbsTol
  | Period_t_points
  | Rate of int
  | Rated of int
  | Rateun of int
  | Rateund of int
  | Stochiometric_coef of int * int
  | Jacobian_stochiometric_coef of int * int * int
  | Jacobian_rate of int * int
  | Jacobian_rated of int * int
  | Jacobian_rateun of int * int
  | Jacobian_rateund of int * int
  | N_max_stoc_coef
  | N_rules
  | N_ode_var
  | N_var
  | N_obs
  | N_rows
  | Tmp
  | Current_time
  | Time_scale_factor
  | NonNegative

let string_of_array_name var =
  match var with
  | NonNegative -> "nonnegative"
  | Rate _ -> "k"
  | Rated _ -> "kd"
  | Rateun _ -> "kun"
  | Rateund _ -> "kdun"
  | Stochiometric_coef _ -> "stoc"
  | Expr _ -> "var"
  | Obs _ -> "obs"
  | Init _ -> "init"
  | Initbis _ -> "Init"
  | Concentration _ -> "y"
  | Deriv _ -> "dydt"
  | Jacobian _ -> "jac"
  | Jacobian_var _ -> "jacvar"
  | Jacobian_rate _ -> "jack"
  | Jacobian_rated _ -> "jackd"
  | Jacobian_rateun _ -> "jackun"
  | Jacobian_rateund _ -> "jackund"
  | Jacobian_stochiometric_coef _ -> "jacstoc"
  | Tinit -> "tinit"
  | Tend -> "tend"
  | InitialStep -> "initialstep"
  | MaxStep -> "maxstep"
  | RelTol -> "reltol"
  | AbsTol -> "abstol"
  | Period_t_points -> "period"
  | N_ode_var -> "nodevar"
  | N_var -> "nvar"
  | N_obs -> "nobs"
  | N_rows -> "nrows"
  | N_rules -> "nrules"
  | N_max_stoc_coef -> "max_stoc_coef"
  | Tmp -> "tmp"
  | Current_time -> "t"
  | Time_scale_factor -> "t_correct_dimmension"

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

module VarOrd = struct
  type t = variable

  let compare = compare
end

module VarMap = Map.Make (VarOrd)
module VarSet = Set.Make (VarOrd)

type t = {
  logger: Loggers.t;
  env: (ode_var_id, ode_var_id) Alg_expr.e Locality.annot VarMap.t ref;
  id_map: int StringMap.t ref;
  fresh_meta_id: int ref;
  fresh_reaction_id: int ref;
  fresh_obs_id: int ref;
  const: VarSet.t ref;
  id_of_parameters: string VarMap.t ref;
  dangerous_parameters: VarSet.t ref;
  idset: Mods.StringSet.t ref;
  csv_sep: string;
}

let lift f a = f a.logger
let get_encoding_format t = lift Loggers.get_encoding_format t
let fprintf t = lift Loggers.fprintf t
let print_newline t = lift Loggers.print_newline t
let print_breakable_hint t = lift Loggers.print_breakable_hint t
let flush_buffer t fmt = lift Loggers.flush_buffer t fmt
let flush_logger t = lift Loggers.flush_logger t
let formatter_of_logger t = lift Loggers.formatter_of_logger t
let string_of_un_op t = lift Loggers_string_of_op.string_of_un_op t
let string_of_bin_op t = lift Loggers_string_of_op.string_of_bin_op t
let string_of_compare_op t = lift Loggers_string_of_op.string_of_compare_op t
let string_of_un_bool_op t = lift Loggers_string_of_op.string_of_un_bool_op t
let string_of_bin_bool_op t = lift Loggers_string_of_op.string_of_bin_bool_op t

let extend_logger ~csv_sep logger =
  {
    logger;
    id_map = ref StringMap.empty;
    fresh_meta_id = ref 1;
    fresh_reaction_id = ref 1;
    fresh_obs_id = ref 1;
    env = ref VarMap.empty;
    const = ref VarSet.empty;
    id_of_parameters = ref VarMap.empty;
    dangerous_parameters = ref VarSet.empty;
    idset = ref Mods.StringSet.empty;
    csv_sep;
  }

let odeFileName =
  List.fold_left
    (fun map (key, value) -> Loggers.FormatMap.add key (ref value) map)
    Loggers.FormatMap.empty
    [
      Octave, "ode.m";
      Matlab, "ode.m";
      DOTNET, "network.net";
      SBML, "network.xml";
      Maple, "ode.mws";
      Mathematica, "ode.nb";
    ]

let get_odeFileName backend =
  try Loggers.FormatMap.find backend odeFileName
  with Not_found ->
    let output = ref "" in
    let _ = Loggers.FormatMap.add backend output odeFileName in
    output

let set_odeFileName backend name =
  let reference = get_odeFileName backend in
  reference := name

let set_ode ~mode f = set_odeFileName mode f
let get_ode ~mode = !(get_odeFileName mode)

let string_of_variable_octave var =
  match var with
  | Rate int
  | Rated int
  | Rateun int
  | Rateund int
  | Expr int
  | Obs int
  | Init int
  | Initbis int
  | Concentration int
  | Deriv int ->
    Printf.sprintf "%s(%i)" (string_of_array_name var) int
  | Jacobian_rate (int1, int2)
  | Jacobian_rated (int1, int2)
  | Jacobian_rateun (int1, int2)
  | Jacobian_rateund (int1, int2)
  | Jacobian (int1, int2)
  | Jacobian_var (int1, int2)
  | Stochiometric_coef (int1, int2) ->
    Printf.sprintf "%s(%i,%i)" (string_of_array_name var) int1 int2
  | Jacobian_stochiometric_coef (int1, int2, int3) ->
    Printf.sprintf "%s(%i,%i,%i)" (string_of_array_name var) int1 int2 int3
  | MaxStep | InitialStep | AbsTol | RelTol | Tinit | Tend | NonNegative
  | Period_t_points | N_ode_var | N_var | N_obs | N_rules | N_rows
  | N_max_stoc_coef | Tmp | Current_time | Time_scale_factor ->
    string_of_array_name var

type side = LHS | RHS

let rem_underscore s =
  let l = String.split_on_char '_' s in
  String.concat "" l

let string_of_variable_mathematica ~side var =
  let side_ext =
    match side with
    | LHS -> "_"
    | RHS -> ""
  in
  match var with
  | Rate int
  | Rated int
  | Rateun int
  | Rateund int
  | Obs int
  | Concentration int
  | Deriv int
  | Expr int ->
    Printf.sprintf "%s%i[t%s]" (string_of_array_name var) int side_ext
  | Stochiometric_coef (int1, int2) ->
    Printf.sprintf "%s%i_%i[t%s]" (string_of_array_name var) int1 int2 side_ext
  | Init int | Initbis int ->
    Printf.sprintf "%s%i" (string_of_array_name var) int
  | Jacobian_rate _ | Jacobian_rated _ | Jacobian_rateun _ | Jacobian_rateund _
  | Jacobian _ | Jacobian_var _ | Jacobian_stochiometric_coef _ ->
    ""
  | NonNegative | Tinit | Tend | MaxStep | InitialStep | AbsTol | RelTol
  | Period_t_points | N_ode_var | N_var | N_obs | N_rules | N_rows
  | N_max_stoc_coef | Tmp | Current_time | Time_scale_factor ->
    rem_underscore (string_of_array_name var)

let string_of_variable_maple var =
  match var with
  | Rate int
  | Rated int
  | Rateun int
  | Rateund int
  | Obs int
  | Concentration int
  | Deriv int
  | Expr int ->
    Printf.sprintf "%s%i(t)" (string_of_array_name var) int
  | Init int | Initbis int ->
    Printf.sprintf "%s%i" (string_of_array_name var) int
  | Stochiometric_coef (int1, int2) ->
    Printf.sprintf "%s%i_%i" (string_of_array_name var) int1 int2
  | Jacobian_rate _ | Jacobian_rated _ | Jacobian_rateun _ | Jacobian_rateund _
  | Jacobian _ | Jacobian_var _ | Jacobian_stochiometric_coef _ ->
    ""
  | NonNegative | Tinit | Tend | MaxStep | InitialStep | AbsTol | RelTol
  | Period_t_points | N_ode_var | N_var | N_obs | N_rules | N_rows
  | N_max_stoc_coef | Tmp | Current_time | Time_scale_factor ->
    string_of_array_name var

let string_of_variable ~side loggers variable =
  match Loggers.get_encoding_format loggers.logger with
  | Loggers.Matlab | Loggers.Octave -> string_of_variable_octave variable
  | Loggers.Mathematica -> string_of_variable_mathematica ~side variable
  | Loggers.Maple -> string_of_variable_maple variable
  | Loggers.Matrix | Loggers.HTML_Graph | Loggers.Js_Graph | Loggers.HTML
  | Loggers.HTML_Tabular | Loggers.DOT | Loggers.TXT | Loggers.TXT_Tabular
  | Loggers.XLS | Loggers.SBML | Loggers.DOTNET | Loggers.GEPHI | Loggers.Json
    ->
    ""

let variable_of_derived_variable var id =
  match var with
  | Rate int -> Jacobian_rate (int, id)
  | Rated int -> Jacobian_rated (int, id)
  | Rateun int -> Jacobian_rateun (int, id)
  | Rateund int -> Jacobian_rateund (int, id)
  | Expr int -> Jacobian_var (int, id)
  | Concentration int -> Jacobian (int, id)
  | Stochiometric_coef (int1, int2) ->
    Jacobian_stochiometric_coef (int1, int2, id)
  | NonNegative -> assert false
  | Obs _ -> assert false
  | Init _ -> assert false
  | Initbis _ -> assert false
  | Deriv _ -> assert false
  | Jacobian_rate _ -> assert false
  | Jacobian_rated _ -> assert false
  | Jacobian_rateun _ -> assert false
  | Jacobian_rateund _ -> assert false
  | Jacobian_stochiometric_coef _ -> assert false
  | Jacobian _ -> assert false
  | Jacobian_var _ -> assert false
  | Tinit -> assert false
  | Tend -> assert false
  | MaxStep -> assert false
  | InitialStep -> assert false
  | AbsTol -> assert false
  | RelTol -> assert false
  | Period_t_points -> assert false
  | N_ode_var -> assert false
  | N_var -> assert false
  | N_obs -> assert false
  | N_rules -> assert false
  | N_rows -> assert false
  | N_max_stoc_coef -> assert false
  | Tmp -> assert false
  | Current_time -> assert false
  | Time_scale_factor -> assert false

let get_expr t v = try Some (VarMap.find v !(t.env)) with Not_found -> None

let set_dangerous_global_parameter_name t var =
  t.dangerous_parameters := VarSet.add var !(t.dangerous_parameters)

let forbidden_char c =
  match c with
  | '*' | '+' | '-' | '(' | ')' -> true
  | _ -> false

let has_forbidden_char t string =
  (match get_encoding_format t with
  | DOTNET -> true
  | Matrix | HTML_Graph | Js_Graph | HTML | HTML_Tabular | DOT | TXT
  | TXT_Tabular | XLS | GEPHI | Octave | Matlab | Maple | Mathematica | Json
  | SBML ->
    false)
  &&
  let rec aux k n =
    if k = n then
      false
    else
      forbidden_char string.[k] || aux (k + 1) n
  in
  aux 0 (String.length string)

let flag_dangerous t id string =
  if has_forbidden_char t string then set_dangerous_global_parameter_name t id

let set_expr t v expr =
  let const = Alg_expr.is_constant expr in
  let () =
    if not const then
      t.const := VarSet.remove v !(t.const)
    else
      t.const := VarSet.add v !(t.const)
  in
  t.env := VarMap.add v expr !(t.env)

let is_const t v = VarSet.mem v !(t.const)

let get_fresh_reaction_id t =
  let output = !(t.fresh_reaction_id) in
  let () = t.fresh_reaction_id := succ output in
  output

let get_id_of_global_parameter t var =
  try VarMap.find var !(t.id_of_parameters) with Not_found -> ""

let set_id_of_global_parameter t var id =
  let () = t.id_of_parameters := VarMap.add var id !(t.id_of_parameters) in
  flag_dangerous t var id

let rec allocate_fresh_name t name potential_suffix =
  if Mods.StringSet.mem name !(t.idset) then
    allocate_fresh_name t (name ^ potential_suffix) potential_suffix
  else
    name

let allocate t name =
  let () = t.idset := Mods.StringSet.add name !(t.idset) in
  ()

let is_dangerous_ode_variable t var = VarSet.mem var !(t.dangerous_parameters)
let get_fresh_meta_id logger = Tools.get_ref logger.fresh_meta_id
let get_fresh_obs_id logger = Tools.get_ref logger.fresh_obs_id
let csv_sep logger = logger.csv_sep
let lift t = t.logger
