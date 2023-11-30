(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type preprocessed_ast = {
  conf: Configuration.t;
  story_compression: bool * bool * bool;
  formatCflow: string;
  cflowFile: string option;
  ast_compiled_data: LKappa_compiler.ast_compiled_data;
  overwrite_init:
    (LKappa.rule_mixture, Raw_mixture.t, int) Ast.init_statement list option;
  overwrite_t0: float option;
}

type compilation_result = {
  conf: Configuration.t;
  env: Model.t;
  contact_map: Contact_map.t;
  updated_alg_vars: int list;
  story_compression: (bool * bool * bool) option;
  formatCflow: string;
  cflowFile: string option;
  init_l: (Primitives.alg_expr * Primitives.elementary_rule) list;
  counter_opt: Counter.t option;
}

let preprocess_ast ~warning ~debug_mode ?kasim_args cli_args
    (ast : (_, _, _, _, _) Ast.compil) : preprocessed_ast =
  let () = Format.printf "+ simulation parameters@." in
  let conf, story_compression, formatCflow, cflowFile =
    Configuration.parse ast.Ast.configurations
  in
  let () = Format.printf "+ Sanity checks@." in
  let syntax_version = cli_args.Run_cli_args.syntaxVersion in
  let var_overwrite, initialMix =
    match kasim_args with
    | None -> [], None
    | Some kasim_args ->
      kasim_args.Kasim_args.alg_var_overwrite, kasim_args.Kasim_args.initialMix
  in
  let ast_compiled_data : LKappa_compiler.ast_compiled_data =
    LKappa_compiler.compil_of_ast ~warning ~debug_mode ~syntax_version
      ~var_overwrite ast
  in
  let overwrite_init, overwrite_t0 =
    match initialMix with
    | None -> None, None
    | Some file ->
      let compil =
        match syntax_version with
        | Ast.V4 -> Klexer4.compile Format.std_formatter Ast.empty_compil file
        | Ast.V3 ->
          KappaLexer.compile Format.std_formatter Ast.empty_compil file
      in
      let conf, _, _, _ = Configuration.parse compil.Ast.configurations in
      ( Some
          (LKappa_compiler.init_of_ast ~warning ~syntax_version
             ast_compiled_data.agents_sig ast_compiled_data.contact_map
             ast_compiled_data.token_names.NamedDecls.finder
             ast_compiled_data.alg_vars_finder compil.Ast.init),
        conf.Configuration.initial )
  in
  {
    conf;
    story_compression;
    formatCflow;
    cflowFile;
    ast_compiled_data;
    overwrite_init;
    overwrite_t0;
  }

let get_ast_from_list_of_files syntax_version file_list =
  let compiling_function =
    match syntax_version with
    | Ast.V4 -> Klexer4.compile Format.std_formatter
    | Ast.V3 -> KappaLexer.compile Format.std_formatter
  in
  List.fold_left compiling_function Ast.empty_compil file_list

let get_ast_from_cli_args cli_args =
  get_ast_from_list_of_files cli_args.Run_cli_args.syntaxVersion
    cli_args.Run_cli_args.inputKappaFileNames

let get_preprocessed_ast_from_cli_args ~warning ~debug_mode
    ?(kasim_args = Kasim_args.default) cli_args =
  let ast : (Ast.agent, Ast.mixture, Ast.mixture, string, Ast.rule) Ast.compil =
    get_ast_from_list_of_files cli_args.Run_cli_args.syntaxVersion
      cli_args.Run_cli_args.inputKappaFileNames
  in
  preprocess_ast ~warning ~debug_mode cli_args ~kasim_args ast

type compilation_pack = {
  compilation_result: compilation_result;
  alg_overwrite: (int * Primitives.alg_expr) list;
  overwrite_t0: float option;
}

let get_pack_from_preprocessed_ast kasim_args ~(compile_mode_on : bool)
    (preprocessed_ast : preprocessed_ast) : compilation_pack =
  let n, w, s = preprocessed_ast.story_compression in
  let () = Format.printf "+ Compiling...@." in
  let env, has_tracking, init_l =
    Eval.compile ~outputs:Outputs.go
      ~pause:(fun f -> f ())
      ~return:(fun x -> x)
      ~debug_mode:!Parameter.debug_modeOn ~sharing:kasim_args.Kasim_args.sharing
      ?rescale_init:kasim_args.Kasim_args.rescale
      ?overwrite_init:preprocessed_ast.overwrite_init
      ?overwrite_t0:preprocessed_ast.overwrite_t0 ~compile_mode_on
      preprocessed_ast.ast_compiled_data.agents_sig
      preprocessed_ast.ast_compiled_data.token_names
      preprocessed_ast.ast_compiled_data.contact_map
      preprocessed_ast.ast_compiled_data.result
  in
  let story_compression =
    if has_tracking && (n || w || s) then
      Some preprocessed_ast.story_compression
    else
      None
  in
  {
    compilation_result =
      {
        conf = preprocessed_ast.conf;
        env;
        contact_map = preprocessed_ast.ast_compiled_data.contact_map;
        updated_alg_vars = preprocessed_ast.ast_compiled_data.updated_alg_vars;
        story_compression;
        formatCflow = preprocessed_ast.formatCflow;
        cflowFile = preprocessed_ast.cflowFile;
        init_l;
        counter_opt = None;
      };
    alg_overwrite = [];
    overwrite_t0 = preprocessed_ast.overwrite_t0;
  }

let get_pack_from_marshalizedfile ~warning kasim_args cli_args marshalized_file
    : compilation_pack =
  assert (marshalized_file <> "");
  try
    let d = open_in_bin marshalized_file in
    let () =
      if cli_args.Run_cli_args.inputKappaFileNames <> [] then
        warning ~pos:Loc.dummy (fun f ->
            Format.pp_print_string f
              "Simulation package loaded, all kappa files are ignored")
    in
    let () =
      Format.printf "+ Loading simulation package %s...@." marshalized_file
    in
    let compilation_result : compilation_result = Marshal.from_channel d in
    let () = Stdlib.close_in d in
    let alg_overwrite =
      List.map
        (fun (s, v) ->
          ( Model.num_of_alg (Loc.annot_with_dummy s) compilation_result.env,
            Alg_expr.CONST v ))
        kasim_args.Kasim_args.alg_var_overwrite
    in
    match kasim_args.Kasim_args.initialMix with
    | None -> { compilation_result; alg_overwrite; overwrite_t0 = None }
    | Some file ->
      let compil =
        get_ast_from_list_of_files cli_args.Run_cli_args.syntaxVersion [ file ]
      in
      let overwrite_t0 : float option =
        (Configuration.parse compil.Ast.configurations |> fun (a, _, _, _) -> a)
        |> fun conf -> conf.Configuration.initial
      in

      let raw_inits =
        LKappa_compiler.init_of_ast ~warning
          ~syntax_version:cli_args.Run_cli_args.syntaxVersion
          (Model.signatures compilation_result.env)
          compilation_result.contact_map
          (Model.tokens_finder compilation_result.env)
          (Model.algs_finder compilation_result.env)
          compil.Ast.init
      in
      let init_l =
        Eval.compile_inits ~debug_mode:!Parameter.debug_modeOn ~warning
          ?rescale:kasim_args.Kasim_args.rescale ~compile_mode_on:false
          compilation_result.contact_map compilation_result.env raw_inits
      in
      {
        compilation_result = { compilation_result with init_l };
        alg_overwrite;
        overwrite_t0;
      }
  with
  | ExceptionDefn.Malformed_Decl _ as e -> raise e
  | _exn ->
    Format.printf
      "Simulation package seems to have been created with a different version \
       of KaSim, aborting...";
    exit 1

let get_compilation_from_pack ~warning kasim_args cli_args
    (pack : compilation_pack) : compilation_result =
  let init_t_from_files =
    Option_util.unsome
      (Option_util.unsome 0. pack.compilation_result.conf.Configuration.initial)
      pack.overwrite_t0
  in
  let init_t, max_time, init_e, max_event, plot_period =
    match kasim_args.Kasim_args.unit with
    | Kasim_args.Time ->
      ( Option_util.unsome init_t_from_files cli_args.Run_cli_args.minValue,
        cli_args.Run_cli_args.maxValue,
        None,
        None,
        (match cli_args.Run_cli_args.plotPeriod with
        | Some a -> Configuration.DT a
        | None ->
          Option_util.unsome (Configuration.DT 1.)
            pack.compilation_result.conf.Configuration.plotPeriod) )
    | Kasim_args.Event ->
      ( init_t_from_files,
        None,
        Some
          (int_of_float (Option_util.unsome 0. cli_args.Run_cli_args.minValue)),
        Option_util.map int_of_float cli_args.Run_cli_args.maxValue,
        (match cli_args.Run_cli_args.plotPeriod with
        | Some a -> Configuration.DE (int_of_float (ceil a))
        | None ->
          Option_util.unsome (Configuration.DE 1)
            pack.compilation_result.conf.Configuration.plotPeriod) )
  in
  let counter =
    Counter.create ~init_t ?init_e ?max_time ?max_event ~plot_period
      ~nb_rules:(Model.nb_rules pack.compilation_result.env)
      ()
  in
  let env =
    if
      cli_args.Run_cli_args.batchmode
      && kasim_args.Kasim_args.marshalizeOutFile = None
    then
      Model.propagate_constant ~warning ?max_time:(Counter.max_time counter)
        ?max_events:(Counter.max_events counter)
        ~updated_vars:pack.compilation_result.updated_alg_vars
        ~alg_overwrite:pack.alg_overwrite pack.compilation_result.env
    else
      Model.overwrite_vars pack.alg_overwrite pack.compilation_result.env
  in
  { pack.compilation_result with env; counter_opt = Some counter }

let get_compilation_from_preprocessed_ast ~warning ?(compile_mode_on = false)
    ?(kasim_args = Kasim_args.default) cli_args preprocessed_ast :
    compilation_result =
  let pack =
    get_pack_from_preprocessed_ast kasim_args ~compile_mode_on preprocessed_ast
  in
  get_compilation_from_pack ~warning kasim_args cli_args pack

let get_compilation ~warning ~debug_mode ?(compile_mode_on = false)
    ?(kasim_args = Kasim_args.default) cli_args : compilation_result =
  let (pack : compilation_pack) =
    match kasim_args.Kasim_args.marshalizedInFile with
    | "" ->
      let preprocessed_ast =
        get_preprocessed_ast_from_cli_args ~warning ~debug_mode cli_args
      in
      get_pack_from_preprocessed_ast kasim_args ~compile_mode_on
        preprocessed_ast
    | marshalized_file ->
      get_pack_from_marshalizedfile ~warning kasim_args cli_args
        marshalized_file
  in
  get_compilation_from_pack ~warning kasim_args cli_args pack
