open Superarg

type count = Embeddings | Occurrences

type t = {
  backend : string ref ;
  rate_convention : string ref ;
  count : string ref ;
  show_reactions : bool ref ;
  compute_jacobian : bool ref ;
  octave_output : string option ref ;
  matlab_output : string option ref ;
  maple_output : string option ref ;
  mathematica_output : string option ref ;
  sbml_output : string option ref ;
  dotnet_output : string option ref ;
  data_file : string option ref ;
  with_symmetries : string ref ;
  show_symmetries : bool ref ;
  views : bool ref ;
  dbonds : bool ref ;
  site_across : bool ref;
  nonnegative : bool ref ;
  show_time_advance : bool ref;
  initial_step : float ref ;
  max_step : float ref ;
  relative_tolerance : float ref ;
  absolute_tolerance : float ref ;
  smash_reactions : bool ref ;
  propagate_constants : bool ref ;
  print_efficiency : bool ref ;
  max_size_for_species : int option ref ;
}

let default : t =
  {
    backend = ref "octave" ;
    rate_convention = ref "Divide_by_nbr_of_autos_in_lhs" ;
    count = ref "Embeddings" ;
    show_reactions = ref true ;
    compute_jacobian = ref true ;
    dotnet_output = ref (Some "network.net") ;
    octave_output = ref (Some "ode.m")  ;
    matlab_output = ref (Some "ode.m") ;
    maple_output = ref (Some "ode.mws") ;
    mathematica_output = ref (Some "ode.nb") ;
    sbml_output = ref (Some "network.xml") ;
    data_file = ref (Some "data.csv") ;
    with_symmetries = ref "None" ;
    show_symmetries = ref false ;
    views = ref true ;
    dbonds = ref true ;
    site_across = ref true ;
    nonnegative = ref false ;
    show_time_advance = ref false ;
    initial_step = ref 0.00001 ;
    max_step = ref 0.02 ;
    absolute_tolerance = ref 0.001 ;
    relative_tolerance = ref 0.001 ;
    smash_reactions = ref false ;
    propagate_constants = ref false ;
    print_efficiency = ref false ;
    max_size_for_species = ref None ;
  }

let options (t :t)  : (Superarg.key * Superarg.spec * Superarg.msg *
                       Superarg.category list * Superarg.level) list =
   [
  "--output",
  Superarg.MultiExt
    ["--dotnet-output",".net";
     "--maple-output",".mws";
     "--mathematica-output",".nb";
     "--matlab-output",".m";
     "--octave-output",".m";
     "--sbml-output",".xml"],
  "Prefix for file name output",
  ["0_data_set";"1_output";"2_semantics";"3_integration_settings";"4_model_reduction";"5_static_analysis";"6_debug_mode"],Normal;
  "--ode-backend",
  Superarg.Choice (
    [ "dotnet", "dotnet (BNGL) backend";
      "maple", "maple backend (in progress)";
      "mathematica", "mathematica backend (in progress)";
      "matlab", "matlab backend";
      "octave", "octave backend";
      "sbml", "sbml backend"],
    ["Dotnet";"DOTNET";"Octave";"OCTAVE";"Matlab";"MATLAB";"Mathematica";"MATHEMATICA";"Maple";"MAPLE";"Sbml";"SBML"],t.backend),
  "Select the backend format",
  ["1_output"],Normal;
  "--dotnet-output",
  Superarg.String_opt t.dotnet_output,
  "ODEs file for dotnet backend",
  ["1_output"],Hidden;
  "--maple-output",
  Superarg.String_opt t.maple_output,
  "ODEs file for maple backend",
  ["1_output"],Hidden;
  "--mathematica-output",
  Superarg.String_opt t.mathematica_output,
  "ODEs file for mathematica backend",
  ["1_output"],Hidden;
  "--matlab-output",
  Superarg.String_opt t.matlab_output,
  "ODEs file for matlab backend",
  ["1_output"],Hidden;
  "--octave-output",
  Superarg.String_opt t.octave_output,
  "ODEs file for octave backend",
  ["1_output"],Hidden;
  "--sbml-output",
  Superarg.String_opt t.sbml_output,
  "ODEs file for sbml backend",
  ["1_output"],Hidden;
  "--propagate-constants",
  Superarg.Bool t.propagate_constants,
  "propagate constants",
  ["1_output"],Hidden;
  "--constant-propagation",
  Superarg.Bool t.propagate_constants,
  "propagate constants",
  ["1_output"],Normal;
  "--rate-convention",
  Superarg.Choice (
    [ "KaSim","do not divide by anything";
      "Divide_by_nbr_of_autos_in_lhs","divide by the number of autos in the lhs of rules";
      "Biochemist","divide by the number of autos in the lhs of rules that induce an auto also in the rhs"],
    ["kasim";"KASIM";"Kasim";"DIVIDE_BY_NBR_OF_AUTOS_IN_LHS";"divide_by_nbr_of_autos_in_lhs";"biobhemist";"BIOCHEMIST"],t.rate_convention),
    "convention for dividing constant rates",
    ["2_semantics"],Normal;
  "--count",
  Superarg.Choice (
    [ "Embeddings","count the number of embeddings of patterns into species";
      "Occurrences","count the number of occurrences of species"],
    ["embeddings";"EMBEDDINGS";"occurrences";"OCCURRENCES"],t.count),
    "tune whether we cound in embeddings or in occurrences",
  ["2_semantics"],Normal;
  "--truncate",
  Superarg.Int_opt t.max_size_for_species,
  "truncate the network by discarding species with size greater than the argument",
  ["2_semantics"],Normal;
  "--max-size-for-species",
  Superarg.Int_opt t.max_size_for_species,
  "truncate the network by discarding species with size greater than the argument",
  ["2_semantics"],Hidden;
  "--show-reactions",
  Superarg.Bool t.show_reactions,
    "Annotate ODEs by the corresponding chemical reactions",
    ["1_output"],Normal ;
  "--smash-reactions",
  Superarg.Bool t.smash_reactions,
  "Gather identical reactions in the ODEs",
  ["1_output";"3_integration_settings"],Normal ;
  "--compute-jacobian",
  Superarg.Bool t.compute_jacobian,
  "Enable/disable the computation of the Jacobian of the ODEs \n\t (not available yet)",
  ["3_integration_settings"],Normal  ;
  "--with-symmetries",
  Superarg.Choice (
    ["None", "no symmetries reduction";
     "Backward", "use the symmetries satisfied by the rules and the algebraic expressions";
     "Forward", "use the symmetries satisfied by the rules and the initial state"],
    ["none";"NONE";"BACKWARD";"backward";"forward";"FORWARD";"true";"TRUE";"True";"false";"FALSE";"False"],
    t.with_symmetries),
    "Tune which kind of bisimulation is used to reduce the set of species",
    ["2_semantics";"4_model_reduction"],Normal;
  "--show-symmetries",
  Superarg.Bool t.show_symmetries,
  "Display the equivalence relations over the sites",
    ["4_model_reduction"],Normal;
    "--views-domain",
  Superarg.Bool t.views,
    "Enable/disable views analysis when detecting symmetric sites",
    ["5_static_analysis"],Expert    ;
  "--double-bonds-domain",
  Superarg.Bool t.dbonds,
  "Enable/disable double bonds analysis when detecting symmetric sites",
  ["5_static_analysis"],Expert   ;
  "--site-across-bonds-domain",
  Superarg.Bool t.site_across ,
  "Enable/disable the analysis of the relation amond the states of sites in
      connected agents",  ["5_static_analysis"],Expert    ;
  "--nonnegative",
  Superarg.Bool t.nonnegative,
  "Enable/disable the correction of negative concentrations in stiff ODE systems",
  ["3_integration_settings"],Normal;
  "--show-time-advance",
  Superarg.Bool t.show_time_advance,
  "Display time advance during numerical integration",
  ["6_debug_mode"],Expert;
  "--initial-step",
  Superarg.Float t.initial_step,
  "Initial integration step",
  ["3_integration_settings"],Normal ;
  "--max-step",
  Superarg.Float t.max_step,
  "Maximum integration step",
  ["3_integration_settings"],Normal;
  "--relative-tolerance",
  Superarg.Float t.relative_tolerance,
  "tolerance to relative rounding errors",
  ["3_integration_settings"],Normal;
  "--absolute-tolerance",
  Superarg.Float t.absolute_tolerance,
  "tolerance to absolute rounding errors",
  ["3_integration_settings"],Normal;
  "--print-efficiency",
  Superarg.Bool t.print_efficiency,
  "prompt CPU time and various datas",
  ["6_debug_mode"],Expert;
]

let get_option options =
  let title = Version.version_kade_full_name in
  let () = SuperargTk.parse ~title options FileNames.input in
  !FileNames.input

let build_kasa_parameters ~called_from t t_common =
  Config.with_views_analysis := !(t.views) ;
  Config.with_parallel_bonds_analysis := !(t.dbonds) ;
  Config.with_site_across_bonds_analysis := !(t.site_across) ;
  Config.trace := t_common.Common_args.debug ;
  Remanent_parameters.get_parameters
    ~called_from
    ()
