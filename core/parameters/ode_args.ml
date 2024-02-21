open Superarg

type count = Embeddings | Occurrences

type t = {
  backend: string ref;
  rule_rate_convention: string ref;
  reaction_rate_convention: string ref;
  count: string ref;
  internal_meaning: string ref;
  show_reactions: bool ref;
  compute_jacobian: bool ref;
  octave_output: string option ref;
  matlab_output: string option ref;
  maple_output: string option ref;
  mathematica_output: string option ref;
  sbml_output: string option ref;
  dotnet_output: string option ref;
  data_file: string option ref;
  with_symmetries: string ref;
  show_symmetries: bool ref;
  views: bool ref;
  dbonds: bool ref;
  site_across: bool ref;
  nonnegative: bool ref;
  show_time_advance: bool ref;
  initial_step: float ref;
  max_step: float ref;
  relative_tolerance: float ref;
  absolute_tolerance: float ref;
  smash_reactions: bool ref;
  propagate_constants: bool ref;
  print_efficiency: bool ref;
  max_size_for_species: int option ref;
  csv_sep: string ref;
}

let default : t =
  {
    backend = ref "octave";
    rule_rate_convention = ref "Divide_by_nbr_of_autos_in_lhs";
    reaction_rate_convention = ref "Divide_by_nbr_of_autos_in_lhs";
    count = ref "Embeddings";
    internal_meaning = ref "Embeddings";
    show_reactions = ref true;
    compute_jacobian = ref true;
    dotnet_output = ref (Some "network.net");
    octave_output = ref (Some "ode.m");
    matlab_output = ref (Some "ode.m");
    maple_output = ref (Some "ode.mws");
    mathematica_output = ref (Some "ode.nb");
    sbml_output = ref (Some "network.xml");
    data_file = ref (Some "data.csv");
    with_symmetries = ref "None";
    show_symmetries = ref false;
    views = ref true;
    dbonds = ref true;
    site_across = ref true;
    nonnegative = ref false;
    show_time_advance = ref false;
    initial_step = ref 0.00001;
    max_step = ref 0.02;
    absolute_tolerance = ref 0.001;
    relative_tolerance = ref 0.001;
    smash_reactions = ref false;
    propagate_constants = ref false;
    print_efficiency = ref false;
    max_size_for_species = ref None;
    csv_sep = ref " ";
  }

let combine l1 l2 =
  List.fold_left
    (fun list a1 ->
      List.fold_left (fun list (a2 : int) -> (a1, a2) :: list) list l2)
    [] l1

let options (t : t) :
    (Superarg.key
    * Superarg.spec
    * Superarg.msg
    * (Superarg.category * Superarg.position) list
    * Superarg.level)
    list =
  [
    ( "--void",
      Superarg.Void,
      "",
      combine
        [
          Common_args.data_set;
          Common_args.output;
          Common_args.semantics;
          Common_args.integration_settings;
          Common_args.model_reduction;
          Common_args.static_analysis;
          Common_args.debug_mode;
        ]
        [ 50; 51; 52 ],
      Normal );
    ( "--output",
      Superarg.MultiExt
        [
          "--dotnet-output", ".net";
          "--maple-output", ".mws";
          "--mathematica-output", ".nb";
          "--matlab-output", ".m";
          "--octave-output", ".m";
          "--sbml-output", ".xml";
        ],
      "Prefix for file name output",
      [
        Common_args.data_set, 101;
        Common_args.output, 101;
        Common_args.semantics, 101;
        Common_args.integration_settings, 101;
        Common_args.model_reduction, 101;
        Common_args.static_analysis, 101;
        Common_args.debug_mode, 101;
      ],
      Normal );
    ( "--ode-backend",
      Superarg.Choice
        ( [
            "dotnet", "dotnet (BNGL) backend";
            "maple", "maple backend (in progress)";
            "mathematica", "mathematica backend (in progress)";
            "matlab", "matlab backend";
            "octave", "octave backend";
            "sbml", "sbml backend";
          ],
          [
            "Dotnet";
            "DOTNET";
            "Octave";
            "OCTAVE";
            "Matlab";
            "MATLAB";
            "Mathematica";
            "MATHEMATICA";
            "Maple";
            "MAPLE";
            "Sbml";
            "SBML";
          ],
          t.backend ),
      "Select the backend format",
      [ Common_args.output, 1 ],
      Normal );
    ( "--dotnet-output",
      Superarg.String_opt t.dotnet_output,
      "ODEs file for dotnet backend",
      [ Common_args.output, 2 ],
      Hidden );
    ( "--maple-output",
      Superarg.String_opt t.maple_output,
      "ODEs file for maple backend",
      [ Common_args.output, 3 ],
      Hidden );
    ( "--mathematica-output",
      Superarg.String_opt t.mathematica_output,
      "ODEs file for mathematica backend",
      [ Common_args.output, 4 ],
      Hidden );
    ( "--matlab-output",
      Superarg.String_opt t.matlab_output,
      "ODEs file for matlab backend",
      [ Common_args.output, 5 ],
      Hidden );
    ( "--octave-output",
      Superarg.String_opt t.octave_output,
      "ODEs file for octave backend",
      [ Common_args.output, 6 ],
      Hidden );
    ( "--sbml-output",
      Superarg.String_opt t.sbml_output,
      "ODEs file for sbml backend",
      [ Common_args.output, 7 ],
      Hidden );
    ( "--propagate-constants",
      Superarg.Bool t.propagate_constants,
      "propagate constants",
      [ Common_args.output, 8 ],
      Hidden );
    ( "--constant-propagation",
      Superarg.Bool t.propagate_constants,
      "propagate constants",
      [ Common_args.output, 9 ],
      Normal );
    ( "--csv-separator",
      Superarg.String t.csv_sep,
      "separator symbol in CSV files",
      [ Common_args.output, 10 ],
      Normal );
    ( "--rate-convention",
      Superarg.Choice
        ( [
            "KaSim", "do not divide by anything";
            ( "Divide_by_nbr_of_autos_in_lhs",
              "divide by the number of autos in the lhs of rules" );
            ( "Biochemist",
              "divide by the number of autos in the lhs of rules that induce \
               an auto also in the rhs" );
          ],
          [
            "kasim";
            "KASIM";
            "Kasim";
            "DIVIDE_BY_NBR_OF_AUTOS_IN_LHS";
            "divide_by_nbr_of_autos_in_lhs";
            "biobhemist";
            "BIOCHEMIST";
          ],
          t.rule_rate_convention ),
      "convention for dividing constant rates",
      [ Common_args.semantics, 1 ],
      Hidden );
    ( "--rule-rate-convention",
      Superarg.Choice
        ( [
            "KaSim", "do not divide by anything";
            ( "Divide_by_nbr_of_autos_in_lhs",
              "divide by the number of autos in the lhs of rules" );
            ( "Biochemist",
              "divide by the number of autos in the lhs of rules that induce \
               an auto also in the rhs" );
          ],
          [
            "kasim";
            "KASIM";
            "Kasim";
            "DIVIDE_BY_NBR_OF_AUTOS_IN_LHS";
            "divide_by_nbr_of_autos_in_lhs";
            "biobhemist";
            "BIOCHEMIST";
          ],
          t.rule_rate_convention ),
      "convention for dividing constant rates (for rules)",
      [ Common_args.semantics, 1 ],
      Normal );
    ( "--reaction-rate-convention",
      Superarg.Choice
        ( [
            "KaSim", "do not divide by anything";
            ( "Divide_by_nbr_of_autos_in_lhs",
              "divide by the number of autos in the lhs of reactions" );
            ( "Biochemist",
              "divide by the number of autos in the lhs of reactions that \
               induce an auto also in the rhs" );
          ],
          [
            "kasim";
            "KASIM";
            "Kasim";
            "DIVIDE_BY_NBR_OF_AUTOS_IN_LHS";
            "divide_by_nbr_of_autos_in_lhs";
            "biobhemist";
            "BIOCHEMIST";
          ],
          t.reaction_rate_convention ),
      "convention for dividing constant rates (for reactions)",
      [ Common_args.semantics, 2 ],
      Normal );
    ( "--count",
      Superarg.Choice
        ( [
            ( "Embeddings",
              "count the number of embeddings of patterns into species" );
            "Occurrences", "count the number of occurrences of species";
          ],
          [ "embeddings"; "EMBEDDINGS"; "occurrences"; "OCCURRENCES" ],
          t.count ),
      "tune whether we count in embeddings or in occurrences",
      [ Common_args.semantics, 3 ],
      Normal );
    ( "--internal-meaning",
      Superarg.Choice
        ( [
            ( "Embeddings",
              "Variables for Kappa species denote numbers of embeddings" );
            ( "Occurrences",
              "Variables for Kappa species denote numbers of occurrences" );
          ],
          [ "embeddings"; "EMBEDDINGS"; "occurrences"; "OCCURRENCES" ],
          t.internal_meaning ),
      "tune the meaning of variable for Kappa species (whether it is the \
       number of embeddings or the number of occurrences)",
      [ Common_args.semantics, 4 ],
      Hidden );
    ( "--truncate",
      Superarg.Int_opt t.max_size_for_species,
      "truncate the network by discarding species with size greater than the \
       argument",
      [ Common_args.semantics, 4 ],
      Normal );
    ( "--max-size-for-species",
      Superarg.Int_opt t.max_size_for_species,
      "truncate the network by discarding species with size greater than the \
       argument",
      [ Common_args.semantics, 5 ],
      Hidden );
    ( "--show-reactions",
      Superarg.Bool t.show_reactions,
      "Annotate ODEs by the corresponding chemical reactions",
      [ Common_args.output, 10 ],
      Normal );
    ( "--smash-reactions",
      Superarg.Bool t.smash_reactions,
      "Gather identical reactions in the ODEs",
      [ Common_args.output, 11; Common_args.integration_settings, 1 ],
      Normal );
    ( "--compute-jacobian",
      Superarg.Bool t.compute_jacobian,
      "Enable/disable the computation of the Jacobian of the ODEs \n\
       \t (not available yet)",
      [ Common_args.integration_settings, 2 ],
      Normal );
    ( "--with-symmetries",
      Superarg.Choice
        ( [
            "None", "no symmetries reduction";
            ( "Backward",
              "use the symmetries satisfied by the rules and the algebraic \
               expressions" );
            ( "Forward",
              "use the symmetries satisfied by the rules and the initial state"
            );
          ],
          [
            "none";
            "NONE";
            "BACKWARD";
            "backward";
            "forward";
            "FORWARD";
            "true";
            "TRUE";
            "True";
            "false";
            "FALSE";
            "False";
          ],
          t.with_symmetries ),
      "Tune which kind of bisimulation is used to reduce the set of species",
      [ Common_args.semantics, 5; Common_args.model_reduction, 1 ],
      Normal );
    ( "--show-symmetries",
      Superarg.Bool t.show_symmetries,
      "Display the equivalence relations over the sites",
      [ Common_args.model_reduction, 2 ],
      Normal );
    ( "--views-domain",
      Superarg.Bool t.views,
      "Enable/disable views analysis when detecting symmetric sites",
      [ Common_args.static_analysis, 1 ],
      Expert );
    ( "--double-bonds-domain",
      Superarg.Bool t.dbonds,
      "Enable/disable double bonds analysis when detecting symmetric sites",
      [ Common_args.static_analysis, 2 ],
      Expert );
    ( "--site-across-bonds-domain",
      Superarg.Bool t.site_across,
      "Enable/disable the analysis of the relation amond the states of sites \
       in connected agents",
      [ Common_args.static_analysis, 3 ],
      Expert );
    ( "--nonnegative",
      Superarg.Bool t.nonnegative,
      "Enable/disable the correction of negative concentrations in stiff ODE \
       systems",
      [ Common_args.integration_settings, 3 ],
      Normal );
    ( "--show-time-advance",
      Superarg.Bool t.show_time_advance,
      "Display time advance during numerical integration",
      [ Common_args.debug_mode, 1 ],
      Expert );
    ( "--initial-step",
      Superarg.Float t.initial_step,
      "Initial integration step",
      [ Common_args.integration_settings, 4 ],
      Normal );
    ( "--max-step",
      Superarg.Float t.max_step,
      "Maximum integration step",
      [ Common_args.integration_settings, 5 ],
      Normal );
    ( "--relative-tolerance",
      Superarg.Float t.relative_tolerance,
      "tolerance to relative rounding errors",
      [ Common_args.integration_settings, 6 ],
      Normal );
    ( "--absolute-tolerance",
      Superarg.Float t.absolute_tolerance,
      "tolerance to absolute rounding errors",
      [ Common_args.integration_settings, 7 ],
      Normal );
    ( "--print-efficiency",
      Superarg.Bool t.print_efficiency,
      "prompt CPU time and various datas",
      [ Common_args.debug_mode, 2 ],
      Expert );
  ]

let get_option options =
  let title = Version.version_kade_full_name in
  let input = ref [] in
  let () = SuperargTk.parse ~title options input in
  !input

let build_kasa_parameters ~called_from t t_common =
  Config.with_views_analysis := !(t.views);
  Config.with_parallel_bonds_analysis := !(t.dbonds);
  Config.with_site_across_bonds_analysis := !(t.site_across);
  Config.trace := t_common.Common_args.debug;
  Remanent_parameters.get_parameters ~called_from ()
