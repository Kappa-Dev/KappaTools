type count = Embeddings | Occurrences

type t = {
  mutable backend : string ;
  mutable rate_convention : string ;
  mutable count : string ;
  mutable show_reactions : bool ;
  mutable compute_jacobian : bool ;
  mutable octave_output : string option ;
  mutable matlab_output : string option ;
  mutable maple_output : string option ;
  mutable mathematica_output : string option ;
  mutable sbml_output : string option ;
  mutable with_symmetries : string option ;
  mutable show_symmetries : bool ;
  mutable views : bool ;
  mutable dbonds : bool ;
  mutable site_across : bool ;
  mutable nonnegative : bool ;
  mutable show_time_advance : bool ;
  mutable initial_step : float ;
  mutable max_step : float ;
  mutable relative_tolerance : float ;
  mutable absolute_tolerance : float ;
}

let default : t =
  {
    backend = "Octave" ;
    rate_convention = "Divide_by_nbr_of_autos_in_lhs" ;
    count = "Embeddings" ;
    show_reactions = true ;
    compute_jacobian = true ;
    octave_output = None  ;
    matlab_output = None ;
    maple_output = None ;
    mathematica_output = None ;
    sbml_output = None ;
    with_symmetries = None ;
    show_symmetries = false ;
    views = true ;
    dbonds = true ;
    site_across = true ;
    nonnegative = false ;
    show_time_advance = false ;
    initial_step = 0.00001 ;
    max_step = 0.02 ;
    absolute_tolerance = 0.001 ;
    relative_tolerance = 0.001 ;
  }

let options (t :t)  : (string * Arg.spec * string) list = [
  "--ode-backend",
   Arg.String (fun backend -> t.backend <- backend),
  "Available backends are Octave, Maple (in progress), Matlab, Mathematica (in progress), and SBML " ;
   "--maple-output",
   Arg.String (fun backend -> t.maple_output <- Some backend),
   "ODEs file for maple backend";
  "--mathematica-output",
   Arg.String (fun backend -> t.mathematica_output <- Some backend),
   "ODEs file for mathematica backend";
  "--matlab-output",
  Arg.String (fun backend -> t.matlab_output <- Some backend),
  "ODEs file for matlab backend";
  "--octave-output",
  Arg.String (fun backend -> t.octave_output <- Some backend),
  "ODEs file for octave backend";
  "--sbml-output",
  Arg.String (fun backend -> t.sbml_output <- Some backend),
  "ODEs file for sbml backend";
  "--rate-convention",
  Arg.String (fun rate_convention -> t.rate_convention <- rate_convention),
  "Tune which correction is applied to rule rates \n\t KaSim -> we do not divide; \n\t Divide_by_nbr_of_autos_in_lhs -> we divide by the number of autos in the lhs \n\t Biochemist -> we divide by the number of autos in the lhs that induce an auto in the rhs" ;
  "--count",
  Arg.String (fun count -> t.count <- count),
  "Tune whether ode variables denote number of occurrences or number of embeddings" ;
  "--show-reactions",
  Arg.Bool (fun show_reactions -> t.show_reactions <- show_reactions),
  "Annotate ODEs by the corresponding chemical reactions" ;
  "--compute-jacobian",
  Arg.Bool (fun compute_jacobian -> t.compute_jacobian <- compute_jacobian),
  "Enable/disable the computation of the Jacobian of the ODEs \n\t (not available yet)" ;
  "--with-symmetries",
  Arg.String (fun with_symmetries -> t.with_symmetries <- Some with_symmetries),
  "Tune which kind of bisimulation is used to reduce the set of species";
  "--show-symmetries",
  Arg.Bool (fun show_symmetries -> t.show_symmetries <- show_symmetries),
  "Display the equivalence relations over the sites" ;
  "--views-domain",
  Arg.Bool (fun views -> t.views <- views),
  "Enable/disable views analysis when detecting symmetric sites" ;
  "--double-bonds-domain",
  Arg.Bool (fun dbonds -> t.dbonds <- dbonds),
  "Enable/disable double bonds analysis when detecting symmetric sites" ;
  "--site-across-bonds-domain",
  Arg.Bool (fun site_across -> t.site_across <- site_across),
  "Enable/disable the analysis of the relation amond the states of sites in
      connected agents";
  "--nonnegative",
  Arg.Bool (fun nonneg -> t.nonnegative <- nonneg),
  "Enable/disable the correction of negative concentrations in stiff ODE systems";
  "--show-time-advance",
  Arg.Bool (fun timeadv -> t.show_time_advance <- timeadv),
  "Display time advance during numerical integration";
  "--initial-step",
  Arg.Float (fun step -> t.initial_step <- step),
  "Initial integration step";
  "--max-step",
  Arg.Float (fun step -> t.max_step <- step),
  "Maximum integration step";
  "--relative-tolerance",
  Arg.Float (fun err -> t.relative_tolerance <- err),
  "tolerance to relative rounding errors";
  "--absolute-tolerance",
  Arg.Float (fun err -> t.absolute_tolerance <- err),
  "tolerance to absolute rounding errors";
]
let build_kasa_parameters ~called_from t t_common =
  Config.with_views_analysis := t.views ;
  Config.with_parallel_bonds_analysis := t.dbonds ;
  Config.with_site_across_bonds_analysis := t.site_across ;
  Config.trace := t_common.Common_args.debug ;
  Remanent_parameters.get_parameters
    ~called_from
    ()
