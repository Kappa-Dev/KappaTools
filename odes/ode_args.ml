type rate_convention = KaSim | Biochemist
type count = Embeddings | Occurrences

type t = {
  mutable backend : string ;
  mutable rate_convention : string ;
  mutable count : string ;
  mutable show_reactions : bool ;
  mutable compute_jacobian : bool ;
}

let default : t =
  {
    backend = "Octave" ;
    rate_convention = "KaSim" ;
    count = "Embeddings" ;
    show_reactions = true ;
    compute_jacobian = false ;
  }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("--ode-backend",
   Arg.String (fun backend -> t.backend <- backend),
   "Available backends are Octave and Matlab") ;
  "--rate-convention",
  Arg.String (fun rate_convention -> t.rate_convention <- rate_convention),
  "Tune whether or not rule rates are divided by the number of automorphisms in the lhs \n\t (KaSim -> we do not divide; Biochemist -> we divide) \n\t The Biochemist option is not available yet" ;
  "--count",
  Arg.String (fun count -> t.count <- count),
  "Tune whether ode variables denote number of occurrences or number of embeddings" ;
  "--show-reactions",
  Arg.Bool (fun show_reactions -> t.show_reactions <- show_reactions),
  "Annotate ODEs by the corresponding chemical reactions" ;
  "--compute-jacobian",
  Arg.Bool (fun compute_jacobian -> t.compute_jacobian <- compute_jacobian),
  "Enable/disable the computation of the Jacobian of the ODEs \n\t (not available yet)"
]
