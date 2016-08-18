type rate_convention = KaSim | Biochemist

type t = {
  mutable backend : string ;
  mutable rate_convention : string ;
}

let default : t =
  {
    backend = "Octave" ;
    rate_convention = "KaSim" ;
  }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("--ode-backend",
   Arg.String (fun backend -> t.backend <- backend),
   "Available backends are Octave and Matlab") ;
  "--rate-convention",
  Arg.String (fun rate_convention -> t.rate_convention <- rate_convention),
  "Tune whether or not rule rates are divided by the number of automorphisms in the lhs @ (KaSim -> we do not divide; Biochemist -> we divide) @ The Biochemist option is not available yet "
]
