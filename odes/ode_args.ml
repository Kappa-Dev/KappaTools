type t = {
  mutable backend : string ;
}

let default : t =
  {
    backend = "Octave" ;
  }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("--ode-backend",
   Arg.String (fun backend -> t.backend <- backend),
   "Available backends are Octave and Matlab")
]
