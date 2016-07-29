type t = {
  mutable minTimeValue : float;
  mutable backend : string ;
}

let default : t =
  {
    minTimeValue = 0. ;
    backend = "Octave" ;
  }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("-t-init",
   Arg.Float (fun time -> t.minTimeValue <- time),
   "Min time of simulation (arbitrary time unit)");
  ("--ode-backend",
   Arg.String (fun backend -> t.backend <- backend),
   "Available backends are Octave and Matlab")
]
