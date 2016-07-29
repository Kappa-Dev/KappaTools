type t = {
	   mutable minTimeValue        : float;
	        }

let default : t = { minTimeValue = 0. ; }

let options (t :t)  : (string * Arg.spec * string) list = [
  ("-t-init",
   Arg.Float (fun time -> t.minTimeValue <- time),
   "Min time of simulation (arbitrary time unit)");
  ]
