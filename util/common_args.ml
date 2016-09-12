type t = { mutable backtrace           : bool ;
	   mutable debug               : bool ;
	   mutable timeIndependent     : bool }

let default : t = { backtrace = false ;
		    debug = false;
		    timeIndependent = false }

let options t : (string * Arg.spec * string) list = [
    ("--version",
     Arg.Unit (fun () -> Format.print_string Version.version_msg;
			 Format.print_newline () ; exit 0),
     "display KaSim version");
    ("--debug", Arg.Unit (fun () -> t.debug <- true),
     "Enable debug mode") ;
    ("--backtrace", Arg.Unit (fun () -> t.backtrace <- true),
     "Backtracing exceptions") ;
    ("--gluttony",
     Arg.Unit (fun () -> Gc.set { (Gc.get()) with
				  Gc.space_overhead = 500 (*default 80*) } ;),
     "Lower gc activity for a faster but memory intensive simulation") ;
    ("--time-independent",
     Arg.Unit (fun () -> t.timeIndependent <- true),
     "Disable the use of time is story heuritics (for test suite)")
]
