(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let data_set = "Data set",0
let output = "Output",1
let semantics = "Semantics",2
let integration_settings = "Integration settings",3
let model_reduction = "Model reduction",4
let static_analysis = "Static analysis" , 5
let debug_mode = "Debug mode",6

type t = { mutable backtrace           : bool ;
	   mutable debug               : bool ;
	   mutable timeIndependent     : bool }

type t_gui = {
  backtrace_gui : bool ref ;
  debug_gui : bool ref ;
  timeIndependent_gui : bool ref ;
  version_gui : bool ref ;
  gluttony_gui : bool ref ;
}
let default : t = { backtrace = false ;
		    debug = false;
		    timeIndependent = false }

let default_gui =
  {
    backtrace_gui = ref false ;
    debug_gui = ref false ;
    timeIndependent_gui = ref false ;
    version_gui = ref false ;
    gluttony_gui = ref false
  }

let do_version =
  (fun () -> Format.print_string Version.version_msg;
    Format.print_newline () ; exit 0)

let do_gluttony =
  (fun () -> Gc.set { (Gc.get()) with
                      Gc.space_overhead = 500 (*default 80*) } ;)

let get_from_gui t_gui =
  let () =
    if !(t_gui.version_gui) then
      do_version ()
  in
  let () =
    if !(t_gui.gluttony_gui) then
      do_gluttony ()
  in
  {
    backtrace = !(t_gui.backtrace_gui) ;
    debug = !(t_gui.debug_gui) ;
    timeIndependent = !(t_gui.timeIndependent_gui);
    }

let copy_from_gui t_gui t =
  let t_tmp = get_from_gui t_gui in
  t.backtrace <- t_tmp.backtrace;
  t.debug <- t_tmp.debug;
  t.timeIndependent <- t_tmp.timeIndependent

let options_gen t t_gui : (string * Arg.spec * Superarg.spec * string *
                           (Superarg.category * Superarg.position) list * Superarg.level) list = [
    ("--version",
     Arg.Unit do_version,
     Superarg.Bool t_gui.version_gui,
     "display version number",
     [],Superarg.Hidden);
    ("--debug", Arg.Unit (fun () -> t.debug <- true),
     Superarg.Bool t_gui.debug_gui,
     "Enable debug mode",
     [debug_mode,1],Superarg.Expert) ;
    ("--backtrace", Arg.Unit (fun () -> t.backtrace <- true),
     Superarg.Bool t_gui.backtrace_gui,
     "Backtracing exceptions",
     [debug_mode,2],Superarg.Expert) ;
    ("--gluttony",
     Arg.Unit do_gluttony,
     Superarg.Bool t_gui.gluttony_gui,
     "Lower gc activity for a faster but memory intensive simulation",
     [debug_mode,3],Superarg.Expert) ;
    ("--time-independent",
     Arg.Unit (fun () -> t.timeIndependent <- true),
     Superarg.Bool t_gui.timeIndependent_gui,
     "Disable the use of time is story heuritics (for test suite)",
     [debug_mode,4],Superarg.Hidden;)
]

let options t =
  List.rev_map
    (fun (a,b,_,c,_,_) -> a,b,c)
    (List.rev (options_gen t default_gui))

let options_gui t_gui =
  List.rev_map
    (fun (a,_,b,c,d,e) -> a,b,c,d,e)
    (List.rev (options_gen default t_gui))
