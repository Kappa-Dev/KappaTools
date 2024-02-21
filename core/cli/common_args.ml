(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let data_set = "Data set", 0, None
let output = "Output", 1, None
let semantics = "Semantics", 2, None
let integration_settings = "Integration settings", 3, None
let model_reduction = "Model reduction", 4, None
let static_analysis = "Static analysis", 5, Some Superarg.Expert
let debug_mode = "Debug mode", 6, Some Superarg.Expert

type t = { mutable backtrace: bool; mutable debug: bool }

type t_gui = {
  backtrace_gui: bool ref;
  debug_gui: bool ref;
  version_gui: bool ref;
  gluttony_gui: bool ref;
}

let default : t = { backtrace = false; debug = false }

let default_gui =
  {
    backtrace_gui = ref false;
    debug_gui = ref false;
    version_gui = ref false;
    gluttony_gui = ref false;
  }

let do_version () =
  Format.print_string Version.version_msg;
  Format.print_newline ();
  exit 0

let do_gluttony () =
  Gc.set { (Gc.get ()) with Gc.space_overhead = 500 (*default 80*) }

let get_from_gui t_gui =
  let () = if !(t_gui.version_gui) then do_version () in
  let () = if !(t_gui.gluttony_gui) then do_gluttony () in
  { backtrace = !(t_gui.backtrace_gui); debug = !(t_gui.debug_gui) }

let copy_from_gui t_gui t =
  let t_tmp = get_from_gui t_gui in
  t.backtrace <- t_tmp.backtrace;
  t.debug <- t_tmp.debug

let options_gen t t_gui :
    (string
    * Arg.spec
    * Superarg.spec
    * string
    * (Superarg.category * Superarg.position) list
    * Superarg.level)
    list =
  [
    ( "--version",
      Arg.Unit do_version,
      Superarg.Bool t_gui.version_gui,
      "display version number",
      [],
      Superarg.Hidden );
    ( "--debug",
      Arg.Unit (fun () -> t.debug <- true),
      Superarg.Bool t_gui.debug_gui,
      "Enable debug mode",
      [ debug_mode, 1 ],
      Superarg.Expert );
    ( "--backtrace",
      Arg.Unit (fun () -> t.backtrace <- true),
      Superarg.Bool t_gui.backtrace_gui,
      "Backtracing exceptions",
      [ debug_mode, 2 ],
      Superarg.Expert );
    ( "--gluttony",
      Arg.Unit do_gluttony,
      Superarg.Bool t_gui.gluttony_gui,
      "Lower gc activity for a faster but memory intensive simulation",
      [ debug_mode, 3 ],
      Superarg.Expert );
  ]

let options t =
  List.rev_map
    (fun (a, b, _, c, _, _) -> a, b, c)
    (List.rev (options_gen t default_gui))

let options_gui t_gui =
  List.rev_map
    (fun (a, _, b, c, d, e) -> a, b, c, d, e)
    (List.rev (options_gen default t_gui))
