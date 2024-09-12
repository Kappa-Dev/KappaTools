(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let pp_exception f = function
  | Exit -> Format.pp_print_string f "Exit"
  | Not_found -> Format.pp_print_string f "Not_found"
  | Arg.Bad x -> Format.fprintf f "Arg.Bad(%s)" x
  | Sys.Break -> Format.pp_print_string f "Sys.Break"
  | Stack.Empty -> Format.pp_print_string f "Stack.Empty"
  | Queue.Empty -> Format.pp_print_string f "Queue.Empty"
  | Stream.Error x -> Format.fprintf f "Stream.Error %s" x
  | Stream.Failure -> Format.pp_print_string f "Stream.Failure"
  | Arg.Help x -> Format.fprintf f "Arg.Help(%s)" x
  | Parsing.Parse_error -> Format.pp_print_string f "Parsing.Parse_error"
  | Scanf.Scan_failure x -> Format.fprintf f "Scanf.Scan.failure(%s)" x
  | Lazy.Undefined -> Format.pp_print_string f "Lazy.Undefined"
  | UnixLabels.Unix_error (er, x, y) ->
    Format.fprintf f "UnixLabels.Unix_error(%s,%s,%s)"
      (UnixLabels.error_message er)
      x y
  | Unix.Unix_error (er, x, y) ->
    Format.fprintf f "Unix.Unix_error(%s,%s,%s)" (Unix.error_message er) x y
  | Failure x -> Format.fprintf f "Failure(%s)" x
  | Stack_overflow -> Format.pp_print_string f "Stack_overflow"
  | exc -> Format.pp_print_string f (Printexc.to_string exc)

