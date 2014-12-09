(* LablTK GUI for option selection Superarg.

   Copyright (C) Antoine Mine' 2006
   Light Version (Jerome Feret)
   This LIGHT version does not require labltk
   
 *)

open Superarg



exception Exit of string list


(* MAIN *)
(* **** *)

let parse parameters (a:Superarg.t) (def:string list ref) =
  Superarg.check parameters a;
  (* drop the first command-line argument: it is the executable name *)
  let args = List.tl (Array.to_list Sys.argv) in
  (* if no argument or "--gui" given, launch the gui, otherwise, parse args *)
  let rem = 
    if args=[] || List.exists ((=) "--gui") args 
    then parse_list parameters a ["--help"] else Superarg.parse_list parameters a args
  in
  if rem<>[] then def := rem
