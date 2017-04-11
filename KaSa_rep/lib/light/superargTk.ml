(* LablTK GUI for option selection Superarg.

   Copyright (C) Antoine Mine' 2006
   Light Version (Jerome Feret)
   This LIGHT version does not require labltk
   
 *)

exception Exit of string list


	   
(* MAIN *)
(* **** *)

let parse (a:Superarg.t) (def:string list ref) =
  Superarg.check a;
  (* drop the first command-line argument: it is the executable name *)
  let args = List.tl (Array.to_list Sys.argv) in
  (* if no argument or "--gui" given, launch the gui, otherwise, parse args *)
  let rem =
    if args=[] || List.exists ((=) "--gui") args
    then Superarg.parse_list a ["--help"] else Superarg.parse_list a args
  in
  if rem<>[] then def := rem
