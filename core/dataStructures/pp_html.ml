(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let graph_page title ?subtitle deps header core f =
  let dependency f t =
    Format.fprintf f "<script src=\"%s\" charset=\"utf-8\"></script>" t
  in
  let () = Format.fprintf f "@[<v><!doctype html>@,@,<html>@," in
  let () = Format.fprintf f "@[<v 2><head>@,<meta charset=\"utf-8\">@," in
  let () = Format.fprintf f "<title>%t</title>@," title in
  let () = Pp.list ~trailing:Pp.space Pp.space dependency f deps in
  let () = Format.fprintf f "%t@]@,</head>@," header in
  let () = Format.fprintf f "@[<v 2><body>@,<div class=\"container\">@," in
  let () =
    Format.fprintf f "<h1>@[%t%t@]</h1>@," title (fun f ->
        match subtitle with
        | None -> ()
        | Some t -> Format.fprintf f "@,<small>%t</small>" t)
  in
  Format.fprintf f "%t@,</div>@]@,</body>@,</html>@]@." core
