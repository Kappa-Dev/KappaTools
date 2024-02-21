(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let url_args ?(default = []) key : string list =
  (* Hosts the user specified on the url in the form
     key=...&key=..&...
  *)
  let args = Url.Current.arguments in
  match List.map snd (List.filter (fun (k, _) -> k = key) args) with
  | [] -> default
  | some -> some
