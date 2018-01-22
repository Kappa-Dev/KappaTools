(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5
open Lwt.Infix

let tab_is_active, set_tab_is_active = React.S.create false
let tab_was_active = ref false


let navli () = ReactiveData.RList.empty

let content () = []

let parent_hide () = set_tab_is_active false
let parent_shown () = set_tab_is_active !tab_was_active

let onload () =
  let () = Common.jquery_on
      "#navstories" "hide.bs.tab"
      (fun _ -> let () = tab_was_active := false in set_tab_is_active false) in
  let () = Common.jquery_on
      "#navstories" "shown.bs.tab"
      (fun _ -> let () = tab_was_active := true in set_tab_is_active true) in
  ()
let onresize () : unit = ()
