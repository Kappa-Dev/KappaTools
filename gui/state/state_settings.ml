(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let fontSizeParamId = Js.string "kappappFontSize"
let defaultFontSize = 1.4
let minFontSize = 0.2
let maxFontSize = 3.
let currentFontSize = ref defaultFontSize

let initFromStorage () =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> currentFontSize := defaultFontSize)
    (fun st ->
      currentFontSize :=
        Js.Opt.case
          (st##getItem fontSizeParamId)
          (fun () -> defaultFontSize)
          Js.parseFloat)

let set_parameters_as_default () =
  let v' = string_of_float !currentFontSize in
  Js.Optdef.iter Dom_html.window##.localStorage (fun st ->
      st##setItem fontSizeParamId (Js.string v'))

let updateFontSize ~delta =
  let () =
    currentFontSize :=
      max minFontSize (min (!currentFontSize +. delta) maxFontSize)
  in
  let v' = string_of_float !currentFontSize in
  let () =
    Dom_html.document##.body##.style##.fontSize := Js.string (v' ^ "em")
  in
  ()

let agent_coloring = Js.Unsafe.obj [||]

let init () : unit Lwt.t =
  let () = initFromStorage () in
  Lwt.return_unit

let sync () : unit Lwt.t = Lwt.return_unit
