(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let raw_version_string = "$Format:%D$"

let extract_tag_re =
  Re.compile
    (Re.seq [ Re.str "tag: "; Re.group (Re.rep (Re.compl [ Re.char ',' ])) ])

let version_string =
  match Re.exec_opt extract_tag_re raw_version_string with
  | Some gr -> Re.Group.get gr 1
  | None -> Git_version.t

let version_msg = "Kappa Simulator: " ^ version_string
let version_kasa_full_name = "Kappa Static Analyzer (" ^ version_string ^ ")"
let version_kade_full_name = "KaDE (" ^ version_string ^ ")"
let tk_is_initialized = ref false
