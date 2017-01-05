(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val navli :
  Ui_simulation.t -> Html_types.flow5_without_interactive Tyxml_js.Html5.elt list
val navcontent : Ui_simulation.t -> [> Html_types.div ] Tyxml_js.Html5.elt list
val onload : Ui_simulation.t -> unit
val onresize : Ui_simulation.t -> unit
