(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

(* good old cake pattern *)

class manager
    (system_process : Kappa_facade.system_process) : Api.manager =
  let project = new Environment_memory.project
  in
  object
    inherit Manager_project.manager_project project system_process
    inherit Manager_simulation.manager_simulation project system_process
    inherit Manager_file. manager_file project
  end;;
