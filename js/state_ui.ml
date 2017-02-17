open Lwt.Infix

let sync_all () : unit Lwt.t =
  Lwt.return_unit

let init_all () =
  Lwt.return_unit >>=
  State_parameter.init >>=
  State_perturbation.init >>=
  State_runtime.init >>=
  State_project.init >>=
  State_simulation.init >>=
  Ui_state.init >>=
  sync_all

let onload () = Common.async init_all
let sync () = Lwt.return_unit
