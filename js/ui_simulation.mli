(* The state of the simulation

   There is a need to for the ui to be aware of the simulation state.
   In particular to prevent multiple starts of the simulation.
 *)
type t
(* Creates the ui simulation. *)
val create : unit -> t
(* Get the signal containing the simulation state *)
type simulation_status = | STOPPED | INITALIZING | RUNNING | PAUSED

val simulation_status : t -> simulation_status React.signal
val simulation_output : t -> Api_types_v1_j.state option React.signal
val perturb_simulation : t -> code:string -> unit Lwt.t
val continue_simulation : t -> unit Lwt.t
val pause_simulation : t -> unit Lwt.t
val stop_simulation : t -> unit Lwt.t
val flush_simulation : t -> unit Lwt.t
val start_simulation : t -> unit Lwt.t
