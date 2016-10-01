open Lwt
(** Interface to kappa runtime *)
(* Error messages *)
let msg_process_not_running =
  "process not running"
let msg_process_already_paused =
  "process already paused"
let msg_process_not_paused =
  "process not paused"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"
let msg_missing_perturbation_context =
  "Invalid runtime state missing missing perturbation context"



(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process =
  object
    method log : ?exn:exn -> string -> unit Lwt.t
    method yield : unit -> unit Lwt.t
  end

(** Trivial implementation primarily for unit testing. *)
class null_process : system_process =
  object
    method log ?exn (_ : string) =
      let () = ignore(exn) in
      Lwt.return_unit
    method yield () = Lwt.return_unit
  end;;

(** State of the running simulation.
*)
type t =
  { mutable is_running : bool
  ; mutable run_finalize : bool
  ; counter : Counter.t
  ; log_buffer : Buffer.t
  ; log_form : Format.formatter
  ; mutable plot : Api_types_v1_j.plot
  ; mutable distances : Api_types_v1_j.distances
  ; mutable snapshots : Api_types_v1_j.snapshot list
  ; mutable flux_maps : Api_types_v1_j.flux_map list
  ; mutable files : Api_types_v1_j.file_line list
  ; mutable error_messages : Api_types_v1_j.errors
  ; contact_map : Primitives.contact_map
  ; env : Environment.t
  ; mutable domain : Connected_component.Env.t
  ; mutable graph : Rule_interpreter.t
  ; mutable state : State_interpreter.t
  ; store_distances : bool
  ; init_l : (Alg_expr.t * Primitives.elementary_rule * Location.t) list
  ; has_tracking : (bool * bool * bool) option
  ; lastyield : float
  }

let parse
    ~(system_process : system_process)
    ~(kappa_code : string)
  : (t,Api_types_j.errors) Api_types_j.result_data Lwt.t
  = let () = ignore(system_process) in
    let () = ignore(kappa_code) in
  failwith "parse"

let start
    ~(system_process : system_process)
    ~(parameter : Api_types_j.simulation_parameter)
    ~(t : t)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  failwith "start"

let pause
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  failwith "start"

let stop
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  failwith "start"

let perturbation
    ~(system_process : system_process)
    ~(t : t)
    ~(perturbation:Api_types_j.simulation_perturbation)
  : (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  failwith "start"

let continue
    ~(system_process : system_process)
    ~(t : t)
    ~(parameter : Api_types_j.simulation_parameter)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  let () = ignore(parameter) in
  failwith "start"

let info
    ~(system_process : system_process)
    ~(t : t) :
  (Api_types_j.simulation_info,Api_types_j.errors)
    Api_types_j.result_data
    Lwt.t =
  let () = ignore(system_process) in
  let () = ignore(t) in
  failwith "info"
