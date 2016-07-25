(** Network/ODE generation
  * Creation: 20/07/2016
  * Last modification: Time-stamp: <Jul 25 2016>
 *)

module A = Odes.Make (Dummy_interface.Interface)

let main () =
  let compil = A.get_compil (A.get_input_files ()) in
  let network = A.network_from_compil compil in
  let out_channel = open_out (A.get_m_output_file compil) in
  let logger = Loggers.open_logger_from_channel ~mode:Loggers.Octave out_channel in
  let () = A.export_network logger compil network in
  let () = Loggers.flush_logger logger in
  let () = close_out out_channel in
  ()

let () = main ()
