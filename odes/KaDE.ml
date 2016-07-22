module A = Odes.Make (Dummy_interface.Interface)

let main () =
  let compil = A.get_compil () in
  let network = A.network_from_compil compil in
  let out_channel = open_out "ode.m" in
  let logger = Loggers.open_logger_from_channel ~mode:Loggers.Octave out_channel in
  let () = A.export_network logger network in
  let () = Loggers.flush_logger logger in
  let () = close_out out_channel in
  ()

let () = main ()
