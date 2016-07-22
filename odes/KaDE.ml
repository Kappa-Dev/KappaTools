module A = Odes.Make (Dummy_interface.Interface)

let main () =
  let compil = A.get_compil () in
  A.network_from_compil compil
