let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Tools") message exn (fun () -> default) 

let fst_option x = 
  match x 
  with 
  | Some (x,_) -> Some x 
  | _ -> None 

let snd_option x = 
  match x 
  with 
  | Some (_,x) -> Some x 
  | _ -> None 
    
let make_id_compatible_with_dot_format parameters error string =
  error,parameters.Remanent_parameters_sig.make_id_compatible_with_dot string 
