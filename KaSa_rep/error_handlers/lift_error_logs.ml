let for_KaSim f = f (fun _ _ _ _ _ -> ()) () ()
let lift_generic_binary_for_KaSim f =
  (fun a b -> snd (for_KaSim f a b))
let lift_generic_binary_binary_for_KaSim f =
  (fun a b -> let _,b,c = for_KaSim f a b in b,c)
let lift_generic_ternary_for_KaSim f =
  (fun a b c -> snd (for_KaSim f a b c))

let lift_gen_unary dump f a =
  let parameters = Remanent_parameters.dummy_parameters () in
  let error = Exception.empty_error_handler in
  let error,output = f parameters error a in
  let _ = dump parameters error in
  output
    
let lift_gen_binary dump f a b =
  let parameters = Remanent_parameters.dummy_parameters () in
  let error = Exception.empty_error_handler in
  let error,output = f parameters error a b in
  let _ = dump parameters error in
  output
let lift_gen_ternary dump f a b c =
  let parameters = Remanent_parameters.dummy_parameters () in
  let error = Exception.empty_error_handler in
  let error,output = f parameters error a b c in
  let _ = dump parameters error in
  output

let lift_with_on_the_fly_logging_unary f a = lift_gen_unary Exception.print f a
    
let lift_with_on_the_fly_logging_binary f a b = lift_gen_binary Exception.print f a b
								
let lift_with_on_the_fly_logging_ternary f a b c = lift_gen_binary Exception.print f a b c 

let lift_without_logging_unary f a = lift_gen_unary (fun _ _ -> ()) f a
						    
let lift_without_logging_binary f a b = lift_gen_binary (fun _ _ _ -> ()) f a b 

let lift_without_logging_ternary f a b c = lift_gen_ternary (fun _ _ _ _ -> ()) f a b c 
