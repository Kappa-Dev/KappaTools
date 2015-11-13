let for_KaSim f = f (fun _ _ _ _ _ -> ()) () ()
let lift_generic_binary_for_KaSim f =
  (fun a b -> snd (for_KaSim f a b))
let lift_generic_binary_binary_for_KaSim f =
  (fun a b -> let _,b,c = for_KaSim f a b in b,c)
let lift_generic_ternary_for_KaSim f =
  (fun a b c -> snd (for_KaSim f a b c))
