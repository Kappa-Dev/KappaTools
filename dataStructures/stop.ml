type ('a,'b) stop =
    Stop of 'b
  | Success of 'a

let success a = Success a
let stop a = Stop a

let success_or_stop f g x =
  match x
  with
  | Success a -> f a
  | Stop a -> g a 
 	       
