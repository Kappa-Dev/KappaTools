type 'a t = 'a list

let create _i _default = []
let add x t = x :: t
let iter f t = List.iter f (List.rev t)
let clean _ = []
