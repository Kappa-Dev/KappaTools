module Buffers =
struct
  type 'a t = 'a list

let create i default = []

let add x t = x::t

let iter f t = List.iter f (List.rev t)
end
