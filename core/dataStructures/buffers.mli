module type Buffers = sig
  type 'a t

  val create : int -> 'a -> 'a t
  val add : 'a -> 'a t -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
  val clean : 'a t -> 'a t
end
