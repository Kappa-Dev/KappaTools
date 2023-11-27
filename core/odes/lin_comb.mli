module type Lin_comb = sig
  type mix
  type id
  type elt
  type t

  val of_expr :
    (id -> (mix, id) Alg_expr.e Locality.annot option) ->
    (mix, id) Alg_expr.e Locality.annot ->
    t option

  val print :
    sep:string ->
    product:string ->
    (Ode_loggers_sig.t -> mix -> unit) ->
    (Ode_loggers_sig.t -> id -> unit) ->
    Ode_loggers_sig.t ->
    t ->
    unit
end

module Lin : Lin_comb with type mix = int and type id = int
