type cache

val init_cache: unit -> cache
val nauto: Ode_args.rate_convention -> cache -> LKappa.rule_mixture -> Raw_mixture.t -> cache * int
