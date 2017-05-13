type 'a site_partition =
    {
      over_binding_states: 'a list list ;
      over_internal_states: 'a list list ;
      over_full_states: 'a list list ;
    }

type bwd_bisim_info =
  int site_partition array * bool Mods.DynArray.t * Signature.s * (LKappa_auto.cache ref)

val empty : 'a site_partition
val map: ('a -> 'b) -> 'a site_partition -> 'b site_partition
val clean: 'a site_partition -> 'a site_partition
val print:
  Loggers.t ->
  ('agent -> Format.formatter -> 'site -> unit) ->
  (Format.formatter -> 'agent -> unit) ->
  'agent -> 'site site_partition -> unit
