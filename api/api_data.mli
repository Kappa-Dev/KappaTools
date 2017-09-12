type simulation_detail_output =
  (Api_types_t.plot option,
   Api_types_t.flux_map list,
   string list Mods.StringMap.t,
   Api_types_t.snapshot list,
   string)
    Api_types_t.simulation_output

val api_message_errors :
  ?severity:Api_types_t.severity ->
  ?region:Api_types_t.range -> string -> Api_types_t.errors
val api_exception_errors : exn -> Api_types_t.errors
val links_of_mix :
  Raw_mixture.agent list -> Mods.Int2Map.elt Mods.Int2Map.t
val api_mixture :
  Signature.s -> Raw_mixture.agent list -> Api_types_t.site_node array
val label_snapshot : Signature.s -> Data.snapshot -> Api_types_t.snapshot
val api_snapshot_site_graph :
  Api_types_t.snapshot -> Api_types_t.site_graph
val print_site_node :
  ?link_store:(int * int Mods.Int2Map.t) ref ->
  int -> Format.formatter -> Api_types_t.site_node -> unit
val print_site_nodes_dot :
  int -> Format.formatter -> Api_types_t.site_node array -> unit
val api_snapshot_dot : Api_types_t.snapshot -> string
val print_site_nodes :
  Format.formatter -> Api_types_t.site_node array -> unit
val api_snapshot_kappa : Api_types_t.snapshot -> string
val api_contact_map :
  Signature.s -> Contact_map.t -> Api_types_t.site_node array
val api_simulation_status :
  Api_types_t.simulation_progress -> simulation_detail_output ->
  Api_types_t.simulation_info
val plot_values : ?separator:string -> Api_types_t.plot -> string
val agent_count : Api_types_t.site_graph -> int
