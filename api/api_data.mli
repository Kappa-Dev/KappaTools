type simulation_detail_output =
  (Api_types_t.plot option,
   Api_types_t.flux_map list,
   string list Mods.StringMap.t,
   Api_types_t.snapshot list,
   string)
    Api_types_t.simulation_output

val api_message_errors :
  ?severity:Api_types_t.severity ->
  ?region:Api_types_t.range -> string -> Api_types_t.message
val api_exception_errors : exn -> Api_types_t.errors
val api_snapshot_dot : Api_types_t.snapshot -> string
val api_snapshot_kappa : Api_types_t.snapshot -> string
val api_simulation_status :
  Api_types_t.simulation_progress -> simulation_detail_output ->
  Api_types_t.simulation_info
val agent_count : Api_types_t.site_graph -> int
