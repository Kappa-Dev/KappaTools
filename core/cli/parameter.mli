val xlsweakFileName : string
val xlsstrongFileName : string
val get_cache_size : unit -> int option
val dump_grid_after_branching_during_strong_compression : bool
val dump_grid_after_branching_during_weak_compression : bool
val dump_grid_before_strong_compression : bool
val dump_grid_before_weak_compression : bool
val blacklist_events : bool ref
val time_independent : bool ref
val do_local_cut : bool
val do_detect_separable_components : bool
val look_up_for_better_cut : bool
val look_down_for_better_cut : bool
val log_number_of_causal_flows : bool
val defaultLiftSetSize : int ref
val defaultHeapSize : int ref
val debugModeOn : bool ref
val do_global_cut : bool
val cut_pseudo_inverse_event : bool
val defaultExtArraySize : int ref
val defaultGraphSize : int ref
