type parameters

val parameters : parameters

val fold_over_the_causal_past_of_observables_with_a_progress_bar_while_reshaking_the_trace :
  Utilities.parameter ->
  shall_we_compute:'a ->
  shall_we_compute_profiling_information:'b ->
  Utilities.kappa_handler ->
  Utilities.profiling_info ->
  Utilities.error_log ->
  Utilities.shall_we ->
  Utilities.shall_we ->
  parameters ->
  (int ->
  Utilities.error_log * Utilities.profiling_info * Utilities.trace ->
  Utilities.error_log * Utilities.profiling_info * Utilities.trace) ->
  (Utilities.parameter ->
  Utilities.kappa_handler ->
  Utilities.profiling_info ->
  Utilities.error_log ->
  Utilities.trace ->
  Utilities.error_log * Utilities.profiling_info * Utilities.trace) ->
  ( Utilities.trace,
    Utilities.trace_runtime_info list,
    'story_table,
    'story_table )
  Utilities.ternary ->
  Utilities.trace ->
  'story_table ->
  Utilities.error_log * Utilities.profiling_info * 'story_table
