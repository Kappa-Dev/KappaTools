let create_flux env counter name =
  let size = Environment.nb_syntactic_rules env + 1 in
  {
    Data.flux_name = name;
    Data.flux_start = Counter.current_time counter;
    Data.flux_hits = Array.make size 0;
    Data.flux_fluxs = Array.make_matrix size size 0.;
  }

let incr_flux_flux of_rule on_rule v flux =
  flux.Data.flux_fluxs.(of_rule).(on_rule) <-
    flux.Data.flux_fluxs.(of_rule).(on_rule) +. v

let incr_flux_hit of_rule flux =
  flux.Data.flux_hits.(of_rule) <- succ flux.Data.flux_hits.(of_rule)

let get_flux_name flux = flux.Data.flux_name
let flux_has_name name flux = flux.Data.flux_name = name

let stop_flux env counter flux =
  let size = Environment.nb_syntactic_rules env + 1 in
  let flux_rules =
    Array.init size
	       (Format.asprintf "%a" (Environment.print_ast_rule ~env))
  in
  { Data.flux_rules = flux_rules; Data.flux_data = flux ;
    Data.flux_end = Counter.current_time counter }
