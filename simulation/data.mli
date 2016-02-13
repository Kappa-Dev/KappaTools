type flux_data = {
    flux_name : string;
    flux_start : float;
    flux_hits : int array;
    flux_fluxs : float array array;
  }
type flux_map =
  { flux_rules : string array;
    flux_data : flux_data;
    flux_end : float;
  }

type t = Flux of flux_map
       | Plot of float * Nbr.t array
