(* There are slight differences between the api datatyep as
   and the simulator datatypes.  This class serves to map
   the two implementations.
*)
module Api_types = ApiTypes_j
open Api_types

let plot_pg_store ~plot
                  ~file
                  ~title
                  ~descr : Pp_svg.store
  = { Pp_svg.file = file;
      Pp_svg.title = title;
      Pp_svg.descr = descr;
      Pp_svg.legend = Array.init (List.length plot.legend) (fun i -> List.nth plot.legend i);
      Pp_svg.points = List.map (fun (observable : observable) ->
                                (observable.Api_types.time,Array.map (fun x -> Nbr.F x) (Array.of_list observable.values) )
                               )
                               plot.observables }

let api_file_line (file_line : Data.file_line) : Api_types.file_line =
  { Api_types.file_name = file_line.Data.file_name
  ; Api_types.line = file_line.Data.line
  }

let api_flux_data (flux_data : Data.flux_data) : Api_types.flux_data =
  { Api_types.flux_name = flux_data.Data.flux_name
  ; Api_types.flux_start = flux_data.Data.flux_start
  ; Api_types.flux_hits = Array.to_list flux_data.Data.flux_hits
  ; Api_types.flux_fluxs = List.map Array.to_list (Array.to_list flux_data.Data.flux_fluxs)
  }

let api_flux_map (flux_map : Data.flux_map) : Api_types.flux_map =
  { Api_types.flux_rules = Array.to_list flux_map.Data.flux_rules
  ; Api_types.flux_data = api_flux_data flux_map.Data.flux_data
  ; Api_types.flux_end = flux_map.Data.flux_end
  }

let api_link (link : Raw_mixture.link) : Api_types.link =
  match link with
    Raw_mixture.FREE -> None
  | Raw_mixture.VAL v -> Some v

let api_internal (internal : Raw_mixture.internal) : Api_types.internal =
  internal
let api_agent (agent : Raw_mixture.agent) : Api_types.agent =
  { Api_types.a_id = agent.Raw_mixture.a_id;
    Api_types.a_type = agent.Raw_mixture.a_type;
    Api_types.a_ports = List.map api_link (Array.to_list agent.Raw_mixture.a_ports);
    Api_types.a_ints = List.map api_internal (Array.to_list agent.Raw_mixture.a_ints); }
let api_mixture (mixture : Raw_mixture.t) : Api_types.raw_mixture =
  List.map api_agent mixture

let api_snapshot (snapshot : Data.snapshot) : Api_types.snapshot =
  { Api_types.snap_file = snapshot.Data.snap_file
  ; Api_types.snap_event = snapshot.Data.snap_event
  ; Api_types.agents = List.map (fun (agent,mixture) -> { Api_types.agent = agent ; Api_types.mixture = api_mixture mixture } ) snapshot.Data.agents
  ; Api_types.tokens = List.map (fun (token,value) ->  { token = token; value = Nbr.to_float value}) (Array.to_list snapshot.Data.tokens)
}
