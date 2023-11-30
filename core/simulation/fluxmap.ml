(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let create_flux env counter din_kind =
  let size = Model.nb_syntactic_rules env + 1 in
  {
    Data.din_kind;
    Data.din_start = Counter.current_time counter;
    Data.din_hits = Array.make size 0;
    Data.din_fluxs = Array.make_matrix size size 0.;
  }

let incr_flux_flux of_rule on_rule v flux =
  flux.Data.din_fluxs.(of_rule).(on_rule) <-
    flux.Data.din_fluxs.(of_rule).(on_rule) +. v

let incr_flux_hit of_rule flux =
  flux.Data.din_hits.(of_rule) <- succ flux.Data.din_hits.(of_rule)

let stop_flux env counter din_data =
  let size = Model.nb_syntactic_rules env + 1 in
  let din_rules =
    Array.init size (fun x ->
        Format.asprintf "%a" (Model.print_ast_rule ~noCounters:false ~env) x)
  in
  let () =
    match din_data.Data.din_kind with
    | Primitives.ABSOLUTE -> ()
    | Primitives.RELATIVE | Primitives.PROBABILITY ->
      Array.iteri
        (fun i ->
          Array.iteri (fun j x ->
              din_data.Data.din_fluxs.(i).(j) <-
                (if din_data.Data.din_hits.(i) = 0 then
                   x
                 else
                   x /. float_of_int din_data.Data.din_hits.(i))))
        din_data.Data.din_fluxs
  in
  { Data.din_rules; Data.din_data; Data.din_end = Counter.current_time counter }
