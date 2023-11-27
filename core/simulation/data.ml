(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let print_initial_inputs ?uuid conf env inputs_form init =
  let noCounters = false in
  let () =
    match uuid with
    | None -> ()
    | Some uuid -> Format.fprintf inputs_form "// \"uuid\" : \"%i\"@." uuid
  in
  let () =
    Format.fprintf inputs_form "%a@.%a@." Configuration.print conf
      (Kappa_printer.env_kappa ~noCounters)
      env
  in
  let sigs = Model.signatures env in
  Format.fprintf inputs_form "@.@[<v>%a@]@."
    (Pp.list Pp.space (fun f (n, r) ->
         let _, ins_fresh =
           Pattern_compiler.lkappa_of_elementary_rule sigs (Model.domain env) r
         in
         let () =
           if ins_fresh <> [] then (
             let () =
               Format.fprintf f "@[<hov 2>%%init:@ @[%a@]@ @[%a@]@]"
                 (Kappa_printer.alg_expr ~noCounters ~env)
                 n
                 (Raw_mixture.print ~noCounters ~created:false
                    ~initial_comma:false ~sigs)
                 ins_fresh
             in
             if r.Primitives.delta_tokens <> [] then Format.pp_print_space f ()
           )
         in
         Pp.list Pp.space
           (fun f (nb, tk) ->
             Format.fprintf f "@[<hov 2>%%init:@ @[%a@]@ %a@]"
               (Kappa_printer.alg_expr ~noCounters ~env)
               (fst (Alg_expr.mult (Loc.annot_with_dummy n) nb))
               (Model.print_token ~env) tk)
           f r.Primitives.delta_tokens))
    init

type snapshot = {
  snapshot_event: int;
  snapshot_time: float;
  snapshot_agents: (int * User_graph.connected_component) list;
  snapshot_tokens: (string * Nbr.t) array;
}

let print_snapshot ?uuid f s =
  let () =
    Format.fprintf f "@[<v>// Snapshot [Event: %d]@,"
      (*", Time: %f"*) s.snapshot_event
  in
  Format.fprintf f "%a%%def: \"T0\" \"%s\"@,@,%a@,%a@]@."
    (Pp.option ~with_space:false (fun f x ->
         Format.fprintf f "// \"uuid\" : \"%i\"@," x))
    uuid
    (JsonUtil.std_json_string_of_float s.snapshot_time)
    (Pp.list Pp.space (fun f (i, mix) ->
         Format.fprintf f "@[<hov 2>%%init: %i /*%i agents*/ %a@]" i
           (Array.fold_left (fun s e -> s + Array.length e) 0 mix)
           User_graph.print_cc mix))
    s.snapshot_agents
    (Pp.array Pp.space (fun _ f (na, el) ->
         Format.fprintf f "%%init: %a %s" Nbr.print el na))
    s.snapshot_tokens

let print_dot_snapshot ?uuid f s =
  let () =
    Format.fprintf f "@[<v>// Snapshot [Event: %d]@,"
      (*", Time: %f"*) s.snapshot_event
  in
  Format.fprintf f "%adigraph G{@,%a@,%a}@]@."
    (Pp.option ~with_space:false (fun f x ->
         Format.fprintf f "// \"uuid\" : \"%i\"@," x))
    uuid
    (Pp.listi Pp.cut (fun i f (nb, mix) ->
         Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
         Format.fprintf f
           "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]" i nb
           (User_graph.print_dot_cc i)
           mix))
    s.snapshot_agents
    (Pp.array Pp.cut (fun i f (na, el) ->
         Format.fprintf f "token_%d [label = \"%s (%a)\" , shape=none]" i na
           Nbr.print el))
    s.snapshot_tokens

let write_snapshot ob s =
  let () = Buffer.add_char ob '{' in
  let () =
    JsonUtil.write_field "snapshot_event" Yojson.Basic.write_int ob
      s.snapshot_event
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "snapshot_time" Yojson.Basic.write_float ob
      s.snapshot_time
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "snapshot_agents"
      (JsonUtil.write_list
         (JsonUtil.write_compact_pair Yojson.Basic.write_int
            User_graph.write_connected_component))
      ob s.snapshot_agents
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "snapshot_tokens"
      (JsonUtil.write_array
         (JsonUtil.write_compact_pair Yojson.Basic.write_string Nbr.write_t))
      ob s.snapshot_tokens
  in
  Buffer.add_char ob '}'

let read_snapshot p lb =
  let snapshot_event, snapshot_time, snapshot_agents, snapshot_tokens =
    Yojson.Basic.read_fields
      (fun (e, ti, a, t) key p lb ->
        if key = "snapshot_event" then
          Yojson.Basic.read_int p lb, ti, a, t
        else if key = "snapshot_time" then
          e, Yojson.Basic.read_number p lb, a, t
        else if key = "snapshot_agents" then
          ( e,
            ti,
            Yojson.Basic.read_list
              (JsonUtil.read_compact_pair Yojson.Basic.read_int
                 User_graph.read_connected_component)
              p lb,
            t )
        else (
          let () = assert (key = "snapshot_tokens") in
          ( e,
            ti,
            a,
            Yojson.Basic.read_array
              (JsonUtil.read_compact_pair Yojson.Basic.read_string Nbr.read_t)
              p lb )
        ))
      (-1, nan, [], [||]) p lb
  in
  { snapshot_event; snapshot_time; snapshot_agents; snapshot_tokens }

let string_of_snapshot = JsonUtil.string_of_write write_snapshot

let snapshot_of_string s =
  read_snapshot (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

type din_data = {
  din_kind: Primitives.din_kind;
  din_start: float;
  din_hits: int array;
  din_fluxs: float array array;
}

type din = { din_rules: string array; din_data: din_data; din_end: float }

let write_din ob f =
  let () = Buffer.add_char ob '{' in
  let () =
    JsonUtil.write_field "din_kind" Primitives.write_din_kind ob
      f.din_data.din_kind
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "din_start" Yojson.Basic.write_float ob
      f.din_data.din_start
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "din_end" Yojson.Basic.write_float ob f.din_end
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "din_rules"
      (JsonUtil.write_array Yojson.Basic.write_string)
      ob f.din_rules
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "din_hits"
      (JsonUtil.write_array Yojson.Basic.write_int)
      ob f.din_data.din_hits
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "din_fluxs"
      (JsonUtil.write_array (JsonUtil.write_array Yojson.Basic.write_float))
      ob f.din_data.din_fluxs
  in
  Buffer.add_char ob '}'

let read_din p lb =
  let din_kind, din_start, din_hits, din_fluxs, din_rules, din_end =
    Yojson.Basic.read_fields
      (fun (k, s, h, f, r, e) key p lb ->
        if key = "din_kind" then
          Primitives.read_din_kind p lb, s, h, f, r, e
        else if key = "din_start" then
          k, Yojson.Basic.read_number p lb, h, f, r, e
        else if key = "din_hits" then
          k, s, Yojson.Basic.read_array Yojson.Basic.read_int p lb, f, r, e
        else if key = "din_fluxs" then
          ( k,
            s,
            h,
            Yojson.Basic.read_array
              (Yojson.Basic.read_array Yojson.Basic.read_number)
              p lb,
            r,
            e )
        else if key = "din_end" then
          k, s, h, f, r, Yojson.Basic.read_number p lb
        else (
          let () = assert (key = "din_rules") in
          k, s, h, f, Yojson.Basic.read_array Yojson.Basic.read_string p lb, e
        ))
      (Primitives.ABSOLUTE, nan, [||], [||], [||], nan)
      p lb
  in
  {
    din_rules;
    din_end;
    din_data = { din_kind; din_start; din_hits; din_fluxs };
  }

let string_of_din = JsonUtil.string_of_write write_din

let din_of_string s =
  read_din (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let print_dot_din ?uuid desc flux =
  let () =
    Format.fprintf desc "@[<v>%a"
      (Pp.option ~with_space:false (fun f x ->
           Format.fprintf f "// \"uuid\" : \"%i\",@," x))
      uuid
  in
  let () =
    Format.fprintf desc
      "digraph G{ label=\"Dynamic influence network\" ; labelloc=\"t\" ; "
  in
  let () =
    Format.fprintf desc "node [shape=box,style=filled,fillcolor=lightskyblue]@,"
  in
  let () =
    Pp.array
      (fun _ -> ())
      (fun s ->
        Pp.array Pp.empty (fun d f v ->
            if v = 0. then
              ()
            else (
              let color, arrowhead =
                if v < 0. then
                  "red3", "tee"
                else
                  "green3", "normal"
              in
              Format.fprintf f
                "@[<h>\"%s\" -> \"%s\" \
                 [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];@]@,"
                flux.din_rules.(s) flux.din_rules.(d)
                (abs (int_of_float v))
                v color arrowhead
            )))
      desc flux.din_data.din_fluxs
  in
  Format.fprintf desc "}@]@."

let print_html_din desc flux =
  Pp_html.graph_page
    (fun f -> Format.pp_print_string f "Dynamic influence map")
    ~subtitle:(fun f ->
      Format.pp_print_string f
        "between t = <span id=\"begin_time\"></span>s and t = <span \
         id=\"end_time\"></span>s (<span id=\"nb_events\"></span> events)")
    [
      "http://d3js.org/d3.v4.min.js";
      "https://code.jquery.com/jquery-3.3.1.min.js";
    ]
    (fun f ->
      let () =
        Format.fprintf f "@[<v 2><style>@,.chord path {@ fill-opacity: .67;@ "
      in
      Format.fprintf f "stroke: #000;@ stroke-width: .5px;@ }@]@,</style>")
    (fun f ->
      let () = Format.fprintf f "<div id=\"flux_container\"></div>@," in
      let () = Format.fprintf f "@[<hv 2><form>@," in
      let () = Format.fprintf f "@[<v 2><div class=\"form-group\">@," in
      let () =
        Format.fprintf f "<label for=\"correction\">Correction</label>@,"
      in
      let () =
        Format.fprintf f
          "<select id=\"select_correction\" class=\"form-control\" \
           id=\"correction\">@,"
      in
      let () = Format.fprintf f "<option value=\"none\">None</option>@," in
      let () =
        Format.fprintf f "<option value=\"hits\">Rule occurences</option>@,"
      in
      let () =
        Format.fprintf f
          "<option value=\"time\">Time</option>@]@,</select>@,</div>@,"
      in
      let () =
        Format.fprintf f
          "<input id=\"toggle_selected_rules\" type=\"button\" value=\"Toggle \
           selected rules\">@,"
      in
      let () = Format.fprintf f "@[<v 2><label class=\"checkbox-inline\">@," in
      let () =
        Format.fprintf f
          "<input id=\"checkbox_self_influence\" type=\"checkbox\">@,"
      in
      let () =
        Format.fprintf f "Rules self influence@]@,</label>@]@,</form>@,"
      in
      let () = Format.fprintf f "<form id=\"menu\"></form>@," in
      let () =
        Format.fprintf f "@[<v 2><script>@,%s@,</script>@]@,"
          Resource_strings.common_js
      in
      let () =
        Format.fprintf f "@[<v 2><script>@,%s@,</script>@]@,"
          Resource_strings.flux_js
      in

      let () =
        Format.fprintf f
          "@[<v 2><script>@,\"use strict\"@,@[var flux =@ %s;@]@,"
          (string_of_din flux)
      in
      let () = Format.fprintf f "var ids = {@[@," in
      let () = Format.fprintf f "\"beginTimeId\" : \"begin_time\",@ " in
      let () = Format.fprintf f "\"endTimeId\" : \"end_time\",@ " in
      let () =
        Format.fprintf f "\"selectCorrectionId\" : \"select_correction\",@ "
      in
      let () = Format.fprintf f "\"nbEventsId\" : \"nb_events\",@ " in
      let () = Format.fprintf f "\"rulesCheckboxesId\" : \"menu\",@ " in
      let () =
        Format.fprintf f "\"toggleRulesId\" : \"toggle_selected_rules\",@ "
      in
      let () =
        Format.fprintf f
          "\"checkboxSelfInfluenceId\" : \"checkbox_self_influence\"};@]@ "
      in
      let () = Format.fprintf f "window.onload = function(){ @[@," in
      let () = Format.fprintf f "var flux_map = new fluxMap(ids);@ " in
      let () = Format.fprintf f "flux_map.setFlux(flux); }; @]@," in
      Format.fprintf f "@]@,</script>")
    desc

type plot = { plot_legend: string array; plot_series: float option array list }

let add_plot_line new_observables plot =
  let new_values = Array.map (fun nbr -> Nbr.to_float nbr) new_observables in
  {
    plot_legend = plot.plot_legend;
    plot_series = new_values :: plot.plot_series;
  }

let init_plot env =
  let noCounters = false in
  let plot_legend =
    Model.map_observables
      (fun o ->
        Format.asprintf "@[%a@]" (Kappa_printer.alg_expr ~noCounters ~env) o)
      env
  in
  { plot_legend; plot_series = [] }

let write_plot ob f =
  let () = Buffer.add_char ob '{' in
  let () =
    JsonUtil.write_field "legend"
      (JsonUtil.write_array Yojson.Basic.write_string)
      ob f.plot_legend
  in
  let () = JsonUtil.write_comma ob in
  let () =
    JsonUtil.write_field "series"
      (JsonUtil.write_list
         (JsonUtil.write_array (JsonUtil.write_option Yojson.Basic.write_float)))
      ob f.plot_series
  in
  Buffer.add_char ob '}'

let read_plot p lb =
  let plot_legend, plot_series =
    Yojson.Basic.read_fields
      (fun (l, s) key p lb ->
        if key = "series" then
          ( l,
            Yojson.Basic.read_list
              (Yojson.Basic.read_array
                 (JsonUtil.read_option Yojson.Basic.read_number))
              p lb )
        else (
          let () = assert (key = "legend") in
          Yojson.Basic.read_array Yojson.Basic.read_string p lb, s
        ))
      ([||], []) p lb
  in
  { plot_legend; plot_series }

let string_of_plot = JsonUtil.string_of_write write_plot

let plot_of_string s =
  read_plot (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let print_plot_sep is_tsv f =
  Format.pp_print_string f
    (if is_tsv then
       "\t"
     else
       ",")

let print_plot_legend ~is_tsv f a =
  Format.fprintf f "@[<h>%a@]@."
    (Pp.array (print_plot_sep is_tsv) (fun _ f x -> Format.fprintf f "\"%s\"" x))
    a

let print_plot_line ~is_tsv pp f l =
  Format.fprintf f "@[<h>%a@]@."
    (Pp.array (print_plot_sep is_tsv) (fun _ -> pp))
    l

let export_plot ~is_tsv plot =
  Format.asprintf "%a%a"
    (print_plot_legend ~is_tsv)
    plot.plot_legend
    (Pp.list Pp.empty
       (print_plot_line ~is_tsv (Pp.option (fun f -> Format.fprintf f "%e"))))
    (List.rev plot.plot_series)

let print_warning ?pos f msg =
  let pr f () = Format.fprintf f "Warning: @[%t@]" msg in
  match pos with
  | Some pos -> Format.fprintf f "@[<v>%a@]@." (Loc.print_annoted pr) ((), pos)
  | None -> Format.fprintf f "@[%a@]@." pr ()

type file_line = { file_line_name: string option; file_line_text: string }

type t =
  | DIN of string * din
  | DeltaActivities of int * (int * (float * float)) list
  | Plot of Nbr.t array  (** Must have length >= 1 (at least [T] or [E]) *)
  | Print of file_line
  | TraceStep of Trace.step
  | Snapshot of string * snapshot
  | Log of string
  | Species of string * float * User_graph.connected_component
  | Warning of Loc.t option * (Format.formatter -> unit)
