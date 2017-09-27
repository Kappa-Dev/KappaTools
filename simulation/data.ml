(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type snapshot = {
  snapshot_file : string;
  snapshot_event : int;
  snapshot_time : float;
  snapshot_agents : (int * User_graph.connected_component) list;
  snapshot_tokens : (string * Nbr.t) array;
}

let print_snapshot ?uuid f s =
  let () = Format.fprintf
      f "@[<v># Snapshot [Event: %d]@,"(*", Time: %f"*)s.snapshot_event in
  Format.fprintf
    f "%a%%def: \"T0\" \"%g\"@,@,%a@,%a@]@."
    (Pp.option ~with_space:false (fun f x -> Format.fprintf f "# \"uuid\" : \"%i\"@," x)) uuid
    s.snapshot_time
    (Pp.list Pp.space (fun f (i,mix) ->
         Format.fprintf f "%%init: %i /*%i agents*/ @[<h>%a@]" i
           (Array.length mix)
           (User_graph.print_cc ~explicit_free:false ~compact:false) mix))
    s.snapshot_agents
    (Pp.array Pp.space (fun _ f (na,el) ->
         Format.fprintf
           f "%%init: %a %s" Nbr.print el na))
    s.snapshot_tokens

let print_dot_snapshot ?uuid f s =
  let () = Format.fprintf
      f "@[<v># Snapshot [Event: %d]@,"(*", Time: %f"*)s.snapshot_event in
  Format.fprintf
    f "%adigraph G{@,%a@,%a}@]@."
    (Pp.option ~with_space:false (fun f x -> Format.fprintf f "// \"uuid\" : \"%i\"@," x)) uuid
    (Pp.listi
       Pp.cut
       (fun i f (nb,mix) ->
          Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
          Format.fprintf
            f "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]"
            i nb (User_graph.print_dot_cc i) mix))
    s.snapshot_agents
    (Pp.array Pp.cut (fun i f (na,el) ->
         Format.fprintf
           f "token_%d [label = \"%s (%a)\" , shape=none]"
           i na Nbr.print el))
    s.snapshot_tokens

let write_snapshot ob s =
  let () = Bi_outbuf.add_char ob '{' in
  let () = JsonUtil.write_field
      "snapshot_file" Yojson.Basic.write_string ob s.snapshot_file in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_event" Yojson.Basic.write_int ob s.snapshot_event in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_time" Yojson.Basic.write_float ob s.snapshot_time in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_agents"
      (JsonUtil.write_list
         (JsonUtil.write_compact_pair
            Yojson.Basic.write_int User_graph.write_connected_component))
      ob s.snapshot_agents in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "snapshot_tokens"
      (JsonUtil.write_array
         (JsonUtil.write_compact_pair Yojson.Basic.write_string Nbr.write_t))
      ob s.snapshot_tokens in
  Bi_outbuf.add_char ob '}'

let read_snapshot p lb =
  let
    snapshot_file,snapshot_event,snapshot_time,snapshot_agents,snapshot_tokens =
    Yojson.Basic.read_fields
      (fun (f,e,ti,a,t) key p lb ->
         if key = "snapshot_file" then (Yojson.Basic.read_string p lb,e,ti,a,t)
         else if key = "snapshot_event" then
           (f,Yojson.Basic.read_int p lb,ti,a,t)
         else if key = "snapshot_time" then
           (f,e,Yojson.Basic.read_number p lb,a,t)
         else if key = "snapshot_agents" then
           (f,e,ti,Yojson.Basic.read_list
              (JsonUtil.read_compact_pair
                 Yojson.Basic.read_int User_graph.read_connected_component) p lb,t)
         else let () = assert (key = "snapshot_tokens") in
           (f,e,ti,a,Yojson.Basic.read_array
              (JsonUtil.read_compact_pair Yojson.Basic.read_string Nbr.read_t)
              p lb)
      )
      ("",-1,nan,[],[||]) p lb in
  {snapshot_file;snapshot_event;snapshot_time;snapshot_agents;snapshot_tokens}

let string_of_snapshot ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  let () = write_snapshot ob x in
  Bi_outbuf.contents ob

let snapshot_of_string s =
  read_snapshot (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

type flux_data = {
  flux_name : string;
  flux_kind : Primitives.flux_kind;
  flux_start : float;
  flux_hits : int array;
  flux_fluxs : float array array;
}
type flux_map = {
  flux_rules : string array;
  flux_data : flux_data;
  flux_end : float;
}

let write_flux_map ob f =
  let () = Bi_outbuf.add_char ob '{' in
  let () = JsonUtil.write_field
      "flux_name" Yojson.Basic.write_string ob f.flux_data.flux_name in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "flux_kind" Primitives.write_flux_kind ob f.flux_data.flux_kind in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field
      "flux_start" Yojson.Basic.write_float ob f.flux_data.flux_start in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "flux_end"
      Yojson.Basic.write_float ob f.flux_end in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "flux_rules"
      (JsonUtil.write_array Yojson.Basic.write_string) ob f.flux_rules in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "flux_hits"
      (JsonUtil.write_array Yojson.Basic.write_int) ob f.flux_data.flux_hits in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "flux_fluxs"
      (JsonUtil.write_array (JsonUtil.write_array Yojson.Basic.write_float))
      ob f.flux_data.flux_fluxs in
  Bi_outbuf.add_char ob '}'

let read_flux_map p lb =
  let
    (flux_name,flux_kind,flux_start,flux_hits,flux_fluxs,flux_rules,flux_end) =
    Yojson.Basic.read_fields
      (fun (n,k,s,h,f,r,e) key p lb ->
         if key = "flux_name" then (Yojson.Basic.read_string p lb,k,s,h,f,r,e)
         else if key = "flux_kind" then
           (n,Primitives.read_flux_kind p lb,s,h,f,r,e)
         else if key = "flux_start" then
           (n,k,Yojson.Basic.read_number p lb,h,f,r,e)
         else if key = "flux_hits" then
           (n,k,s,Yojson.Basic.read_array Yojson.Basic.read_int p lb,f,r,e)
         else if key = "flux_fluxs" then
           (n,k,s,h,Yojson.Basic.read_array
              (Yojson.Basic.read_array Yojson.Basic.read_number) p lb,r,e)
         else if key = "flux_end" then
           (n,k,s,h,f,r,Yojson.Basic.read_number p lb)
         else let () = assert (key = "flux_rules") in
           (n,k,s,h,f,Yojson.Basic.read_array Yojson.Basic.read_string p lb,e))
      ("",Primitives.ABSOLUTE,nan,[||],[||],[||],nan) p lb in
  { flux_rules;flux_end;
    flux_data={ flux_name;flux_kind;flux_start;flux_hits;flux_fluxs } }

let string_of_flux_map ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  let () = write_flux_map ob x in
  Bi_outbuf.contents ob

let flux_map_of_string s =
  read_flux_map (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let print_dot_flux_map ?uuid desc flux =
  let () = Format.fprintf desc "@[<v>%a"
      (Pp.option ~with_space:false
         (fun f x -> Format.fprintf f "// \"uuid\" : \"%i\",@," x))
         uuid in
  let () = Format.fprintf
      desc "digraph G{ label=\"Flux map\" ; labelloc=\"t\" ; " in
  let () = Format.fprintf
      desc "node [shape=box,style=filled,fillcolor=lightskyblue]@," in
  let () =
    Pp.array
      (fun _ -> ())
      (fun s ->
         Pp.array
           Pp.empty
           (fun d f v ->
              if v=0. then ()
              else
                let color,arrowhead =
                  if v<0. then ("red3","tee") else ("green3","normal") in
                Format.fprintf
                  f
                  "@[<h>\"%s\" -> \"%s\" [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];@]@,"
                  flux.flux_rules.(s)
                  flux.flux_rules.(d)
                  (abs (int_of_float v)) v color arrowhead))
      desc flux.flux_data.flux_fluxs in
  Format.fprintf desc "}@]@."

let print_html_flux_map desc flux =
  Pp_html.graph_page
    (fun f -> Format.pp_print_string f "Dynamic influence map")
    ~subtitle:(fun f -> Format.pp_print_string
                  f "between t = <span id=\"begin_time\"></span>s and t = <span id=\"end_time\"></span>s (<span id=\"nb_events\"></span> events)")
    ["http://d3js.org/d3.v4.min.js"]
    (fun f ->
       let () =
         Format.fprintf
           f "@[<v 2><style>@,.chord path {@ fill-opacity: .67;@ " in
       Format.fprintf
         f "stroke: #000;@ stroke-width: .5px;@ }@]@,</style>")
    (fun f ->
       let () = Format.fprintf f "@[<hv 2><form>@," in
       let () = Format.fprintf f "@[<v 2><div class=\"form-group\">@," in
       let () =
         Format.fprintf f "<label for=\"correction\">Correction</label>@," in
       let () =
         Format.fprintf
           f
           "<select id=\"select_correction\" class=\"form-control\" id=\"correction\">@," in
       let () =
         Format.fprintf f "<option value=\"none\">None</option>@," in
       let () = Format.fprintf
           f "<option value=\"hits\">Rule occurences</option>@," in
       let () = Format.fprintf
           f "<option value=\"time\">Time</option>@]@,</select>@,</div>@," in
       let () = Format.fprintf
           f "<input id=\"toggle_selected_rules\" type=\"button\" value=\"Toggle selected rules\">@," in
       let () = Format.fprintf f "@[<v 2><label class=\"checkbox-inline\">@," in
       let () =
         Format.fprintf
           f
           "<input id=\"checkbox_self_influence\" type=\"checkbox\">@," in
       let () =
         Format.fprintf f "Rules self influence@]@,</label>@]@,</form>@," in
       let () = Format.fprintf f "<form id=\"menu\"></form>@," in
       let () = Format.fprintf
           f "@[<v 2><script>@,%s@,</script>@]@," Resource_strings.common_js in
       let () = Format.fprintf
           f "@[<v 2><script>@,%s@,</script>@]@," Resource_strings.flux_js in

       let () = Format.fprintf
           f "@[<v 2><script>@,\"use strict\"@,@[var flux =@ %s;@]@,"
           (string_of_flux_map flux) in
       let () = Format.fprintf f "var ids = {@[@," in
       let () = Format.fprintf f "\"beginTimeId\" : \"begin_time\",@ " in
       let () = Format.fprintf f "\"endTimeId\" : \"end_time\",@ " in
       let () = Format.fprintf f "\"selectCorrectionId\" : \"select_correction\",@ " in
       let () = Format.fprintf f "\"nbEventsId\" : \"nb_events\",@ " in
       let () = Format.fprintf f "\"rulesCheckboxesId\" : \"menu\",@ " in
       let () = Format.fprintf f "\"toggleRulesId\" : \"toggle_selected_rules\",@ " in
       let () = Format.fprintf f "\"checkboxSelfInfluenceId\" : \"checkbox_self_influence\"};@]@ " in
       let () = Format.fprintf f "window.onload = function(){ @[@," in
       let () = Format.fprintf f "var flux_map = new fluxMap(ids);@ " in
       let () = Format.fprintf f "flux_map.setFlux(flux); }; @]@," in
       Format.fprintf f "@]@,</script>")
    desc

type file_line = {
  file_line_name : string option;
  file_line_text : string;
}

type t =
  | Flux of flux_map
  | DeltaActivities of int * (int * (float * float)) list
  | Plot of Nbr.t array (** Must have length >= 1 (at least [T] or [E]) *)
  | Print of file_line
  | TraceStep of Trace.step
  | Snapshot of snapshot
  | Log of string
  | Species of string * float * User_graph.connected_component
