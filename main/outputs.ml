let print_desc : (string,out_channel * Format.formatter) Hashtbl.t =
  Hashtbl.create 2

let get_desc file =
  try snd (Hashtbl.find print_desc file)
  with Not_found ->
    let d_chan = Kappa_files.open_out file in
    let d = Format.formatter_of_out_channel d_chan in
    (Hashtbl.add print_desc file (d_chan,d) ; d)

let close_desc () =
  Hashtbl.iter (fun _file (d_chan,_d) -> close_out d_chan) print_desc


let dot_of_flux flux =
  let printer desc =
    let () = Format.fprintf
        desc "@[<v>digraph G{ label=\"Flux map\" ; labelloc=\"t\" ; " in
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
                    flux.Data.flux_rules.(s)
                    flux.Data.flux_rules.(d)
                    (abs (int_of_float v)) v color arrowhead))
        desc flux.Data.flux_data.Data.flux_fluxs in
    Format.fprintf desc "}@]@."
  in
  Kappa_files.with_flux flux.Data.flux_data.Data.flux_name printer

let print_json_of_flux f flux =
  let () = Format.fprintf
      f "@[<v>{@ \"bioBeginTime\" : %e,@ \"bioEndTime\" : %e,@ "
      flux.Data.flux_data.Data.flux_start flux.Data.flux_end in
  let () =
    Format.fprintf
      f "@[\"rules\" :@ @[[%a]@]@],@ @[\"hits\" :@ @[[%a]@]@],@ "
      (Pp.array Pp.comma (fun _ f x -> Format.fprintf f "\"%s\"" x))
      flux.Data.flux_rules
      (Pp.array Pp.comma (fun _ -> Format.pp_print_int))
      flux.Data.flux_data.Data.flux_hits in
  Format.fprintf
    f "@[\"fluxs\" :@ @[[%a]@]@]@ }@]"
    (Pp.array
       Pp.comma
       (fun _ f x ->
          Format.fprintf
            f "@[[%a]@]"
            (Pp.array Pp.comma (fun _ f y -> Format.fprintf f "%e" y)) x))
    flux.Data.flux_data.Data.flux_fluxs

let json_of_flux flux =
  Kappa_files.with_flux
    flux.Data.flux_data.Data.flux_name (fun f -> print_json_of_flux f flux)

let html_of_flux flux =
  Kappa_files.with_flux
    flux.Data.flux_data.Data.flux_name
    (Pp_html.graph_page
       (fun f -> Format.pp_print_string f "Dynamic influence map")
       ~subtitle:(fun f -> Format.pp_print_string
                     f "between t = <span id=\"begin_time\"></span>s and t = <span id=\"end_time\"></span>s (<span id=\"nb_events\"></span> events)")
       ["http://d3js.org/d3.v3.min.js"]
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
              f "@[<v 2><script>@,\"use strict\"@,@[var flux =@ %a;@]@,"
              print_json_of_flux flux in
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
          Format.fprintf f "@]@,</script>"))

let output_flux out =
  if Filename.check_suffix out.Data.flux_data.Data.flux_name ".html"
  then html_of_flux out
  else if Filename.check_suffix out.Data.flux_data.Data.flux_name ".json"
  then json_of_flux out
  else dot_of_flux out

let jsonDistancesDescr = ref false
let distances = ref (None : (string array * (float * int) list array) option)

let create_distances names in_json =
  let () = jsonDistancesDescr := in_json in
  distances := Some (names, Array.make (Array.length names) [])

let print_json_of_unary_distances f unary_distances rules =
  (*unary_distances: (float, int) option list array
    one_big_list: (int, float, int) list*)
  let one_big_list =
    Tools.array_fold_lefti
      (fun i l ls ->
         List.fold_left (fun acc (t,d) -> (i,t,d)::acc) l ls)
      [] unary_distances in
  Format.fprintf
    f "[%a]@."
    (Pp.list
       Pp.comma
       (fun f (id,time,distance) ->
          Format.fprintf
            f "@[{ \"rule\" : \"%s\", @ \"time\" : %e, @ \"distance\" : %d }@]"
            rules.(id) time distance)) (List.rev one_big_list)

let json_of_unary_distances unary_distances rules =
  Kappa_files.with_unary_distances
    (fun f -> print_json_of_unary_distances f unary_distances rules)

let print_out_of_unary_distances f distances rule_name =
  let () = Format.fprintf f "Rule %s: " rule_name in
  let () = Format.fprintf f "@[<h>%s @]@." "time distance" in
  Format.fprintf f "@[%a@]"
    (Pp.list Pp.space (fun f (time,distance) ->
         Format.fprintf f "@[ %e @ %d @]@."
           time distance)) distances

let out_of_unary_distances unary_distances rules =
  Array.iteri (fun id distances_list ->
      match distances_list with
      | [] -> ()
      | ls ->
        (*create the file *)
        let filename = Kappa_files.get_distances () in
        let filename_string = filename^(string_of_int id)^".out" in
        let d = Kappa_files.open_out filename_string in
        let f = Format.formatter_of_out_channel d in
        (*print data*)
        let () = print_out_of_unary_distances f ls rules.(id) in
        (*close the file*)
        close_out d) unary_distances

let output_unary_distances in_json distances_data distances_rules =
  if in_json
  then json_of_unary_distances distances_data distances_rules
  else out_of_unary_distances distances_data distances_rules

type fd = {
  desc:out_channel;
  form:Format.formatter;
  is_tsv:bool;
}

type format = Raw of fd | Svg of Pp_svg.store

let plotDescr = ref None

let close_plot () =
  match !plotDescr with
  | None -> ()
  | Some (Raw plot) -> close_out plot.desc
  | Some (Svg s) -> Pp_svg.to_file s

let print_header_raw is_tsv f a =
  let print_sep =
    if is_tsv then fun f -> Format.pp_print_string f "\t"
    else !Parameter.plotSepChar in
  Format.fprintf f "@[<h>%s%t%a@]@."
    (if is_tsv then "time" else "# time") print_sep
    (Pp.array print_sep (fun _ -> Format.pp_print_string)) a

let print_values_raw is_tsv f (time,l) =
  let print_sep =
    if is_tsv then fun f -> Format.pp_print_string f "\t"
    else !Parameter.plotSepChar in
  Format.fprintf f "@[<h>%t%s%t%a@]@."
    !Parameter.plotSepChar (string_of_float time) print_sep
    (Pp.array print_sep (fun _ -> Nbr.print)) l

let create_plot (filename,title,head) =
  let format =
    if Filename.check_suffix filename ".svg" then
      Svg {Pp_svg.file = filename;
           Pp_svg.title = title;
           Pp_svg.descr = "";
           Pp_svg.legend = head;
           Pp_svg.points = [];
          }
    else
      let d_chan = Kappa_files.open_out filename in
      let d = Format.formatter_of_out_channel d_chan in
      let is_tsv = Filename.check_suffix filename ".tsv" in
      let () = if not is_tsv then Format.fprintf d "# %s@." title in
      let () = print_header_raw is_tsv d head in
      Raw {desc=d_chan; form=d; is_tsv} in
  plotDescr := Some format

let plot_now l =
  match !plotDescr with
  | None -> assert false
  | Some (Raw fd) -> print_values_raw fd.is_tsv fd.form l
  | Some (Svg s) -> s.Pp_svg.points <- l :: s.Pp_svg.points

let print_snapshot sigs f s =
  Format.fprintf
    f "@[<v>%a@,%a@]"
    (Pp.list Pp.space (fun f (i,mix) ->
         Format.fprintf f "%%init: %i @[<h>%a@]" i
           (Raw_mixture.print ~compact:false sigs) mix))
    s.Data.snapshot_agents
    (Pp.array Pp.space (fun _ f (na,el) ->
         Format.fprintf
           f "%%init: %s <- %a" na Nbr.print el))
    s.Data.snapshot_tokens

let print_dot_snapshot sigs f s =
  Format.fprintf
    f "@[<v>digraph G{@,%a@,%a}@]"
    (Pp.listi
       Pp.cut
       (fun i f (nb,mix) ->
          Format.fprintf f "@[<v 2>subgraph cluster%d{@," i;
          Format.fprintf
            f "counter%d [label = \"%d instance(s)\", shape=none];@,%a}@]"
            i nb (Raw_mixture.print_dot sigs i) mix))
    s.Data.snapshot_agents
    (Pp.array Pp.cut (fun i f (na,el) ->
         Format.fprintf
           f "token_%d [label = \"%s (%a)\" , shape=none]"
           i na Nbr.print el))
    s.Data.snapshot_tokens

let snapshot env s =
  if Filename.check_suffix s.Data.snapshot_file ".dot" then
    Kappa_files.with_snapshot
      s.Data.snapshot_file s.Data.snapshot_event "dot"
      (fun f -> Format.fprintf f "%a@." (print_dot_snapshot env) s)
  else
    Kappa_files.with_snapshot
      s.Data.snapshot_file s.Data.snapshot_event "ka"
      (fun f -> Format.fprintf f "%a@." (print_snapshot env) s)

let go env = function
  | Data.Snapshot s -> snapshot env s
  | Data.Flux f -> output_flux f
  | Data.Plot (x,y) -> plot_now (x,y)
  | Data.Print p ->
    let desc =
      match p.Data.file_line_name with
        None -> Format.formatter_of_out_channel stdout
      | Some file -> get_desc file
    in
    Format.fprintf desc "%s@." p.Data.file_line_text
  | Data.Log s -> Format.printf "%s@." s
  | Data.UnaryDistance d ->
    match !distances with
    | None -> ()
    | Some (_,tab) ->
      tab.(d.Data.distance_rule) <-
        (d.Data.distance_time,d.Data.distance_length)::tab.(d.Data.distance_rule)

let close () =
  let () = close_plot () in
  let () =
    match !distances with
    | None -> ()
    | Some (rules,data) ->
      output_unary_distances !jsonDistancesDescr data rules in
  close_desc ()
