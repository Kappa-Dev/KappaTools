open Mods

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
      (Pp.array Pp.comma (fun _ f x -> Format.fprintf f "\"%s\"" x)) flux.Data.flux_rules
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

type fd = {
  desc:out_channel;
  form:Format.formatter;
}

type format = Raw of fd | Svg of Pp_svg.store

let plotDescr = ref None

let close_plot () =
  match !plotDescr with
  | None -> ()
  | Some plot ->
     match plot with
     | Raw plot -> close_out plot.desc
     | Svg s -> Pp_svg.to_file s

let print_header_raw f a =
  Format.fprintf f "@[<h>%s%t%a@]@."
                 (if !Parameter.emacsMode then "time" else "# time")
                 !Parameter.plotSepChar
                 (Pp.array !Parameter.plotSepChar
                           (fun _ -> Format.pp_print_string)) a

let print_values_raw f (time,l) =
  Format.fprintf f "@[<h>%t%E%t%a@]@."
                 !Parameter.plotSepChar time !Parameter.plotSepChar
                 (Pp.array !Parameter.plotSepChar (fun _ -> Nbr.print)) l

let create_plot filename head =
  let title =
    if !Parameter.marshalizedInFile <> ""
    then !Parameter.marshalizedInFile ^" output"
    else match !Parameter.inputKappaFileNames with
         | [ f ] -> f^" output"
         | _ -> "KaSim output" in
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
      let () = print_header_raw d head in
      Raw {desc=d_chan; form=d} in
  plotDescr :=
    Some format

let plot_now l =
  match !plotDescr with
  | None -> assert false
  | Some (Raw fd) -> print_values_raw fd.form l
  | Some (Svg s) -> s.Pp_svg.points <- l :: s.Pp_svg.points

let unary_distances_list = ref []

let print_header_distances f rule_id distance =
  let () = Format.fprintf f "Rule %i: " rule_id in
  let () = Format.fprintf f "@[<h>%s%t"
                          (if !Parameter.emacsMode then "time" else "# time")
                          !Parameter.plotSepChar in
  let rec print_nb i = if (i <= distance) then
                         let () = Format.fprintf f "%i%t" i !Parameter.plotSepChar in
                         print_nb (i+1)
                       else Format.fprintf f "@]@." in
  print_nb 0

let create_files_list ids_list filename =
  List.map (fun (id,dist) ->
    let filename_string = filename^(string_of_int id)^".out" in
    let d = Kappa_files.open_out filename_string in
    let f = Format.formatter_of_out_channel d in
    let () = print_header_distances f id dist in
    (id,{desc=d; form=f}))
    ids_list

let print_distances f time arr =
  let () = Format.fprintf f "@[<h>%t%E%t"
                          !Parameter.plotSepChar time !Parameter.plotSepChar in
  let () = DynArray.iter
             (fun i -> Format.fprintf f "%i%t" i !Parameter.plotSepChar) arr in
  Format.fprintf f " @]@."

let print_time_distances time_distances files_list=
  List.iter (fun (time, rules_arr) ->
             List.iter (fun (id,fd) ->
                        match rules_arr.(id) with
                        | None -> assert false
                        | Some distances ->
                           print_distances fd.form time distances)
                       files_list)
            time_distances

(* format the list time_distances in order to have max_distance columns for all rules at all times*)
let format_unary_distances time_distances max_distances =
  List.map
    (fun (time,rules_arr) ->
     let () =
       List.iter (fun (id, maxd) ->
                 match rules_arr.(id) with
                 | Some dyn_arr -> if ((DynArray.length dyn_arr) < maxd) then
                                     DynArray.set dyn_arr (maxd-1) 0
                 | None -> rules_arr.(id) <-
                             Some (DynArray.make maxd 0)) max_distances in
     (time, rules_arr)) time_distances

let close_distances () =
  let rec ids_max_list rules_arr i =
    if (i < (Array.length rules_arr)) then
      let new_list = ids_max_list rules_arr (i+1) in
      match rules_arr.(i) with Some dyn_arr ->
                               (i,DynArray.length dyn_arr)::new_list
                             | None -> new_list
    else [] in
  match !unary_distances_list with
  | [] ->  ()
  | (_,last_rules_arr) :: _ as time_distances ->
     let ids_max_dist = ids_max_list last_rules_arr 0 in
     let formatted = format_unary_distances time_distances ids_max_dist in
     let files_list =
       create_files_list ids_max_dist (Kappa_files.get_distances ()) in
     let () = print_time_distances formatted files_list in
     List.iter (fun (_,fd) -> close_out fd.desc) files_list

let unary_distances time arr =
  let deepcopy arr =
    let arr' = Array.copy arr in
    Array.map (fun dist_arr ->
               match dist_arr with
               | None -> None
               | Some dyn_arr -> Some (DynArray.copy dyn_arr)) arr' in
  unary_distances_list := (time, (deepcopy arr))::!unary_distances_list

let print_snapshot sigs f s =
  Format.fprintf
    f "@[<v>%a@,%a@]"
    (Pp.list Pp.space (fun f (i,mix) ->
                       Format.fprintf f "%%init: %i @[<h>%a@]" i
                                      (Raw_mixture.print ~compact:false sigs) mix))
    s.Data.agents
    (Pp.array Pp.space (fun _ f (na,el) ->
                        Format.fprintf
                          f "%%init: %s <- %a" na Nbr.print el))
    s.Data.tokens

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
    s.Data.agents
    (Pp.array Pp.cut (fun i f (na,el) ->
                      Format.fprintf
                        f
                        "token_%d [label = \"%s (%a)\" , shape=none]"
                        i na Nbr.print el))
    s.Data.tokens

let snapshot env s =
  if Filename.check_suffix s.Data.snap_file ".dot" then
    Kappa_files.with_snapshot
      s.Data.snap_file s.Data.snap_event "dot"
      (fun f -> Format.fprintf f "%a@." (print_dot_snapshot env) s)
  else
    Kappa_files.with_snapshot
      s.Data.snap_file s.Data.snap_event "ka"
      (fun f -> Format.fprintf f "%a@." (print_snapshot env) s)

let go env = function
  | Data.Snapshot s -> snapshot env s
  | Data.Flux f -> output_flux f
  | Data.Plot (x,y) -> plot_now (x,y)
  | Data.Print p ->
     let desc =
       match p.Data.file_name with
         None -> Format.formatter_of_out_channel stdout
       | Some file -> get_desc file
     in
     Format.fprintf desc "%s@." p.Data.line
  | Data.UnaryDistances (x,y) -> unary_distances x y

let close () =
  let () = close_plot () in
  let () = close_distances () in
  close_desc ()
