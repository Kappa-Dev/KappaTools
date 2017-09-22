(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let print_desc : (string,out_channel * Format.formatter) Hashtbl.t =
  Hashtbl.create 2

let species_desc : (string,out_channel * Format.formatter) Hashtbl.t =
  Hashtbl.create 2

let uuid = Random.State.bits (Random.State.make_self_init ())

let get_desc file tbl =
  try snd (Hashtbl.find tbl file)
  with Not_found ->
    let d_chan = Kappa_files.open_out file in
    let d = Format.formatter_of_out_channel d_chan in
    (Hashtbl.add tbl file (d_chan,d) ; d)

let close_desc () =
  Hashtbl.iter (fun _file (d_chan,_d) -> close_out d_chan) print_desc;
  Hashtbl.iter (fun _file (d_chan,_d) -> close_out d_chan) species_desc

let dot_of_flux flux =
  let printer desc =
    let () = Format.fprintf desc "@[<v>// \"uuid\" : \"%i\",@," uuid in
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
  let () = Format.fprintf
      f "@[<v>\"normalized\" : %b,@ \"uuid\" : \"%i\",@,"
      (flux.Data.flux_data.Data.flux_kind <> Primitives.ABSOLUTE) uuid in
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

let actsDescr = ref None
let emptyActs = ref true

let init_activities env = function
  | None -> ()
  | Some s ->
    let desc = Kappa_files.open_out s in
    let form = Format.formatter_of_out_channel desc in
    let nb_r = Model.nb_syntactic_rules env in
    let () = actsDescr := Some (desc,form,nb_r) in
    let () = Format.fprintf form "@[<v>{@,rules:@[[" in
    let () =
      Tools.iteri
        (fun x -> Format.fprintf form "\"%a\",@," (Model.print_ast_rule ~env) x)
        nb_r in
    Format.fprintf form "\"%a\"]@],@,data:[@," (Model.print_ast_rule ~env) nb_r

let close_activities () =
  match !actsDescr with
  | None -> ()
  | Some (c,f,_) ->
    let () = Format.fprintf f "@,]}@]@." in
    close_out c

let output_activities r flux =
  match !actsDescr with
  | None -> ()
  | Some (_,f,last) ->
    let () =
      if !emptyActs then emptyActs := false else Format.fprintf f ",@," in
    let () = Format.fprintf f "@[[%i" r in
    let () =
      for i = 0 to last do
        let k,k' = try List.assoc i flux with Not_found -> 0.,0. in
        Format.fprintf f ",%f" (k'-.k)
      done in
    Format.fprintf f "]@]"

type fd = {
  desc:out_channel;
  form:Format.formatter;
  is_tsv:bool;
}

type format = StandBy of (string * string * string array)
            | Raw of fd | Svg of Pp_svg.store

let plotDescr = ref None

let close_plot () =
  match !plotDescr with
  | (None | Some (StandBy _)) -> ()
  | Some (Raw plot) -> close_out plot.desc
  | Some (Svg s) -> Pp_svg.to_file s

let print_header_raw is_tsv f a =
  let print_sep =
    if is_tsv then fun f -> Format.pp_print_string f "\t"
    else Pp.comma in
  Format.fprintf f "@[<h>%a@]@."
    (Pp.array print_sep (fun _ f x -> Format.fprintf f "\"%s\"" x)) a

let print_values_raw is_tsv f l =
  let print_sep =
    if is_tsv then fun f -> Format.pp_print_string f "\t"
    else Pp.comma in
  Format.fprintf f "@[<h>%a@]@."
    (Pp.array print_sep (fun _ -> Nbr.print_option)) l

let traceDescr = ref None
let traceNotEmpty = ref false

let close_trace () =
  match !traceDescr with
  | None -> ()
  | Some desc ->
    let () = output_string desc "]\n}\n" in
    let () = close_out desc in
    traceDescr := None

let initialize activities_file trace_file plotPack env =
  let () =
    match trace_file with
    | None -> ()
    | Some s ->
      let desc = Kappa_files.open_out s in
      let () = Trace.init_trace_file ~uuid env desc in
      traceDescr := Some desc in
  let () = init_activities env activities_file in
  match plotPack with
  | None -> ()
  | Some pack -> plotDescr := Some (StandBy pack)

let launch_plot (filename,title,head) =
  let format =
    if Filename.check_suffix filename ".svg" then
      Svg {Pp_svg.file = filename;
           Pp_svg.title = title;
           Pp_svg.descr = "\"uuid\" : \""^string_of_int uuid^"\"";
           Pp_svg.legend = head;
           Pp_svg.points = [];
          }
    else
      let d_chan = Kappa_files.open_out filename in
      let d = Format.formatter_of_out_channel d_chan in
      let is_tsv = Filename.check_suffix filename ".tsv" in
      let () = if not is_tsv then Format.fprintf d "# %s@." title in
      let () = if not is_tsv
        then Format.fprintf d "# \"uuid\" : \"%i\"@." uuid in
      let () = print_header_raw is_tsv d head in
      Raw {desc=d_chan; form=d; is_tsv} in
    plotDescr := Some format

let rec plot_now l =
  match !plotDescr with
  | None -> assert false
  | Some (StandBy p) -> let () = launch_plot p in plot_now l
  | Some (Raw fd) -> print_values_raw fd.is_tsv fd.form l
  | Some (Svg s) -> s.Pp_svg.points <- l :: s.Pp_svg.points

let snapshot s =
  if Filename.check_suffix s.Data.snapshot_file ".dot" then
    Kappa_files.with_snapshot
      s.Data.snapshot_file s.Data.snapshot_event "dot"
      (fun f -> Format.fprintf f "%a@." (Data.print_dot_snapshot ~uuid) s)
  else if Filename.check_suffix s.Data.snapshot_file ".json" then
    Kappa_files.with_channel_fresh
      s.Data.snapshot_file (string_of_int s.Data.snapshot_event) "json"
      (fun d -> (JsonUtil.write_to_channel Data.write_snapshot) d s)
  else
    Kappa_files.with_snapshot
      s.Data.snapshot_file s.Data.snapshot_event "ka"
      (fun f -> Format.fprintf f "%a@." (Data.print_snapshot ~uuid) s)

let print_species time f mixture =
  Format.fprintf
    f "%g: @[<h>%a@]@." time
    (User_graph.print_cc ~explicit_free:false ~compact:false) mixture

let go = function
  | Data.Snapshot s -> snapshot s
  | Data.Flux f -> output_flux f
  | Data.DeltaActivities (r,flux) -> output_activities r flux
  | Data.Plot x -> plot_now x
  | Data.Print p ->
    let desc =
      match p.Data.file_line_name with
        None -> Format.formatter_of_out_channel stdout
      | Some file -> get_desc file print_desc
    in
    Format.fprintf desc "%s@." p.Data.file_line_text
  | Data.Log s -> Format.printf "%s@." s
  | Data.TraceStep step ->
    begin match !traceDescr with
      | None -> ()
      | Some d ->
        let () =
          if !traceNotEmpty then output_char d ',' else traceNotEmpty := true in
        Yojson.Basic.to_channel d (Trace.step_to_yojson step)
    end
  | Data.Species (file,time,mixture) ->
     let desc = get_desc file species_desc in
     print_species time desc mixture

let inputsDesc = ref None

let close_input ?event () =
  match !inputsDesc with
  | None -> ()
  | Some inputs ->
    let () =
      match event with
      | None -> ()
      | Some event -> Format.fprintf
                        (Format.formatter_of_out_channel inputs)
                        "@.%%mod: [E] = %i do $STOP@." event in
    close_out inputs

let close ?event () =
  let () = close_plot () in
  let () = close_trace () in
  let () = close_activities () in
  let () = close_input ?event () in
  close_desc ()

let initial_inputs conf env contact_map init ~filename =
  let inputs = Kappa_files.open_out_fresh filename "" "ka" in
  let inputs_form = Format.formatter_of_out_channel inputs in
  let () = Format.fprintf inputs_form "# \"uuid\" : \"%i\"@." uuid in
  let () = Format.fprintf inputs_form
      "%a@.%a@." Configuration.print conf
      (Kappa_printer.env_kappa contact_map) env in
  let sigs = Model.signatures env in
  let () = Format.fprintf inputs_form "@.@[<v>%a@]@."
      (Pp.list Pp.space
         (fun f (n,r,_) ->
            let ins_fresh,_,_ =
              Primitives.Transformation.raw_mixture_of_fresh
                sigs r.Primitives.inserted in
            if ins_fresh = [] then
              Pp.list Pp.space (fun f (nb,tk) ->
                  Format.fprintf f "@[<h>%%init: %a %a@]"
                    (Kappa_printer.alg_expr ~env)
                    (fst (Alg_expr.mult (Locality.dummy_annot n) nb))
                    (Model.print_token ~env) tk)
                f r.Primitives.delta_tokens
              else
                Format.fprintf f "@[<h>%%init: %a %a@]"
                  (Kappa_printer.alg_expr ~env) n
                  (Raw_mixture.print
                     ~explicit_free:true ~compact:false ~created:false ~sigs)
                  (List.map snd ins_fresh))) init in
  inputsDesc := Some inputs

let input_modifications env event mods =
  match !inputsDesc with
  | None -> ()
  | Some inputs ->
    Format.fprintf
      (Format.formatter_of_out_channel inputs)
      "%%mod: [E] = %i do %a@."
      event (Pp.list Pp.colon (Kappa_printer.modification ~env)) mods
