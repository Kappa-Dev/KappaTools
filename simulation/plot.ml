type fd = {
  desc:out_channel;
  form:Format.formatter;
}

type format = Raw of fd | Svg of Pp_svg.store

type t = Wait of string | Ready of format

let plotDescr = ref (Wait "__dummy")

let create filename = plotDescr := Wait filename

let value width =
  match !plotDescr with
  | Wait _ -> ""
  | Ready plot ->
     match plot with
     | Raw _ ->  ""
     | Svg s -> Pp_svg.to_string ~width s

let close () =
  match !plotDescr with
  | Wait _ -> ()
  | Ready plot ->
     match plot with
     | Raw plot ->  close_out plot.desc
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

let set_up filename env init_va =
  let head =
    Environment.map_observables
      (Format.asprintf "%a" (Kappa_printer.alg_expr ~env))
      env in
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
	   Pp_svg.points = [init_va];
	  }
    else
      let d_chan = Kappa_files.open_out filename in
      let d = Format.formatter_of_out_channel d_chan in
      let () = print_header_raw d head in
      let () = print_values_raw d init_va in
      Raw {desc=d_chan; form=d} in
  plotDescr :=
    Ready format

let plot_now env time observables_values =
  match !plotDescr with
  | Wait f -> set_up f env (time,observables_values)
  | Ready (Raw fd) ->
	print_values_raw fd.form (time,observables_values)
  | Ready (Svg s) ->
     s.Pp_svg.points <- (time,observables_values) :: s.Pp_svg.points

let fill counter env observables_values =
  match !plotDescr with
  | Wait _ -> ()
  | Ready _ ->
     let points, _counter =
       Counter.to_plot_points counter in
     List.iter (fun t -> plot_now env t observables_values) points
