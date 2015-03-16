open State
open Mods

type fd = {
  desc:out_channel;
  form:Format.formatter;
}

type format = Raw of fd | Svg of Pp_svg.store

type plot = {
  format: format;
  mutable last_point : int
}

type t = Wait of string | Ready of plot

let plotDescr = ref (Wait "__dummy")

let create filename = plotDescr := Wait filename

let value width =
  match !plotDescr with
  | Wait _ -> ""
  | Ready plot ->
     match plot.format with
     | Raw _ ->  ""
     | Svg s -> Pp_svg.to_string ~width s

let close form counter =
  match !plotDescr with
  | Wait _ -> ()
  | Ready plot ->
     let n = ref (!Parameter.progressBarSize - counter.Counter.ticks) in
     let () = while !n > 0 do
		Format.fprintf form "%c" !Parameter.progressBarSymbol ;
		n := !n-1
	      done in
     match plot.format with
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

let set_up filename env counter ?time state =
  let head = observables_header state in
  let init_va = observables_values env counter ?time state in
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
    Ready {format = format; last_point = 0}

let next_point counter time_increment =
  match counter.Counter.dT with
  | Some dT ->
     int_of_float
       ((counter.Counter.time +. time_increment -. counter.Counter.init_time)
	/. dT)
  | None ->
     match counter.Counter.dE with
     | None -> 0
     | Some dE ->
	(counter.Counter.events - counter.Counter.init_event) / dE

let set_last_point plot p = plot.last_point <- p

let plot_now env counter ?time state =
  match !plotDescr with
  | Wait f -> set_up f env counter ?time state
  | Ready plot ->
     match plot.format with
     | Raw fd ->
	print_values_raw fd.form (observables_values env counter ?time state)
     | Svg s ->
	s.Pp_svg.points <-
	  observables_values env counter ?time state :: s.Pp_svg.points

let fill form state counter env time_increment =
  let () =
    match !plotDescr with
    | Wait _ -> ()
    | Ready plot ->
       let next = next_point counter time_increment in
       let last = plot.last_point in
       let n = next - last in
       let () = set_last_point plot next in
       match counter.Counter.dE with
       | Some _ ->
	  if n>1 then
	    invalid_arg ("Plot.fill: invalid increment "^string_of_int n)
	  else
	    if n <> 0
	    then plot_now env counter state
       | None ->
	  match counter.Counter.dT with
	  | None -> ()
	  | Some dT ->
	     let n = ref n
	     and output_time = ref ((float_of_int last) *. dT) in
	     while (!n > 0) && (Counter.check_output_time counter !output_time) do
	       output_time := !output_time +. dT ;
	       Counter.tick form counter !output_time counter.Counter.events ;
	       plot_now env counter ~time:!output_time  state;
	       n:=!n-1 ;
	     done in
  Counter.tick form counter counter.Counter.time counter.Counter.events
