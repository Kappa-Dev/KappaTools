open State
open Mods
open ExceptionDefn

type t = {
  desc:out_channel;
  form:Format.formatter;
  mutable last_point : int
}

let plotDescr : t option ref = ref None

let create filename env state counter =
  let d_chan = Tools.kasim_open_out filename in
  let d = Format.formatter_of_out_channel d_chan in
  let () = print_observables_header d state in
  let () = print_observables_values d counter.Counter.time env counter state in
  plotDescr :=
    Some { desc=d_chan; form=d; last_point = 0 }

let close counter =
  match !plotDescr with
  | None -> ()
  | Some plot ->
     let n = ref (!Parameter.progressBarSize - counter.Counter.ticks) in
     let () = while !n > 0 do
		Format.printf "%c" !Parameter.progressBarSymbol ;
		n := !n-1
	      done in
     close_out plot.desc

let next_point counter time_increment =
  match counter.Counter.dT with
  | Some dT ->
     int_of_float
       ((counter.Counter.time +. time_increment -. counter.Counter.init_time)
	/. dT)
  | None ->
     match counter.Counter.dE with
     | None -> invalid_arg "Plot.next_point: No point interval"
     | Some dE ->
	(counter.Counter.events - counter.Counter.init_event) / dE

let set_last_point plot p = plot.last_point <- p

let fill state counter env time_increment =
  let () =
    match !plotDescr with
    | None -> ()
    | Some plot ->
       let next = next_point counter time_increment in
       let last = plot.last_point in
       let n = next - last in
       let () = set_last_point plot next in
       match counter.Counter.dE with
       | Some dE ->
	  if n>1 then
	    invalid_arg (Printf.sprintf "Plot.fill: invalid increment %d" n)
	  else
	    if n <> 0
	    then print_observables_values plot.form counter.Counter.time env counter state
       | None ->
	  match counter.Counter.dT with
	  | None -> ()
	  | Some dT ->
	     let n = ref n
	     and output_time = ref ((float_of_int last) *. dT) in
	     while (!n > 0) && (Counter.check_output_time counter !output_time) do
	       output_time := !output_time +. dT ;
	       Counter.tick counter !output_time counter.Counter.events ;
	       print_observables_values plot.form !output_time env counter state;
	       n:=!n-1 ;
	     done in
  Counter.tick counter counter.Counter.time counter.Counter.events
