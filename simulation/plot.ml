open State
open Mods
open ExceptionDefn

type t = {
  desc:out_channel;
  form:Format.formatter;
  file:string;
  mutable last_point : int
}

let create filename state =
  let d_chan = open_out filename in
  let d = Format.formatter_of_out_channel d_chan in
  let () = print_observables_header d state in
  {desc=d_chan;
   form=d;
   file=filename;
   last_point = 0
  }

let close plot = close_out plot.desc

let next_point counter time_increment =
  match counter.Counter.dT with
  | None ->
     begin
       match counter.Counter.dE with
       | None -> invalid_arg "Plot.next_point: No point interval"
       | Some dE ->
	  let point =
	    (counter.Counter.events - counter.Counter.init_event) / dE in
	  point
     end
  | Some dT ->
     int_of_float
       ((counter.Counter.time +. time_increment -. counter.Counter.init_time) /. dT) 

let output state time plot env counter =
  if not !Parameter.plotModeOn then ()
  else
    print_observables_values plot.form time env counter state

let set_last_point plot p = plot.last_point <- p

let fill state counter plot env time_increment = 
	if not !Parameter.plotModeOn then Counter.tick counter counter.Counter.time counter.Counter.events
	else
		match counter.Counter.dE with
			| Some dE ->
				let next = next_point counter time_increment 
				and last = plot.last_point 
				in
					let n = next - last in
						if n>1 then invalid_arg (Printf.sprintf "Plot.fill: invalid increment %d" n)
						else
							if n = 0 then ()
							else (	
								output state counter.Counter.time plot env counter ;
								set_last_point plot (last+1) ;
								Counter.tick counter counter.Counter.time counter.Counter.events ; 
								)
			| None ->
				match counter.Counter.dT with
					| None -> ()
					| Some dT ->
						let next = next_point counter time_increment 
						and last = plot.last_point 
						in
							(*Printf.printf "x_%d -> x_%d (%f)\n" last next dT ;*)
							let n = ref (next - last) 
							and output_time = ref ((float_of_int last) *. dT)
							in
								if !n = 0 then ()
								else
									begin
										while (!n > 0) && (Counter.check_output_time counter !output_time) do
											output_time := !output_time +. dT ;
											Counter.tick counter !output_time counter.Counter.events ;
											output state !output_time plot env counter ;
											set_last_point plot (plot.last_point + 1) ;
											n:=!n-1 ;
										done ;
									end 

let flush_ticks counter =
	if not !Parameter.plotModeOn then ()
	else
		let n = ref (!Parameter.progressBarSize - counter.Counter.ticks) in
		while !n > 0 do
			Printf.printf "%c" !Parameter.progressBarSymbol ;
			n := !n-1
		done
	 	
						
	
