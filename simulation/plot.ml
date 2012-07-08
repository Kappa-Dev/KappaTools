open State
open Mods
open ExceptionDefn

type t = {
	mutable desc:out_channel option ; 
	file:string ; 
	mutable last_point : int 
	}

let create filename = 
	{desc=None;
	file=filename;
	last_point = 0
	}

let close plot = match plot.desc with Some d -> close_out d | None -> ()

let next_point counter time_increment =
	match counter.Counter.dT with 
		| None -> 
			begin
				match counter.Counter.dE with
					| None -> invalid_arg "Plot.next_point: No point interval"
					| Some dE ->
						let point = (counter.Counter.events - counter.Counter.init_event) / dE in
						point
			end
		| Some dT -> 
			let point = int_of_float ((counter.Counter.time +. time_increment -. counter.Counter.init_time) /. dT) 
			in
				point
		
let output state time event plot env counter =
	if not !Parameter.plotModeOn then ()
	else
		let d =
			match plot.desc with
				| None -> (*first value*)
					begin
						if !Parameter.debugModeOn then Debug.tag (Printf.sprintf "\t *Creating data file...") ; 
						let d = open_out plot.file in
							if !Parameter.emacsMode then Printf.fprintf d "time"
							else Printf.fprintf d "# time" ;
							List.iter
							(fun obs ->
								Printf.fprintf d "%c%s" !Parameter.plotSepChar obs.label 
							) state.observables ;
							Printf.fprintf d "\n" ;
							plot.desc <- Some d ;
							d
					end
				| Some d -> d
		in
			Printf.fprintf d "%c%E" !Parameter.plotSepChar time ;
			List.iter
			(fun obs ->
				let inst = fun v_i -> State.instance_number v_i state env
				and values = fun i -> State.value state i {counter with Counter.time = time ; Counter.events = event} env
				and v_of_token id = 
					let x = try state.State.token_vector.(id) with _ -> failwith "Plot.output: Invalid token id"
					in Num.F x
				in
					let v = 
						match obs.expr with
							| Dynamics.CONST v -> v
							| Dynamics.VAR f -> f inst values time event (Counter.null_event counter) (Sys.time()) v_of_token
					in 
					match v with
						| Num.I x -> Printf.fprintf d "%c%d" !Parameter.plotSepChar x
						| Num.F x -> Printf.fprintf d "%c%E" !Parameter.plotSepChar x
			) state.observables ;
			Printf.fprintf d "\n" ;
			flush d

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
								output state counter.Counter.time counter.Counter.events plot env counter ;
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
											output state !output_time counter.Counter.events plot env counter ;
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
	 	
						
	
