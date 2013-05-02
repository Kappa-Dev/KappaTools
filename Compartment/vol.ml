open Random_tree
open Mods

(**Volume management*)

class virtual compartment id volume state counter causal plot env =
	object (self)
	val mutable id:int = id
	val mutable volume:float = volume
	val mutable state:State.implicit_state = state
	val mutable counter:Counter.t = counter
	val mutable causal:(Compression_main.D.S.PH.B.PB.CI.Po.K.P.log_info * Compression_main.D.S.PH.B.PB.CI.Po.K.step list) = causal
	val mutable plot:Plot.t = plot
	val mutable env:Environment.t = env  
	
	method get_local_clock = counter.Counter.time 
	method set_local_clock = fun t -> (counter.Counter.time <- t)
	
	method virtual send : unit -> (float * Mixture.t * int) (*(time_stamp,mixture,vol_id)*)
	method virtual receive : (float * Mixture.t * int) -> unit

end
		
class virtual passive id volume state counter causal plot env =
	object inherit compartment id volume state counter causal plot env
end
	
class virtual active id volume state counter causal plot env =
	object inherit compartment id volume state counter causal plot env
	
	method private run t_max =
		let (story_profiling,event_list) = causal in
		counter <- {counter with Counter.max_time = Some t_max} ;
  	try
	 		Run.loop state story_profiling event_list counter plot env
  	with
  		| Invalid_argument msg -> 
  			begin
  				let s = (* Printexc.get_backtrace() *) "" in Printf.eprintf "\n***Runtime error %s occuring in Volume[%d]***\n%s\n" msg id s ;
  				exit 1
  			end
  		| ExceptionDefn.UserInterrupted f -> 
  			begin
  				flush stdout ; 
  				Printf.eprintf "\n***User interrupted simulation in Volume[%d]***\n" id ;
  				exit 0
  			end
  		| ExceptionDefn.Deadlock -> counter.Counter.time <- t_max
end
		

(*						
let run_vol time_step vol env =
	let time_out = vol.c.Mods.Counter.time +. time_step in
	let counter = {vol.c with Mods.Counter.max_time = Some (vol.c.Mods.Counter.time +. time_step)}
	in
	try
		begin
		Run.loop vol.s vol.grid vol.story_profiling vol.event_list counter vol.plot env ;
		(vol,env)
  	end
	with
		| Invalid_argument msg -> 
			begin
				(*if !Parameter.debugModeOn then (Debug.tag "State dumped! (dump.ka)" ; let desc = open_out "dump.ka" in State.snapshot state counter desc env ; close_out desc) ; *)
			  let s = (* Printexc.get_backtrace() *) "" in Printf.eprintf "\n***Runtime error %s occuring in Volume[%d]***\n%s\n" msg vol.id s ;
				exit 1
			end
		| ExceptionDefn.UserInterrupted f -> 
			begin
				flush stdout ; 
				Printf.eprintf "\n***User interrupted simulation in Volume[%d]***\n%s\n" msg vol.id s ;
				close_desc (Some env) (*closes all other opened descriptors*) ;
				exit 0
			end
		| ExceptionDefn.Deadlock -> 
			({vol with c=vol.Mods.Counter.time <- time_out},env)		
*)