(*Expert mode values*)
let interrupt_on_exception = false 
let defaultExtArraySize = ref 5
let defaultGraphSize = ref 5
let defaultLiftSetSize = ref 5
let defaultHeapSize = ref 5
let defaultLogSize = ref 10
let debugModeOn = ref false
let progressBarSymbol = ref '#'
let progressBarSize = ref 60
let plotSepChar = ref ' '
let dumpIfDeadlocked = ref false
let maxConsecutiveClash = ref 10
let backtrace = ref false
let (rescale:int option ref) = ref None
let eclipseMode = ref false
let useColor = ref true
let cache_size = ref (Some 1) (*(None:int option) *)

(*User definable values*)
let (maxEventValue:int option ref) = ref None
let (maxTimeValue:float option ref) = ref None
let (pointNumberValue:int option ref) = ref None
let (seedValue:int option ref) = ref None
let plotModeOn = ref false
let compileModeOn = ref false
let implicitSignature = ref false
let dotOutput = ref false
let fluxModeOn = ref false
let snapshotHighres = ref true
let causalModeOn = ref true
let weakcompressionModeOn = ref true 

(*Computed values*)
let (timeIncrementValue:float option ref) = ref None

let cpuTime = ref 0.0
let initSimTime () = cpuTime := Sys.time ()  

(*Name convention*)
let outputDirName = ref (Sys.getcwd ())
let snapshotFileName = ref "snap"

let dumpFileName = ref "dump.ka"
let cflowFileName = ref "cflow.dot" 
let with_weak_compression = "weakly_compressed"
let without_compression = "without_compression" 
let influenceFileName = ref ""
let fluxFileName = ref ""
let outputDataName = ref "data.out"
let inputKappaFileNames:(string list ref) = ref [] 
let marshalizedInFile = ref "" 
let marshalizedOutFile = ref ""

let setOutputName () = 
	let set name = 
		if !name <> "" then name := Filename.concat !outputDirName !name 
	in
	set snapshotFileName ;
	set dumpFileName ;
	set influenceFileName ;
	set fluxFileName ;
	set marshalizedOutFile ;
	set cflowFileName ; 
	set outputDataName 

let checkFileExists () =
	let check file =  
		match file with
			| "" -> ()
			| file -> 
				if Sys.file_exists file then 
					begin
						Printf.fprintf stderr "File '%s' already exists do you want to erase (y/N)? \n" file ; flush stderr ;
						let answer = Tools.read_input () in
						if answer="y" then () else exit 1
					end
				else ()
	in
	check !influenceFileName ;
	check !fluxFileName ;
	check !marshalizedOutFile ; 
	let points = match !pointNumberValue with None -> false | Some _ -> true in
	if points then check !outputDataName

let (openOutDescriptors:out_channel list ref) = ref []
let (openInDescriptors:in_channel list ref) = ref []

let add_out_desc d = openOutDescriptors := d::!openOutDescriptors  
let add_in_desc d = openInDescriptors := d::!openInDescriptors  

type compression_mode = 
    { 
      causal_trace:bool;
      weak_compression:bool;
      strong_compression:bool
    }
      
let get_compression_mode () = 
  {
    causal_trace=(!causalModeOn);
    weak_compression=(!weakcompressionModeOn);
    strong_compression=false;
  }

let get_causal_trace x = x.causal_trace 
let get_causal_trace_only x = not (x.weak_compression or x.strong_compression)
let get_weak_compression x = x.weak_compression
let get_strong_compression x = x.strong_compression
let get_cache_size x = !cache_size 
