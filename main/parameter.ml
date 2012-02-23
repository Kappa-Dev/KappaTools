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
let backtrace = ref false
let (rescale:int option ref) = ref None
let eclipseMode = ref false

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
let causalModeOn = ref false

(*Computed values*)
let (timeIncrementValue:float option ref) = ref None

(*Name convention*)
let outputDirName = ref (Sys.getcwd ())
let snapshotFileName = ref "snap"

let dumpFileName = ref "dump.ka"
let influenceFileName = ref ""
let fluxFileName = ref "flux.dot"
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

(*Profiling*)
(*let profiling:Profiling.t = Profiling.create 10*) 
