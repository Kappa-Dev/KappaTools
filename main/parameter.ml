(*Expert mode values*)
let batchmode = ref false
let interrupt_on_exception = false
let defaultExtArraySize = ref 5
let defaultGraphSize = ref 5
let defaultLiftSetSize = ref 5
let defaultHeapSize = ref 5
let debugModeOn = ref false
let progressBarSymbol = ref '#'
let progressBarSize = ref 60
let plotSepChar = ref (fun f -> Format.pp_print_space f ())
let dumpIfDeadlocked = ref false
let maxConsecutiveClash = ref 2
let backtrace = ref false
let (rescale:int option ref) = ref None
let eclipseMode = ref false
let useColor = ref true
let safeModeOn = ref false
let emacsMode = ref false

(* expert option for stories *)
  (** Memory **)
  (* Number of potential states that are put in the cache per binding site, so as to handler with side effects in stories. None -> Unlimited cache *) 
  let cache_size = ref (None:int option) 

  (** Precomputation **)
  (* Cut concurrent events (for all observables) before generating the blackboard *)
  let do_global_cut = true

  (* Cut pseudo-inverse events *) 
  let cut_pseudo_inverse_event = true
    
  (* Cut concurrent events (for the current observale) before generating the blackboard *)
  let do_local_cut = true

  (* Cut separable components *)
  let do_detect_separable_components = true
   
  (** Propagation heuristics **)
  (* Whenever we do not know whether an event has to be selected or, not, check whether this is not the last one that can parform a requested action *)
  let look_up_for_better_cut = true 
  
  (* Whenever an event is removed, checked whether there is not only one left to perform a required action *)   
  let look_down_for_better_cut = true 

  let log_number_of_causal_flows = true 

(*User definable values*)
let tmp_var_name = ref ""
let alg_var_overwrite : (string * Nbr.t) list ref = ref []
let (seedValue:int option ref) = ref None
let compileModeOn = ref false
let implicitSignature = ref false
let dotSnapshots = ref false
let dotCflows = ref true
let reduceCflows = ref false
let fluxModeOn = ref false
let snapshotHighres = ref true

let causalModeOn = ref false
let weakCompression = ref false 
let strongCompression = ref false 
let mazCompression = ref false 
let showIntroEvents = ref false

(*XLS output for the grids during compression*)
let dump_grid_before_weak_compression = false
let dump_grid_before_strong_compression = false
let dump_grid_after_branching_during_weak_compression = false
let dump_grid_after_branching_during_strong_compression = false
let xlsweakFileName = "grid_weak_compression"
let xlsstrongFileName = "grid_strong_compression"

(*Computed values*)
let (timeIncrementValue:float option ref) = ref None

let cpuTime = ref 0.0
let initSimTime () = cpuTime := Sys.time ()  

(*Name convention*)
let inputKappaFileNames:(string list ref) = ref [] 
let marshalizedInFile = ref "" 
let (openInDescriptors:in_channel list ref) = ref []
let add_in_desc d = openInDescriptors := d::!openInDescriptors

type sort_algo_for_stories = Bucket | Fusion
type current_compression_mode = Weak | Strong | Causal
type compression_mode = 
    { 
      causal_trace:bool;
      weak_compression:bool;
      strong_compression:bool
    }
      
let get_compression_mode () = 
  {
    causal_trace=(!mazCompression);
    weak_compression=(!weakCompression);
    strong_compression=(!strongCompression);
  }

let get_causal_trace x = x.causal_trace 
let get_causal_trace_only x = not (x.weak_compression || x.strong_compression)
let get_weak_compression x = x.weak_compression
let get_strong_compression x = x.strong_compression
let get_cache_size () = !cache_size
