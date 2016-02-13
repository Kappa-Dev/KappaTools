module Stat_null_events :
sig
  type t

  val init : unit -> t

  val nb : t -> int
  val nb_consecutive : t -> int
  val print_detail : Format.formatter -> t -> unit
  val reset_consecutive : t -> t

  val incr_no_more_binary : t -> t
  val incr_no_more_unary : t -> t
  val incr_clashing_instance : t -> t
  val incr_time_correction : t -> t
end =
  struct
    type t = int array

    let all = 0
    let consecutive = 1
    let no_more_binary = 2
    let no_more_unary = 3
    let clashing_instance = 4
    let time_correction = 5
    let init () = Array.make 6 0

    let nb t = t.(all)
    let nb_consecutive t = t.(consecutive)
    let reset_consecutive t = let () = t.(consecutive) <- 0 in t

    let incr_t t i = t.(i) <- succ t.(i)
    let incr_one t i =
      let () = incr_t t all in
      let () = incr_t t consecutive in
      let () = incr_t t i in
      t

    let incr_no_more_binary t = incr_one t no_more_binary
    let incr_no_more_unary t = incr_one  t no_more_unary
    let incr_clashing_instance t = incr_one t clashing_instance
    let incr_time_correction t = incr_one t time_correction

    let print_detail f t =
      let () = Format.pp_open_vbox f 0 in
      let () = Format.fprintf
                 f "\tValid embedding but no longer unary when required: %f@,"
                 ((float_of_int t.(no_more_unary)) /. (float_of_int t.(all))) in
      let () = Format.fprintf
                 f "\tValid embedding but not binary when required: %f@,"
                 ((float_of_int t.(no_more_binary)) /. (float_of_int t.(all))) in
      let () = Format.fprintf
                 f "\tClashing instance: %f@,"
                 ((float_of_int t.(clashing_instance)) /. (float_of_int t.(all))) in
      (*let () =
        Format.fprintf f "\tLazy negative update of non local instances: %f@,"
                       ((float_of_int n) /. (float_of_int t.(all))) in*)
      Format.fprintf
        f "\tPerturbation interrupting time advance: %f@]@."
        ((float_of_int t.(time_correction)) /. (float_of_int t.(all)))
  end

type t = {
    mutable time:float ;
    mutable events:int ;
    mutable stories:int ;
    mutable last_tick : (int * float) ;
    mutable last_point : int;
    mutable initialized : bool ;
    mutable ticks : int ;
    mutable stat_null : Stat_null_events.t ;
    init_time : float ;
    init_event : int ;
    max_time : float option ;
    max_events : int option ;
    plot_points : int ;
    dE : int option ;
    dT : float option ;
  }

let inc_tick c = c.ticks <- c.ticks + 1
let current_time c = c.time
let current_event c = c.events
let nb_null_event c = Stat_null_events.nb c.stat_null
let consecutive_null_event c = Stat_null_events.nb_consecutive c.stat_null
let inc_time c dt = c.time <- (c.time +. dt)
let inc_stories c =c.stories <- (c.stories + 1)
let inc_events c =c.events <- (c.events + 1)
let check_time c =
  match c.max_time with None -> true | Some max -> c.time < max
let check_output_time c ot =
  match c.max_time with None -> true | Some max -> ot < max
let check_events c =
  match c.max_events with None -> true | Some max -> c.events < max
let one_constructive_event c dt =
  let () = c.stat_null <- Stat_null_events.reset_consecutive c.stat_null in
  let () = inc_events c in
  let () = inc_time c dt in
  check_time c && check_events c
let one_no_more_binary_event c dt =
  let () = c.stat_null <- Stat_null_events.incr_no_more_binary c.stat_null in
  let () = inc_time c dt in
  check_time c && check_events c
let one_no_more_unary_event c dt =
  let () = c.stat_null <- Stat_null_events.incr_no_more_unary c.stat_null in
  let () = inc_time c dt in
  check_time c && check_events c
let one_clashing_instance_event c dt =
  let () = c.stat_null <- Stat_null_events.incr_clashing_instance c.stat_null in
  let () = inc_time c dt in
  check_time c && check_events c
let one_time_correction_event c ti =
  let () = c.time <- Nbr.to_float ti in
  let () = c.stat_null <- Stat_null_events.incr_time_correction c.stat_null in
  check_time c && check_events c
let print_efficiency f c = Stat_null_events.print_detail f c.stat_null
let current_story c =
   { Mods.story_id = c.stories; Mods.story_time = current_time c;
     Mods.story_event = current_event c; Mods.profiling_info = (); }
let next_story c =
  let () = inc_stories c in
  current_story c
let max_time c = c.max_time
let max_events c = c.max_events
let plot_points c = c.plot_points
let set_tick c (i,x) = c.last_tick <- (i,x)
let event_percentage (counter : t) : int option =
  match counter.max_events with
  | None -> None
  | Some va -> Some (100 * (counter.events - counter.init_event)
                     / (va - counter.init_event))
let event (counter : t) : int =
    counter.events
let time_percentage (counter : t) : int option =
  match counter.max_time with
  | None -> None
  | Some va -> Some (int_of_float (100. *. (counter.time -. counter.init_time)
                                   /. (va -. counter.init_time)))
let time (counter : t) : float =
    counter.time
let tracked_events (counter : t) : int = counter.stories

let compute_dT points mx_t =
  if points <= 0 then None else
    match mx_t with
    | None -> None
    | Some max_t -> Some (max_t /. (float_of_int points))

let compute_dE points mx_e =
  if points <= 0 then None else
    match mx_e with
    | None -> None
    | Some max_e ->
       Some (max (max_e / points) 1)

let tick f counter =
  let () =
    if not counter.initialized then
      let c = ref !Parameter.progressBarSize in
      while !c > 0 do
        Format.pp_print_string f "_" ;
        c:=!c-1
      done ;
      Format.pp_print_newline f () ;
      counter.initialized <- true
  in
  let last_event,last_time = counter.last_tick in
  let n_t =
    match counter.max_time with
    | None -> 0
    | Some tmax ->
       int_of_float
         ((counter.time -. last_time) *.
            (float_of_int !Parameter.progressBarSize) /. tmax)
  and n_e =
    match counter.max_events with
    | None -> 0
    | Some emax ->
       if emax = 0 then 0
       else
         let nplus =
           (counter.events * !Parameter.progressBarSize) / emax in
         let nminus =
           (last_event * !Parameter.progressBarSize) / emax in
         nplus-nminus
  in
  let n = ref (max n_t n_e) in
  if !n>0 then set_tick counter (counter.events,counter.time) ;
  while !n > 0 do
    Format.fprintf f "%c" !Parameter.progressBarSymbol ;
    if !Parameter.eclipseMode then Format.pp_print_newline f ();
    inc_tick counter ;
    n:=!n-1
  done;
  Format.pp_print_flush f ()

let complete_progress_bar form counter =
  let n = ref (!Parameter.progressBarSize - counter.ticks) in
  let () = while !n > 0 do
             Format.fprintf form "%c" !Parameter.progressBarSymbol ;
             n := !n-1
           done in
  Format.pp_print_newline form ()

let create nb_points init_t init_e mx_t mx_e =
  let dE =
    compute_dE nb_points (Tools.option_map (fun x -> x - init_e) mx_e) in
  let dT =
    compute_dT nb_points (Tools.option_map (fun x -> x -. init_t) mx_t)
  in
  {time = init_t ;
   events = init_e ;
   stories = -1 ;
   stat_null = Stat_null_events.init () ;
   max_time = mx_t ;
   max_events = mx_e ;
   plot_points = nb_points;
   last_tick = (init_e,init_t);
   dE = dE ;
   dT = dT ;
   init_time = init_t ;
   init_event = init_e ;
   initialized = false ;
   last_point = 0 ;
   ticks = 0 ;
  }
let reinitialize counter =
  counter.time <- counter.init_time;
  counter.events <- counter.init_event;
  counter.stories <- -1;
  counter.last_tick <- (counter.init_event, counter.init_time);
  counter.last_point <- 0;
  counter.initialized <- false;
  counter.ticks <- 0;
  counter.stat_null <- Stat_null_events.init ()

let next_point counter =
  match counter.dT with
  | Some dT ->
     int_of_float
       ((current_time counter -. counter.init_time)
        /. dT)
  | None ->
     match counter.dE with
     | None -> 0
     | Some dE ->
        (current_event counter - counter.init_event) / dE

let to_plot_points counter =
  let next = next_point counter in
  let last = counter.last_point in
  let () = counter.last_point <- next in
  let n = next - last in
  match counter.dT with
  | Some dT ->
     let n = ref n in
     let acc = ref [] in
     let output_time = ref ((float_of_int last) *. dT) in
     while (!n > 0) && (check_output_time counter !output_time) do
       output_time := !output_time +. dT ;
       acc:=!output_time::!acc;
       n:=!n-1 ;
     done;
     !acc,counter
  | None ->
     match max_events counter with
     | Some _ ->
        if n>1 then
          invalid_arg
            ("Counter.to_plot_points: invalid increment "^string_of_int n)
        else
          (if n <> 0 then [counter.time] else []),counter
     | None -> [],counter

let fill ~outputs counter observables_values =
  let points, _counter =
    to_plot_points counter in
  List.iter (fun t -> outputs (Data.Plot (t,observables_values))) points
