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

module Progress_report =
struct
  type bar = {
    mutable ticks : int ;
    bar_size : int ;
    bar_char : char ;
  }

  type text = {
    mutable last_length : int;
    mutable last_time : float;
  }

  type t = Bar of bar | Text of text

  let inc_tick c = c.ticks <- c.ticks + 1

  let create bar_size bar_char f =
    if Unix.isatty Unix.stdout
    then Text { last_length = 0; last_time = -5.;  }
    else
      let () =
        for _ = bar_size downto 1 do
          Format.pp_print_string f "_"
        done in
      let () = Format.pp_print_newline f () in
      Bar { ticks = 0; bar_size; bar_char; }

  let pp_not_null f x =
    match classify_float x with
    | FP_normal ->
      Format.fprintf f " (%.2f%%)" (x *. 100.)
    | FP_subnormal | FP_zero | FP_infinite | FP_nan -> ()

  let pp_text time t_r event e_r f s =
    let string =
      Format.asprintf "%.2f time units%a in %i events%a"
        time pp_not_null t_r event pp_not_null e_r in
    let () =
      Format.fprintf f "%s%s@?" (String.make s.last_length '\b') string in
    s.last_length <- String.length string

  let tick time t_r event e_r f = function
    | Bar s ->
      let n_t = t_r *. (float_of_int s.bar_size) in
      let n_e = e_r *. (float_of_int s.bar_size) in
      let n = ref (int_of_float (max n_t n_e) - s.ticks) in
      while !n > 0 do
        Format.fprintf f "%c" s.bar_char;
        if !Parameter.eclipseMode then Format.pp_print_newline f ();
        inc_tick s; decr n
      done;
      Format.pp_print_flush f ()
    | Text s ->
      let run = Sys.time () in
      if run -. s.last_time > 2. then
        let () = pp_text time t_r event e_r f s in s.last_time <- run

  let complete_progress_bar time event form t =
    let () =
      match t with
      | Bar t ->
        for _ = t.bar_size - t.ticks downto 1 do
          Format.fprintf form "%c" t.bar_char
        done
      | Text s -> pp_text time 1. event 1. form s in
    Format.pp_print_newline form ()
end

type period = DE of int | DT of float
type t = {
    mutable time:float ;
    mutable events:int ;
    mutable stories:int ;
    mutable last_point : int;
    mutable stat_null : Stat_null_events.t ;
    init_time : float ;
    init_event : int ;
    mutable progress_report : Progress_report.t option ;
    mutable plot_period : period;
    mutable max_time : float option ;
    mutable max_event : int option ;
  }

let current_story c = c.stories
let current_time c = c.time
let current_event c = c.events
let nb_null_event c = Stat_null_events.nb c.stat_null
let consecutive_null_event c = Stat_null_events.nb_consecutive c.stat_null
let inc_time c dt = c.time <- (c.time +. dt)
let inc_stories c = c.stories <- (c.stories + 1)
let inc_events c =c.events <- (c.events + 1)
let check_time c =
  match c.max_time with None -> true | Some max -> c.time < max
let check_output_time c ot =
  match c.max_time with None -> true | Some max -> ot <= max
let check_events c =
  match c.max_event with None -> true | Some max -> c.events < max
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
let max_time c = c.max_time
let max_events c = c.max_event
let plot_period c = match c.plot_period with DT dt -> dt | DE de -> float de

let time_ratio t =
  match t.max_time with
  | None -> 0.
  | Some tmax ->
    if tmax > t.init_time then
      (t.time -. t.init_time) /. (tmax -. t.init_time)
    else 0.

  let event_ratio t =
    match t.max_event with
    | None -> 0.
    | Some emax ->
      if emax = 0 then 0.
      else float_of_int (t.events - t.init_event) /.
           float_of_int (emax - t.init_event)

let event (counter : t) : int = counter.events
let event_percentage t : int option =
  let p_e = event_ratio t in
  match classify_float p_e with
    | FP_normal -> Some (int_of_float (p_e *. 100.))
    | FP_subnormal | FP_zero | FP_infinite | FP_nan -> None

let time (counter : t) : float = counter.time
let time_percentage t : int option =
  let p_t = time_ratio t in
  match classify_float p_t with
    | FP_normal -> Some (int_of_float (p_t *. 100.))
    | FP_subnormal | FP_zero | FP_infinite | FP_nan -> None

let set_max_time c t = c.max_time <- t
let set_max_events c e = c.max_event <- e


let tracked_events (counter : t) : int option =
  if counter.stories >= 0 then Some counter.stories else None

let set_plot_period (t :t) plot_period : unit = t.plot_period <- plot_period

let create ?(init_t=0.) ?(init_e=0) ?max_time ?max_event ~plot_period =
  {time = init_t ;
   events = init_e ;
   stories = -1 ;
   stat_null = Stat_null_events.init () ;
   plot_period = plot_period ;
   init_time = init_t ;
   init_event = init_e ;
   max_time; max_event;
   progress_report = None;
   last_point = 0 ;
  }

let reinitialize counter =
  counter.progress_report <- None;
  counter.time <- counter.init_time;
  counter.events <- counter.init_event;
  counter.stories <- -1;
  counter.last_point <- 0;
  counter.stat_null <- Stat_null_events.init ()

let rec tick f c =
  match c.progress_report with
  | None ->
    let () =
      c.progress_report <-
        Some (Progress_report.create
                !Parameter.progressBarSize !Parameter.progressBarSymbol f) in
    tick f c
  | Some pr ->
    Progress_report.tick c.time (time_ratio c) c.events (event_ratio c) f pr

  let current_simulation_info c =
  { Trace.Simulation_info.story_id = current_story c;
    Trace.Simulation_info.story_time = current_time c;
    Trace.Simulation_info.story_event = current_event c;
    Trace.Simulation_info.profiling_info = (); }
  let next_story c =
    let () = inc_stories c in
    current_simulation_info c

let complete_progress_bar f c =
  match c.progress_report with
  | None -> ()
  | Some pr -> Progress_report.complete_progress_bar c.time c.events f pr

let next_point counter =
  match counter.plot_period with
  | DT dT ->
    if dT <= 0. then 0 else
      int_of_float
        ((min (Tools.unsome infinity (max_time counter)) (current_time counter)
          -. counter.init_time) /. dT)
  | DE dE ->
    if dE <= 0 then 0 else
      (current_event counter - counter.init_event) / dE

let to_plot_points counter =
  let next = next_point counter in
  let last = counter.last_point in
  let () = counter.last_point <- next in
  let n = next - last in
  match counter.plot_period with
  | DT dT ->
    snd
      (Tools.recti
         (fun (time,acc) _ ->
            time -. dT,
            if check_output_time counter time then time::acc else acc)
         ((float_of_int next) *. dT,[]) n),counter
  | DE _ ->
    if n>1 then
      invalid_arg
        ("Counter.to_plot_points: invalid increment "^string_of_int n)
    else
      (if n <> 0 then [counter.time] else []),counter

let fill ~outputs counter observables_values =
  let points, _counter =
    to_plot_points counter in
  if observables_values <> [||] then
    List.iter (fun t -> outputs (Data.Plot (t,observables_values))) points
