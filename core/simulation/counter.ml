(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Efficiency : sig
  type t = {
    consecutive: int array;
    mutable consecutive_blocked: int;
    mutable no_more_binary: int;
    mutable no_more_unary: int;
    mutable clashing_instance: int;
    mutable time_correction: int;
  }

  val init : int -> t
  val nb : t -> int
  val nb_consecutive : rule_id:int -> t -> int
  val nb_consecutive_blocked : t -> int
  val print_detail : current_event:int -> Format.formatter -> t -> unit
  val reset_consecutive : rule_id:int -> t -> t
  val reset_consecutive_blocked : t -> t
  val incr_no_more_binary : rule_id:int -> t -> t
  val incr_no_more_unary : rule_id:int -> t -> t
  val incr_clashing_instance : rule_id:int -> t -> t
  val incr_time_correction : t -> t
  val incr_consecutive_blocked : t -> t
  val write_t : Buffer.t -> t -> unit
  val string_of_t : ?len:int -> t -> string
  val read_t : Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  val t_of_string : string -> t
end = struct
  type t = {
    consecutive: int array;
    mutable consecutive_blocked: int;
    mutable no_more_binary: int;
    mutable no_more_unary: int;
    mutable clashing_instance: int;
    mutable time_correction: int;
  }

  let init size =
    {
      consecutive = Array.make size 0;
      consecutive_blocked = 0;
      no_more_binary = 0;
      no_more_unary = 0;
      clashing_instance = 0;
      time_correction = 0;
    }

  let nb t =
    t.no_more_binary + t.no_more_unary + t.clashing_instance + t.time_correction

  let nb_consecutive ~rule_id t = t.consecutive.(rule_id)
  let nb_consecutive_blocked t = t.consecutive_blocked

  let reset_consecutive ~rule_id t =
    let () = t.consecutive.(rule_id) <- 0 in
    t

  let reset_consecutive_blocked t =
    let () = t.consecutive_blocked <- 0 in
    t

  let incr_consecutive_blocked t =
    let () = t.consecutive_blocked <- succ t.consecutive_blocked in
    t

  let incr_no_more_binary ~rule_id t =
    let () = t.no_more_binary <- succ t.no_more_binary in
    let () = t.consecutive.(rule_id) <- succ t.consecutive.(rule_id) in
    t

  let incr_no_more_unary ~rule_id t =
    let () = t.no_more_unary <- succ t.no_more_unary in
    let () = t.consecutive.(rule_id) <- succ t.consecutive.(rule_id) in
    t

  let incr_clashing_instance ~rule_id t =
    let () = t.clashing_instance <- succ t.clashing_instance in
    let () = t.consecutive.(rule_id) <- succ t.consecutive.(rule_id) in
    t

  let incr_time_correction t =
    let () = t.time_correction <- succ t.time_correction in
    t

  let print_detail ~current_event f t =
    let all = float_of_int (nb t) in
    let events = float_of_int current_event in
    let () = Format.pp_open_vbox f 0 in
    let () =
      if all > 0. then
        Format.fprintf f
          "@[%.2f%% of event loops were productive.@ Null event cause:@]@,"
          (100. *. events /. (all +. events))
    in
    let () =
      if t.no_more_unary > 0 then
        Format.fprintf f
          "\tValid embedding but no longer unary when required: %.2f%%@,"
          (100. *. float_of_int t.no_more_unary /. all)
    in
    let () =
      if t.no_more_binary > 0 then
        Format.fprintf f
          "\tValid embedding but not binary when required: %.2f%%@,"
          (100. *. float_of_int t.no_more_binary /. all)
    in
    let () =
      if t.clashing_instance > 0 then
        Format.fprintf f "\tClashing instance: %.2f%%@,"
          (100. *. float_of_int t.clashing_instance /. all)
    in
    let () =
      if t.time_correction > 0 then
        Format.fprintf f "\tPerturbation interrupting time advance: %.2f%%@,"
          (100. *. float_of_int t.time_correction /. all)
    in
    Format.fprintf f "@]"

  let to_yojson t =
    `Assoc
      [
        "consecutive", JsonUtil.of_array JsonUtil.of_int t.consecutive;
        "consecutive_blocked", `Int t.consecutive_blocked;
        "no_more_binary", `Int t.no_more_binary;
        "no_more_unary", `Int t.no_more_unary;
        "clashing_instance", `Int t.clashing_instance;
        "time_correction", `Int t.time_correction;
      ]

  let of_yojson = function
    | `Assoc l as x when List.length l = 6 ->
      {
        consecutive =
          (JsonUtil.to_array Yojson.Basic.Util.to_int)
            (Yojson.Basic.Util.member "consecutive" x);
        consecutive_blocked =
          Yojson.Basic.Util.to_int
            (Yojson.Basic.Util.member "consecutive_blocked" x);
        no_more_binary =
          Yojson.Basic.Util.to_int (Yojson.Basic.Util.member "no_more_binary" x);
        no_more_unary =
          Yojson.Basic.Util.to_int (Yojson.Basic.Util.member "no_more_unary" x);
        clashing_instance =
          Yojson.Basic.Util.to_int
            (Yojson.Basic.Util.member "clashing_instance" x);
        time_correction =
          Yojson.Basic.Util.to_int
            (Yojson.Basic.Util.member "time_correction" x);
      }
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Invalid simulation efficiency", x))

  let write_t ob f = Yojson.Basic.to_buffer ob (to_yojson f)

  let string_of_t ?(len = 1024) x =
    let ob = Buffer.create len in
    write_t ob x;
    Buffer.contents ob

  let read_t p lb = of_yojson (Yojson.Basic.from_lexbuf ~stream:true p lb)
  let t_of_string s = read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
end

type t = {
  mutable time: float;
  mutable events: int;
  mutable stories: int;
  mutable last_point: int;
  mutable stat_null: Efficiency.t;
  init_time: float;
  init_event: int;
  mutable plot_period: Configuration.period;
  mutable max_time: float option;
  mutable max_event: int option;
}

let current_story c = c.stories
let current_time c = c.time
let current_event c = c.events
let nb_null_event c = Efficiency.nb c.stat_null

let consecutive_null_event ~rule_id c =
  Efficiency.nb_consecutive ~rule_id c.stat_null

let consecutive_blocked c = Efficiency.nb_consecutive_blocked c.stat_null
let inc_stories c = c.stories <- c.stories + 1
let inc_events c = c.events <- c.events + 1

let check_time c =
  match c.max_time with
  | None -> true
  | Some max -> c.time <= max

let check_output_time c ot =
  match c.max_time with
  | None -> true
  | Some max -> ot <= max

let check_events c =
  match c.max_event with
  | None -> true
  | Some max -> c.events < max

let one_time_advance c dt =
  let () = c.time <- c.time +. dt in
  check_time c

let one_constructive_event ~rule_id c =
  let () = c.stat_null <- Efficiency.reset_consecutive ~rule_id c.stat_null in
  let () = c.stat_null <- Efficiency.reset_consecutive_blocked c.stat_null in
  let () = inc_events c in
  check_time c && check_events c

let one_no_more_binary_event ~rule_id c =
  let () = c.stat_null <- Efficiency.incr_no_more_binary ~rule_id c.stat_null in
  check_time c && check_events c

let one_no_more_unary_event ~rule_id c =
  let () = c.stat_null <- Efficiency.incr_no_more_unary ~rule_id c.stat_null in
  check_time c && check_events c

let one_clashing_instance_event ~rule_id c =
  let () =
    c.stat_null <- Efficiency.incr_clashing_instance ~rule_id c.stat_null
  in
  check_time c && check_events c

let one_time_correction_event ?ti c =
  match Option_util.bind Nbr.to_float ti with
  | None -> false
  | Some ti ->
    let () = c.time <- ti in
    let () = c.stat_null <- Efficiency.incr_time_correction c.stat_null in
    check_time c && check_events c

let one_blocked_event c =
  let () = c.stat_null <- Efficiency.incr_consecutive_blocked c.stat_null in
  check_time c && check_events c

let get_efficiency c = c.stat_null

let print_efficiency f c =
  Efficiency.print_detail ~current_event:(current_event c) f c.stat_null

let init_time c = c.init_time
let max_time c = c.max_time
let max_events c = c.max_event
let plot_period c = c.plot_period

let time_ratio t =
  match t.max_time with
  | None -> None
  | Some tmax ->
    if tmax > t.init_time then
      Some ((t.time -. t.init_time) /. (tmax -. t.init_time))
    else
      None

let event_ratio t =
  match t.max_event with
  | None -> None
  | Some emax ->
    if emax = 0 then
      None
    else
      Some
        (float_of_int (t.events - t.init_event)
        /. float_of_int (emax - t.init_event))

let set_max_time c t = c.max_time <- t
let set_max_events c e = c.max_event <- e

let tracked_events (counter : t) : int option =
  if counter.stories >= 0 then
    Some counter.stories
  else
    None

let set_plot_period (t : t) plot_period : unit = t.plot_period <- plot_period

let create ?(init_t = 0.) ?(init_e = 0) ?max_time ?max_event ~plot_period
    ~nb_rules () =
  {
    time = init_t;
    events = init_e;
    stories = -1;
    stat_null = Efficiency.init nb_rules;
    plot_period;
    init_time = init_t;
    init_event = init_e;
    max_time;
    max_event;
    last_point = 0;
  }

let reinitialize counter =
  counter.time <- counter.init_time;
  counter.events <- counter.init_event;
  counter.stories <- -1;
  counter.last_point <- 0;
  counter.stat_null <-
    Efficiency.init (Array.length counter.stat_null.Efficiency.consecutive)

let next_step_simulation_info c =
  {
    Trace.Simulation_info.story_id = current_story c;
    Trace.Simulation_info.story_time = current_time c;
    Trace.Simulation_info.story_event = current_event c + 1;
    Trace.Simulation_info.profiling_info = ();
  }

let current_simulation_info c =
  {
    Trace.Simulation_info.story_id = current_story c;
    Trace.Simulation_info.story_time = current_time c;
    Trace.Simulation_info.story_event = current_event c;
    Trace.Simulation_info.profiling_info = ();
  }

let next_story c =
  let () = inc_stories c in
  next_step_simulation_info c

let positive_plot_period counter =
  match plot_period counter with
  | Configuration.DE e -> e > 0
  | Configuration.DT t -> t > 0.

let next_point counter dt =
  match counter.plot_period with
  | Configuration.DT dT ->
    if dT <= 0. then
      0
    else
      int_of_float
        ((min
            (Option_util.unsome infinity (max_time counter))
            (dt +. current_time counter)
         -. counter.init_time)
        /. dT)
  | Configuration.DE dE ->
    if dE <= 0 then
      0
    else
      (current_event counter - counter.init_event) / dE

let to_plot_points counter dt =
  let next = next_point counter dt in
  let last = counter.last_point in
  let () = counter.last_point <- next in
  let n = next - last in
  match counter.plot_period with
  | Configuration.DT dT ->
    ( snd
        (Tools.recti
           (fun (time, acc) _ ->
             ( time -. dT,
               if check_output_time counter time then
                 time :: acc
               else
                 acc ))
           (counter.init_time +. (float_of_int next *. dT), [])
           n),
      counter )
  | Configuration.DE _ ->
    if n = 1 then
      [ counter.time ], counter
    else if n = 0 then
      [], counter
    else
      invalid_arg
        ("Counter.to_plot_points: invalid increment " ^ string_of_int n)

let fill ~outputs counter ~dt =
  let points, counter' = to_plot_points counter dt in
  List.iter (fun time -> outputs counter' time) points

let fake_time t time = { t with time }
