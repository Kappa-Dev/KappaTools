(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type bar = { mutable ticks: int; bar_size: int; bar_char: char }

type text = {
  mutable last_length: int;
  mutable last_event_nb: int;
  mutable last_time: float;
}

type t = Bar of bar | Text of text

let inc_tick c = c.ticks <- c.ticks + 1

let create bar_size bar_char =
  if Unix.isatty Unix.stdout then
    Text { last_length = 0; last_time = -5.; last_event_nb = 0 }
  else (
    let () =
      for _ = bar_size downto 1 do
        Format.pp_print_string Format.std_formatter "_"
      done
    in
    let () = Format.pp_print_newline Format.std_formatter () in
    Bar { ticks = 0; bar_size; bar_char }
  )

let pp_not_null f x =
  match x with
  | Some x -> Format.fprintf f " (%.2f%%)" (x *. 100.)
  | None -> ()

let pp_text delta_t time t_r event e_r f s =
  let string =
    Format.asprintf "%.2f time units%a in %i events%a%t" time pp_not_null t_r
      event pp_not_null e_r (fun f ->
        match delta_t with
        | None -> ()
        | Some dt ->
          Format.fprintf f " (just did %.1f events/s)"
            (float_of_int (event - s.last_event_nb) /. dt))
  in
  let () =
    Format.fprintf f "%s%s%s@?"
      (String.make s.last_length '\b')
      string
      (String.make (max 0 (s.last_length - String.length string)) ' ')
  in
  s.last_length <- String.length string

let rec aux_tick something s n =
  if n <= 0 then
    if something then
      Format.pp_print_flush Format.std_formatter ()
    else
      ()
  else (
    let () = Format.pp_print_char Format.std_formatter s.bar_char in
    let () = inc_tick s in
    aux_tick true s (pred n)
  )

let tick ~efficiency time t_r event e_r = function
  | Bar s ->
    let n_t = Option_util.unsome 0. t_r *. float_of_int s.bar_size in
    let n_e = Option_util.unsome 0. e_r *. float_of_int s.bar_size in
    aux_tick false s (int_of_float (max n_t n_e) - s.ticks)
  | Text s ->
    let run = Sys.time () in
    let dt = run -. s.last_time in
    if dt > 0.5 then (
      let () =
        pp_text
          (if efficiency then
             Some dt
           else
             None)
          time t_r event e_r Format.std_formatter s
      in
      let () = s.last_event_nb <- event in
      s.last_time <- Sys.time ()
    )

let complete_progress_bar time event t =
  (match t with
  | Bar t -> aux_tick false t (t.bar_size - t.ticks)
  | Text s -> pp_text None time None event None Format.std_formatter s);
  Format.pp_print_newline Format.std_formatter ()
