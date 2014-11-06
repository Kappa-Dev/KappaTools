exception IntFound of int
exception StringFound of string
exception MapFound of (int Mods.IntMap.t)
exception False
exception True
exception Break of int

(* 0:unary rule with binary instance *)
(* 1:binary rule with unary instance *)
(* 2:clashing instance *)
(* 3:overapproximation clash *)
(* 4:invalid injection clash *)
(* 5: perturbation interrupting time*)
exception Null_event of int
exception Deadlock
exception UserInterrupted of (float -> int -> string)
exception StopReached of string

exception Syntax_Error of (Tools.pos option) * string
exception Malformed_Decl of string Term.with_pos
exception Semantics_Error of Tools.pos * string
exception Unsatisfiable

let warning_buffer:(out_channel -> unit) list ref = ref []

let warning ?pos msg =
  let pr f =
    match pos with
    | Some pos -> Printf.fprintf f "%a\n" Pp.position pos
    | None -> Printf.fprintf f ""
  in
  warning_buffer :=
    (fun f -> Printf.fprintf f "%tWarning: %t\n" pr msg)::
      !warning_buffer

let deprecated ~pos entry msg =
  warning ~pos (fun f -> Printf.fprintf f "Deprecated %s syntax: %t" entry msg)

let flush_warning () =
  prerr_string "\n";
  let l = List.rev !warning_buffer in
  List.iter (fun s -> Printf.eprintf "%t" s) l;
  flush stderr
