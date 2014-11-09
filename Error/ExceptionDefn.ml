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

let warning_buffer:(Format.formatter -> unit) list ref = ref []

let warning ?pos msg =
  let pr f =
    match pos with
    | Some pos -> Format.fprintf f "%a@," Pp.position pos
    | None -> Format.fprintf f ""
  in
  warning_buffer :=
    (fun f -> Format.fprintf f "@[<v>%tWarning: @[%t@]@]@." pr msg)::
      !warning_buffer

let deprecated ~pos entry msg =
  warning ~pos (fun f -> Format.fprintf f "Deprecated %s syntax:@ %t" entry msg)

let flush_warning () =
  Format.pp_print_newline Format.err_formatter ();
  let l = List.rev !warning_buffer in
  List.iter (fun s -> Format.eprintf "%t" s) l
