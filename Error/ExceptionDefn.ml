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

exception Syntax_Error of string Term.with_pos
exception Malformed_Decl of string Term.with_pos
exception Internal_Error of string Term.with_pos
exception Unsatisfiable

let warning_buffer:
      ((Lexing.position * Lexing.position) option*(Format.formatter -> unit)) list ref = ref []

let warning ?pos msg =
  warning_buffer :=(pos,msg)::!warning_buffer

let deprecated ~pos entry msg =
  warning ~pos (fun f -> Format.fprintf f "Deprecated %s syntax:@ %t" entry msg)

let flush_warning f =
  let () = Format.pp_print_newline f () in
  let l = List.rev !warning_buffer in
  List.iter (fun (pos,msg) ->
	     let pr f =
	       match pos with
	       | Some pos -> Format.fprintf f "%a@," Pp.position pos
	       | None -> Format.fprintf f ""
	     in
	     Format.fprintf f "@[<v>%tWarning: @[%t@]@]@." pr msg) l
