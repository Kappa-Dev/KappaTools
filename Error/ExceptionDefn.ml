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
exception Semantics_Error of Tools.pos * string
exception Unsatisfiable

let warning_buffer:string list ref = ref []

let warning ?with_pos msg = 
	let str = 
		match with_pos with
			| Some pos -> (Tools.string_of_pos pos)^" "
			| None -> ""
	in
		warning_buffer := ("WARNING: "^str^msg^"\n")::!warning_buffer

let flush_warning () = 
	prerr_string "\n";
	let l = List.rev !warning_buffer in
	List.iter (fun s -> prerr_string s) l ;
	flush stderr
