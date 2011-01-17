exception IntFound of int
exception StringFound of string
exception MapFound of (int Mods.IntMap.t)
exception False
exception True
exception Null_event
exception Deadlock
exception UserInterrupted of string
exception StopReached of string

exception Syntax_Error of string
exception Semantics_Error of Tools.pos * string

let warning ?with_pos msg = 
	let _ = 
		match with_pos with
			| Some pos -> prerr_string ((Tools.string_of_pos pos)^" ")
			| None -> ()
	in
		prerr_string ("WARNING: "^msg^"\n") ; flush stderr

