let tag f s = Format.fprintf f "%s@." s

let tag_if_debug s =
  if !Parameter.debugModeOn
  then Format.kfprintf (fun f -> Format.pp_print_newline f ())
		       Format.std_formatter s
  else Format.ifprintf Format.std_formatter s

let global_sigs : Signature.s ref = ref (Signature.create [])

let tag_begin_n f s = let _ = Format.fprintf f "%s " s in Format.print_flush () 
let tag_end_n f n =
  if n = 0
  then Format.fprintf f "(no change)@." 
  else if n>0 
  then Format.fprintf f "(-%i events)@." n  
  else Format.fprintf f "(+%i events)@." (-n)
    
