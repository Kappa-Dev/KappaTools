let tag s = print_string (s^"\n") ; flush stdout

let tag_if_debug s =
  if !Parameter.debugModeOn
  then Format.kfprintf (fun f -> Format.pp_print_newline f ())
		       Format.std_formatter s
  else Format.ifprintf Format.std_formatter s
