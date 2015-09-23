let dot_of_flux env (file,flux) =
  let p i = i - Environment.nb_syntactic_rules env in
  let printer desc =
    let () = Format.fprintf
	       desc "@[<v>digraph G{ label=\"Flux map\" ; labelloc=\"t\" ; " in
    let () = Format.fprintf
	       desc "node [shape=box,style=filled,fillcolor=lightskyblue]@," in
    (*let () =
      Pp.array
	Pp.space
	(fun i f _ ->
	 Format.fprintf
	   f "\"%a\" ;" (Environment.print_ast_rule ~env) (p i)) desc flux in*)
    let () =
      Pp.array
	(fun _ -> ())
	(fun s ->
	 Pp.array
	   Pp.empty
	   (fun d f v ->
	    if v=0. then ()
	    else
	      let color,arrowhead =
		if v<0. then ("red3","tee") else ("green3","normal") in
	      Format.fprintf
		f
		"@[<h>\"%a\" -> \"%a\" [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];@]@,"
		(Environment.print_ast_rule ~env) (p s)
		(Environment.print_ast_rule ~env) (p d)
		(abs (int_of_float v)) v color arrowhead)) desc flux in
    Format.fprintf desc "}@]@."
  in
  Kappa_files.with_flux file printer
