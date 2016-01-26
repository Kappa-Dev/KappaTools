let dot_of_flux env (file,interresting,flux) =
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
	      if interresting.(s) && interresting.(d) then
	      Format.fprintf
		f
		"@[<h>\"%a\" -> \"%a\" [weight=%d,label=\"%.3f\",color=%s,arrowhead=%s];@]@,"
		(Environment.print_ast_rule ~env) s
		(Environment.print_ast_rule ~env) d
		(abs (int_of_float v)) v color arrowhead)) desc flux in
    Format.fprintf desc "}@]@."
  in
  Kappa_files.with_flux file printer

let json_of_flux env (file,interresting,flux) =
  Kappa_files.with_flux
    file
    (fun f ->
     Format.fprintf
       f "@[<v>{@ @[\"matrix\" :@ @[[%a]@]@],@ @[\"rules\" :@ @[[%a]@]@]@ }@]"
       (Pp.array_with_empty
	  Pp.comma
	  (fun s x ->
	   if interresting.(s) then
	     Some
	       (fun f ->
		Format.fprintf
		  f "@[[%a]@]"
		  (Pp.array_with_empty
		     Pp.comma (fun d y ->
			       if interresting.(d) then
				 Some
				   (fun f -> Format.pp_print_float f y)
			       else None))
		  x)
	   else None))
       flux
       (Pp.array_with_empty
	  Pp.comma
	  (fun i _ ->
	   if interresting.(i) then
	     Some (fun f ->
		   Format.fprintf
		     f "\"%a\"" (Environment.print_ast_rule ~env) i)
	   else None))
       flux)

let html_of_flux env (file,interresting,flux) =
  Kappa_files.with_flux
    file
    (Pp_html.graph_page
       (fun f -> Format.pp_print_string f "Flux map")
       ["http://d3js.org/d3.v3.min.js"]
       (fun f ->
       let () =
	  Format.fprintf
	    f "@[<v 2><style>@,.chord path {@ fill-opacity: .67;@ " in
       Format.fprintf
	 f "stroke: #000;@ stroke-width: .5px;@ })@]@,</style>")
       (fun f ->
	let () =
	  Format.fprintf
	    f "@[<v 2><script>@,var matrix = @[[%a];@]@,"
	    (Pp.array_with_empty
	       Pp.comma
	       (fun s x ->
		if interresting.(s) then
		  Some
		    (fun f ->
		     Format.fprintf
		       f "@[[%a]@]"
		       (Pp.array_with_empty
			  Pp.comma
			  (fun d y ->
			   if interresting.(d) then
			     Some (fun f -> Format.pp_print_float f (abs_float y))
			   else None))
		       x)
		else None))
	    flux in
	let () =
	  Format.fprintf
	    f "var fill = @[[%a];@]@,"
	    (Pp.array_with_empty
	       Pp.comma
	       (fun s x ->
		if interresting.(s) then
		  Some (fun f ->
			Format.fprintf
			  f "@[[%a]@]"
			  (Pp.array_with_empty
			     Pp.comma
			     (fun d y ->
			      if interresting.(d) then
				Some
				  (fun f ->
				   Format.pp_print_string
				     f
				     (if y < 0. then "\"#FF0000\"" else "\"#00FF00\""))
			      else None))
			  x)
		else None))
	    flux in
	let () =
	  Format.fprintf
	    f "var labels = @[[%a];@]@,"
	    (Pp.array_with_empty
	       Pp.comma
	       (fun i _ ->
		if interresting.(i) then
		  Some
		    (fun f ->
		     Format.fprintf
		       f "\"%a\"" (Environment.print_ast_rule ~env) i)
		else None))
	    flux in
	let () =
	  Format.fprintf
	    f "var chord = @[d3.@,layout.@,chord()@,.padding(.01)" in
	let () =
	  Format.fprintf
	    f "@,.sortSubgroups(d3.descending)@,.matrix(matrix);@]@," in
	let () =
	  Format.fprintf
	    f "@[var width = 960,@ height = 700,@ " in
	let () =
	  Format.fprintf
	    f "innerRadius = Math.min(width, height) * .37;@]@," in
	let () =
	  Format.fprintf
	    f "var arc = @[d3@,.svg@,.arc()@,.innerRadius(innerRadius)" in
	let () =
	  Format.fprintf
	    f "@,.outerRadius(innerRadius + 8);@]@," in
	let () =
	  Format.fprintf
	    f "var svg = @[d3@,.select(\"body\")@,.select(\"svg\")" in
	let () =
	  Format.fprintf
	    f "@,.attr(\"width\", width)@,.attr(\"height\", height)" in
	let () =
	  Format.fprintf
	    f "@,.select(\"g\").attr(\"transform\", \"translate(\" + width / 2 + \",\" + height / 2 + \")\");@]@," in
	let () =
	  Format.fprintf
	    f "@[svg.append(\"g\")@,.attr(\"class\", \"chord\")" in
	let () =
	  Format.fprintf
	    f "@,.selectAll(\"path\")@,.data(chord.chords)@,.enter()" in
	let () =
	  Format.fprintf
	    f "@,.append(\"path\")@,.attr(\"d\", d3.svg.chord().radius(innerRadius))" in
	let () =
	  Format.fprintf
	    f "@,.style(\"fill\", function(d) { return fill[d.source.index][d.target.index]; })@,.style(\"opacity\", 1);@]@," in
	let () =
	  Format.fprintf
	    f "var legends = @[svg@,.append(\"g\")@,.selectAll(\"g\")@,.data(chord.groups)" in
	let () =
	  Format.fprintf
	    f "@,.enter()@,.append(\"g\");@]@," in
	  let () =
	  Format.fprintf
	    f "@[legends@,.append(\"text\")@,.each(function(d) { d.angle = (d.startAngle + d.endAngle) / 2; })" in
	let () =
	  Format.fprintf
	    f "@,.attr(\"dy\", \".1em\")@,.attr(\"transform\",@[ function(d) {@ " in
	let () =
	  Format.fprintf
	    f "return \"rotate(\" + (d.angle * 180 / Math.PI - 90) + \")\"@ " in
	let () =
	  Format.fprintf
	    f "+ \"translate(\" + (innerRadius + 10) + \")\"@ + (d.angle > Math.PI ? \"rotate(180)\" : \"\");@ }@])" in
	let () =
	  Format.fprintf
	    f "@,.style(\"text-anchor\", function(d) { return d.angle > Math.PI ? \"end\" : null; })" in
	let () =
	  Format.fprintf
	    f "@,.text(function(d) { return labels[d.index]; });@]@," in
	let () =
	  Format.fprintf
	    f "legends@,.append(\"path\")@,.style(\"fill\", \"#222222\")"in
	let () =
	  Format.fprintf
	    f "@,.attr(\"d\", arc)@,.on(\"mouseover\", fade(.1))@,.on(\"mouseout\", fade(1));@]@," in
	let () =
	  Format.fprintf
	    f "// Returns an event handler for fading a given chord group.@," in
	let () =
	  Format.fprintf
	    f "@[function fade(opacity) {@ return function(g, i) {@ " in
	let () = Format.fprintf f "svg@,.selectAll(\".chord path\")@,." in
	let () =
	  Format.fprintf
	    f "filter(function(d) { return d.source.index != i && d.target.index != i; })" in
	let () =
	  Format.fprintf
	    f "@,.transition()@,.style(\"opacity\", opacity);@ };@ }@]" in
	Format.fprintf f "@]@,</script>@,"))

let output_flux env (file,_,_ as b) =
  if Filename.check_suffix file ".html"
  then html_of_flux env b
  else if Filename.check_suffix file ".json"
  then json_of_flux env b
  else dot_of_flux env b
