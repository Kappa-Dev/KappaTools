open Ocamlbuild_plugin

let doc_intro = "dev/api_intro"

let () =
  dispatch (function
	     | Before_options ->
		Options.use_ocamlfind := true
	     | After_rules ->
		dep ["ocaml"; "doc"; "extension:html"] & [doc_intro];
		flag ["ocaml"; "doc"; "extension:html"] &
		  (S[A"-t"; A"KaSim API";
		     A"-intro"; P doc_intro;
		     A"-colorize-code"])
	     | After_options | Before_rules | Before_hygiene | After_hygiene -> ())
