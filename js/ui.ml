module Html5 = Tyxml_js.Html5

let raw_compilation_tab = <:html5<<li><a>Compilation</a></li> >>

let compiled_env_div =
  Html5.div
    ~a:[Html5.a_class ["panel-body"]]
	 [Tyxml_js.R.Html5.pcdata
	    (React.S.bind
	       Storage.model_env
	       (fun env ->
		React.S.const (Format.asprintf "%a" Kappa_printer.env env)))]
let raw_compilation_panel =
  <:html5<<div class="panel panel-body">
<div class="panel-header">Environment</div>
  $compiled_env_div$</div> >>
let raw_menu = <:html5<<ul class="nav nav-tabs">
<li class="active"><a href="#">Output</a></li>
$raw_compilation_tab$
</ul> >>

let raw_panel = Tyxml_js.To_dom.of_div raw_compilation_panel
