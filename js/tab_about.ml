(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let navli () = []

let content () = [%html {|
<h2>The Kappa Language</h2>
<p>Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF<p>
<p>Kappa Language softwares are distributed under the terms of the <a target="_blank" href="https://www.gnu.org/licenses/lgpl-3.0.html">GNU Lesser General Public License Version 3</a>.
 Source code is published on <a target="_blank" href="https://github.com/Kappa-Dev/KaSim">https://github.com/Kappa-Dev/KaSim</a>.</p>

<p>This user interface was developed in the Fontana lab under DARPA grant W911NF-14-1-0367 .</p>

<p>Reference manual is online at <a target="_blank" href="http://dev.executableknowledge.org">http://dev.executableknowledge.org</a>.</p>

<ul>
<li>Text editor used is <a target="_blank" href="http://codemirror.net">CodeMirror</a>. It comes with a lot of features. Check out its <a target="_blank" href="http://codemirror.net/doc/manual.html#commands">manual</a>.</li>
<li>Written in <a target="_blank" href="http://ocaml.org">
  <!--<img src="http://ocaml.org/logo/Colour/SVG/colour-logo.svg"
       alt="-->OCaml<!--"
       style="border: none; width: 150px;" /> -->
</a>. Compiled by <a target="_blank" href="http://ocsigen.org/js_of_ocaml/">js_of_ocaml</a>.</li>
<li>Visualizations relies on <a target="_blank" href="https://d3js.org">d3</a>.</li>
<li>Layout is done thanks to <a target="_blank" href="http://getbootstrap.com">bootstrap</a> and <a target="_blank" href="https://jqueryui.com">jQuery</a>.</li>
</ul>

<h3>Nominal contribution</h3>
<dl class="dl-horizontal"><dt>Pierre Boutillier</dt><dd>General design and maintenance, protocols</dd><dt>Xing Li</dt><dd>Outputs visualizations, General layout</dd><dt>Mutaamba Maasha</dt><dd>Transparent multi backend architecture, protocols, Output visualizations, General layout</dd></dl>
|}]

let onload () = ()

let onresize () = ()
