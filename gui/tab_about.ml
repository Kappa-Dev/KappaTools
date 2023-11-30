(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

module Html = Tyxml_js.Html5

let navli () = ReactiveData.RList.empty

let content () =
  [%html
    {|
<h2>The Kappa Language</h2>
<p>Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF<p>
<p>Kappa Language software is distributed under the terms of the <a target="_blank" href="https://www.gnu.org/licenses/lgpl-3.0.html">GNU Lesser General Public License Version 3</a>.
 Source code is published on <a target="_blank" href="https://github.com/Kappa-Dev/KaSim">https://github.com/Kappa-Dev/KaSim</a>.</p>

<p>This user interface was developed in the <a target="_blank" href="https://fontana.hms.harvard.edu">Fontana Lab</a> under DARPA grant W911NF-14-1-0367. Visualizations were developed in the <a target="_blank" href="https://creativecoding.soe.ucsc.edu/news.php">Creative Coding Lab</a> of Angus Forbes</p>

<p>Reference manual is online at <a target="_blank" href="https://www.kappalanguage.org">https://www.kappalanguage.org</a>.</p>

<ul>
<li>Written in <a target="_blank" href="http://ocaml.org">
  <!--<img src="http://ocaml.org/logo/Colour/SVG/colour-logo.svg"
       alt="-->OCaml<!--"
       style="border: none; width: 150px;" /> -->
</a>. Compiled by <a target="_blank" href="http://ocsigen.org/js_of_ocaml/">js_of_ocaml</a>.</li>
<li>Text editor used is <a target="_blank" href="http://codemirror.net">CodeMirror</a>. It comes with a lot of features. Check out its <a target="_blank" href="http://codemirror.net/doc/manual.html#commands">manual</a>.</li>
<li>Visualization relies on <a target="_blank" href="https://d3js.org">d3</a>.</li>
<li>Layout uses <a target="_blank" href="http://getbootstrap.com">bootstrap</a> and <a target="_blank" href="https://jqueryui.com">jQuery</a>.</li>
</ul>

<h3>Nominal contribution</h3>
<dl class="dl-horizontal"><dt>Pierre Boutillier</dt><dd>General design and maintenance, protocols</dd><dt>Xing Li</dt><dd>Outputs visualizations, General layout</dd><dt>Mutaamba Maasha</dt><dd>Transparent multi backend architecture, protocols, Output visualizations, General layout</dd></dl>
|}]

let onload () = ()
let onresize () = ()
