<img
src="https://rawgithub.com/Kappa-Dev/KaSim/master/man/img/KaSim-Logo.svg"
alt="KaSim logo" title="Stochastic Kappa Simulator" align="right" height="90"/>
# KaSim

[![Join the chat at https://gitter.im/Kappa-Dev/KaSim](https://badges.gitter.im/Kappa-Dev/KaSim.svg)](https://gitter.im/Kappa-Dev/KaSim?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Gitter chat channel](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/Kappa-Dev?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)


KaSim is a stochastic simulator for rule-based models written in Kappa. KaSa is a static analyser for Kappa models.

## User manual

The reference manual is available online in [pdf](https://github.com/Kappa-Dev/KaSim/releases) and [html](http://kappa-dev.github.io/docs/KaSim-manual-master/KaSim_manual.htm);

The latex sources are available in the man/ directory. To compile the manuel, in addition of a decent LaTeX distribution you need [gnuplot](http://www.gnuplot.info/) and [graphviz](http://www.graphviz.org/) to generate images (make sure that dot is in the PATH of your OS). To generate the pdf of the manuel type

`make doc`

## Installation

To compile KaSim, you need the OCaml native compiler version 4.02.3 or
above and the findib library. To check whether you have them, type

`ocamlfind ocamlopt -version`

If you use a package manager (or opam, the OCaml package manager), OCaml
compilers and findlib are really likely provided by it. Else, OCaml native
compilers can be downloaded on [INRIA's website](http://caml.inria.fr/). The
Windows bundle contains findlib. Findlib sources are available on
[camlcity.org](http://projects.camlcity.org/projects/findlib.html).

To create KaSim binaries, simply type

`make bin/KaSim`

This should produce KaSim binaries. You will need your own plotting program
(like gnuplot) to visualize curves.

## Usage

In order to run a simulation of 1000 rule applications, type

`bin/KaSim kappa_file_1 ... kappa_file_n -e 1000 -p 1000 -o data_file`

This will produce a data file of 1000 point (-p option) containing the
trajectory that was produced during the simulation.

Type:

`bin/KaSim --help`

for a complete list of options.
