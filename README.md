<img
src="https://rawgithub.com/Kappa-Dev/KaSim/master/man/img/KaSim-Logo.svg"
alt="KaSim logo" title="Stochastic Kappa Simulator" align="right" height="90"/>
# KaSim

[![Build Status](https://travis-ci.org/Kappa-Dev/KaSim.svg?branch=master)](https://travis-ci.org/Kappa-Dev/KaSim)
[![Join the chat at https://gitter.im/Kappa-Dev/KaSim](https://badges.gitter.im/Kappa-Dev/KaSim.svg)](https://gitter.im/Kappa-Dev/KaSim?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

KaSim is a stochastic simulator for rule-based models written in Kappa. KaSa is
a static analyser for Kappa models.

## User manual

The reference manual is available online in
[pdf](https://github.com/Kappa-Dev/KaSim/releases) and
[html](http://tools.kappalanguage.org/docs/KaSim-manual-master/KaSim_manual.htm);

The latex sources are available in the man/ directory. To compile the
manuel, in addition of a decent LaTeX distribution you need
[gnuplot](http://www.gnuplot.info/) and
[graphviz](http://www.graphviz.org/) to generate images (make sure
that `dot` is in the PATH of your OS). To generate the pdf of the manual
type

`make doc`

## Installation

[Released versions](https://github.com/Kappa-Dev/KaSim/releases) come with
binaries for MacOS, Windows and Debian derivatives (as Ubuntu). [Nightly
builds](https://tools.kappalanguage.org/nightly-builds/) of the master branch
are built for these platforms by the continuous integration tools.

If you want or need your own build,
 - Install [opam](https://opam.ocaml.org/doc/Install.html) (the OCaml package manager)  and
initialize it (by issuing `opam init`)
 - In the source directory, type `opam pin add -n .` and validate
 - Install all the dependencies by `opam install --deps-only kappa-agents`
 - `make all`

If you would like to get `WebSim` (the Kappa REST service server), in addition of the previous steps: `opam install kappa-server`

Nothing worked so far. Well, you're pretty much on your own... Kappa
tools depend upon the OCaml native compiler version 4.03.0 or above as
well as _dune_, _findlib_, _Lwt_ (>= 2.6.0), _Re_, _Fmt_, _Logs_ and
_Yojson_ libraries. Find any way to install them and you'll be only a
`make all` away from getting Kappa binaries...

## Usage

In order to run a simulation for 100 time units printing observables values
every 0.5 time unit, type

`bin/KaSim kappa_file_1 ... kappa_file_n -l 100 -p 0.5 -o data_file`

This will produce a data file of 200 point containing the
trajectory that was produced during the simulation.

Type:

`bin/KaSim --help`

for a complete list of options.
