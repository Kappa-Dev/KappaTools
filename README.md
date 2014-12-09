<img
src="https://raw.githubusercontent.com/Kappa-Dev/KaSim/master/man/img/KaSim-logo.png"
alt="KaSim logo" title="Stochastic Kappa Simulator" align="right" />
# KaSim

KaSim is a stochastic simulator for rule-based models written in Kappa. A pdf of
the reference manual is available
[here](https://github.com/Kappa-Dev/KaSim/releases);

the latex sources are available in the man/ directory.

## Installation

To compile KaSim, you need the ocaml native compiler versoin 4.01 or
above and the findib library. To check whether you have them, type

`ocamlfind ocamlopt -version`

If you use a package manager (or opam, the Ocaml package manager), Ocaml
compilers and findlib are really likely provided by it. Else, Ocaml native
compilers can be downloaded on [INRIA's website](http://caml.inria.fr/). The
Windows bundle contains findlib. Findlib sources are available on
[camlcity.org](http://projects.camlcity.org/projects/findlib.html).

To create kaSim binaries, simply type

`make bin/KaSim`

This should produce KaSim binaries. You will need your own plotting program
(like gnuplot) to visualize curves.

## Usage

In order to run a simulation of 1000 rule applications, type

`bin/KaSim -i kappa_file -e 1000 -p 1000 -o data_file`

This will produce a data file of 1000 point (-p option) containing the
trajectory that was produced during the simulation.

Type:

`bin/KaSim --help`

for a complete list of options.
