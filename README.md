# KaSim

KaSim is a stochastic simulator for rule-based models written in Kappa.

<img src="http://www.pps.jussieu.fr/~jkrivine/homepage/Research_files/droppedImage.jpg" alt="KaSim logo" title="Stochastic Kappa Simulator" align="right" />

The reference manual is available in the man/ directory of the sources.

## Installation

To install you need the ocaml native compiler. To check whether you have it, type 

`ocamlopt -version` 

Ocaml native compilers can be downloaded on [INRIA's website](http://caml.inria.fr/). To create KaSim binaries, simply type 

`make`

This should produce KaSim binaries. You will need your own plotting program (like gnuplot) to visualize curves.

## Usage

In order to run a simulation of 1000 rule applications, type

`KaSim -i kappa_file -e 1000 -p 1000 -o data_file`

This will produce a data file of 1000 point (-p option) containing the trajectory that was produced during the simulation.

Type:

`KaSim --help` 

for a complete list of options.

