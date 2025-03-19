<img
src="https://rawgithub.com/Kappa-Dev/KaSim/master/man/img/KaSim-Logo.svg"
alt="KaSim logo" title="Stochastic Kappa Simulator" align="right" height="90"/>
# KappaTools

[![Build Status](https://github.com/Kappa-Dev/KappaTools/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/Kappa-Dev/KappaTools/actions/workflows/ci.yml?query=branch%3Amaster)
[![Join the chat at https://gitter.im/Kappa-Dev/KaSim](https://badges.gitter.im/Kappa-Dev/KaSim.svg)](https://gitter.im/Kappa-Dev/KaSim?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

KaSim is a stochastic simulator for rule-based models written in Kappa. KaSa is
a static analyser for Kappa models.

Kappy is a python library to launch and analyse runs and outputs of
Kappa models.


## Quick startup

If you are new to Kappa, the easiest way to start experimenting with it is using the webapp.

<p align="center">
<img alt="Editor screenshot with contact map" src="./dev/screenshots/editor1.png" width="45%">
<img alt= "Plot screenshot" src="./dev/screenshots/plot.png" width="45%">
</p>
<p align="center">
<img alt="Editor screenshot with contact map 2" src="./dev/screenshots/editor2.png" width="45%">
<img alt="Stories screenshot" src="./dev/screenshots/story.png" width="45%">
</p>


It's available [directly in your browser](https://tools.kappalanguage.org/try/?model=https%3A//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/abc.ka),
or for more performance as a downloadable [electron app](https://tools.kappalanguage.org/nightly-builds/), available for MacOS, Windows and Linux.

Kappa tools are also available as Command-Line Interface programs, which you can either build following the [instructions below](#core-tools),
or find the binaries included with the [electron app](https://tools.kappalanguage.org/nightly-builds/) in subdir `resources/bin`.

If you would like to use python to interact with the Kappa tools, the `kappy` lib is where to look. Here's an example of its usage with `ipython`

```python
In [2]: import kappy

In [3]: model_text = "%agent: A(x)\nA(x[.]), A(x[.]) <-> A(x[1]), A(x[1]) @ 1e-2,1\n%plot: |A(x[.])|\n%init: 100 A()"

In [4]: kappa_client = kappy.KappaStd()

In [5]: kappa_client.add_model_string(model_text)
Out[5]: [...]

In [6]: kappa_client.project_parse()
Out[6]: [...]

In [7]: kappa_client.simulation_start(kappy.SimulationParameter(.1,"[T] > 10"))
Out[7]: {'simulation_artifact_simulation_seed': 297327779}

In [8]: kappa_client.wait_for_simulation_stop()
Out[8]: [...]

In [9]: kappa_client.simulation_plot()
Out[9]:
  [6.7, 48.0],
[...]
  [0.4, 60.0],
  [0.3, 50.0],
  [0.2, 64.0],
  [0.1, 62.0],
  [0.0, 100.0]]}
```

See the [install instructions](#kappy) to start using kappy.


## User manual
See [documentation page on kappalanguage.org](https://kappalanguage.org/documentation).

Kappy [API
documentation is online](https://kasim.readthedocs.io/en/latest/kappy.html).

The latex sources of the "older" reference manual (and KaSa one) are
available in the `man/` directory. To compile the manuel, in addition of
a decent LaTeX distribution you need
[gnuplot](http://www.gnuplot.info/) and
[graphviz](http://www.graphviz.org/) to generate images (make sure
that `dot` is in the PATH of your OS). To generate the pdf of the
manual type

`make doc`

## Installation

### Core tools

[Released versions](https://github.com/Kappa-Dev/KaSim/releases) come with
binaries for MacOS, Windows and Debian derivatives (as Ubuntu). [Nightly
builds](https://tools.kappalanguage.org/nightly-builds/) of the master branch
are built for these platforms by the continuous integration tools.

If you want or need your own build,
 - Install [opam](https://opam.ocaml.org/doc/Install.html) (the OCaml
   package manager) and initialize it (by issuing `opam init`)
 - In the source directory, install all the dependencies by `opam install 
   --deps-only .`
 - `dune build`

You can be more fine grained if you only need the command-line tools
(and therefore could install less dependencies) by doing `opam install
--deps-only kappa-binaries` followed by `make all`

If nothing worked for you so far. Well, you're pretty much on your
own... Kappa tools depend upon the OCaml native compiler version
4.05.0 or above as well as _dune_, _findlib_, _Lwt_ (>= 2.6.0), _Re_,
_Fmt_, _Logs_ and _Yojson_ libraries. Find any way to install them and
you'll be only a `make all` away from getting Kappa binaries...

### Kappy

You should be able to `pip install kappy`.

- Under MacOS and linux (and if you're not using a python version so
  cutting edge that we haven't notice its release yet), _wheels_ that
  contain the core binaries should be available.
- For other platforms/python versions, you need to get kappa agents by
  yourself thanks to the *opam* package manager by `opam install
  kappa-binaries kappa-agents` (or use an externaly hosted REST API)
- In order to develop in kappy and run all its tests, you need to
  follow the "get your own build section" above as well as install
  _requests_ (and _future_).

## Usage

### KaSim

In order to run a simulation for 100 time units printing observables values
every 0.5 time unit, type

`bin/KaSim kappa_file_1 ... kappa_file_n -l 100 -p 0.5 -o data_file`

This will produce a data file of 200 point containing the
trajectory that was produced during the simulation.

Type:

`bin/KaSim --help`

for a complete list of options.

### Kappy

Do:

```python
import kappy
client = kappy.KappaStd()
```

to get a kappa client that uses a kappa agent installed locally. Add a
string argument specifing the `path/to/KaSimAgent` to use a specific agent.

A minimal example of usage is:

```python
model = "\
%agent: A(x[x.A]) \
%var: n_0 100 \
%var: k_on 1e-2 \
'rule' A(x[.]), A(x[.]) <-> A(x[1]), A(x[1]) @ k_on, 1 \
%plot: |A(x[.])| \
%init: n_0 A()"
client.add_model_string(model)
client.project_parse()
sim_params = kappy.SimulationParameter(pause_condition="[T] > 100",plot_period=1)
client.simulation_start(sim_params)
client.wait_for_simulation_stop()
results = client.simulation_plot()
client.simulation_delete()
# Rerun with some overwritten values for algebraic variables
client.project_parse(k_on=5e-2,n_0=500)
client.simulation_start(sim_params)
client.wait_for_simulation_stop()
results' = client.simulation_plot()
client.shutdown()
```

## Tests

Launch the core/integration tests by `make check`.

Regenerate the reference files if you've changed something in the
outputs by `make build-tests`

Launch python tests by `nosetests` (after having followed the "Get
your own build" section).
