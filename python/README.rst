Kappy
-----

Do

>>> import kappy
>>> client = kappy.KappaRest("http\://url_of/the_server","project_name")

to get a kappa client that uses the REST API hosted by
*http://url_of/the_server* and deals with project *project_name*.

Do

>>> import kappy
>>> client = kappy.KappaStd()

to get a kappa client that uses a kappa agent installed locally. Add a
string argument specifing the *path/to/KaSimAgent* to use a specific agent.

A kappa agent can be obtained thanks to the *opam* package manager through::

  $ opam install atdgen lwt
  $ opam pin add --dev KaSim

The methods of *client* is described in the **kappa_common.py**
file in the source distribution.

A minimal example of usage is

>>> model = "\
%agent: A(x[x.A]) \
%var: n_0 100 \
%var: k_on 1e-2 \
'rule' A(x[.]), A(x[.]) <-> A(x[1]), A(x[1]) @ k_on, 1 \
%plot: |A(x[.])| \
%init: n_0 A()"
>>> client.add_model_string(model)
>>> client.project_parse()
>>> sim_params = kappy.SimulationParameter(pause_condition="[T] > 100",plot_period=1)
>>> client.simulation_start(sim_params)
>>> client.wait_for_simulation_stop()
>>> results = client.simulation_plot()
>>> client.simulation_delete()
>>> # Rerun with some overwritten values for algebraic variables
>>> client.project_parse(k_on=5e-2,n_0=500)
>>> client.simulation_start(sim_params)
>>> client.wait_for_simulation_stop()
>>> results' = client.simulation_plot()
>>> client.shutdown()
