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
