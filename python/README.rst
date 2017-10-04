Kappy
-----

Do::
  >>> import kappy
  >>> client = kappy.KappaRest("http://url_of/the_server","project_name")
to get a kappa client that uses the REST API hosted by
*http://url_of/the_server* and deals with project *project_name*.

Do::
  >>> import kappy
  >>> client = kappy.KappaStd("KaSimAgent")
to get a kappa client that uses a kappa agent installed locally. Replace
*KaSimAgent* by *path/to/KaSimAgent* if the executable is not in your path.

A kappa agent can be obtained thanks to the *opam* package manager through::
  $ opam install atdgen lwt
  $ opam pin add --dev KaSim

The methods of *client* can be infered by looking at **kappa_rest.py**
file in the source distribution but have still to get documented properly...
