# Contributing to visualization engines

## General API

Each visualization engine is an object initialized with as first
argument the id of the `div` in which it must insert its elements.

Each of these objects exposes 2 methods:

- `setData` that takes as argument a string containing the data in a
JSON format and has to draw the received data. Sending `null` as
argument should clear the space (aka draw "nothing").

- `redraw` without argument which is called when the size of the area
  has changed.

For now, renderers infer the height and the width they have to render.
This may not be optimal.

The code of visualizer engines lives in the [viz/] directory of the
repository.

## Gathering data & offline visualization

In the Kappapp, there are export forms for downloading outputed data
all over the place. If you select the JSON format and "export", you
get the exact text the rendering engine receive as argument of
`setData`.

JSON exports can be used to revisualized data by using the [offline
visualizer](viz/index.html). Select the kind of data you want to
vizualize in the top navbar and drag&drop your JSON file in the
window.

This framework is also the recommended way to easily develop rendering
engines. Adding an engine in the `viz/index.html`file should be fairly
straight forward by taking as example an existing one. Then manage to
do something that renders the data it receives inside the correct
`div` taking all the available space it has and we'll be in a very
good situation for integration in the Kappapp.

## JSON format of kappa expressions.

JSON formats to represent contact maps, snapshots, (observed) patterns
(and maybe at some point rule but not for now) follow the
same squeleton:

* a `graph` is an 2d matrix of `agent`s or holes represented
  by `null`. (There is no holes in contact_map and snapshots)

* an `agent` is an object with 2 fields: a string "node_type" and an
  array of `site`s "node_sites".

Here the representation of the agent `A` with no site:
`{"node_type":"A","node_sites":[]}`.

* a `site` is an object with 2 fields: a (string) "site_name" and a
  `site_type` "site_type".

* the `site_type` can be
  * a `"counter"` that we'll skip for now as they are still
    experimental
  * a `"port"` that is represented by the construction
    `["port",{"port_links":[],"port_states":[]}]`

The field "port_link" takes as argument an array of `link state`s and
"port_states" an array of `internal state`s.

As an example, the site x with 2 internal states `u` and `p` and
connected to someone else is represented as
`{"site_name":"x","site_type":["port",{"port_links":true,"port_states":["p","u"]}]}`

* a internal state is either a string or `null` which encodes the
  special case ANY. There is at most 1 internal state in patterns and
  snapshots. ANY only occurs in patterns.

* a link state can be either FREE, WHATEVER, SOME, SOME_OF_TYPE(site
  label, agent label) or THIS(pointers). Again, following their
  invarients, contact_maps and snashots contain only FREE of
  THIS(pointers) and there is at most 1 pointer in the list for
  patterns and snapshots.
  * WHATEVER is represented by `null`.
  * SOME by the boolean `true`.
  * FREE by the empty list `[]`.
  * SOME_OF_TYPE by the object `{"site_name": string, "agent_type": string }`
  * Explicit links are represented in a pointer format in THIS.

  That means that you give the destination of the edge by giving its
  coordinates in the graph (the pair of a pair of int and a int:
  `(position_of_the_agent_in_the_matrix, id_of_the_site_in_the_agent)`).

As an example, here the representation of the kappa graph `A(x[5]), B(b[5 8]), A(x[.] y[8])` (This example, while being a concise illustration, is impossible in Kappa outputs: having several links for 1 site is contact_map specific but having 2 agents of the same type as well as a free site is impossible in contact maps...)

```
[ [
    {
        "node_type":"A",
        "node_sites":[{"site_name":"x","site_type":["port",{"port_links":[[[0,1],0]],"port_states":[]}]}]
    },
    {
        "node_type":"B",
        "node_sites":[{"site_name":"b","site_type":["port",{"port_links":[[[0,0],0],[[0,2],1]],"port_states":[]}]}]
    },
    {
        "node_type":"A",
        "node_sites":[
            {"site_name":"x","site_type":["port",{"port_links":[],"port_states":[]}]},
            {"site_name":"y","site_type":["port",{"port_links":[[[0,1],0]],"port_states":[]}]}
        ]
    }
] ]
```

