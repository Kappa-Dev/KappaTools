open Config
open Geometry

type state = Free | Bound | None

let show_binding_type = false
let space_between_proteins = 0.6

let config =
  {
    config with
    agent_width = 0.30 *. config.agent_width;
    agent_height = 0.23 *. config.agent_width;
    site_width = 0.6 *. config.site_width;
    site_height = 0.6 *. config.site_height;
    state_width = 1.2 *. config.state_width;
    state_height = 1.2 *. config.state_height;
    free_width = 0.8 *. config.free_width;
    free_height = 0.6 *. config.free_height;
    bound_height = 0.5 *. config.bound_height;
    rule_length = 0.4 *. config.rule_length;
    rule_margin = 0.1 *. config.rule_margin;
    show_agent_names = true;
    show_site_names = true;
    show_state_names = true;
    color_agents = false;
    color_sites = false;
    color_states = false;
    txt_font = 12;
  }

let [ (a, [ (x, []); (y, [ yu; yp ]); (z, [ zu; zp ]) ]) ], signature =
  add_in_signature
    [
      ( "$\texttt{A}$",
        [ Shape "ellipse" ],
        [
          "$\texttt{x}$", [ Direction n ], [];
          ( "$\texttt{y}$",
            [ Direction se ],
            [ "$\texttt{u}$", []; "$\texttt{p}$", [] ] );
          ( "$\texttt{z}$",
            [ Direction sw ],
            [ "$\texttt{0}$", []; "$\texttt{1}$", [] ] );
        ] );
    ]
    (snd (init config))

let [ (_, [ (ax, _); (ay, _); (az, _) ]) ], p =
  add_in_graph
    [
      ( a,
        0.,
        0.,
        [],
        [
          x, [], [];
          y, [], [ Internal_state (yp, [ Direction e ]) ];
          z, [], [ Internal_state (zu, [ Direction w ]) ];
        ] );
    ]
    signature

let _, p =
  add_free_list
    [ ax, []; ay, []; az, [] ]
    p (* remove if you do not want the symbols -| *)

let _ = dump "syntax_agent.ladot" p

let ( [
        (a, [ (ax, _); (ay, _); (az, _) ]);
        (a', [ (ax', _); (ay', _); (az', _) ]);
        (a'', [ (ax'', _); (ay'', _); (az'', _) ]);
      ],
      ring ) =
  add_in_graph
    [
      ( a,
        0.,
        0.,
        [],
        [
          x, [ Direction se ], [];
          y, [ Direction sw ], [ Internal_state (yu, [ Direction w ]) ];
          z, [ Direction n ], [ Internal_state (zp, [ Direction ne ]) ];
        ] );
      ( a,
        -0.77,
        -1.,
        [],
        [
          x, [ Direction n ], [];
          y, [ Direction e ], [ Internal_state (yu, [ Direction ne ]) ];
          z, [], [ Internal_state (zp, [ Direction sw ]) ];
        ] );
      ( a,
        0.77,
        -1.,
        [],
        [
          x, [ Direction w ], [];
          y, [ Direction n ], [ Internal_state (yu, [ Direction ne ]) ];
          z, [ Direction se ], [ Internal_state (zp, [ Direction e ]) ];
        ] );
    ]
    signature

let ring = add_link_list [ ax, ay''; ay, ax'; ay', ax'' ] ring

let _, ring =
  add_free_list
    [ az, [ Direction n ]; az', [ Direction w ]; az'', [ Direction se ] ]
    ring (* remove if you do not want the symbols -| *)

let _ = dump "syntax_ring.ladot" ring
