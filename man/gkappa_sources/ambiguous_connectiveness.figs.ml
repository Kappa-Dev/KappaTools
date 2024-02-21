open Config
open Geometry

type state = Free | Bound | None

let show_binding_type = false
let space_between_proteins = 0.6

let config =
  {
    config with
    agent_width = 0.25 *. config.agent_width;
    agent_height = 0.34 *. config.agent_height;
    site_width = 0.4 *. config.site_width;
    site_height = 0.4 *. config.site_height;
    free_width = 0.6 *. config.free_width;
    free_height = 0.6 *. config.free_height;
    bound_height = 0.5 *. config.bound_height;
    rule_length = 0.4 *. config.rule_length;
    rule_margin = 0.1 *. config.rule_margin;
    show_agent_names = false;
    show_site_names = false;
    show_state_names = false;
    color_agents = true;
    color_sites = true;
    txt_font = 12;
  }

let build_rule = build_rule ~hgap:(Some 0.4)

let [ (r, [ (left, []); (right, []) ]) ], signature =
  add_in_signature
    [
      ( "$\text{\raisebox{.5ex}{\agent{\small A}{}}}$",
        [ Shape "hexagon" ],
        [
          "$\text{\site{\small l}{}{}}$", [ Direction ne ], [];
          "$\text{\site{\small r}{}{}}$", [ Direction se ], [];
        ] );
    ]
    (snd (init config))

let ( [
        (agent_a, [ (site_a, _); (site_b, _) ]);
        (agent_a', [ (site_a', _); (site_b', _) ]);
      ],
      protein ) =
  add_in_graph
    [
      r, 0., 0., [], [ left, [], []; right, [], [] ];
      ( r,
        0.8,
        0.,
        [],
        [ left, [ Direction nw ], []; right, [ Direction sw ], [] ] );
    ]
    signature

let [ (pagent_a, [ (psite_b, _) ]); (pagent_a', [ (psite_b', _) ]) ], pattern =
  add_in_graph
    [
      r, 0., 0., [], [ right, [], [] ];
      r, 0.8, 0., [], [ right, [ Direction sw ], [] ];
    ]
    signature

let axx =
  add_free_list
    [ site_a, []; site_a', [] ]
    (add_link_list [ site_b, site_b' ] protein)

let ayy =
  add_free_list
    [ site_b, []; site_b', [] ]
    (add_link_list [ site_a, site_a' ] protein)

let axxyy = add_link_list [ site_a, site_a'; site_b, site_b' ] protein
let paxx = add_link_list [ psite_b, psite_b' ] pattern
let paxxfree = add_free_list [ psite_b, []; psite_b', [] ] pattern
let la, lhs' = paxxfree
let rhs' = paxx
let _, lhs = ayy
let rhs = axxyy

let f bool =
  let rhs =
    if bool then
      cross rhs
    else
      rhs
  in
  let sigmal, sigmar, _, rule =
    build_rule ~reversible:false signature
      (fun _ -> ([], [], []), lhs)
      (fun _ -> ([], [], []), rhs)
  in
  let sigmal', sigmar', _, rule' =
    build_rule ~reversible:false signature
      ~directives:
        [
          Comment
            (if bool then
               "$1$"
             else
               "$1\{1\}$");
        ]
      (fun _ -> ([], [], []), lhs')
      (fun _ -> ([], [], []), rhs')
  in
  let dim_rule = corners lhs in
  let dim_rule' = corners lhs' in
  let delta_x =
    match dim_rule, dim_rule' with
    | None, _ | _, None -> 0.
    | Some (min, max, _, _), Some (min', max', _, _) ->
      max -. min +. min' -. max'
  in
  let square = move_remanent_above 0.6 rule rule' in
  let rule' =
    translate_graph { abscisse = delta_x *. 0.88; ordinate = 0. } rule'
  in
  let sigmau, sigmad, square = disjoint_union square rule' in
  let square =
    add_proj
      [
        ( lift_agent sigmad (lift_agent sigmal' pagent_a),
          lift_agent sigmau (lift_agent sigmal agent_a) );
        ( lift_agent sigmad (lift_agent sigmal' pagent_a'),
          lift_agent sigmau (lift_agent sigmal agent_a') );
      ]
      square
  in
  let square =
    add_proj
      [
        ( lift_agent sigmad (lift_agent sigmar' pagent_a),
          lift_agent sigmau (lift_agent sigmar agent_a) );
        ( lift_agent sigmad (lift_agent sigmar' pagent_a'),
          lift_agent sigmau (lift_agent sigmar agent_a') );
      ]
      square
  in
  square

let with_cross = f true
let no_cross = f false
let no_cross = move_remanent_right_to 1. no_cross with_cross
let _, _, both = disjoint_union no_cross with_cross
let _ = dump "amb_noamb.ladot" both
