open Config
open Geometry

let show_binding_type = false

let config =
  {
    config with
    site_width = 0.25 *. config.site_width;
    site_height = 0.25 *. config.site_height;
    state_height = 0.7 *. config.state_height;
    state_width = 0.7 *. config.state_width;
    free_width = 0.6 *. config.free_width;
    free_height = 0.4 *. config.free_height;
    bound_height = 0.3 *. config.bound_height;
    rule_length = 0.65 *. config.rule_length;
    rule_margin = 0.2 *. config.rule_margin;
    show_agent_names = false;
    show_site_names = false;
    show_state_names = false;
    txt_font = 10;
  }

(*let ne = of_degree 30.
  let se = of_degree 150.*)

let build_rule ?mul =
  let h =
    match mul with
    | None -> 0.8
    | Some f -> 0.8 *. f
  in
  build_rule ~hgap:(Some h)

let ( [ (a, [ (a_x, []) ]); (b, [ (b_x, []); (b_y, [ b_y_u; b_y_p ]) ]) ],
      signature ) =
  add_in_signature
    [
      ( "$\text{\agent{A}{}}$",
        [
          Shape "circle";
          Width (0.13 *. config.agent_width);
          Height (0.16 *. config.agent_width);
        ],
        [ "$\text{\site{l}{}{}}$", [ Direction e ], [] ] );
      ( "$\text{\agent{B}{}}$",
        [
          Shape "rectangle";
          Height (0.24 *. config.agent_height);
          Width (0.15 *. config.agent_width);
        ],
        [
          "$\text{\site{r}{}{}}$", [ Direction w ], [];
          ( "$\text{\site{l}{}{}}$",
            [ Direction e ],
            [ "u", [ Direction ne ]; "p", [ Direction ne ] ] );
        ] );
    ]
    (snd (init config))

let [ (agent_a, [ (site_a, _) ]); (agent_b, [ (site_b_x, _) ]) ], couple =
  add_in_graph
    [ a, 0., 0., [], [ a_x, [], [] ]; b, 0.6, 0., [], [ b_x, [], [] ] ]
    signature

let add_bond s1 s2 graph =
  let graph = add_link_list [ s1, s2 ] graph in
  ([], [], []), graph

let add_free s1 s2 graph =
  let _, graph = add_free_list [ s1, []; s2, [] ] graph in
  ([], [], []), graph

let bind ?directives ?file g s1 s2 =
  build_rule ?directives ?file g (add_free s1 s2) (add_bond s1 s2)

let _, _, _, bind_g =
  bind ~directives:[ Comment "$k$" ] ~file:"sect1-bind.ladot" couple site_a
    site_b_x

let add_state site state graph =
  let _, graph =
    add_internal_state ~directives:[ Direction se ] site state graph
  in
  ([], [], []), graph

let activate ?directives ?file g s su sp =
  build_rule ?directives ?file g (add_state s su) (add_state s sp)

let _ =
  let site_b_y, g = add_site agent_b b_y couple in
  let _, g = add_bond site_a site_b_x g in
  activate ~directives:[ Comment "$kp$" ] ~file:"sect1-phos.ladot" g site_b_y
    b_y_u b_y_p

let refined_domain state =
  let s, g = add_site agent_b b_y couple in
  snd (add_state s state g)

let _, _, _, bind_u =
  bind ~directives:[ Comment "$k$" ] ~file:"sect1-bind-ref-u.ladot"
    (refined_domain b_y_u) site_a site_b_x

let _, _, _, bind_p =
  bind ~directives:[ Comment "$k$" ] ~file:"sect1-bind-ref-p.ladot"
    (refined_domain b_y_p) site_a site_b_x

let ag, fictitious = add_empty_node 0. 0. signature

let sigmafu, _, bind_u =
  let g = move_remanent_right_to 0.2 bind_u fictitious in
  disjoint_union fictitious g

let sigmafu = lift_agent sigmafu

let sigmafp, _, bind_p =
  let g = move_remanent_right_to 0.2 bind_p fictitious in
  disjoint_union fictitious g

let sigmafp = lift_agent sigmafp

let sigmad, sigmau, bindup =
  let bind_u = move_remanent_above 0.5 bind_u bind_p in
  disjoint_union bind_u bind_p

let sigmad = lift_agent sigmad
let sigmau = lift_agent sigmau

let _, sigmaf, bind_g =
  let g = move_remanent_right_to 0.2 fictitious bind_g in
  disjoint_union bind_g g

let sigmaf = lift_agent sigmaf

let sigmal, sigmar, graph =
  let g = move_remanent_right_to 1.5 bindup bind_g in
  disjoint_union (translate_graph { abscisse = 0.; ordinate = 0.45 } bind_g) g

let sigmal = lift_agent sigmal
let sigmar = lift_agent sigmar
let fl = sigmal (sigmaf ag)
let fu = sigmar (sigmafu (sigmau ag))
let fp = sigmar (sigmafp (sigmad ag))
let sigmau x = sigmar (sigmaf (sigmau x))
let graph = add_proj ~color:"black" [ fl, fp; fl, fu ] graph
let _ = dump "ref.ladot" graph

let [ (a, [ (a_n, []); (a_e, [ u; p ]) ]) ], signature =
  add_in_signature
    [
      ( "$\text{\agent{A}{}}$",
        [
          Shape "circle";
          Width (0.13 *. config.agent_width);
          Height (0.16 *. config.agent_width);
        ],
        [
          "$\text{\site{n}{}{}}$", [ Direction n ], [];
          ( "$\text{\site{n}{}{}}$",
            [ Direction e ],
            [ "u", [ Direction ne ]; "p", [ Direction ne ] ] );
        ] );
    ]
    (snd (init config))

let ( [
        (a, [ (sn, _); (se_, _) ]);
        (a', [ (sn', _); (se', _) ]);
        (a'', [ (sn'', _); (se'', _) ]);
        (a''', [ (sn''', _); (se''', _) ]);
      ],
      g ) =
  add_in_graph
    [
      a, 0., 0., [], [ a_n, [], []; a_e, [], [ Internal_state (u, []) ] ];
      a, 0.6, 0., [], [ a_n, [], []; a_e, [], [ Internal_state (u, []) ] ];
      ( a,
        1.2,
        0.,
        [],
        [ a_n, [], [ Free_site [] ]; a_e, [], [ Internal_state (u, []) ] ] );
      a, 1.8, 0., [], [ a_n, [], [ Free_site [] ]; a_e, [], [] ];
    ]
    signature

let left g =
  let _, g = add_free_list [ sn, []; sn', [] ] g in
  let _, g = add_internal_state se''' u g in
  ([], [], []), g

let right g =
  let g = add_link_list [ sn, sn' ] g in
  let _, g = add_internal_state se''' p g in
  ([], [], []), g

let _ = build_rule ~file:"auto.ladot" ~directives:[ Comment "$k$" ] g left right

let [ (a, [ (a_x, []); (a_y, []) ]) ], signature =
  add_in_signature
    [
      ( "$\text{\agent{A}{}}$",
        [
          Shape "rectangle";
          Width (0.10 *. config.agent_width);
          Height (0.16 *. config.agent_width);
        ],
        [
          "$\text{\site{a}{}{}}$", [ Direction ne ], [];
          "$\text{\site{b}{}{}}$", [ Direction se ], [];
        ] );
    ]
    (snd (init config))

let ( [
        (agent_a, [ (site_a, _); (site_b, _) ]);
        (agent_a', [ (site_a', _); (site_b', _) ]);
      ],
      dimer ) =
  add_in_graph
    [
      a, 0., 0., [], [ a_x, [], []; a_y, [], [] ];
      a, 0.6, 0., [], [ a_x, [ Direction nw ], []; a_y, [ Direction sw ], [] ];
    ]
    signature

let build_rule b1 b2 g =
  let sx, sy, s =
    if b1 then
      site_a, site_b, "t"
    else
      site_b, site_a, "f"
  in
  let sx', sy', s' =
    if b2 then
      site_a', site_b', "t"
    else
      site_b', site_a', "f"
  in
  let n =
    if b1 && b2 then
      "1"
    else if b1 then
      "3"
    else if b2 then
      "4"
    else
      "2"
  in
  let file = "sym" ^ s ^ s' ^ ".ladot" in
  let _, domain = add_free_list [ sy, []; sy', [] ] g in
  let free g = ([], [], []), snd (add_free_list [ sx, []; sx', [] ] g) in
  let bind g = ([], [], []), add_link_list [ sx, sx' ] g in
  build_rule ~file ~directives:[ Comment ("$k_" ^ n ^ "$") ] domain free bind

let _ = build_rule true true dimer
let _ = build_rule false false dimer
let _ = build_rule true false dimer
