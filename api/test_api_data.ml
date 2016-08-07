open OUnit2
module Api_types = ApiTypes_j

let pp_diff_string formatter (x,y) =
  Format.fprintf formatter "'%s'\n != '\n'%s" x y

(* let plot_tsv (plot : Api_types.plot) : string = *)
let test_state : Api_types.state =
  Api_types.state_of_string Resource_strings.test_message_json

let api_snapshot_dot_sample_1 test_ctxt =
  let snapshot : Api_types.snapshot =
    List.nth test_state.Api_types.snapshots 1
  in
  let expected =
    "digraph G{\n"^
    "\n"^
    "subgraph cluster1{\n"^
    "  counter1 [label = \"714 instance(s)\", shape=none];\n"^
    "  node1_0 [label = \"B(x)\", color = \"#4c4f90\", style=filled];\n"^
    "  node1_1 [label = \"A(x,c)\", color = \"#cd31a2\", style=filled];\n"^
    "  node1_0 -> node1_1 [taillabel=\"x\", headlabel=\"x\", dir=none];\n"^
    "  }\n"^
    "subgraph cluster2{\n"^
    "  counter2 [label = \"286 instance(s)\", shape=none];\n"^
    "  node2_0 [label = \"B(x)\", color = \"#4c4f90\", style=filled];\n"^
    "  }\n"^
    "subgraph cluster3{\n"^
    "  counter3 [label = \"286 instance(s)\", shape=none];\n"^
    "  node3_0 [label = \"A(x,c)\", color = \"#cd31a2\", style=filled];\n"^
    "  }\n"^
    "subgraph cluster4{\n"^
    "  counter4 [label = \"10 instance(s)\", shape=none];\n"^
    "  node4_0 [label = \"C(x1~p,x2~p)\", color = \"#603574\", style=filled];\n"^
    "  }\n"^
    "}"
  in
  let actual = Api_data.api_snapshot_dot snapshot in
  assert_equal
    ~pp_diff:pp_diff_string
    expected
    actual;;

let api_snapshot_dot_sample_2 test_ctxt =
  let snapshot : Api_types.snapshot =
    List.nth test_state.Api_types.snapshots 10
  in
  let expected =
    "digraph G{\n"^
    "\n"^
    "subgraph cluster1{\n"^
    "  counter1 [label = \"739 instance(s)\", shape=none];\n"^
    "  node1_0 [label = \"B(x)\", color = \"#4c4f90\", style=filled];\n"^
    "  node1_1 [label = \"A(x,c)\", color = \"#cd31a2\", style=filled];\n"^
    "  node1_0 -> node1_1 [taillabel=\"x\", headlabel=\"x\", dir=none];\n"^
    "  }\n"^
    "subgraph cluster2{\n"^
    "  counter2 [label = \"259 instance(s)\", shape=none];\n"^
    "  node2_0 [label = \"B(x)\", color = \"#4c4f90\", style=filled];\n"^
    "  }\n"^
    "subgraph cluster3{\n"^
    "  counter3 [label = \"258 instance(s)\", shape=none];\n"^
    "  node3_0 [label = \"A(x,c)\", color = \"#cd31a2\", style=filled];\n"^
    "  }\n"^
    "\n"^
    "\n"^
    "subgraph cluster6{\n"^
    "  counter6 [label = \"2 instance(s)\", shape=none];\n"^
    "  node6_0 [label = \"C(x1~p,x2~u)\", color = \"#603574\", style=filled];\n"^
    "  node6_1 [label = \"A(x,c)\", color = \"#cd31a2\", style=filled];\n"^
    "  node6_2 [label = \"B(x)\", color = \"#4c4f90\", style=filled];\n"^
    "  node6_0 -> node6_1 [taillabel=\"x2\", headlabel=\"c\", dir=none];\n"^
    "  node6_1 -> node6_2 [taillabel=\"x\", headlabel=\"x\", dir=none];\n"^
    "  }\n"^
    "\n"^
    "subgraph cluster8{\n"^
    "  counter8 [label = \"1 instance(s)\", shape=none];\n"^
    "  node8_0 [label = \"C(x1~p,x2~u)\", color = \"#603574\", style=filled];\n"^
    "  node8_1 [label = \"A(x,c)\", color = \"#cd31a2\", style=filled];\n"^
    "  node8_0 -> node8_1 [taillabel=\"x2\", headlabel=\"c\", dir=none];\n"^
    "  }\n"^
    "subgraph cluster9{\n"^
    "  counter9 [label = \"6 instance(s)\", shape=none];\n"^
    "  node9_0 [label = \"C(x1~p,x2~p)\", color = \"#603574\", style=filled];\n"^
    "  }\n"^
    "subgraph cluster10{\n"^
    "  counter10 [label = \"1 instance(s)\", shape=none];\n"^
    "  node10_0 [label = \"C(x1~p,x2~u)\", color = \"#603574\", style=filled];\n"^
    "  }\n"^
    "}"
  in
  let actual = Api_data.api_snapshot_dot snapshot in
  assert_equal
    ~pp_diff:pp_diff_string
    expected
    actual;;

let api_snapshot_kappa_multiple_links test_ctxt =
  let snapshot : Api_types.snapshot =
    List.nth test_state.Api_types.snapshots 11
  in
  let expected =
    ""
    ^"%init: 6.000000 C(x1,x2)\n"
    ^"%init: 2.000000 C(x1,x2!5),A(x,c!5)\n"
    ^"%init: 1.000000 C(x1,x2!3),A(x!4,c!3),B(x!4)\n"
    ^"%init: 1.000000 B(x!1),C(x1!2,x2),A(x!1,c!2)\n"
    ^"%init: 694.000000 B(x!0),A(x!0,c)\n"
    ^"%init: 304.000000 B(x)\n"
    ^"%init: 302.000000 A(x,c)"
  in
  let actual = Api_data.api_snapshot_kappa snapshot in
  assert_equal
    ~pp_diff:pp_diff_string
    expected
    actual;;

let api_snapshot_kappa_single test_ctxt =
  let snapshot : Api_types.snapshot =
    List.nth test_state.Api_types.snapshots 0
  in
  let expected =
    ""
    ^"%init: 10.000000 C(x1,x2)\n"
    ^"%init: 264.000000 A(x,c)\n"
    ^"%init: 264.000000 B(x)\n"
    ^"%init: 736.000000 B(x!0),A(x!0,c)"
  in
  let actual = Api_data.api_snapshot_kappa snapshot in
  assert_equal
    ~pp_diff:pp_diff_string
    expected actual;;

let api_plot_tsv_sample_plot test_ctxt =
  let sample_plot : Api_types.plot  =
    match test_state.Api_types.plot with
    | Some plot -> plot
    | None -> failwith "invalid state"
  in
  let sample_plot : Api_types.plot  =
    { sample_plot
      with Api_types.observables =
             [List.hd sample_plot.Api_types.observables] }
  in
  let expected = Api_data.plot_values sample_plot in
  let actual = Api_data.plot_values sample_plot in
  assert_equal
    ~pp_diff:pp_diff_string
    expected actual;;


let suite : OUnit2.test =
  "test_api_data">:::
  ["api_snapshot_dot_sample_1">:: api_snapshot_dot_sample_1
  ;"api_snapshot_dot_sample_2">:: api_snapshot_dot_sample_2
  ;"api_snapshot_kappa_single">:: api_snapshot_kappa_single
  ;"api_plot_tsv_sample_plot" >:: api_plot_tsv_sample_plot
  ;"api_snapshot_kappa_multiple_links" >:: api_snapshot_kappa_multiple_links ]
;;
