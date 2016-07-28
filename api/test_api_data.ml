open OUnit2
module Api_types = ApiTypes_j

let pp_diff_string formatter (x,y) =
  Format.fprintf formatter "%s\n != \n%s" x y

(* let plot_tsv (plot : Api_types.plot) : string = *)
let test_state : Api_types.state =
  Api_types.state_of_string Resource_strings.test_message_json

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
  ["api_snapshot_kappa_single">:: api_snapshot_kappa_single
  ;"api_plot_tsv_sample_plot" >:: api_plot_tsv_sample_plot
  ;"api_snapshot_kappa_multiple_links" >:: api_snapshot_kappa_multiple_links ]
;;
