open OUnit2
module Api_types = ApiTypes_j

(* let plot_tsv (plot : Api_types.plot) : string = *)
let test_state : Api_types.state =
  Api_types.state_of_string Resource_strings.test_message_json
let api_snapshot_kappa_single test_ctxt =
  let snapshot : Api_types.snapshot =
    List.nth test_state.Api_types.snapshots 0
  in
  let expected = "\n"
                ^"%init: 736.000000 B(x!1),A(x!1,c)\n"
                ^"%init: 264.000000 B(x)\n"
                ^"%init: 264.000000 A(x,c)\n"
                ^"%init: 10.000000 C(x1,x2)"
  in
  let actual = Api_data.api_snapshot_kappa snapshot in
  assert_equal expected actual;;

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
  let expected = Api_data.plot_tsv sample_plot in
  let actual = "time,'AB','Cuu','Cpu','Cpp',0,'snapshots'\n"
              ^"90.,721.,0.,0.,10.,0.,14."
  in
  (* let () = print_string expected in *)
  assert_equal expected actual;;

let suite : OUnit2.test =
"test_api_data">:::
  ["api_snapshot_kappa_single">:: api_snapshot_kappa_single
  ;"api_plot_tsv_sample_plot" >:: api_plot_tsv_sample_plot ]
;;
