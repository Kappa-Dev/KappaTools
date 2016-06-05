open OUnit2
open Lwt
module Api_types = ApiTypes_j

(* let plot_tsv (plot : Api_types.plot) : string = *)
let test_state : Api_types.state =
  Api_types.state_of_string Resource_strings.test_message_json
let api_session_list_empty test_ctxt =
  let sm = new Api.Base.session in
  let expected = Lwt_main.run (sm#sessionList ()) in
  let actual = `Right [] in
  assert_equal expected actual;;

let suite : OUnit2.test =
"test_api_data">:::
  ["api_session_list_empty">:: api_session_list_empty ]
;;
