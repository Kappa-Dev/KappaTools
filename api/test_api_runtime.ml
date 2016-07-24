open OUnit2
open Lwt

let test_initalize_service_state test_ctxt =
  let info : Api_types_j.service_info Api_types_j.result =
    (Lwt_main.run (new Api_runtime.service_manager#service_info ())) in
  Api_common.result_map
    ~ok:(fun _ (info : Api_types_j.service_info) ->
	let expected = [info.Api_types_j.sessions ;
			info.Api_types_j.processes ]
	in
	let actual = [0 ; 0] in
	assert_equal expected actual)
    ~error:(fun _ _ -> assert_failure "failed to retrive service info")
    ~result:info

let test_initalize_session_state test_ctxt =
  let info : Api_types_j.session_info Api_types_j.result =
    (Lwt_main.run (new Api_runtime.session_manager#session_info ())) in
  Api_common.result_map
    ~ok:(fun _ (ok: Api_types_j.session_info) ->
	let expected = [] in
	assert_equal expected ok)
    ~error:(fun _ _ ->
	assert_failure "failed to retrive session info")
    ~result:info

let test_create_session_state test_ctxt =
  let session_manager : Api_runtime.session_manager =
    new Api_runtime.session_manager in
  let session_id : string = "25587" in
  let info : Api_types_j.session_info Api_types_j.result =
    (Lwt_main.run
       ((session_manager#session_create session_id)
	>>=
	(fun _ -> session_manager#session_info ()))) in
  Api_common.result_map
    ~ok:(fun _ (actual: Api_types_j.session_info) ->
	let expected = [session_id] in
	assert_equal expected actual)
    ~error:(fun _ _ ->
	assert_failure "failed to retrive session info")
    ~result:info

let suite : OUnit2.test =
  "test_api_data">:::
  [ "test_initalize_service_state" >:: test_initalize_service_state ;
    "test_initalize_session_state" >:: test_initalize_session_state ]
;;
