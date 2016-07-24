open OUnit2

let all_tests : OUnit2.test =
  test_list [ Test_webapp_common.suite ;
              Test_api_runtime.suite ]
;;

let () =
  run_test_tt_main all_tests
;;
