open OUnit2
let all_tests : OUnit2.test =
  test_list [ Test_api_data.suite ;
              Test_utility.suite ]
;;

let () =
  run_test_tt_main all_tests
;;
