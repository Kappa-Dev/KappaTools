(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open OUnit2

let all_tests : OUnit2.test =
  test_list [ Test_webapp_common.suite ;
              Test_api_runtime.suite ]
;;

let () =
  run_test_tt_main all_tests
;;
