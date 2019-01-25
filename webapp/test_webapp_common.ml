(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open OUnit2
open Lwt

let test_create_url_matcher test_ctxt =
  let matcher : Webapp_common.url_matcher =
    Webapp_common.create_url_matcher "/{a}b/{c}"
  in
  let test_urls : string list = ["/b/c";"/ab/c"] in
  let actual =
    List.map
      (Webapp_common.match_url [(1,matcher)])
      test_urls
  in
  let expected = [ None
		 ; Some (1,[("a","a");("c","c")]) ] in
  assert_equal expected actual;;

let test_match_root_url test_ctxt =
  let matcher : Webapp_common.url_matcher =
    Webapp_common.create_url_matcher "/"
  in
  let test_urls : string list = ["";"/";"/a"] in
  let actual =
    List.map
      (Webapp_common.match_url [(1,matcher)])
      test_urls
  in
  let expected = [ None
		 ; Some (1,[]) ; None ] in
  assert_equal expected actual;;

let suite : OUnit2.test =
  "test_api_data">:::
  ["test_create_url_matcher" >:: test_create_url_matcher ;
   "test_match_root_url" >:: test_match_root_url ]
;;
