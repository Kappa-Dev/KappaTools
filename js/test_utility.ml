open OUnit2

let utility_split test_ctxt =
  let expected =
    List.map
      (fun s -> Utility.split s '\n')
      ["";"a";"\n";"a\n";"\na";"a\nb";"a\nb\n";"a\nb\nc"] in
  let actual =
    [("", None);
     ("a", None);
     ("", Some "");
     ("a", Some "");
     ("", Some "a");
     ("a", Some "b");
     ("a", Some "b\n");
     ("a", Some "b\nc")]
in
  assert_equal
    expected actual;;


let suite : OUnit2.test =
  "test_api_data">:::
  ["utility_split">:: utility_split ]
;;
