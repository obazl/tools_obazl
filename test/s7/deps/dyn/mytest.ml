open OUnit2

let test1 _ = assert_equal "Hello" (Hello.msg)

let suite =
"suite">:::
 ["test1">:: test1]

let () =
  run_test_tt_main suite
