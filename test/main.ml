open OUnit2
open Character

(* Test Plan: We use glass-box testing to test important and necessary functions from Character.ml *)

let intro_tests =
  [
    ( "looking up questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (lookup 1 []) );
  ]

let suite = "test suite for final project" >::: List.flatten [ intro_tests ]
let _ = run_test_tt_main suite