open OUnit2
open Character

let character_tests =
  [
    ( "current game" >:: fun _ ->
      assert_equal 3 (current_game { balance = 0; currgame = 2; prevgames = 4 })
    );
  ]

let command_tests = []
let state_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten [ character_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite