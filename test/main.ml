open OUnit2
(* open Character *)

(* Test Plan: *)

let character_tests =
  [ (* ( "current game" >:: fun _ ->
         assert_equal 3 (current_game { balance = 0; currgame = 2; prevgames = 4 })
       ); *) ]

let suite = "test suite for final project" >::: List.flatten [ character_tests ]
let _ = run_test_tt_main suite