open OUnit2
open Character

(* Test Plan: We use glass-box testing to test important and necessary functions
    from Character.ml. However, not all functions present in Character.mli are
    tested in this file. This is because many of our helper functions entail
    printing values to the terminal, and the correctness of these helper
    functions can be determined from playing the game and ensure output is as
    expected (a glass-box principal). Thus, the functions tested here are
    related to back-end transactions and maintenance of the pet's state,
    which are tested by OUnit. Similarly, the terminal-based functions are
    manually tested. *)

let trivia_tests =
  [
    ( "looking up questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup 1 []) );
    ( "looking up 1 questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question1 1 [] "1")
    );
    ( "looking up question 1 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question2 1 [] "2")
    );
    ( "looking up question 2 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question3 1 [] "3")
    );
    ( "looking up question 3 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question4 1 [] "4")
    );
    ( "looking up question 4 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question5 1 [] "5")
    );
    ( "looking up question 4 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question5 1 [] "1")
    );
    ( "looking up question 3 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question4 1 [] "2")
    );
    ( "looking up question 2 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question3 1 [] "4")
    );
    ( "looking up question 1 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question2 1 [] "3")
    );
    ( "looking up 1 questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question1 1 [] "5")
    );
    ( "looking up question 5 with no difficulty raises an error" >:: fun _ ->
      assert_raises (Invalid_argument "Random.int") (fun () ->
          lookup_five_questions []) );
  ]

let store_tests =
  [
    ("max hunger is 3" >:: fun _ -> assert_equal 3 get_max_hunger);
    ( "string of inventory on an empty inventory is an empty string" >:: fun _ ->
      assert_equal "" (string_of_inventory []) );
    ( "string of inventory prints properly" >:: fun _ ->
      assert_equal "Cake, Biscuit" (string_of_inventory [ "Cake"; "Biscuit" ])
    );
    ( "food bank costs are stored correctly" >:: fun _ ->
      assert_equal
        [ (1, (1, "Biscuit x1")); (2, (3, "Cake x1")) ]
        food_bank_find_cost );
    ( "looking up in an empty store raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_store 1 []) );
    ( "looking up the cost of a biscuit key finds the entry in the food bank"
    >:: fun _ ->
      assert_equal (1, "Biscuit x1") (lookup_store 1 food_bank_find_cost) );
    ( "looking up the cost of a cake key finds the entry in the food bank"
    >:: fun _ ->
      assert_equal (3, "Cake x1") (lookup_store 2 food_bank_find_cost) );
    ( "looking up the cost of an invalid key raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () ->
          lookup_store 3 food_bank_find_cost) );
    ( "add test for add_item_to_inventory with a expanded inventory \
       sequentially works properly"
    >:: fun _ ->
      assert_equal
        [ "Biscuit x2"; "Cake x2" ]
        (add_item_to_inventory "Cake x1"
           (add_item_to_inventory "Biscuit x1" [ "Biscuit x1"; "Cake x1" ])) );
    ( "add test for add_item_to_inventory with a expanded inventory ,more \
       times, sequentially works properly"
    >:: fun _ ->
      assert_equal
        [ "Biscuit x2"; "Cake x3" ]
        (add_item_to_inventory "Cake x1"
           (add_item_to_inventory "Cake x1"
              (add_item_to_inventory "Biscuit x1" [ "Biscuit x1"; "Cake x1" ])))
    );
    ( "cannot add over 10 items to a list" >:: fun _ ->
      assert_raises (Failure "int_of_string") (fun () ->
          add_item_to_inventory "Biscuit"
            [
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
            ]) );
    ( "cannot add over 10 items to a list with varying food" >:: fun _ ->
      assert_raises (Failure "int_of_string") (fun () ->
          add_item_to_inventory "Biscuit"
            [
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
            ]) );
    ( "add test for add_item_to_inventory with single entry" >:: fun _ ->
      assert_equal [ "Cake x2" ] (if_not_in_inventory "Cake x1" [ "Cake x1" ])
    );
    ( "add test for add_item_to_inventory with a expanded inventory" >:: fun _ ->
      assert_equal
        [ "Biscuit x2"; "Cake x1" ]
        (if_not_in_inventory "Biscuit x1" [ "Biscuit x1"; "Cake x1" ]) );
    ( "adding an item to something not in the inventory initially is correctly \
       stored in the list"
    >:: fun _ -> assert_equal [ "Biscuit" ] (if_not_in_inventory "Biscuit" [])
    );
    ( "cannot add over 10 items to a list" >:: fun _ ->
      assert_raises (Failure "int_of_string") (fun () ->
          if_not_in_inventory "Biscuit"
            [
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
            ]) );
    ( "cannot add over 10 items of two types to a list" >:: fun _ ->
      assert_raises (Failure "int_of_string") (fun () ->
          if_not_in_inventory "Biscuit"
            [
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Biscuit";
              "Cake";
              "Cake";
              "Cake";
              "Cake";
              "Cake";
              "Cake";
              "Cake";
              "Cake";
              "Cake";
            ]) );
    ( "cannot add over 10 items to a list with varying food" >:: fun _ ->
      assert_raises (Failure "int_of_string") (fun () ->
          if_not_in_inventory "Biscuit"
            [
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
              "Biscuit";
              "Cake";
            ]) );
    ( "add test for if_not_in_inventory" >:: fun _ ->
      assert_equal [ "Biscuit x2" ]
        (if_not_in_inventory "Biscuit x1" [ "Biscuit x1" ]) );
    ( "add test for if_not_in_inventory with a expanded inventory" >:: fun _ ->
      assert_equal
        [ "Biscuit x1"; "Cake x2" ]
        (if_not_in_inventory "Cake x1" [ "Biscuit x1"; "Cake x1" ]) );
    ( "add test for if_not_in_inventory with a expanded inventory sequentially \
       works properly"
    >:: fun _ ->
      assert_equal
        [ "Biscuit x2"; "Cake x2" ]
        (if_not_in_inventory "Biscuit x1"
           (if_not_in_inventory "Cake x1" [ "Biscuit x1"; "Cake x1" ])) );
    ( "add test for add_item_to_inventory with a expanded inventory ,more \
       times, sequentially works properly"
    >:: fun _ ->
      assert_equal
        [ "Biscuit x2"; "Cake x3" ]
        (if_not_in_inventory "Cake x1"
           (if_not_in_inventory "Cake x1"
              (if_not_in_inventory "Biscuit x1" [ "Biscuit x1"; "Cake x1" ])))
    );
    ( "enumerating an empty inventory returns an empty list" >:: fun _ ->
      assert_equal [] (enumerate_inventory [] 1) );
    ( "enumerating an inventory is printed properly" >:: fun _ ->
      assert_equal
        [ "1: Biscuit"; "2: Cake" ]
        (enumerate_inventory [ "Biscuit"; "Cake" ] 1) );
    ( "an extraneous item is still enumerated for testing purposes" >:: fun _ ->
      assert_equal
        [ "1: Biscuit"; "2: Cake"; "3: Extra" ]
        (enumerate_inventory [ "Biscuit"; "Cake"; "Extra" ] 1) );
    ( "the food dictionary is properly printed" >:: fun _ ->
      assert_equal [ ("Biscuit", 1); ("Cake", 2) ] food_dict );
    ( "the value of Biscuit is 1" >:: fun _ ->
      assert_equal 1 (get_hunger_value "Biscuit" food_dict) );
    ( "the value of Cake is 2" >:: fun _ ->
      assert_equal 2 (get_hunger_value "Cake" food_dict) );
    ( "raises NoSuchItem on an empty inventory" >:: fun _ ->
      assert_raises (NoSuchItem "NOT SUPPOSED TO HAPPEN") (fun () ->
          get_hunger_value "Cake" []) );
    ( "raises NoSuchItem on trying to find a different food in an inventory"
    >:: fun _ ->
      assert_raises (NoSuchItem "NOT SUPPOSED TO HAPPEN") (fun () ->
          get_hunger_value "Strawberry" [ ("Cake", 1); ("Biscuit", 2) ]) );
    ( "raises NoSuchItem on trying to find an item in an empty inventory"
    >:: fun _ ->
      assert_raises (NoSuchItem "There is no item at this index!") (fun () ->
          get_food_in_inventory 0 [] 0) );
    ( "returns the food item at associated index properly" >:: fun _ ->
      assert_equal "Cake" (get_food_in_inventory 0 [ "Cake" ] 0) );
    ( "returns the food item at associated index properly and tests acc \
       increment"
    >:: fun _ ->
      assert_equal "Biscuit" (get_food_in_inventory 1 [ "Cake"; "Biscuit" ] 0)
    );
    ( "depleting food from empty inventory returns empty list" >:: fun _ ->
      assert_equal [] (deplete_food "Cake" []) );
    ( "depleting food from an inventory correctly decrements" >:: fun _ ->
      assert_equal
        [ "Cake, x1"; "Biscuit, x1" ]
        (deplete_food "Biscuit" [ "Cake, x1"; "Biscuit, x2" ]) );
    ( "depleting food from a single value of food then removes it from the list"
    >:: fun _ ->
      assert_equal [ "Biscuit, x2" ]
        (deplete_food "Cake" [ "Cake, x1"; "Biscuit, x2" ]) );
    ( "depleting food sequentially returns the correct list" >:: fun _ ->
      assert_equal [ "Biscuit, x1" ]
        (deplete_food "Biscuit"
           (deplete_food "Cake" [ "Cake, x1"; "Biscuit, x2" ])) );
    ( "depleting an arbitrary food item returns the same inventory " >:: fun _ ->
      assert_equal
        [ "Cake, x1"; "Biscuit, x2" ]
        (deplete_food "Cracker" [ "Cake, x1"; "Biscuit, x2" ]) );
  ]

let suite =
  "test suite for CS 3110 Final Project"
  >::: List.flatten [ trivia_tests; store_tests ]

let _ = run_test_tt_main suite