open OUnit2
open Character

(* Test Plan: We use glass-box testing to test important and necessary functions
    from Character.ml. Not all functions from Character.ml are exposed in
    Character.mli. This is because some functions in Character.ml are used to
    print information to the user in the terminal, which is not necessary to
    test. *)

let menu_tests =
  [
    ( "looking up questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (lookup 1 []) );
    ( "looking up 1 questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (fun () -> lookup_one_question 1 [] "2")
    );
    ( "looking up 5 questions with no difficulty raises an error" >:: fun _ ->
      assert_raises (Failure "oops!") (fun () -> lookup_five_questions []) );
    ("max hunger is 3" >:: fun _ -> assert_equal 3 get_max_hunger);
    ( "string of inventory on an empty inventory is an empty string" >:: fun _ ->
      assert_equal "" (string_of_inventory []) );
    ( "string of inventory prints properly" >:: fun _ ->
      assert_equal "Cake, Biscuit" (string_of_inventory [ "Cake"; "Biscuit" ])
    );
  ]

let store_tests =
  [
    ( "food bank costs are stored correctly" >:: fun _ ->
      assert_equal
        [ (1, (1, "Biscuit x1")); (2, (3, "Cake x1")) ]
        food_bank_find_cost );
    ( "looking up in an empty store raises an error" >:: fun _ ->
      assert_raises (Failure "Oops!") (lookup_store 1 []) );
    ( "looking up the cost of a biscuit key finds the entry in the food bank"
    >:: fun _ ->
      assert_equal (1, "Biscuit x1") (lookup_store 1 food_bank_find_cost) );
    ( "looking up the cost of a cake key finds the entry in the food bank"
    >:: fun _ ->
      assert_equal (3, "Cake x1") (lookup_store 2 food_bank_find_cost) );
    ( "adding an item to an inventory is correctly stored in the list"
    >:: fun _ -> assert_equal [ "Biscuit" ] (add_item_to_inventory "Biscuit" [])
    );
    ( "cannot add over 10 items to a list" >:: fun _ ->
      assert_raises (ItemLimit "\n YOU ARE AT ITEM LIMIT") (fun () ->
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
      assert_raises (ItemLimit "\n YOU ARE AT ITEM LIMIT") (fun () ->
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
    ( "adding an item to something not in the inventory initially is correctly \
       stored in the list"
    >:: fun _ -> assert_equal [ "Biscuit" ] (if_not_in_inventory "Biscuit" [])
    );
    ( "cannot add over 10 items to a list" >:: fun _ ->
      assert_raises (ItemLimit "\n YOU ARE AT ITEM LIMIT") (fun () ->
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
    ( "cannot add over 10 items to a list with varying food" >:: fun _ ->
      assert_raises (ItemLimit "\n YOU ARE AT ITEM LIMIT") (fun () ->
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
    ( "enumerating an empty inventory returns an empty list" >:: fun _ ->
      assert_equal [] (enumerate_inventory [] 1) );
    ( "enumerating an inventory is printed properly" >:: fun _ ->
      assert_equal
        [ "1: Biscuit"; "2: Cake" ]
        (enumerate_inventory [ "Biscuit"; "Cake" ] 1) );
    ( "the food dictionary is properly printed" >:: fun _ ->
      assert_equal [ ("Biscuit", 1); ("Cake", 2) ] food_dict );
    ( "the value of Biscuit is 1" >:: fun _ ->
      assert_equal 1 (get_hunger_value "Biscuit" food_dict) );
    ( "the value of Cake is 2" >:: fun _ ->
      assert_equal 2 (get_hunger_value "Cake" food_dict) );
    ( "raises NoSuchItem on an empty inventory" >:: fun _ ->
      assert_raises (NoSuchItem "NOT SUPPOSED TO HAPPEN")
        (get_hunger_value "Cake" []) );
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
  ]

let suite =
  "test suite for CS 3110 Final Project"
  >::: List.flatten [ menu_tests; store_tests ]

let _ = run_test_tt_main suite