open Yojson.Basic.Util

type t = {
  balance : int; (* current balance *)
  hunger : int; (* current hunger level *)
  name : string;
  inventory : string list;
  level : int * int;
}

let data_dir_prefix = "data" ^ Filename.dir_sep

let trivia_questions_json =
  Yojson.Basic.from_file (data_dir_prefix ^ "trivia_questions.json")

let maze_encounters_json =
  Yojson.Basic.from_file (data_dir_prefix ^ "maze_encounters.json")

(* makes a new pet with the given name *)
let make_pet pet_name =
  { balance = 0; hunger = 3; name = pet_name; inventory = []; level = (0, 0) }

let parse_questions j =
  let question =
    ( j |> member "id" |> to_int,
      ( j |> member "ans" |> to_string,
        j |> member "ans_desc" |> to_string,
        j |> member "question" |> to_string ) )
  in
  question

let get_questions_from_json json =
  let easy_questions =
    json |> member "easy_questions" |> to_list |> List.map parse_questions
  in
  let medium_questions =
    json |> member "medium_questions" |> to_list |> List.map parse_questions
  in
  let hard_questions =
    json |> member "hard_questions" |> to_list |> List.map parse_questions
  in

  (easy_questions, medium_questions, hard_questions)

let get_max_hunger =
  let p = make_pet "" in
  p.hunger

let string_of_inventory inv = String.concat ", " inv

(* prints all of the characteristics of the pet *)
let print_stats t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_endline
    (new_tt.name ^ "'s Stats: Balance = $" ^ string_of_int t.balance
   ^ "; Hunger = " ^ string_of_int t.hunger ^ "/"
    ^ string_of_int get_max_hunger
    ^ " Inventory = ["
    ^ string_of_inventory t.inventory
    ^ "]")

(* the first message the user sees when they open the game *)

let rec lookup k difficulty =
  match difficulty with
  | [] -> failwith "Oops!"
  | (k', v) :: t -> if k = k' then v else lookup k t

let lookup_one_question1 rand difficulty question_num =
  let v = lookup rand difficulty in
  match v with
  | h1, h2, h3 ->
      print_endline ("Question " ^ question_num ^ ":");
      print_endline h3;
      let x = read_int () in
      if string_of_int x = h1 then
        let () = print_endline "CORRECT!" in
        1
      else
        let () =
          print_endline ("INCORRECT!" ^ " The correct answer was " ^ h2 ^ ".")
        in
        0
(* the death message *)

exception GameOver of string

let lookup_one_question2 rand difficulty question_num =
  let v = lookup rand difficulty in
  match v with
  | h1, h2, h3 ->
      print_endline ("Question " ^ question_num ^ ":");
      print_endline h3;
      let x = read_int () in
      if string_of_int x = h1 then
        let () = print_endline "CORRECT!" in
        1
      else
        let () =
          print_endline ("INCORRECT!" ^ " The correct answer was " ^ h2 ^ ".")
        in
        0

let welcome_message () =
  print_endline
    ("Hello Player! Welcome to Tamagotchi Simulator. In this game you will"
   ^ "\n"
   ^ "be taking care of your new pet! The goal of this game is to make as"
   ^ "\n"
   ^ "much money through minigames so you can buy food and new perks for your \
      pet! " ^ "\n" ^ "\n"
   ^ "Let's start by naming your pet. What would you like to name your pet?");
  let x = read_line () in

  make_pet x

let lookup_one_question3 rand difficulty question_num =
  let v = lookup rand difficulty in
  match v with
  | h1, h2, h3 ->
      print_endline ("Question " ^ question_num ^ ":");
      print_endline h3;
      let x = read_int () in
      if string_of_int x = h1 then
        let () = print_endline "CORRECT!" in
        1
      else
        let () =
          print_endline ("INCORRECT!" ^ " The correct answer was " ^ h2 ^ ".")
        in
        0

let death_exn t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  GameOver
    ("=====================================================================\n\
     \       \n\
     \ GAME OVER: " ^ new_tt.name
   ^ " has unfortunately died from starvation :( \n\
     \ \n\
     \ =====================================================================")

let rec lookup k difficulty =
  match difficulty with
  | [] -> failwith "Oops!"
  | (k', v) :: t -> if k = k' then v else lookup k t

let lookup_one_question4 rand difficulty question_num =
  let v = lookup rand difficulty in
  match v with
  | h1, h2, h3 ->
      print_endline ("Question " ^ question_num ^ ":");
      print_endline h3;
      let x = read_int () in
      if string_of_int x = h1 then
        let () = print_endline "CORRECT!" in
        1
      else
        let () =
          print_endline ("INCORRECT!" ^ " The correct answer was " ^ h2 ^ ".")
        in
        0

let lookup_one_question5 rand difficulty question_num =
  let v = lookup rand difficulty in
  match v with
  | h1, h2, h3 ->
      print_endline ("Question " ^ question_num ^ ":");
      print_endline h3;
      let x = read_int () in
      if string_of_int x = h1 then
        let () = print_endline "CORRECT!" in
        1
      else
        let () =
          print_endline ("INCORRECT!" ^ " The correct answer was " ^ h2 ^ ".")
        in
        0

let rec five_random_numbers difficulty acc =
  let x = Random.int (List.length difficulty) in
  if List.length acc = 5 then acc
  else if List.mem x acc = false then five_random_numbers difficulty (x :: acc)
  else five_random_numbers difficulty acc

let lookup_five_questions difficulty =
  let int_lst = five_random_numbers difficulty [] in
  match int_lst with
  | [] -> failwith "oops!"
  | [ h1; h2; h3; h4; h5 ] ->
      let amt = 0 in
      let amt1 = lookup_one_question1 h1 difficulty "1" + amt in
      let amt2 = lookup_one_question2 h2 difficulty "2" + amt1 in
      let amt3 = lookup_one_question3 h3 difficulty "3" + amt2 in
      let amt4 = lookup_one_question4 h4 difficulty "4" + amt3 in
      let amt5 = lookup_one_question5 h5 difficulty "5" + amt4 in
      print_endline ("You got " ^ string_of_int amt5 ^ "/5 questions right!");
      let easy, medium, _ = get_questions_from_json trivia_questions_json in
      let word =
        if difficulty = easy then amt5
        else if difficulty = medium then amt5 * 2
        else amt5 * 3
      in
      print_endline ("Congrats! You earned $" ^ string_of_int word ^ "!");
      amt5
  | _ -> failwith "oops!"

let rec choose_difficulty t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_endline
    "Choose Difficulty:\n\
     1:Easy (1x Multiplier)\n\
     2:Medium (2x Multiplier)\n\
     3:Hard (3x Multiplier)";
  let x = read_int () in
  let easy, medium, hard = get_questions_from_json trivia_questions_json in
  match x with
  | 1 ->
      let bonus = lookup_five_questions easy in
      {
        balance = t.balance + bonus;
        hunger = t.hunger - 1;
        name = t.name;
        inventory = t.inventory;
        level =
          (match t.level with
          | f, s -> if s < 900 then (f, s + 100) else (f + 1, s mod 1000));
      }
  | 2 ->
      let bonus = lookup_five_questions medium * 2 in
      {
        balance = new_tt.balance + bonus;
        hunger = t.hunger - 1;
        name = t.name;
        inventory = t.inventory;
        level =
          (match t.level with
          | f, s -> if s < 900 then (f, s + 100) else (f + 1, s mod 1000));
      }
  | 3 ->
      let bonus = lookup_five_questions hard * 3 in
      {
        balance = t.balance + bonus;
        hunger = t.hunger - 1;
        name = t.name;
        inventory = t.inventory;
        level =
          (match t.level with
          | f, s -> if s < 900 then (f, s + 100) else (f + 1, s mod 1000));
      }
  | _ ->
      choose_difficulty
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }

(* Generate a random number between 1 and 100 *)

(* Prompt the user to guess a number *)
let rec guess_number t secret =
  print_endline "Guess a number between 1 and 100:";
  try
    let guess = read_int () in
    if guess = secret then
      let _ =
        print_endline
          "\nCONGRATS! You've guessed the correct number! Your reward is $1!"
      in
      {
        balance = t.balance + 1;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
      }
    else if guess < secret then (
      print_endline "Your guess is too low. Try again.";
      guess_number
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
        }
        secret)
    else (
      print_endline "Your guess is too high. Try again.";
      guess_number
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
        }
        secret)
  with _ ->
    guess_number
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
      }
      secret

let trivia_minigame t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_endline "\n";
  print_endline ("Hey " ^ new_tt.name ^ "'s Owner! Welcome to Trivia!");
  print_endline
    "You get money based on how many of the five questions you get correct! \
     Good luck!";
  choose_difficulty
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }

let parse_encounters encounter =
  ( encounter |> member "prompt" |> to_string,
    encounter |> member "ans" |> to_string,
    encounter |> member "right_ans" |> to_string,
    encounter |> member "wrong_ans" |> to_string )

let generate_encounters json =
  let all_encounters =
    json |> member "encounters" |> to_list |> List.map parse_encounters
  in
  let arr = Array.of_list all_encounters in
  let rec generator encounter_array i acc =
    if i = 0 then acc
    else
      let i = Random.int (List.length all_encounters) in
      let e = arr.(i) in
      if e = ("rcs", "rcs", "rcs", "rcs") then generator encounter_array i acc
      else (
        arr.(i) <- ("rcs", "rcs", "rcs", "rcs");
        generator encounter_array (i - 1) (e :: acc))
  in
  generator arr 5 []

let rec iter_encounters encounters t =
  match encounters with
  | [] ->
      print_endline
        "Congrats, you've reached the end of the maze and earned $10!";
      {
        balance = t.balance + 10;
        hunger = t.hunger - 1;
        name = t.name;
        inventory = t.inventory;
        level =
          (match t.level with
          | f, s -> if s < 900 then (f, s + 100) else (f + 1, s mod 1000));
      }
  | h :: tail -> (
      match h with
      | prompt, ans, right_ans, wrong_ans -> (
          print_endline ("\n" ^ prompt);
          print_endline "Type 0 to go left or 1 to go right.";
          let x = read_int () in
          let ans = int_of_string ans in
          match x with
          | 0 ->
              if ans = 0 then (
                print_endline ("\n" ^ right_ans);
                iter_encounters tail t)
              else (
                print_endline ("\n" ^ wrong_ans);
                print_endline
                  "\n\
                   Unfortunately you didn't find the prize. Better luck next \
                   time!";
                {
                  balance = t.balance;
                  hunger = t.hunger - 1;
                  name = t.name;
                  inventory = t.inventory;
                  level =
                    (match t.level with
                    | f, s ->
                        if s < 900 then (f, s + 100) else (f + 1, s mod 1000));
                })
          | 1 ->
              if ans = 1 then (
                print_endline ("\n" ^ right_ans);
                iter_encounters tail t)
              else (
                print_endline ("\n" ^ wrong_ans);
                print_endline
                  "\n\
                  \ Unfortunately you didn't find the prize. Better luck next \
                   time!";
                {
                  balance = t.balance;
                  hunger = t.hunger - 1;
                  name = t.name;
                  inventory = t.inventory;
                  level =
                    (match t.level with
                    | f, s ->
                        if s < 900 then (f, s + 100) else (f + 1, s mod 1000));
                })
          | _ -> failwith ""))

let start_maze t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  let encounters = generate_encounters maze_encounters_json in
  iter_encounters encounters
    {
      balance = new_tt.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }

let maze_minigame t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_endline "\n";
  print_endline ("Hey " ^ new_tt.name ^ "'s Owner! Welcome to Maze!");
  print_endline "Find the prize in the maze for some money. Good luck!\n";
  start_maze
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }

let rec choose_minigame t =
  print_endline "\n";
  print_endline
    "Welcome to the minigame menu!\n\
     Here are your minigame options:\n\
     0: Main Menu\n\
     1: Trivia ($ - $$$)\n\
     2: Maze ($)\n\
     3: Number Guesser ($) \n\
     Please choose an option!";

  try
    let x = read_int () in
    match x with
    | 0 -> t
    | 1 ->
        trivia_minigame
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
            level = t.level;
          }
    | 2 ->
        maze_minigame
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
            level = t.level;
          }
    | 3 ->
        let secret_number = Random.int 100 + 1 in
        guess_number
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
          }
          secret_number
    | _ ->
        choose_minigame
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
            level = t.level;
          }
  with _ ->
    choose_minigame
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }

(* |||||||||||||||||||||||||||||STORE|||||||||||||||||||||||||||||||||||||||||*)
let food_bank_find_cost = [ (1, (1, "Biscuit x1")); (2, (3, "Cake x1")) ]

let lookup_store k bank =
  match bank with
  | [] -> failwith "Oops!"
  | (k', v) :: t -> if k = k' then v else lookup k t

exception ItemLimit of string

let rec add_item_to_inventory (i : string) (lst : string list) : string list =
  match lst with
  | [] -> []
  | h :: t ->
      if
        String.sub h 0 (String.length i - 3)
        = String.sub i 0 (String.length i - 3)
      then
        let get_new_amt =
          string_of_int
            (int_of_string (Char.escaped (String.get h (String.length h - 1)))
            + 1)
        in
        if get_new_amt = "10" then raise (ItemLimit "\n YOU ARE AT ITEM LIMIT")
        else
          let new_word = String.sub h 0 (String.length h - 1) ^ get_new_amt in
          new_word :: add_item_to_inventory i t
      else h :: add_item_to_inventory i t

let if_not_in_inventory i lst =
  if
    lst
    = try add_item_to_inventory i lst with ItemLimit s -> raise (ItemLimit s)
  then i :: lst
  else add_item_to_inventory i lst

let rec food_item t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_stats
    {
      balance = new_tt.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    };
  print_endline
    "Welcome to the Grocery Store!\n\
     Here are your food options:\n\
     1: Biscuit $1 \n\
     2: Cake $3 (2 hunger bars) \n\
     0: Main Menu \n\
     Please choose an option!";
  let new_ttt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  let x = read_int () in
  match x with
  | 0 ->
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }
  | 1 ->
      let y = lookup_store 1 food_bank_find_cost in
      let new_bal = ref (t.balance - fst y) in
      let new_inv =
        try if_not_in_inventory (snd y) t.inventory
        with ItemLimit s ->
          new_bal := t.balance;
          print_endline s;
          t.inventory
      in
      if !new_bal < 0 then
        let _ = print_endline "CANNOT AFFORD" in
        food_item
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
            level = t.level;
          }
      else
        let new_t =
          {
            balance = !new_bal;
            hunger = new_ttt.hunger;
            name = t.name;
            inventory = new_inv;
            level = t.level;
          }
        in
        new_t
  | 2 ->
      let y = lookup_store 2 food_bank_find_cost in
      let new_bal = ref (t.balance - fst y) in
      let new_inv =
        try if_not_in_inventory (snd y) t.inventory
        with ItemLimit s ->
          new_bal := t.balance;
          print_endline s;
          t.inventory
      in
      if !new_bal < 0 then
        let _ = print_endline "CANNOT AFFORD" in
        food_item
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
            level = t.level;
          }
      else
        let new_t =
          {
            balance = !new_bal;
            hunger = t.hunger;
            name = t.name;
            inventory = new_inv;
            level = t.level;
          }
        in
        new_t
  | _ ->
      food_item
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }

let rec enumerate_inventory (inv : string list) (acc : int) =
  match inv with
  | [] -> []
  | h :: t -> (string_of_int acc ^ ": " ^ h) :: enumerate_inventory t (acc + 1)

exception NoSuchItem of string

let food_dict = [ ("Biscuit", 1); ("Cake", 2) ]

(* gets the amt of food that is supposed to replenish *)
let rec get_hunger_value item lst =
  match lst with
  | [] -> raise (NoSuchItem "NOT SUPPOSED TO HAPPEN")
  | (a, b) :: t -> if item = a then b else get_hunger_value item t

(* gets food at the index the user put in *)
let rec get_food_in_inventory idx (inv : string list) acc =
  match inv with
  | [] -> raise (NoSuchItem "There is no item at this index!")
  | h :: t -> if idx = acc then h else get_food_in_inventory idx t (acc + 1)

let rec deplete_food item inventory =
  match inventory with
  | [] -> []
  | h :: t ->
      if item = String.sub h 0 (String.length item) then
        let number =
          string_of_int
            (int_of_string (Char.escaped (String.get h (String.length h - 1)))
            - 1)
        in
        if number = "0" then deplete_food item t
        else
          let new_word = String.sub h 0 (String.length h - 1) ^ number in
          new_word :: deplete_food item t
      else h :: deplete_food item t

let refill_hunger amt t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  let new_hunger = new_tt.hunger + amt in
  if new_hunger >= get_max_hunger then get_max_hunger else new_hunger

let feed_item idx t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  let item = get_food_in_inventory idx new_tt.inventory 1 in
  let refill_amt =
    get_hunger_value (String.sub item 0 (String.length item - 3)) food_dict
  in
  let new_hunger = refill_hunger refill_amt t in
  let new_inv =
    deplete_food (String.sub item 0 (String.length item - 3)) t.inventory
  in

  {
    balance = t.balance;
    hunger = new_hunger;
    name = t.name;
    inventory = new_inv;
    level = t.level;
  }

let rec home_item t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_endline
    ("Welcome to the Dining Room! Here you can feed " ^ new_tt.name
   ^ "!\nHere is your inventory: ["
    ^ string_of_inventory t.inventory
    ^ "] \n"
    ^ String.concat "\n" (enumerate_inventory t.inventory 1)
    ^ "\n0: Main Menu \nPlease choose an option!");
  let x = read_int () in
  match x with
  | 0 ->
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }
  | i -> (
      try
        let eat =
          feed_item i
            {
              balance = t.balance;
              hunger = t.hunger;
              name = t.name;
              inventory = t.inventory;
              level = t.level;
            }
        in
        print_endline "\n*gulps* YUMMM!";
        eat
      with NoSuchItem s ->
        print_endline s;
        home_item
          {
            balance = t.balance;
            hunger = t.hunger;
            name = t.name;
            inventory = t.inventory;
            level = t.level;
          })

let rec choose_store t =
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  let x = read_int () in
  match x with
  | 1 ->
      food_item
        {
          balance = new_tt.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }
  | _ ->
      choose_store
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }

let rec choose_home_activity t =
  let x = read_int () in
  match x with
  | 1 ->
      home_item
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }
  | _ ->
      choose_home_activity
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }

let rec choose_home t =
  print_endline "\n";
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_stats
    {
      balance = new_tt.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    };
  print_endline
    "Welcome to the home menu!\n\
     Here are your home options:\n\
     1: Feed\n\
     Please choose an option!";
  try
    choose_home_activity
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }
  with _ ->
    choose_home
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }

let rec choice_of_store_item t =
  print_endline "\n";
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_stats new_tt;
  print_endline
    "Welcome to the store menu!\n\
     Here are your store options:\n\
     1: Food\n\
     Please choose an option!";
  try
    choose_store
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }
  with _ ->
    choice_of_store_item
      {
        balance = t.balance;
        hunger = t.hunger;
        name = t.name;
        inventory = t.inventory;
        level = t.level;
      }

(* |||||||||||||||||||||||||||||OPTIONS|||||||||||||||||||||||||||||||||||||||||*)
let rec user_options t =
  print_endline "\n";
  let new_tt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  print_stats new_tt;
  print_endline
    ("Hello " ^ t.name
   ^ "'s Owner! This is the main menu. From here you can go to the store to\n\
     \ buy food, play minigames to win money, and go home to feed " ^ t.name
   ^ "'s big stomach!\nPlease choose one of the following options:");
  print_endline "1: Store";
  print_endline "2: Minigames";
  print_endline "3: Home";
  let new_ttt =
    {
      balance = t.balance;
      hunger = t.hunger;
      name = t.name;
      inventory = t.inventory;
      level = t.level;
    }
  in
  match read_int_opt () with
  | Some 1 ->
      let new_t =
        {
          balance = new_ttt.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }
      in
      choice_of_store_item new_t
  | Some 2 ->
      if t.hunger = 0 then raise (death_exn t);
      let new_t =
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }
      in
      let z = choose_minigame new_t in
      user_options z
  | Some 3 ->
      let new_t =
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }
      in
      choose_home new_t
  | _ ->
      user_options
        {
          balance = t.balance;
          hunger = t.hunger;
          name = t.name;
          inventory = t.inventory;
          level = t.level;
        }

let rec game_loop pet =
  let new_t = user_options pet in
  game_loop
    {
      balance = new_t.balance;
      hunger = new_t.hunger;
      name = new_t.name;
      inventory = new_t.inventory;
      level = new_t.level;
    }

let intro () =
  let pet = welcome_message () in
  try game_loop pet with GameOver s -> print_endline s