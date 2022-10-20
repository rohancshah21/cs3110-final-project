type state = { id : string; desc : string }

type t = {
  balance : int; (* current balance *)
  hunger : int; (* current hunger level *)
  current_activity : state; (* current activity *)
  name : string;
}

(* this is the home locations *)
let home = { id = "home"; desc = "this is home" }
let store = { id = "store"; desc = "this the store" }
let minigames = { id = "minigames"; desc = "this the minigames" }

(* makes a new pet with the given name *)
let make_pet pet_name =
  { balance = 0; hunger = 5; current_activity = home; name = pet_name }

(* prints all of the characteristics of the pet *)
let print_pet t =
  print_endline ("Current State of " ^ t.name);
  print_endline ("Balance: " ^ string_of_int t.balance);
  print_endline ("Hunger: " ^ string_of_int t.hunger)

(* the first message the user sees when they open the game *)
let welcome_message =
  print_endline
    ("Hello Player! Welcome to Tomigatchi Simulator. In this game you will"
   ^ "\n"
   ^ "be taking care of your new pet! The goal of this game is to make as"
   ^ "\n"
   ^ " much money through minigames so you can buy food and new perks for your \
      pet! " ^ "\n" ^ "\n"
   ^ "Let's start by naming your pet. What would you like to name your pet?");
  let x = read_line () in
  make_pet x

(* outputs the current activity the user is on *)
let print_current_activity t = t.current_activity

let easy_trivia_bank =
  [
    ( 0,
      ( "4",
        "4: Honey",
        "What is the only food that cannot go bad?\n\
         1: Dark Chocolate\n\
         2: Peanut butter\n\
         3: Canned Tuna\n\
         4: Honey" ) );
    ( 1,
      ( "2",
        "2: Liver",
        "What’s the heaviest organ in the human body?\n\
         1: Brain\n\
         2: Liver\n\
         3: Skin\n\
         4: Heart" ) );
    ( 2,
      ( "4",
        "4: Chihuahua",
        "Which of the following dog breeds is the smallest?\n\
         1: Dachshund\n\
         2: Poodle\n\
         3: Pomeranian\n\
         4: Chihuahua" ) );
    ( 3,
      ( "1",
        "1: Blue whale",
        "What is the biggest animal that has ever lived?\n\
         1: Blue whale\n\
         2: African elephant\n\
         3: Apatosaurus\n\
         4: Spinosaurus" ) );
    ( 4,
      ( "2",
        "2: Sailfish",
        "What is the fastest water animal?\n\
         1: Porpoise\n\
         2: Sailfish\n\
         3: Flying fish\n\
         4: Tuna" ) );
  ]

let medium_trivia_bank =
  [ (0, ("1", "1", "1,2,3,4")); (1, ("1", "1", "1,2,3,4")) ]

let hard_trivia_bank =
  [ (0, ("1", "1", "1,2,3,4")); (1, ("1", "1", "1,2,3,4")) ]

let rec lookup k difficulty =
  match difficulty with
  | [] -> failwith "Oops!"
  | (k', v) :: t -> if k = k' then v else lookup k t

let lookup_one_question rand difficulty question_num =
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
      let amt1 = lookup_one_question h1 difficulty "1" + amt in
      let amt2 = lookup_one_question h2 difficulty "2" + amt1 in
      let amt3 = lookup_one_question h3 difficulty "3" + amt2 in
      let amt4 = lookup_one_question h4 difficulty "4" + amt3 in
      let amt5 = lookup_one_question h5 difficulty "5" + amt4 in
      amt5
  | _ -> failwith "oops!"

let rec choose_difficulty t =
  print_endline "Choose Difficulty:\n1:Easy\n2:Medium\n3:Hard";
  let x = read_int () in
  match x with
  | 1 -> lookup_five_questions easy_trivia_bank
  | 2 -> lookup_five_questions medium_trivia_bank
  | 3 -> lookup_five_questions hard_trivia_bank
  | _ -> choose_difficulty t

let trivia_minigame t =
  print_endline ("Hey " ^ t.name ^ "! Welcome to Trivia!");
  print_endline
    "You get money based on how many of the five questions you get correct! \
     Good luck!";
  choose_difficulty t

let rec choose_minigame t =
  let x = read_int () in
  match x with 1 -> trivia_minigame t | _ -> choose_minigame t

let rec choose_store t =
  let x = read_int () in
  match x with _ -> failwith "not implemented yet"

let rec choose_home t =
  let x = read_int () in
  match x with _ -> failwith "not implemented yet"

let choice_of_minigames t =
  print_endline
    "Welcome to the minigame menu!\n\
     Here are your minigame options:\n\
     1: Trivia\n\
     Please choose an option!";
  choose_minigame t

(* outputs the list of activity options to the user*)
let rec user_options t =
  print_endline "1: Store";
  print_endline "2: Minigames";
  print_endline "3: Home";
  match read_int () with
  | 1 ->
      let new_t =
        {
          balance = t.balance;
          hunger = t.hunger - 1;
          current_activity = store;
          name = t.name;
        }
      in
      choose_store new_t
  | 2 ->
      let new_t =
        {
          balance = t.balance;
          hunger = t.hunger - 1;
          current_activity = minigames;
          name = t.name;
        }
      in
      choice_of_minigames new_t
  | 3 ->
      let new_t =
        {
          balance = t.balance;
          hunger = t.hunger - 1;
          current_activity = home;
          name = t.name;
        }
      in
      choose_home new_t
  | _ -> user_options t

let intro =
  let pet = welcome_message in
  user_options pet
