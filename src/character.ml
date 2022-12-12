open Infobanks

type t = {
  balance : int; (* current balance *)
  hunger : int; (* current hunger level *)
  name : string;
}

(* this is the home locations *)

(* makes a new pet with the given name *)
let make_pet pet_name = { balance = 0; hunger = 5; name = pet_name }

(* prints all of the characteristics of the pet *)
let print_stats t =
  print_endline
    (t.name ^ "'s Stats: Balance = $" ^ string_of_int t.balance ^ "; Hunger = "
   ^ string_of_int t.hunger ^ "/5")

(* the first message the user sees when they open the game *)
let welcome_message =
  print_endline
    ("Hello Player! Welcome to Tomigatchi Simulator. In this game you will"
   ^ "\n"
   ^ "be taking care of your new pet! The goal of this game is to make as"
   ^ "\n"
   ^ "much money through minigames so you can buy food and new perks for your \
      pet! " ^ "\n" ^ "\n"
   ^ "Let's start by naming your pet. What would you like to name your pet?");
  let x = read_line () in

  make_pet x

(* the death message *)

(* let death_message t =
     print_endline
       ("GAME OVER: " ^ t.name ^ "has unfortunately died from starvation :(")

   let check_death t = if t.hunger <= 0 then death_message else ignore *)

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
      print_endline ("You got " ^ string_of_int amt5 ^ "/5 questions right!");
      let word =
        if difficulty = easy_trivia_bank then amt5
        else if difficulty = medium_trivia_bank then amt5 * 2
        else amt5 * 3
      in
      print_endline ("Congrats! You earned $" ^ string_of_int word ^ "!");
      amt5
  | _ -> failwith "oops!"

let rec choose_difficulty t =
  print_endline
    "Choose Difficulty:\n\
     1:Easy (1x Multiplier)\n\
     2:Medium (2x Multiplier)\n\
     3:Hard (3x Multiplier)";
  let x = read_int () in
  match x with
  | 1 ->
      let bonus = lookup_five_questions easy_trivia_bank in
      { balance = t.balance + bonus; hunger = t.hunger - 1; name = t.name }
  | 2 ->
      let bonus = lookup_five_questions medium_trivia_bank * 2 in
      { balance = t.balance + bonus; hunger = t.hunger - 1; name = t.name }
  | 3 ->
      let bonus = lookup_five_questions hard_trivia_bank * 3 in
      { balance = t.balance + bonus; hunger = t.hunger - 1; name = t.name }
  | _ -> choose_difficulty t

let trivia_minigame t =
  print_endline "\n";
  print_endline ("Hey " ^ t.name ^ "'s Owner! Welcome to Trivia!");
  print_endline
    "You get money based on how many of the five questions you get correct! \
     Good luck!";
  choose_difficulty t

let rec choose_minigame t =
  print_endline "\n";
  print_endline
    "Welcome to the minigame menu!\n\
     Here are your minigame options:\n\
     1: Trivia\n\
     Please choose an option!";
  let x = read_int () in
  match x with 1 -> trivia_minigame t | _ -> choose_minigame t

(* |||||||||||||||||||||||||||||STORE|||||||||||||||||||||||||||||||||||||||||*)
let food_bank = [ (1, 1) ]

let lookup_store k bank =
  match bank with
  | [] -> failwith "Oops!"
  | (k', v) :: t -> if k = k' then v else lookup k t

let rec food_item t =
  print_stats t;
  print_endline
    "Welcome to the Grocery Store!\n\
     Here are your food options:\n\
     1: Biscuit $1 \n\
     0: Main Menu \n\
     Please choose an option!";
  let x = read_int () in
  match x with
  | 1 ->
      let y = lookup_store 1 food_bank in
      let new_bal = t.balance - y in
      if new_bal < 0 then
        let _ = print_endline "CANNOT AFFORD" in
        food_item t
      else
        let new_t = { balance = new_bal; hunger = t.hunger; name = t.name } in
        new_t
  | _ -> food_item t

let rec choose_store t =
  let x = read_int () in
  match x with 1 -> food_item t | _ -> choose_store t

let choose_home t =
  let x = read_int () in
  let y = (t, x) in
  match y with _ -> failwith "not implemented yet"

let choice_of_store_item t =
  print_endline "\n";
  print_stats t;
  print_endline
    "Welcome to the store menu!\n\
     Here are your store options:\n\
     1: Food\n\
     Please choose an option!";
  choose_store t

(* |||||||||||||||||||||||||||||OPTIONS|||||||||||||||||||||||||||||||||||||||||*)
let rec user_options t =
  print_endline "\n";
  print_stats t;
  print_endline
    ("Hello " ^ t.name
   ^ "'s Owner! This is the main menu. From here you can go to the store to\n\
     \ buy food, play minigames to win money, and go home to feed " ^ t.name
   ^ "'s big stomach!\nPlease choose one of the following options:");
  print_endline "1: Store";
  print_endline "2: Minigames";
  print_endline "3: Home";
  match read_int_opt () with
  | Some 1 ->
      let new_t = { balance = t.balance; hunger = t.hunger; name = t.name } in
      choice_of_store_item new_t
  | Some 2 ->
      let new_t = { balance = t.balance; hunger = t.hunger; name = t.name } in
      let z = choose_minigame new_t in
      user_options z
  | Some 3 ->
      let new_t = { balance = t.balance; hunger = t.hunger; name = t.name } in
      choose_home new_t
  | _ -> user_options t

(* |||||||||||||||||||||||||||||START GAME|||||||||||||||||||||||||||||||||||||||||*)

let game_loop pet =
  let new_t = user_options pet in
  if new_t.hunger = 0 then print_endline "YOU LOST!"
  else print_endline "YOU WON!"

let intro =
  let pet = welcome_message in
  game_loop pet
