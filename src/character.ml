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

(* outputs the list of activity options to the user*)
let rec user_options t =
  print_endline "1: Store";
  print_endline "2: Minigames";
  print_endline "3: Home";
  match read_int () with
  | 1 ->
      {
        balance = t.balance;
        hunger = t.hunger - 1;
        current_activity = store;
        name = t.name;
      }
  | 2 ->
      {
        balance = t.balance;
        hunger = t.hunger - 1;
        current_activity = minigames;
        name = t.name;
      }
  | 3 ->
      {
        balance = t.balance;
        hunger = t.hunger - 1;
        current_activity = home;
        name = t.name;
      }
  | _ -> user_options t

(* outputs the current activity the user is on *)
let print_current_activity t = t.current_activity
