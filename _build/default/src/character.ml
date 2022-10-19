type state = { id : string; desc : string }

type t = {
  balance : int; (* current balance *)
  hunger : int; (* current hunger level *)
  current_activity : state; (* current activity *)
  name : string;
}

let default_state = { id = "home"; desc = "this is the default state" }

let make_pet pet_name =
  { balance = 0; hunger = 5; current_activity = default_state; name = pet_name }

let print_pet t =
  print_endline ("Current State of " ^ t.name);
  print_endline ("Balance: " ^ string_of_int t.balance);
  print_endline ("Hunger: " ^ string_of_int t.hunger)

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
