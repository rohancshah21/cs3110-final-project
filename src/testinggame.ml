(* Generate a random number between 1 and 100 *)
let secret_number = Random.int 100 + 1

(* Prompt the user to guess a number *)
let rec guess_number () =
  print_endline "Guess a number between 1 and 100:";
  let guess = read_int () in
  if guess = secret_number then print_endline "You guessed the correct number!"
  else if guess < secret_number then (
    print_endline "Your guess is too low. Try again.";
    guess_number ())
  else (
    print_endline "Your guess is too high. Try again.";
    guess_number ())

(* Start the game *)
let () = guess_number ()
