(** Character Module Description: Contains all code for making a Tamagotchi pet, 
    navigating through minigames, the store, menu, and home. Als contains code 
    for exceptions, failures, and general game behavior. *)

type t
(** The abstract type of values representing a Tamagotchi account. *)

val default_maze_bank : (int * int * int) list
(** [default_maze_bank] contains a default orientation of maze questions *)

val maze_bank : (int * int * int) list ref
(** [maze_bank] creates a reference to [default_maze_bank] *)

val make_pet : string -> t
(** [make_pet a] makes a new pet [a] with that name. *)

val data_dir_prefix : string
(** [data_dir_prefix] allows us to use JSON files for data *)

val get_max_hunger : int
(** [get_max_hunger] gets the maximum hunger of a pet, which is 3 *)

val trivia_questions_json : Yojson.Basic.t
(** [trivia_questions_json] loads questions from trivia_questions.json file *)

val maze_encounters_json : Yojson.Basic.t
(** [maze_questions_json] loads questions from maze_questions.json file *)

val parse_questions : Yojson.Basic.t -> int * (string * string * string)
(** [parse_questions j] finds necessary questions from the trivia_questions JSON
   file *)

val parse_encounters : Yojson.Basic.t -> string * string * string * string
(** [parse_encounters encounter] finds necessary encounters from the
   maze_encounters JSON file *)

val generate_encounters :
  Yojson.Basic.t -> (string * string * string * string) list
(** [generate_encounters json] generates 5 random encounters from the 
    maze_encounters JSON file *)

val iter_encounters : (string * string * string * string) list -> t -> t
(** [iter_encounters encounters t] iterates through the generated [encounters]
    and prompts users to make choices *)

val get_questions_from_json :
  Yojson.Basic.t ->
  (int * (string * string * string)) list
  * (int * (string * string * string)) list
  * (int * (string * string * string)) list
(** [get_questions_from_json j] finds necessary questions from the
    trivia_questions JSON file based on difficulty *)

val string_of_inventory : string list -> string
(** [string_of_inventory inv] returns a string concatenation of items in [inv] *)

val print_stats : t -> unit
(** [print_pet a] outputs the status of pet [a]. *)

val welcome_message : unit -> t
(** [welcome_message ()] is the initial message at game start *)

val lookup : 'a -> ('a * 'b) list -> 'b
(** [lookup k difficulty] looks for corresponding questions with certain 
    difficulty*)

val lookup_one_question1 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question1 rand difficulty question_num] loads a random question
    with random difficulty *)

val lookup_one_question2 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question2 rand difficulty question_num] loads a random question
    with random difficulty *)

val lookup_one_question3 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question3 rand difficulty question_num] loads a random question
    with random difficulty *)

val lookup_one_question4 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question4 rand difficulty question_num] loads a random question
    with random difficulty *)

val lookup_one_question5 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question5 rand difficulty question_num] loads a random question
    with random difficulty *)

val five_random_numbers : 'a list -> int list -> int list
(** [five_random_numbers difficulty acc] puts five random numbers into [acc] *)

val lookup_five_questions : (int * (string * string * string)) list -> int
(** [lookup_five_questions difficulty] finds five questions from the trivia 
    bank of [difficulty] *)

val choose_difficulty : t -> t
(** [choose_difficulty t] enables users to choose question difficulty *)

val trivia_minigame : t -> t
(** [trivia_minigame t] begins a trivia minigame *)

val maze_minigame : t -> t
(** [maze_minigame t] starts a new maze minigame *)

val choose_minigame : t -> t
(** [choose_minigame t] lets users choose from set of implemented minigames *)

val guess_number : t -> int -> int -> t
(** [guess_number t secret n_guesses_left] prompt the user to guess a number with
   a certain amount of guesses *)

val start_maze : t -> t
(** [start_maze t] starts a new maze minigame *)

(************ STORE FUNCTIONS ****************)

val choose_store : t -> t
(** [choose_store t] lets users choose a store operation *)

val choice_of_store_item : t -> t
(** [choice_of_store_item t] lets users choose what items they can use in the
    store *)

val choose_home_activity : t -> t
(** [choose_home_activity t] lets users choose a home operation *)

val choose_home : t -> t
(** [choose_home t] brings users home and displays corresponding operations *)

val food_bank_find_cost : (int * (int * string)) list
(** [food_bank_find_cost] is the costs associated with various foods *)

val lookup_store : 'a -> ('a * 'b) list -> 'b
(** [lookup_store k bank] explores the store when the user clicks *)

exception ItemLimit of string
(** Raises: ItemLimit when the limit of an inventory exceeds a specified item limit *)

exception NoSuchItem of string
(** Raises: NoSuchItem when an item is called that does not exist as a possibility in the inventory *)

exception GameOver of string
(** Raises: GameOver when the Tamagotchi dies *)

val death_exn : t -> exn
(** [death_exn t] displays a GameOver exception when a user loses *)

val add_item_to_inventory : string -> string list -> string list
(** [add_item_to_inventory i lst] appends an item to a string list inventory and makes sure to not exceed the max amount *)

val if_not_in_inventory : string -> string list -> string list
(** [if_not_in_inventory i lst] checks if item [i] is not in [lst] and uses [add_item_to_inventory] appropriately *)

val enumerate_inventory : string list -> int -> string list
(** [enumerate_inventory inv acc] converts [inv] entries to numbers and appends them to [acc] *)

val food_dict : (string * int) list
(** [food_dict] is the type of foods and their corresponding values in a list *)

val food_item : t -> t
(** [food_item t] lets users see various food options for their pet *)

val get_hunger_value : 'a -> ('a * 'b) list -> 'b
(** [get_hunger_value item lst] finds the associated value for [item], the key, in [lst] *)

val get_food_in_inventory : int -> string list -> int -> string
(** [get_food_in_inventory idx inv acc] finds the food entry from [inv] corresponding to [idx] index *)

val deplete_food : string -> string list -> string list
(** [deplete_food item inventory] removes [item] from the [inventory] counter and returns the updated [inventory] *)

val refill_hunger : int -> t -> int
(** [refill_hunger amt t] adds hunger points of [amt] if a pet is fed *)

val feed_item : int -> t -> t
(** [feed_item idx t] invokes [refill_hunger] to feed the pet and update its record along with updating any according balances *)

val home_item : t -> t
(** [home_item t] brings users to the home to feed their pet if they want *)

(************ USER OPTIONS ****************)

val user_options : t -> t
(** [user_options t] displays users options on the main menu *)

val game_loop : t -> 'a
(** [game_loop pet] continues the game for the [pet] until failure *)

val intro : unit -> unit
(** [intro ()] starts a game *)
