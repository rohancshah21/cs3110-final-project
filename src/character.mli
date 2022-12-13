type t
(** The abstract type of values representing a Tamagotchi account. *)

val make_pet : string -> t
(** [make_pet a] makes a new pet [a]. *)

val get_max_hunger : int
(** [get_max_hunger] gets the maximum hunger of a pet, which is 3 *)

val string_of_inventory : string list -> string
(** [string_of_inventory inv] returns a string concatenation of items in [inv] *)

val print_stats : t -> unit
(** [print_pet a] outputs the status of pet [a]. *)

val welcome_message : unit -> t
(** [make_pet a] makes a new pet [a]. *)

val lookup : 'a -> ('a * 'b) list -> 'b
(** [lookup k difficulty] finds the trivia bank questions with [difficulty] and returns it *)

val lookup_one_question1 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question1 rand difficulty question_num] randomly finds question 1 from the trivia bank with [difficulty] *)

val lookup_one_question2 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question1 rand difficulty question_num] randomly finds question 2 from the trivia bank with [difficulty] *)

val lookup_one_question3 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question1 rand difficulty question_num] randomly finds question 3 from the trivia bank with [difficulty] *)

val lookup_one_question4 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question1 rand difficulty question_num] randomly finds question 4 from the trivia bank with [difficulty] *)

val lookup_one_question5 :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question1 rand difficulty question_num] randomly finds question 5 from the trivia bank with [difficulty] *)

val five_random_numbers : 'a list -> int list -> int list
(** [five_random_numbers difficulty acc] creates a list in [acc] that has 5 randomly generated numbers *)

val lookup_five_questions : (int * (string * string * string)) list -> int
(** [lookup_five_questions difficulty] finds 5 random questions with [difficulty] from the trivia bank and returns the amount corresponding to the credits for getting questions right based on [difficulty]*)

val choose_difficulty : t -> t
(** [choose_difficulty a] allows users to choose a question difficulty, generate the necessary questions, and ensure that they are credited appropriately *)

val trivia_minigame : t -> t
(** [trivia_minigame a] lets users initiate a game of trivia with a certain difficulty *)

val choose_minigame : t -> t
(** [choose_minigame a] lets users choose a minigame and begin the game *)

val intro : unit -> unit

(************ STORE FUNCTIONS ****************)

val food_bank_find_cost : (int * (int * string)) list
(** [food_bank_find_cost] stores the cost for certain foods in a list *)

val lookup_store : 'a -> ('a * 'b) list -> 'b
(** [lookup_store k bank] finds the value associated with [k] in the bank*)

exception ItemLimit of string
(** Raised when the limit of an inventory exceeds a specified item limit *)

exception NoSuchItem of string
(** Raised when an item is called that does not exist as a possibility in the inventory *)

val add_item_to_inventory : string -> string list -> string list
(** [add_item_to_inventory i lst] appends an item to a string list inventory and makes sure to not exceed the max amount *)

val if_not_in_inventory : string -> string list -> string list
(** [if_not_in_inventory i lst] checks if item [i] is not in [lst] and uses [add_item_to_inventory] appropriately *)

val enumerate_inventory : string list -> int -> string list
(** [enumerate_inventory inv acc] converts [inv] entries to numbers and appends them to [acc] *)

val food_dict : (string * int) list
(** [food_dict] is the type of foods and their corresponding values in a list *)

val get_hunger_value : 'a -> ('a * 'b) list -> 'b
(** [get_hunger_value item lst] finds the associated value for [item], the key, in [lst] *)

val get_food_in_inventory : int -> string list -> int -> string
(** [get_food_in_inventory idx inv acc] finds the food entry from [inv] corresponding to [idx] index *)

val deplete_food : string -> string list -> string list
(** [deplete_food item inventory] removes [item] from the [inventory] counter and returns the updated [inventory] *)

val refill_hunger : int -> t -> int
(** [refill_hunger amt t] adds hunger balance of amount [amt] to [t] *)

val feed_item : int -> t -> t
(** [feed_item idx t] invokes [refill_hunger] to feed the pet and update its record along with updating any according balances *)