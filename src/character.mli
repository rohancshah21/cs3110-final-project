type t
(** The abstract type of values representing Game Dev Character. *)

val user_options : t -> t

val make_pet : string -> t
(** [make_pet a] makes a new pet [a]. *)

val print_stats : t -> unit
(** [print_pet a] outputs the status of pet [a]. *)

val welcome_message : t
(** [make_pet a] makes a new pet [a]. *)

val lookup : 'a -> ('a * 'b) list -> 'b
(** [lookup k difficulty] finds the trivia bank questions with [difficulty] and returns it *)

val lookup_one_question :
  'a -> ('a * (string * string * string)) list -> string -> int
(** [lookup_one_question rand difficulty question_num] randomly finds one question from the trivia bank with [difficulty] *)

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

val intro : unit
