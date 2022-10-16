type t
(** The abstract type of values representing Game Dev Character. *)

val current_game : t -> int
(** [current_game a] is the identifier of the current room in character [a].
    Example: the [current_game] of PS4 is [0]. *)

val get_balance : t -> int
(** [get_balance a] is the identifier of the current room in character [a].
    Example: the [get_balance] of PS4 is [0]. *)

val previous_games : t -> int
(** [previous_games a] is the identifier of the current room in character [a].
    Example: the [previous_games] of PS4 is [0]. *)