type t
(** The abstract type of values representing Game Dev Character. *)

val user_options : t -> t

val make_pet : string -> t
(** [make_pet a] makes a new pet [a]. *)

val print_stats : t -> unit
(** [print_pet a] outputs the status of pet [a]. *)

val welcome_message : t
(** [make_pet a] makes a new pet [a]. *)

val intro : unit