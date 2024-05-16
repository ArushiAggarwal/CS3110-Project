(** Generates pseudorandom guesses for a game.

    This module uses a Linear Congruential Generator (LCG) to generate
    pseudorandom numbers, which are then used to create guesses for a game
    involving 4 integers between 1 and 6 (inclusive), with no duplicates. *)

val make_guess : unit -> int list
(** [make_guess] generates a guess (a list of 4 unique integers between 1 and 6)
    using a pseudorandom seed. *)

val generate_guess : int -> int list
(** [generate_guess seed] generates a list of 4 random integers between 1 and 5
    (inclusive) using the given [seed] value. No duplicates are allowed in the
    generated list. *)

val lcg : int -> int * int
(** [lcg] is a Linear Congruential Generator (LCG) function used for
    pseudorandom number generation. *)

val max_value : int
(** [max_value] is the maximum value for the pseudorandom numbers*)

val random_seed : int
(** [random_seed] creates random seed value that represents the intial value
    used to start the sequence of pseudorandom numbers Can change the seed value
    to generate another sequence of guesses Set to 42 by default by research. *)
