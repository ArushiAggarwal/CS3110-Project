val remove_dupes : 'a list -> 'a list
(** [remove_dupes] removes duplicate elements from a list and sorts the result.*)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** [combine] combines two lists by pairing their elements where each pair
    contains an element from `a` and an element from `b` *)

val perms : 'a list -> 'a list list
(** [perms] Generates all permutations of a list.*)

val score : int list -> int list -> int * int
(** [score] Scores a guess against a code in the Mastermind game. *)

val knuth_algorithm : unit -> int list * int list list
(** [knuth_algorithm] Implements the Knuth algorithm for solving the Mastermind
    game. It returns a pair `(solution, prev_guesses)`, where `solution` is the
    solved code, and `prev_guesses` is a list of all guesses made to reach the
    solution *)
