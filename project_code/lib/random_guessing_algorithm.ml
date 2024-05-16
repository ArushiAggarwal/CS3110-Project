(* Initialize Random *)
let () = Random.self_init ()

(** [random_seed] creates random seed value that represents the intial value
    used to start the sequence of pseudorandom numbers Can change the seed value
    to generate another sequence of guesses Set to 42 by default by research. *)
let random_seed = 42

(** [max_value] is the maximum value for the pseudorandom numbers*)
let max_value = 5

(* Citation: https://rosettacode.org/wiki/Linear_congruential_generator *)

(** [lcg state] is a Linear Congruential Generator (LCG) function used for
    pseudorandom number generation. *)
let lcg state =
  let next_state = ((1103515245 * state) + 1234) mod (1 lsl 31) in
  let random_value = next_state mod max_value in
  (random_value, next_state)

(** [generate_guess seed seed] generates a list of 4 random integers between 1
    and 5 (inclusive) using the given [seed] value. No duplicates are allowed in
    the generated list. *)
let generate_guess seed =
  let rec helper seed acc =
    if List.length acc = 4 then acc
    else
      let random_value, new_seed = lcg seed in
      if List.mem random_value acc then helper new_seed acc
      else helper new_seed (random_value :: acc)
  in
  helper seed []

(** [make_guess ()] generates a guess (a list of 4 unique integers between 1 and
    6) using a pseudorandom seed. *)
let make_guess () =
  List.map (fun x -> x + 1) (generate_guess (Random.int 1073741823))
