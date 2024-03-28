let random_seed = 42
(* A random seed value that represents the intial value used to start the
   sequence of pseudorandom numbers Can change the seed value to generate
   another sequence of guesses *)

(* Initialize Random *)
let () = Random.self_init ()

(* Generate a random integer between 1 and len. Requires len > 1 *)
let max_value = 9
(* A maximum value of 9 with assuming the integers are between 0 and 9 *)

(* Linear Congruential Generator (LCG) for pseudorandom number generation *)
(* Citation: https://rosettacode.org/wiki/Linear_congruential_generator *)
let lcg state =
  let next_state = ((1103515245 * state) + 12345) mod (1 lsl 31) in
  let random_value = next_state mod (max_value + 1) in
  (random_value, next_state)

(* Generate a list of 5 random integers *)
let generate_guess seed =
  let rec helper seed acc =
    if List.length acc = 5 then acc
    else
      let random_value, new_seed = lcg seed in
      helper new_seed (random_value :: acc)
  in
  helper seed []

(** Make a guess using a random integer *)
let make_guess () = generate_guess (Random.int 1073741823)
