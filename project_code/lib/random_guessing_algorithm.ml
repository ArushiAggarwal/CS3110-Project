open Set

let random_seed = 42
(* A random seed value that represents the intial value used to start the
   sequence of pseudorandom numbers Can change the seed value to generate
   another sequence of guesses *)

(* Initialize Random *)
let () = Random.self_init ()

(* Generate a random integer between 1 and len. Requires len > 1 *)
let max_value = 6
(* A maximum value of 9 with assuming the integers are between 0 and 9 *)

(* Linear Congruential Generator (LCG) for pseudorandom number generation *)
(* Citation: https://rosettacode.org/wiki/Linear_congruential_generator *)
let lcg state =
  let next_state = ((1103515245 * state) + 1234) mod (1 lsl 31) in
  let random_value = next_state mod (max_value + 1) in
  (random_value, next_state)

(* Generate a list of 4 random integers *)
let generate_guess seed =
  let rec helper seed acc =
    if List.length acc = 4 then acc
    else
      let random_value, new_seed = lcg seed in
      if List.mem random_value acc then helper new_seed acc
      else helper new_seed (random_value :: acc)
  in
  helper seed []

(** Make a guess using a random integer *)
let make_guess () = generate_guess (Random.int 1073741823)

module ChoiceSet = Make (Int)

let rec makechoices n acc =
  if n = 0 then acc else makechoices (n - 1) (ChoiceSet.add n acc)

let choices = makechoices 8 ChoiceSet.empty
let otherval = 7

let rec new_guess n guess guessed =
  if n = 4 then guess
  else
    let ran = Random.int 6 + 1 in
    if ChoiceSet.mem ran guessed then new_guess n guess guessed
    else
      let _ = guess.(n) <- ran in
      new_guess (n + 1) guess (ChoiceSet.add ran guessed)
