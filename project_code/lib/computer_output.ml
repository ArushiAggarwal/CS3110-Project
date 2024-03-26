let random_seed = 42
(* A random seed value that represents the intial value used to start the
   sequence of pseudorandom numbers Can change the seed value to generate
   another sequence of guesses *)

let max_value = 9
(* A maximum value of 9 with assuming the integers are between 0 and 9 *)

(* Linear Congruential Generator (LCG) for pseudorandom number generation *)
(*Citation: https://rosettacode.org/wiki/Linear_congruential_generator*)
let rec lcg state =
  let next_state = ((1103515245 * state) + 12345) mod (1 lsl 31) in
  let random_value = next_state mod (max_value + 1) in
  (random_value, next_state)
