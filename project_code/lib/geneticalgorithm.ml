(** author: Petros Georgiou (pag238 )*)

open Pin
open Random

let choices = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
let initialguess = [ 0; 1; 2; 3 ]
let maxpop = 60
let maxgen = 100
let crossoverprob = 0.5
let crossovertomutationprob = 0.03
let permutationprob = 0.03
let inversionprob = 0.02
