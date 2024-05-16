(* open List open Pin module PinKnuth : PinType = PinModule

   (* Helper functions *)

   let remove_dupes lst = lst |> List.sort_uniq Stdlib.compare

   (* combine answer and guess*)

   let combine a b = let rec aux a b acc = match (a, b) with | [], _ | _, [] ->
   acc | a :: at, b :: bt -> aux at bt ((a, b) :: acc) in aux (remove_dupes a)
   (remove_dupes b) []

   (** Accessed from chatgpt*) (*create all possible combinations of code*)

   let perms lst = let rec aux = function | [] -> [ [] ] | h :: t -> let p = aux
   t in p @ (combine [ h ] p |> map (fun (a, l) -> a :: l)) in aux lst

   let score guess code = let guess_array = Array.of_list guess in let
   code_array = Array.of_list code in ( PinKnuth.count_reds (PinKnuth.make_pins
   guess_array code_array), 4 - PinKnuth.count_reds (PinKnuth.make_pins
   guess_array code_array) )

   let compare_tuples (_, a) ((_, b) : 'a * ('b -> 'b -> int)) = compare a b

   (*https://stackoverflow.com/questions/62430071/donald-knuth-algorithm-mastermind*)

   (** Accessed chatgpt for calculating and comparing scores*)

   let knuth_algorithm answer = let all_codes = perms [ 1; 2; 3; 4; 5; 6 ] in
   let initial_guess = [ 1; 1; 2; 2 ] in

   let rec aux s guess prev_guesses = let response = score guess answer in if
   response = (4, 0) then ( print_endline "Solution found!"; (guess,
   prev_guesses @ [ guess ])) else let s' = List.filter (fun code -> score guess
   code <> response) s in if s' = [] then failwith "No solution found" else let
   next_guesses = all_codes |> List.filter (fun g -> not (List.mem g
   prev_guesses)) |> List.map (fun g -> let score = (g |> List.filter (fun c ->
   score g c = response) s' |> List.length) ( g, score

   )) |> List.sort compare_tuples |> List.rev in let next_guess = if List.exists
   (fun (g, _) -> List.mem g s') next_guesses then List.find (fun (g, _) ->
   List.mem g s') next_guesses |> fst else List.hd next_guesses |> fst in aux s'
   next_guess (prev_guesses @ [ guess ]) in aux all_codes initial_guess [] *)

open List
open Pin
module PinKnuth : PinType = PinModule

(* Helper functions *)

let remove_dupes lst = lst |> List.sort_uniq Stdlib.compare

(* combine answer and guess*)

let combine a b =
  let rec aux a b acc =
    match (a, b) with
    | [], _ | _, [] -> acc
    | a :: at, b :: bt -> aux at bt ((a, b) :: acc)
  in
  aux (remove_dupes a) (remove_dupes b) []

(** Accessed from chatgpt*)
(*create all possible combinations of code*)

let perms lst =
  let rec aux = function
    | [] -> [ [] ]
    | h :: t ->
        let p = aux t in
        p @ (combine [ h ] p |> map (fun (a, l) -> a :: l))
  in
  aux lst

let score guess code =
  let guess_array = Array.of_list guess in
  let code_array = Array.of_list code in
  ( PinKnuth.count_reds (PinKnuth.make_pins guess_array code_array),
    4 - PinKnuth.count_reds (PinKnuth.make_pins guess_array code_array) )

(*https://stackoverflow.com/questions/62430071/donald-knuth-algorithm-mastermind*)

(** Accessed chatgpt for calculating and comparing scores*)

let knuth_algorithm answer =
  (* Set of all possible codes *)
  let all_codes = perms [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let rec aux s guess prev_guesses =
    match guess with
    | [] -> failwith "No solution found"
    | g :: rest ->
        let score_counts = all_codes |> map (fun _ -> score g answer) in
        let min_count =
          List.fold_left min max_int
            (List.map (fun (_, count) -> count) score_counts)
        in
        let best_guesses =
          score_counts
          |> filter (fun (_, count) -> count = min_count)
          |> map fst
        in
        let next_guess = if List.mem g s then g else best_guesses in
        let response = score next_guess answer in
        if response = (4, 0) then (
          print_endline "we did it :D";
          (next_guess, prev_guesses @ [ next_guess ]))
        else
          let s' = s |> filter (fun code -> score next_guess code = response) in
          aux s' rest (prev_guesses @ [ next_guess ])
  in
  aux all_codes [ [ 1; 1; 2; 2 ] ] []
