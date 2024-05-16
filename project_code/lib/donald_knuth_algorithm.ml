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

   (** Accessed chatgpt for calculating and comparing scores*) let
   knuth_algorithm answer = let all_codes = perms [ 1; 2; 3; 4; 5; 6 ] in let
   initial_guess = [ 1; 1; 2; 2 ] in

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

let compare_tuples t1 t2 =
  match (t1, t2) with
  | (_, a), (_, b) -> a - b

(*https://stackoverflow.com/questions/62430071/donald-knuth-algorithm-mastermind*)

(** Accessed chatgpt for calculating and comparing scores*)

let knuth_algorithm answer =
  (* Set of all possible codes *)
  let all_codes = perms [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let rec aux s guess prev_guesses =
    let response = score guess answer in
    if response = (4, 0) then (
      print_endline "Solution found!";
      (guess, prev_guesses @ [ guess ]))
    else
      let s' = List.filter (fun code -> score guess code <> response) s in
      if s' = [] then failwith "No solution found"
      else
        let next_guesses =
          let filtered_codes =
            List.filter (fun g -> not (List.mem g prev_guesses)) all_codes
          in
          let result =
            List.map
              (fun g ->
                let score =
                  filtered_codes
                  |> List.filter (fun c -> score g c = response)
                  |> List.length
                in
                (g, score))
              filtered_codes
          in
          List.rev (List.sort compare_tuples result)
        in
        let next_guess =
          if List.exists (fun (g, _) -> List.mem g s') next_guesses then
            List.find (fun (g, _) -> List.mem g s') next_guesses |> fst
          else List.hd next_guesses |> fst
        in
        aux s' next_guess (prev_guesses @ [ guess ])
  in
  aux all_codes [ [ 1; 1; 2; 2 ] ] []
