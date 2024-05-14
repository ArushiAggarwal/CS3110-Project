open List
open Pin

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
  ( Pin.count_reds (Pin.make_pins guess_array code_array),
    4 - Pin.count_reds (Pin.make_pins guess_array code_array) )

(*https://stackoverflow.com/questions/62430071/donald-knuth-algorithm-mastermind*)

(** Accessed chatgpt for calculating and comparing scores*)

let knuth_algorithm () =
  (* Set of all possible codes *)
  let all_codes = perms [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let rec aux s guess prev_guesses =
    match guess with
    | [] -> failwith "No solution found"
    | g :: rest ->
        let score_counts = all_codes |> map (fun code -> score g code) in
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
        let response = score next_guess [ 1; 2; 3; 4 ] in
        if response = (4, 0) then (next_guess, prev_guesses @ [ next_guess ])
        else
          let s' = s |> filter (fun code -> score next_guess code = response) in
          aux s' rest (prev_guesses @ [ next_guess ])
  in
  aux all_codes [ [ 1; 1; 2; 2 ] ] []

let solution, prev_guesses = knuth_algorithm ()
