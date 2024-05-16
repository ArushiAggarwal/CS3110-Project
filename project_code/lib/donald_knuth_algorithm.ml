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
  let rec combine x = function
    | [] -> [ [ x ] ]
    | hd :: tl as l -> (x :: l) :: List.map (fun y -> hd :: y) (combine x tl)
  in
  let rec aux = function
    | [] -> [ [] ]
    | h :: t -> List.flatten (List.map (combine h) (aux t))
  in
  aux lst

let rec combinations lst k =
  match (lst, k) with
  | _, 0 -> [ [] ] (* Base case: empty combination *)
  | [], _ -> [] (* Base case: empty list, no combinations *)
  | x :: xs, k ->
      let with_x = List.map (fun c -> x :: c) (combinations xs (k - 1)) in
      let without_x = combinations xs k in
      with_x @ without_x

let combinations_4_of_6 lst = combinations lst 4

let get_score guess code =
  let guess_array = Array.of_list guess in
  let code_array = Array.of_list code in
  ( PinKnuth.count_reds (PinKnuth.make_pins guess_array code_array),
    4 - PinKnuth.count_reds (PinKnuth.make_pins guess_array code_array) )

let compare_tuples t1 t2 =
  match (t1, t2) with
  | (_, a), (_, b) -> a - b

(*https://stackoverflow.com/questions/62430071/donald-knuth-algorithm-mastermind*)

(** Accessed chatgpt for calculating and comparing scores*)

let rec knuth_helper s guess prev_guesses answer all_codes =
  print_string "answer: ";
  print_endline (string_of_int (List.length answer));
  let response = get_score guess answer in
  let a, b = response in
  let () = print_endline (string_of_int a ^ " " ^ string_of_int a) in
  if response = (4, 0) then (
    print_endline "Solution found!";
    (guess, prev_guesses @ [ guess ]))
  else
    (* *)
    let s' = List.filter (fun code -> get_score guess code <> response) s in
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
                |> List.filter (fun c -> get_score g c = response)
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
      knuth_helper s' next_guess (prev_guesses @ [ guess ]) answer all_codes

let knuth_algorithm answer =
  (* Set of all possible codes *)
  let all_codes = combinations_4_of_6 [ 1; 2; 3; 4; 5; 6 ] in
  knuth_helper all_codes [ 1; 1; 2; 2 ] [] answer all_codes
