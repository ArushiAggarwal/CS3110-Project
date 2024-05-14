open List

(* open Printf

   let all_codes = let f acc c1 c2 c3 c4 = let code = [c1; c2; c3; c4] in code
   :: acc in let rec aux acc c1 = if c1 > 6 then acc else let acc = aux acc (c1
   + 1) in let acc = List.fold_left (fun acc c2 -> List.fold_left (fun acc c3 ->
   List.fold_left (fun acc c4 -> f acc c1 c2 c3 c4) acc (c1 .. 6)) acc (c2 ..
   6)) acc (c1 .. 6) in acc in aux [] 1

   let count_blacks code guess = let rec aux code guess blacks = match (code,
   guess) with | [], [] -> blacks | c :: cs, g :: gs when c = g -> aux cs gs
   (blacks + 1) | _ :: cs, _ :: gs -> aux cs gs blacks | _, _ -> failwith
   "Invalid code or guess" in aux code guess 0

   let count_whites code guess = let code_counts = let rec aux code counts =
   match code with | [] -> counts | c :: cs -> let count = if List.mem_assoc c
   counts then (c, (List.assoc c counts) + 1) else (c, 1) in aux cs (count ::
   List.remove_assoc c counts) in aux code [] in let guess_counts = let rec aux
   guess counts = match guess with | [] -> counts | g :: gs -> let count = if
   List.mem_assoc g counts then (g, (List.assoc g counts) + 1) else (g, 1) in
   aux gs (count :: List.remove_assoc g counts) in aux guess [] in let rec aux
   code_counts guess_counts whites = match (code_counts, guess_counts) with |
   [], [] -> whites | (c, cc) :: cs, (g, gc) :: gs when c = g -> aux cs gs
   (whites + min cc gc) | (c, _) :: cs, (g, _) :: gs when c <> g -> aux cs gs
   whites | _, _ -> failwith "Invalid counts" in aux code_counts guess_counts 0

   let score code guess = let blacks = count_blacks code guess in let whites =
   count_whites code guess - blacks in (blacks, whites)

   let solve_mastermind () = let initial_guess = [1; 1; 2; 2] in let rec aux
   codes guess = printf "Guess: %d%d%d%d\n" (List.nth guess 0) (List.nth guess
   1) (List.nth guess 2) (List.nth guess 3); printf "> "; let input = read_line
   () in let blacks, whites = Scanf.sscanf input "%d %d" (fun b w -> (b, w)) in
   if blacks = 4 then printf "Code found!\n" else ( let codes = List.filter (fun
   c -> let s = score c guess in s = (blacks, whites)) codes in let best_guess =
   let rec aux best_score best_guess codes = match codes with | [] -> best_guess
   | c :: cs -> let scores = List.map (fun g -> let s = score c g in (s,
   List.length (List.filter (fun c' -> score c' g = s) codes))) all_codes in let
   max_score = List.fold_left max 0 (List.map snd scores) in let min_score =
   List.fold_left min max_int (List.map snd scores) in let candidates =
   List.filter (fun (_, n) -> n = min_score) scores in let best_score,
   best_guess = if min_score < best_score then let best_guess = if List.exists
   (fun (s, _) -> List.mem s codes) candidates then List.find (fun (s, _) ->
   List.mem s codes) candidates else List.hd candidates in (min_score, fst
   best_guess) else (best_score, best_guess) in aux best_score best_guess cs in
   aux max_int [] codes in aux codes best_guess) in aux all_codes initial_guess

   let () = solve_mastermind () *)

(* Helper functions *)

let remove_dupes lst = lst |> List.sort_uniq Stdlib.compare

let combine a b =
  let rec aux a b acc =
    match (a, b) with
    | [], _ | _, [] -> acc
    | a :: at, b :: bt -> aux at bt ((a, b) :: acc)
  in
  aux (remove_dupes a) (remove_dupes b) []

(** Accessed from chatgpt*)

let perms lst =
  let rec aux = function
    | [] -> [ [] ]
    | h :: t ->
        let p = aux t in
        p @ (combine [ h ] p |> map (fun (a, l) -> a :: l))
  in
  aux lst
