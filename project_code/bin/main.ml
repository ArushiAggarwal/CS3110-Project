(* @author Arushi Aggarwal (aa2555), Petros Georgiou (pag38), Grace Wei
   (gtw25) *)

(* open Project_code.Random_guessing_algorithm *)

(** [to_string lst] converts [lst] to a string of digits *)
let rec to_string acc = function
  | [] -> acc
  | h :: t -> to_string (acc ^ string_of_int h) t

let verify_input s length = String.length s = length

(**[accept_feedback s] checks if a string is "yes", "no", or "quit". It will
   prompt the user to reenter a string if it does not match any of those cases. *)
let rec accept_feedback s =
  match s with
  | "yes" | "quit" -> true
  | "no" -> false
  | _ ->
      print_endline "Invalid feedback. Please try again";
      accept_feedback (read_line ())

(** main function that runs the game *)
let rec run_round_terminal ended i =
  if i <= 0 then print_endline "The computer did not get the answer :("
  else
    match ended with
    | true -> print_endline "The computer wins the round! :)"
    | false -> (
        let () = print_string "The computer guessed: " in
        let guess = Project_code.Random_guessing_algorithm.make_guess () in
        let () =
          print_endline (to_string "" guess);
          print_endline "Is the computer correct? yes/no/quit"
        in
        let the_input = read_line () in
        match accept_feedback the_input with
        | true -> run_round_terminal true 0
        | false -> run_round_terminal false (i - 1))

let rec run_guess_terminal i answer =
  if i = 0 then print_endline ("Answer: " ^ answer ^ ". Thanks for playing!")
  else
    let guess = read_line () in
    if guess = "quit" then run_guess_terminal 0 answer
    else if String.length guess != 4 then
      let () = print_endline "Incorrect length" in
      run_guess_terminal i answer
    else if guess = answer then print_endline "You win!"
    else
      let () = print_endline "Try again" in
      run_guess_terminal (i - 1) answer

let () = print_string "Input the answer (4 digits): "
let answer = read_line ()
let () = assert (verify_input answer 4)

(* run the first round (computer guess) with 12 tries *)
let () = run_round_terminal false 12
let () = print_endline "guess the code (type \"quit\" to stop)"

(* run the second round (user guesses) with 12 tries as per the game *)
let () =
  run_guess_terminal 12
    (to_string "" Project_code.Random_guessing_algorithm.(make_guess ()))
