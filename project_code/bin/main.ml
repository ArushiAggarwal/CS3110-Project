(* @author Arushi Aggarwal (aa2555), Petros Georgiou (pag38), Grace Wei
   (gtw25) *)

(* open Project_code.Computer_output *)

(** [to_string lst] converts [lst] to a string of digits *)
let rec to_string acc = function
  | [] -> acc
  | h :: t -> to_string (acc ^ string_of_int h) t

(* let verify_input s = *)

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
  if i <= 0 then print_endline "Thanks for playing!"
  else
    match ended with
    | true -> print_endline "Thanks for playing!"
    | false -> (
        let () = print_endline "computer guess" in
        let guess = Project_code.Computer_output.make_guess () in
        let () =
          print_endline (to_string "" guess);
          print_endline "Is the computer correct? yes/no/quit"
        in
        let the_input = read_line () in
        match accept_feedback the_input with
        | true -> run_round_terminal true 0
        | false -> run_round_terminal false (i - 1))

let () = print_endline "input the answer"
let answer = read_line ()
let () = print_endline answer
let () = run_round_terminal false 12
