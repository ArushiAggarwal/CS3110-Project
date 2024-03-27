(* @author Arushi Aggarwal (aa2555), Petros Georgiou (pag38), Grace Wei
   (gtw25) *)

open Project_code.Computer_output

(** [to_string lst] converts [lst] to a string of digits *)
let rec to_string = function
  | [] -> ""
  | h :: t -> string_of_int h ^ to_string t

(** main function that runs the game *)
let rec run_round_terminal ended i =
  if i <= 0 then print_endline "Thanks for playing!"
  else
    match ended with
    | true -> print_endline "Thanks for playing!"
    | false -> (
        let () = print_endline "computer guess" in
        let guess = make_guess in
        let () =
          print_endline (to_string guess);
          print_endline "Is the computer correct? yes/no/quit"
        in
        let the_input = read_line () in
        match the_input with
        | "yes" | "quit" -> run_round_terminal true 0
        | _ -> run_round_terminal false (i - 1))

let () = print_endline "input the answer"
let answer = read_line ()
let () = print_endline answer
let () = run_round_terminal false 12
