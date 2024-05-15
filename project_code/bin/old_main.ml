(* (* @author Arushi Aggarwal (aa2555), Petros Georgiou (pag38), Grace Wei
   (gtw25) *)

   (* open Project_code.Random_guessing_algorithm *)

   open Set open Project_code.Pin module PinMain : PinType = PinModule

   (**(** [to_string lst] converts [lst] to a string of digits *) let rec
   to_string acc = function | [] -> acc | h :: t -> to_string (acc ^
   string_of_int h) t

   let verify_input s length = String.length s = length

   (**[accept_feedback s] checks if a string is "yes", "no", or "quit". It will
   prompt the user to reenter a string if it does not match any of those cases.
   *) let rec accept_feedback s = match s with | "yes" | "quit" -> true | "no"
   -> false | _ -> print_endline "Invalid feedback. Please try again";
   accept_feedback (read_line ())

   (** main function that runs the game *) let rec run_round_terminal ended i =
   if i <= 0 then print_endline "The computer did not get the answer :(" else
   match ended with | true -> print_endline "The computer wins the round! :)" |
   false -> ( let () = print_string "The computer guessed: " in let guess =
   Project_code.Computer_output.make_guess () in let () = print_endline
   (to_string "" guess); print_endline "Is the computer correct? yes/no/quit" in
   let the_input = read_line () in match accept_feedback the_input with | true
   -> run_round_terminal true 0 | false -> run_round_terminal false (i - 1))

   let rec run_guess_terminal i answer = if i = 0 then print_endline ("Answer: "
   ^ answer ^ ". Thanks for playing!") else let guess = read_line () in if guess
   = "quit" then run_guess_terminal 0 answer else if String.length guess != 4
   then let () = print_endline "Incorrect length" in run_guess_terminal i answer
   else if guess = answer then print_endline "You win!" else let () =
   print_endline "Try again" in run_guess_terminal (i - 1) answer

   let () = print_string "Input the answer (4 digits): " let answer = read_line
   () let () = assert (verify_input answer 4)

   (* run the first round (computer guess) with 12 tries *) let () =
   run_round_terminal false 12 let () = print_endline "guess the code (type
   \"quit\" to stop)"

   (* run the second round (user guesses) with 12 tries as per the game *) let
   () = run_guess_terminal 12 (to_string ""
   Project_code.Computer_output.(make_guess ())) *)

   let rec to_string_help arr acc int = if int = 4 then acc else to_string_help
   arr (acc ^ string_of_int arr.(int)) (int + 1)

   let to_string2 arr = to_string_help arr "" 0

   module ChoiceSet = Make (Int)

   let rec string_to_guess str n arr = if n = 4 then arr else let _ = arr.(n) <-
   int_of_string (String.make 1 str.[n]) in string_to_guess str (n + 1) arr

   let check_reds arr = PinMain.count_reds arr = 4

   let rec check_duplicates str set1 set2 n = if n = 4 then true else let x =
   String.make 1 str.[n] in if ChoiceSet.mem (int_of_string x) set1 ||
   ChoiceSet.mem (int_of_string x) set2 = false then false else check_duplicates
   str (ChoiceSet.add (int_of_string x) set1) set2 (n + 1)

   let get_feedback (guess : int array) (answer : int array) = PinMain.make_pins
   guess answer

   let rec makechoices n acc = if n = -1 then acc else makechoices (n - 1)
   (ChoiceSet.add n acc)

   let choices = makechoices 7 ChoiceSet.empty

   let rec user_run n answer = if n = 0 then print_string ("Sorry :( The\n
   answer\n was " ^ to_string2 answer ^ ". Thanks I guess") else let guess =
   read_line () in if String.length guess != 4 then let _ = print_string
   "Invalid length\n" in user_run n answer else if check_duplicates guess
   ChoiceSet.empty choices 0 = false then let _ = print_string "Invalid\n
   characters/no duplicates\n" in user_run n answer else let arr =
   string_to_guess guess 0 [| 0; 0; 0; 0 |] in let _ = print_string (to_string2
   arr) in let pins = get_feedback arr answer in let _ = print_string
   (PinMain.to_string_pin pins ^ "\n") in if check_reds pins then print_string
   "You win!" else user_run (n - 1) answer

   let () = let _ = print_string "Welcome lol\n" in user_run 10 [| 0; 4; 5; 1
   |] *)
