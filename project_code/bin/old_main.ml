(* (* @author Arushi Aggarwal (aa2555), Petros Georgiou (pag38), Grace Wei
   (gtw25) *)

   open Project_code.Random_guessing_algorithm *)

(* open Set open Project_code.Pin module PinMain : PinType = PinModule

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

   let rec user_run n answer = if n = 0 then print_string ("Sorry :( The\n\n
   answer\n was " ^ to_string2 answer ^ ". Thanks I guess") else let guess =
   read_line () in if String.length guess != 4 then let _ = print_string
   "Invalid length\n" in user_run n answer else if check_duplicates guess
   ChoiceSet.empty choices 0 = false then let _ = print_string "Invalid\n\n
   characters/no duplicates\n" in user_run n answer else let arr =
   string_to_guess guess 0 [| 0; 0; 0; 0 |] in let _ = print_string (to_string2
   arr) in let pins = get_feedback arr answer in let _ = print_string
   (PinMain.to_string_pin pins ^ "\n") in if check_reds pins then print_string
   "You win!" else user_run (n - 1) answer

   let () = let _ = print_string "Welcome lol\n" in user_run 10 [| 0; 4; 5; 1
   |] *)
