open OUnit2
open Project_code.Pin
open Project_code.Game
open Project_code.Random_guessing_algorithm

let test_random_seed =
  "Test suite to ensure pseudorandom generator works with respect to seed"
  >::: [
         ( "Ensure seed 1 works" >:: fun _ ->
           assert_equal [ 1; 2; 3; 4 ] (generate_guess 1) );
         ( "Ensure seed 42 works" >:: fun _ ->
           assert_equal [ 4; 2; 3; 1 ] (generate_guess 42) );
       ]

let test_random_generation =
  "Test suite to ensure pseudorandom generator make_guess() outputs length 4"
  >::: [
         ( "Ensure seed 1 works" >:: fun _ ->
           assert_equal (List.length (make_guess ())) 4 );
       ]

let test_pin =
  "Testing Pins to ensure the provide the correct feedback for\n\
   a guess with respect to the answer."
  >::: [
         ( "A guess with all correct answers" >:: fun _ ->
           assert_equal "RRRR"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 1; 2; 3; 4 |])) );
         ( "A derrangement 1" >:: fun _ ->
           assert_equal "WWWW"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 4; 3; 2; 1 |])) );
         ( "Maximum amount of nulls are shown" >:: fun _ ->
           assert_equal "WWNN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 5; 2; 1; 6 |] [| 6; 5; 4; 3 |])) );
         ( "A mix + max nulls" >:: fun _ ->
           assert_equal "RWNN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 3; 5; 6; 4 |])) );
         ( "Ensuring pins work with nothing in place number 1" >:: fun _ ->
           assert_equal "RWWN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 4; 6; 5; 1 |] [| 3; 5; 6; 1 |])) );
         ( "Three reds and one null" >:: fun _ ->
           assert_equal "RRRN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 4; 6; 5; 1 |] [| 3; 6; 5; 1 |])) );
         ( "Two reds and two nulls" >:: fun _ ->
           assert_equal "RRNN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 4; 2; 3; 1 |] [| 6; 2; 3; 5 |])) );
         ( "One red and three whites" >:: fun _ ->
           assert_equal "RWWW"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 2; 3; 6; 1 |] [| 6; 2; 3; 1 |])) );
       ]

(* let _ = run_test_tt_main test_pin *)

let test_valid =
  "Test if pin validation function works."
  >::: [
         ( "All caps, true" >:: fun _ ->
           assert_equal true
             (PinModule.check_validation "RWNN" [| 0; 1; 2; 3 |]
                [| 2; 5; 5; 3 |]) );
         ( "All lower case, true" >:: fun _ ->
           assert_equal true
             (PinModule.check_validation "rwnn" [| 0; 1; 2; 3 |]
                [| 2; 5; 5; 3 |]) );
         ( "A mix, true" >:: fun _ ->
           assert_equal true
             (PinModule.check_validation "rWnN" [| 0; 1; 2; 3 |]
                [| 2; 5; 5; 3 |]) );
         ( "Correct amount of chars, not in right order" >:: fun _ ->
           assert_equal false
             (PinModule.check_validation "nWrN" [| 0; 1; 2; 3 |]
                [| 2; 5; 5; 3 |]) );
         ( "Not correct amount chars, incorrect order" >:: fun _ ->
           assert_equal false
             (PinModule.check_validation "nWrN" [| 0; 1; 2; 3 |]
                [| 7; 5; 4; 6 |]) );
         ( "Incorret string length" >:: fun _ ->
           assert_equal false
             (PinModule.check_validation "nWrNN" [| 0; 1; 2; 3 |]
                [| 7; 5; 4; 6 |]) );
         ( "Features incorrect char" >:: fun _ ->
           assert_equal false
             (PinModule.check_validation "nWrP" [| 0; 1; 2; 3 |]
                [| 7; 5; 4; 6 |]) );
         ( "Empty string" >:: fun _ ->
           assert_equal false
             (PinModule.check_validation "" [| 0; 1; 2; 3 |] [| 7; 5; 4; 6 |])
         );
         ( "Max null" >:: fun _ ->
           assert_equal true
             (PinModule.check_validation "WwNn" [| 0; 1; 2; 3 |]
                [| 7; 5; 0; 1 |]) );
         ( "One correct character" >:: fun _ ->
           assert_equal false
             (PinModule.check_validation "W" [| 0; 1; 2; 3 |] [| 7; 5; 0; 1 |])
         );
       ]

(* let _ = run_test_tt_main test_valid *)

let test_all_colors =
  "Tests if the all_colors is working."
  >::: [
         ( "Only reds" >:: fun _ ->
           assert_equal [| 4; 0; 0 |]
             (PinModule.all_colors
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 1; 2; 3; 4 |])) );
         ( "Only whites" >:: fun _ ->
           assert_equal [| 0; 4; 0 |]
             (PinModule.all_colors
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 4; 3; 2; 1 |])) );
         ( "A mix + max nulls" >:: fun _ ->
           assert_equal [| 1; 1; 2 |]
             (PinModule.all_colors
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 5; 6; 1; 4 |])) );
       ]

(* let _ = run_test_tt_main test_all_colors *)

let test_num_reds =
  "Check in we can count correct number of reds"
  >::: [
         ( "All reds" >:: fun _ ->
           assert_equal 4
             (PinModule.count_reds
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 1; 2; 3; 4 |])) );
         ( "No reds" >:: fun _ ->
           assert_equal 0
             (PinModule.count_reds
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 5; 6; 4; 2 |])) );
         ( "Some reds" >:: fun _ ->
           assert_equal 2
             (PinModule.count_reds
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 1; 2; 5; 6 |])) );
       ]

(* let _ = run_test_tt_main test_num_reds *)

let test_to_int_arr =
  "Test if can convert pin to int array"
  >::: [
         ( "Only reds" >:: fun _ ->
           assert_equal [| 0; 0; 0; 0 |]
             (PinModule.to_int_array
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 1; 2; 3; 4 |])) );
         ( "Only whites" >:: fun _ ->
           assert_equal [| 1; 1; 1; 1 |]
             (PinModule.to_int_array
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 4; 3; 2; 1 |])) );
         ( "A mix + max nulls" >:: fun _ ->
           assert_equal [| 0; 1; 2; 2 |]
             (PinModule.to_int_array
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 5; 6; 2; 4 |])) );
         ( "No nulls, red and white" >:: fun _ ->
           assert_equal [| 0; 0; 1; 1 |]
             (PinModule.to_int_array
                (PinModule.make_pins [| 5; 6; 4; 2 |] [| 5; 6; 2; 4 |])) );
       ]

(* let _ = run_test_tt_main test_to_int_arr *)

let test_make_game =
  let test_board =
    {
      game_board = Array.make 12 (Array.make 4 0);
      pin_board = Array.make 12 (Array.make 4 3);
      total_rounds = 12;
      algorithm = "Random";
      player = "1";
      round_number = 0;
      turn_number = 0;
      answer = [| 0; 0; 0; 0 |];
    }
  in
  "Test if making a game works"
  >::: [
         ( "Ensure it works for one side" >:: fun _ ->
           assert_equal test_board (Gamerecord.make_game 12 "Random" "1") );
         ( "Ensure 1 feature is indeed mutable" >:: fun _ ->
           let _ = test_board.round_number <- 7 in
           assert_equal 7 test_board.round_number );
         ( "Ensure other feature is also mutable" >:: fun _ ->
           let _ = test_board.turn_number <- 7 in
           assert_equal 7 test_board.turn_number );
         ( "Ensure can set an answer" >:: fun _ ->
           let _ = Gamerecord.set_answer test_board [| 1; 2; 3; 4 |] in
           assert_equal [| 1; 2; 3; 4 |] test_board.answer );
       ]

(* let _ = run_test_tt_main test_make_game *)

let test_update_game =
  let test_board =
    {
      game_board = Array.make 12 (Array.make 4 0);
      pin_board = Array.make 12 (Array.make 4 3);
      total_rounds = 12;
      algorithm = "Random";
      player = "1";
      round_number = 0;
      turn_number = 0;
      answer = [| 0; 0; 0; 0 |];
    }
  in
  let _ = Gamerecord.set_answer test_board [| 5; 2; 6; 4 |] in
  let _ = Gamerecord.update_game test_board [| 1; 2; 6; 4 |] in
  "Test if everything indeeed updates"
  >::: [
         ( "Ensure guess board is updates after first turn" >:: fun _ ->
           assert_equal [| 1; 2; 6; 4 |]
             test_board.game_board.(test_board.turn_number - 1) );
         ( "Ensure pins are updated" >:: fun _ ->
           assert_equal [| 0; 0; 0; 2 |]
             test_board.pin_board.(test_board.turn_number - 1) );
         ( "Ensure next guess board is updates after second turn" >:: fun _ ->
           let _ = Gamerecord.update_game test_board [| 1; 3; 5; 6 |] in
           assert_equal [| 1; 3; 5; 6 |]
             test_board.game_board.(test_board.turn_number - 1) );
         ( "Ensure pins are updated" >:: fun _ ->
           let _ = Gamerecord.update_game test_board [| 1; 3; 5; 6 |] in
           assert_equal [| 1; 1; 2; 2 |]
             test_board.pin_board.(test_board.turn_number - 1) );
         ( "Allows for a duplicate guess" >:: fun _ ->
           let _ = Gamerecord.update_game test_board [| 1; 3; 5; 6 |] in
           assert_equal [| 1; 3; 5; 6 |]
             test_board.game_board.(test_board.turn_number - 1) );
         ( "Ensure pins are also duplicated" >:: fun _ ->
           let _ = Gamerecord.update_game test_board [| 1; 3; 5; 6 |] in
           assert_equal [| 1; 1; 2; 2 |]
             test_board.pin_board.(test_board.turn_number - 1) );
         ( "Ensure this mutable field has not been affected" >:: fun _ ->
           assert_equal 0 test_board.round_number );
         ( "Ensure this mutable field has been affected" >:: fun _ ->
           assert_equal 3 test_board.turn_number );
       ]

(* let _ = run_test_tt_main test_update_game *)

let guess_n_times game guess n =
  let i = ref n in
  while !i > 0 do
    Gamerecord.update_game game guess;
    i := !i - 1
  done

let test_clear_board =
  let test_board =
    {
      game_board = Array.make 12 (Array.make 4 0);
      pin_board = Array.make 12 (Array.make 4 3);
      total_rounds = 12;
      algorithm = "Random";
      player = "1";
      round_number = 0;
      turn_number = 0;
      answer = [| 0; 0; 0; 0 |];
    }
  in
  let test_board_again =
    {
      game_board = Array.make 12 (Array.make 4 0);
      pin_board = Array.make 12 (Array.make 4 3);
      total_rounds = 12;
      algorithm = "Random";
      player = "1";
      round_number = 1;
      turn_number = 0;
      answer = [| 0; 0; 0; 0 |];
    }
  in
  "Ensures the board resets"
  >::: [
         ( "Clear board with only one edit" >:: fun _ ->
           let _ = Gamerecord.clear_board test_board in
           assert_equal test_board_again test_board );
         ( "Clear board that is completely full" >:: fun _ ->
           let _ = guess_n_times test_board [| 1; 2; 3; 4 |] 11 in
           let _ = test_board_again.round_number <- 1 in
           let _ = Gamerecord.clear_board test_board in
           assert_equal test_board_again test_board );
       ]

(* let _ = run_test_tt_main test_clear_board *)
(************************* QCheck ****************************)

(** [list_test] creates different testing cases for the various check and parts
    of the pesudo randomzier algorithm and does the validation *)
let list_test seed =
  let result = generate_guess seed in
  let is_valid_guess lst =
    List.length lst = 4
    && List.for_all (fun x -> x >= 1 && x <= 6) lst
    && List.length (List.sort_uniq compare lst) = 4
  in
  is_valid_guess result

(** [list_generator] generates arbitrary lists between 1 to 6 . *)
let seed_generator = QCheck2.Gen.(int)

(** [many_random_tests] generates 1000 random tests. Each is based on random
    values in the list. Each list is passed to [list_test] to do the checking *)
let many_random_tests =
  QCheck2.Test.make ~count:1000 ~name:"random guess tests" seed_generator
    ~print:QCheck.Print.(int)
    list_test

(** [ounit_rnd_test] is a single OUnit test that runs all the random tests in
    [many_random_tests]. If any one of those fails, the entire OUnit test fails. *)
let ounit_rnd_test = QCheck_runner.to_ounit2_test many_random_tests

let suite =
  "test all cases"
  >::: [
         test_random_seed;
         test_random_generation;
         test_pin;
         test_valid;
         test_all_colors;
         test_num_reds;
         test_to_int_arr;
         test_make_game;
         test_update_game;
         test_clear_board;
         (* ounit_rnd_test; *)
       ]

let _ = run_test_tt_main suite
