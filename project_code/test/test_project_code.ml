open OUnit2
open Project_code.Pin
open Project_code.Game

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
         ( "A derrangement 2" >:: fun _ ->
           assert_equal "WWWW"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 5; 4; 3; 6 |] [| 6; 5; 4; 3 |])) );
         ( "A mix + max nulls" >:: fun _ ->
           assert_equal "RWNN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 3; 5; 6; 4 |])) );
         ( "Ensuring pins work with nothing in place number 1" >:: fun _ ->
           assert_equal "RWWN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 4; 6; 5; 1 |] [| 3; 5; 6; 1 |])) );
         ( "Ensuring pins work with nothing in place number 2" >:: fun _ ->
           assert_equal "RRRN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 4; 6; 5; 1 |] [| 3; 6; 5; 1 |])) );
       ]

let _ = run_test_tt_main test_pin

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

let _ = run_test_tt_main test_valid

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

let _ = run_test_tt_main test_all_colors

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

let _ = run_test_tt_main test_num_reds

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

let _ = run_test_tt_main test_to_int_arr

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

let test_make_game =
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

let _ = run_test_tt_main test_make_game
let _ = Gamerecord.set_answer test_board [| 5; 2; 6; 4 |]
let _ = Gamerecord.update_game test_board [| 1; 2; 6; 4 |]

let test_update_game =
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

let _ = run_test_tt_main test_update_game

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

let guess_n_times game guess n =
  let i = ref n in
  while !i > 0 do
    Gamerecord.update_game game guess;
    i := !i - 1
  done

let test_clear_board =
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

let _ = run_test_tt_main test_clear_board
