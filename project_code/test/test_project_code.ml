open OUnit2
open Project_code.Pin
open Project_code.Game

(**let test_random = "Test suite to ensure pseudorandom generator works with
   respect to seed." >:::
   [
         ( "Ensure seed 42 works" >:: fun _ ->
           assert_equal [ 5; 6; 3; 4; 7 ] (generate_guess 42) );
       ]

   let _ = run_test_tt_main test_random*)

let test_pin =
  "Testing Pins to ensure the provide the correct feedback for\n\
   a guess with respect to the answer."
  >::: [
         ( "A guess with all correct answers" >:: fun _ ->
           assert_equal "RRRR"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 1; 2; 3; 4 |])) );
         ( "A derrangement" >:: fun _ ->
           assert_equal "WWWW"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 4; 3; 2; 1 |])) );
         ( "A mix + max nulls" >:: fun _ ->
           assert_equal "RWNN"
             (PinModule.to_string_pin
                (PinModule.make_pins [| 1; 2; 3; 4 |] [| 3; 5; 6; 4 |])) );
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

let test to_int_arr =
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
       ]
