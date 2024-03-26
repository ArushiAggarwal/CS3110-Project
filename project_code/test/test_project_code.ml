open OUnit2
open Project_code.Computer_output

let test_Random =
  "Test suite to ensure pseudorandom generator works with respect to seed"
  >::: [
         ( "Ensure seed 42 works" >:: fun _ ->
           assert_equal [ 5; 6; 3; 4; 7 ] (generate_guess 42) );
       ]

let _ = run_test_tt_main test_Random
