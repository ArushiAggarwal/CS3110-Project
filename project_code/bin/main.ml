(* @author *)
open Project_code.Game
open Project_code.Pin

(* global variables *)
let screen_width = 1400
let screen_height = 750

(* colors *)
let purple = 0x6667ab
let pink = 0xf18aad
let red = 0xe8503f
let orange = 0xff9e54
let yellow = 0xfff475
let green = 0x8bc28c

(* values *)
let player_first = ref false
let user_code_ref = ref (Array.make 4 0)
let user_feedback_ref = ref (Array.make 4 "")
let index_ref = ref 0

(* variant type representing which window of the GUI is displayed *)
type screen =
  | Title
  | Algorithm
  | Game
  | PlayerSelection
  | RoundScreen
  | Help
  | GetUserScreen

let map_int_to_color i =
  if i = 1 then yellow
  else if i = 2 then green
  else if i = 3 then red
  else if i = 4 then purple
  else if i = 5 then pink (* else if i = 0 then 0x685044 *)
  else if i = 6 then orange
  else 0x685044

(* mutable reference to the current screen *)
let curr_screen = ref Title

(* setting up array to store user inputs to send to backend *)
let user_inputs = ref [] (* algorithm, player order, rounds *)
let game : Gamerecord.game option ref = ref None

(** draw a button containing [text] at position ([x], [y]), with [color] and
    [text_color] *)
let draw_button text x y w h color text_color =
  Graphics.moveto (x + 10) (y + 10);
  Graphics.set_color color;
  Graphics.fill_rect x y w h;
  Graphics.set_color text_color;
  Graphics.set_text_size 24;
  Graphics.draw_string text

(** [store_in_backend lst] takes a list containing details about the game and
    creates an instance of Gameboard *)
let store_in_backend lst =
  match lst with
  | [ player; rounds; algo ] ->
      print_endline ("rounds" ^ rounds);
      Gamerecord.make_game (int_of_string rounds) algo player
  | _ -> failwith "Error with input"

(** draw the background *)
let draw_details () =
  Graphics.set_color 0xf9dec9;
  Graphics.fill_rect 0 0 screen_width screen_height

(** clear everything on the graph and draw the background *)
let clear_graph () =
  Graphics.clear_graph ();
  draw_details ()

(** draw the text on the title screen *)
let draw_title_text () =
  Graphics.moveto ((screen_width / 2) - 25) ((screen_height / 2) + 125);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 100000;
  Graphics.draw_string "OCAML MASTERMIND"

(** draw the start and help buttons on the title screen *)
let draw_title_buttons () =
  let button_x = (screen_width / 2) - 100 in
  let button_y = (screen_height / 2) - 100 in
  let button_width = 150 in
  let button_height = 50 in
  let button_color1 = 0xaec5eb in
  let button_color2 = 0xe9afa3 in
  let text_color = 0x3a405a in
  draw_button "Start (press 's')" button_x button_y button_width button_height
    button_color1 text_color;
  draw_button "Help (press 'h')" button_x
    (button_y + button_height + 50)
    button_width button_height button_color2 text_color

(** draw the title screen *)
let draw_title_screen () =
  draw_details ();
  draw_title_text ();
  draw_title_buttons ();

  let key = Graphics.read_key () in
  if key = 's' then (
    clear_graph ();
    curr_screen := PlayerSelection)
  else if key = 'h' then (
    clear_graph ();
    curr_screen := Help)
  else ()

(** draw text and buttons on player screen *)
let draw_player_text_and_buttons () =
  Graphics.moveto ((screen_width / 2) - 25) ((screen_height / 2) + 125);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 48;
  Graphics.draw_string "Select your player:";

  let button_x = (screen_width / 2) - 100 in
  let button_y = (screen_height / 2) - 100 in
  let button_width = 200 in
  let button_height = 50 in
  let button_color1 = 0xaec5eb in
  let button_color2 = 0xe9afa3 in
  let text_color = 0x3a405a in
  draw_button "Player 1 (press '1')" button_x button_y button_width
    button_height button_color1 text_color;
  draw_button "Player 2 (press '2')"
    (button_x + button_width + 50)
    button_y button_width button_height button_color2 text_color

(** draw screen where player selects who starts *)
let draw_player_selection_screen () =
  draw_details ();
  draw_player_text_and_buttons ();

  let key = Graphics.read_key () in
  if key = '1' then (
    clear_graph ();
    user_inputs := "player" :: !user_inputs;
    player_first := true;
    curr_screen := RoundScreen)
  else if key = '2' then (
    clear_graph ();
    user_inputs := "computer" :: !user_inputs;
    player_first := false;
    curr_screen := RoundScreen)
  else ()

let draw_round_buttons () =
  let button_width = 100 in
  let button_height = 50 in
  let button_spacing = 50 in
  let start_x = (screen_width / 2) - (button_spacing * 2) in
  let start_y = screen_height / 2 in
  let button_color = 0xaec5eb in
  let text_color = 0x3a405a in

  for i = 1 to 5 do
    let x = start_x + ((i - 1) * (button_width + button_spacing)) in
    let y = start_y in
    draw_button (string_of_int i) x y button_width button_height button_color
      text_color
  done

let draw_round_selection_screen () =
  draw_details ();

  Graphics.moveto ((screen_width / 2) - 25) ((screen_height / 2) + 125);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 48;
  Graphics.draw_string
    "Select number of rounds (press the corresponding number): ";

  draw_round_buttons ();

  let key = Graphics.read_key () in
  if key = '1' || key = '2' || key = '3' || key = '4' || key = '5' then (
    clear_graph ();
    user_inputs := String.make 1 key :: !user_inputs;
    curr_screen := Algorithm)
  else ()

(** [is_valid_length code] checks that [code] is of the correct length *)
let is_valid_length code = Array.length code = 4

(** [is_valid code] checks that [code] has no duplicates *)
let valid_code code =
  let rec aux seen = function
    | [] -> true
    | c :: rest -> if List.mem c seen then false else aux (c :: seen) rest
  in
  aux [] (Array.to_list code)

(* * get the answer from the user *)

let rec get_user_code () =
  Graphics.moveto ((screen_width / 2) - 200) ((screen_height / 2) + 200);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 24;
  Graphics.draw_string "Enter your code (4 digits, no duplicates):";
  index_ref := 0;

  let rec input_loop () =
    if !index_ref < 4 then
      let key = Graphics.read_key () in
      if key >= '0' && key <= '9' then
        let digit = Char.code key - Char.code '0' in
        if not (Array.mem digit !user_code_ref) then (
          !user_code_ref.(!index_ref) <- digit;
          index_ref := !index_ref + 1;
          Graphics.moveto
            ((screen_width / 2) + (50 * (!index_ref - 2)))
            (screen_height / 2);
          Graphics.set_color Graphics.black;
          Graphics.draw_string (String.make 1 key);
          input_loop ())
        else (
          Graphics.moveto ((screen_width / 2) - 200) ((screen_height / 2) + 250);
          Graphics.set_color Graphics.red;
          Graphics.draw_string "Invalid input. Please try again.";
          input_loop ())
      else input_loop ()
    else
      let valid_input =
        is_valid_length !user_code_ref && valid_code !user_code_ref
      in
      if valid_input then (
        Gamerecord.set_answer (Option.get !game) !user_code_ref;
        index_ref := 0;
        curr_screen := Game)
      else (
        Graphics.moveto ((screen_width / 2) - 200) ((screen_height / 2) + 250);
        Graphics.set_color Graphics.red;
        Graphics.draw_string
          "Invalid input. Code must be 4 digits with no duplicates.";
        Array.iteri (fun i _ -> !user_code_ref.(i) <- 0) !user_code_ref;
        index_ref := 0;
        get_user_code ())
  in
  input_loop ()

(** draw the boards for the game and balls *)
let draw_board () =
  (* brown board*)
  Graphics.set_color 0xb9998a;
  Graphics.fill_rect 100 80 600 600;
  Graphics.set_color 0x685044;
  Graphics.draw_rect 100 80 600 600;

  (* white board *)
  Graphics.set_color 0xffffff;
  Graphics.fill_rect 900 ((screen_height / 2) - 125) 400 250;
  Graphics.set_color 0X685044;
  Graphics.draw_rect 150 130 300 500;
  Graphics.draw_rect 500 130 150 500

(** draw the balls to represent the inputs *)
let draw_circles circle_x circle_y_start circle_spacing =
  let circle_radius = 25 in
  Graphics.set_color purple;
  Graphics.fill_circle circle_x circle_y_start circle_radius;
  Graphics.set_color pink;
  Graphics.fill_circle (circle_x + circle_spacing) circle_y_start circle_radius;
  Graphics.set_color orange;
  Graphics.fill_circle
    (circle_x + (2 * circle_spacing))
    circle_y_start circle_radius;

  (* Draw the second row of circles *)
  Graphics.set_color yellow;
  Graphics.fill_circle circle_x (circle_y_start + circle_spacing) circle_radius;
  Graphics.set_color green;
  Graphics.fill_circle
    (circle_x + circle_spacing)
    (circle_y_start + circle_spacing)
    circle_radius;
  Graphics.set_color red;
  Graphics.fill_circle
    (circle_x + (2 * circle_spacing))
    (circle_y_start + circle_spacing)
    circle_radius

(** add label text to the input balls *)
let draw_circle_texts circle_x circle_y_start circle_spacing =
  Graphics.set_color Graphics.black;
  Graphics.moveto (circle_x - 5) (circle_y_start - 12);
  Graphics.draw_string "4";
  Graphics.moveto (circle_x + circle_spacing - 5) (circle_y_start - 12);
  Graphics.draw_string "5";
  Graphics.moveto (circle_x + (2 * circle_spacing) - 5) (circle_y_start - 12);
  Graphics.draw_string "6";
  Graphics.moveto (circle_x - 5) (circle_y_start + circle_spacing - 12);
  Graphics.draw_string "1";
  Graphics.moveto
    (circle_x + circle_spacing - 5)
    (circle_y_start + circle_spacing - 12);
  Graphics.draw_string "2";
  Graphics.moveto
    (circle_x + (2 * circle_spacing) - 5)
    (circle_y_start + circle_spacing - 12);
  Graphics.draw_string "3"

(* let draw_guess_pin guess i = let x = ref 170 in for j = 0 to 4 do
   Graphics.fill_circle (map_int_to_color guess.(j)); Graphics.set_color
   guess.(j); done; *)

(* get user input based for a guess *)
let get_feedback key =
  while key <> 's' do
    if !index_ref < 4 then (
      let key = Graphics.read_key () in
      if key = 'r' || key = 'w' || key = 'n' then
        !user_feedback_ref.(!index_ref) <- String.make 1 key;
      index_ref := !index_ref + 1;
      Graphics.moveto (screen_width / 4) (screen_height / 4);
      Graphics.set_color Graphics.black;
      Graphics.draw_string (String.make 1 key))
  done;
  index_ref := 0

let get_user_guess key =
  while key <> 's' do
    if !index_ref < 4 then (
      let key = Graphics.read_key () in
      if key >= '0' && key <= '9' then
        !user_feedback_ref.(!index_ref) <- String.make 1 key;
      index_ref := !index_ref + 1;
      Graphics.moveto
        ((screen_width / 2) + (50 * (!index_ref - 2)))
        (screen_height / 2);
      Graphics.set_color Graphics.black;
      Graphics.draw_string (String.make 1 key))
  done;
  index_ref := 0

(*################# GRACE TODO ################################ *)
let do_updates key = if !player_first then get_feedback key else ()

(** draw the game screen *)
let draw_game_screen () =
  draw_details ();

  (* if !player_first then Gamerecord.set_answer (Option.get !game)
     !user_code_ref else (); *)

  (* background test *)
  Graphics.moveto ((screen_width / 2) + 300) ((screen_height / 2) + 300);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 48;
  Graphics.draw_string "Play Game!";

  draw_board ();
  Graphics.set_color 0x000000;
  let circle_x = 1000 in
  let circle_y_start = (screen_height / 2) - 75 in
  let circle_spacing = 100 in
  draw_circles circle_x circle_y_start circle_spacing;
  draw_circle_texts circle_x circle_y_start circle_spacing;

  Gamerecord.update_computer_board (Option.get !game)
    (Gamerecord.get_turn (Option.get !game));

  let board = Gamerecord.show_board (Option.get !game) in
  (* let row = Gamerecord.get_round (Option.get !game) in *)
  Array.iteri
    (fun j lst ->
      Array.iteri
        (fun i value ->
          let x = 150 + (i * 50) in
          let y = 130 + (j * 25) in
          Graphics.set_color (map_int_to_color value);
          Graphics.fill_circle x y 10)
        lst)
    board;

  let pin_board = Gamerecord.show_pins (Option.get !game) in
  Array.iteri
    (fun j lst ->
      Array.iteri
        (fun i value ->
          let x = 550 + (i * 50) in
          let y = 130 + (j * 50) in
          Graphics.set_color (map_int_to_color value);
          Graphics.fill_circle x y 5)
        lst)
    pin_board;

  let key = (Graphics.wait_next_event [ Graphics.Key_pressed ]).key in
  do_updates key

(** choose algorithm and move screen *)
let choose_algo () =
  let to_screen = if !player_first then GetUserScreen else Game in
  let key = Graphics.read_key () in
  if key = 'p' then (
    clear_graph ();
    user_inputs := "Random" :: !user_inputs;
    game := Some (store_in_backend (!user_inputs |> List.rev));
    curr_screen := to_screen)
  else if key = 'k' then (
    clear_graph ();
    user_inputs := "Knuth" :: !user_inputs;
    game := Some (store_in_backend (!user_inputs |> List.rev));
    curr_screen := to_screen)
  else ()

(** draw screen to select algorithm *)
let draw_algo_screen () =
  draw_details ();

  Graphics.moveto ((screen_width / 2) - 25) ((screen_height / 2) + 125);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 48;
  Graphics.draw_string "Choose an Algorithm to play against!";

  let button_width = 200 in
  let button_height = 50 in
  let button_spacing = 50 in
  let start_x = (screen_width / 2) - (button_width + button_spacing) in
  let start_y = screen_height / 2 in
  let button_color = 0xe9afa3 in
  let text_color = 0x3a405a in

  draw_button "Pseudo Randomizer (press 'p')" start_x start_y button_width
    button_height button_color text_color;
  draw_button "Knuth Algorithm (press 'k')"
    (start_x + button_width + button_spacing)
    start_y button_width button_height button_color text_color;

  let to_screen = if !player_first then GetUserScreen else Game in
  let key = Graphics.read_key () in
  if key = 'p' then (
    clear_graph ();
    user_inputs := "Random" :: !user_inputs;
    game := Some (store_in_backend (!user_inputs |> List.rev));
    curr_screen := to_screen)
  else if key = 'k' then (
    clear_graph ();
    user_inputs := "Knuth" :: !user_inputs;
    game := Some (store_in_backend (!user_inputs |> List.rev));
    curr_screen := to_screen)
  else ()

let draw_help_screen () =
  draw_details ();

  Graphics.moveto ((screen_width / 2) - 550) ((screen_height / 2) + 200);
  Graphics.set_color 0x3a405a;
  Graphics.set_text_size 48;
  Graphics.draw_string "Rules of OCaml Mastermind";

  let button_x = (screen_width / 2) + 200 in
  let button_y = (screen_height / 2) + 200 in
  let button_width = 200 in
  let button_height = 50 in
  let button_color = 0xaec5eb in
  let text_color = 0x3a405a in
  draw_button "Back to Home (press 's')" button_x button_y button_width
    button_height button_color text_color;

  let text_x = 50 in
  let text_y = (screen_height / 2) + 100 in
  let text_width = screen_width - 100 in
  let text_height = 13 * 30 in
  Graphics.set_color 0x3a405a;
  Graphics.draw_rect text_x (text_y - (12 * 30) + 15) text_width text_height;
  Graphics.set_text_size 20;
  Graphics.moveto (text_x + 10) (text_y + 10);
  Graphics.draw_string
    "1. Press keys to advance through the game. The keys shown on the buttons \
     correspond to keys that need to be pressed.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "2. The player will decide in advance how many games they will play, and \
     each game consists of two rounds.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "3. In each round, the player will either becomes the codemaker or the \
     codebreaker.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "4. The codemaker chooses a pattern of four code pegs to be the answer.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "5. The codebreaker tries to guess the pattern, in both order and color, \
     within eight to twelve turns.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "6. Each guess is made by typing a sequence of four numbers. ";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "7. No duplicates are allowed in neither the answer nor the guesses.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "8. Once placed, the codemaker provides feedback by entering feedback \
     (using the digits 2, 1, 0 in that order), which will show up as pegs in \
     the small holes of the row with the guess.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "9. A colored key peg is placed for each code peg from the guess which is \
     correct in both color and position.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "10. A white key peg indicates a code peg that belongs in the solution, \
     but is incorrectly positioned.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string
    "11. Once feedback is provided, another guess is made; guesses and \
     feedback continue to alternate until either the codebreaker guesses \
     correctly, or all rows on the decoding board are full.";
  Graphics.moveto (text_x + 10) (Graphics.current_y () - 30);
  Graphics.draw_string "12. The winner is the one who wins the most rounds";

  let key = Graphics.read_key () in
  if key = 's' then (
    clear_graph ();
    curr_screen := Title)
  else ()

(* *)
let rec run_mastermind () =
  (* try *)
  (Graphics.open_graph
     (" " ^ string_of_int screen_width ^ "x" ^ string_of_int screen_height);
   match !curr_screen with
   | Title -> draw_title_screen ()
   | PlayerSelection -> draw_player_selection_screen ()
   | RoundScreen -> draw_round_selection_screen ()
   | Algorithm -> draw_algo_screen ()
   | GetUserScreen -> get_user_code ()
   | Game -> draw_game_screen ()
   | Help -> draw_help_screen ());
  run_mastermind ()
(* with exn -> print_endline "Thanks for playing!" *)

let () = run_mastermind ()
