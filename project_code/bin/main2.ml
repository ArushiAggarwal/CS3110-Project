let screen_width = 1400
let screen_height = 750

type screen =
  | Title
  | Algorithm
  | Game
  | PlayerSelection
  | RoundScreen
  | Help

let curr_screen = ref Title

let draw_button text x y w h color =
  Graphics.moveto x y;
  Graphics.set_color color;
  Graphics.fill_rect x y w h;
  Graphics.draw_string text

let draw_details () =
  Graphics.set_color 0x8cd9ff;
  Graphics.fill_rect 0 0 screen_width screen_height

let clear_graph () =
  Graphics.clear_graph ();
  draw_details ()

let draw_board () =
  Graphics.set_color 0x997950;
  Graphics.fill_rect 100 100 100 400

let draw_title_screen () =
  (* Draw title screen elements *)
  Graphics.moveto 100 200;
  Graphics.draw_string "OCaml Mastermind";
  draw_button "Start New Game" 300 100 50 50 Graphics.blue;
  if Graphics.read_key () = 's' then curr_screen := PlayerSelection else ()

let draw_player_selection_screen () =
  Graphics.moveto (screen_width / 2) (screen_height / 3);
  Graphics.draw_string "Select your player:";
  draw_button "Player 1" 300 100 50 50 Graphics.blue;
  draw_button "Player 2" 300 200 50 50 Graphics.blue;
  if Graphics.read_key () = '1' then curr_screen := RoundScreen
  else if Graphics.read_key () = '2' then (
    clear_graph ();
    curr_screen := RoundScreen)
  else ()

let draw_round_selection_screen () =
  Graphics.moveto (screen_width / 2) (screen_height / 3);
  Graphics.draw_string "Select number of rounds:";
  let button_width = 50 in
  let button_height = 50 in
  let button_spacing = 100 in
  let start_x = (screen_width / 2) - (button_spacing * 2) in
  let start_y = screen_height / 2 in
  for i = 1 to 5 do
    let x = start_x + ((i - 1) * button_spacing) in
    let y = start_y in
    draw_button (string_of_int i) x y button_width button_height Graphics.blue;
    if Graphics.read_key () = Char.chr (i + Char.code '0') then (
      clear_graph ();
      curr_screen := Game)
    else ()
  done

let draw_algo_screen () =
  (* Draw algorithm screen elements *)
  Graphics.moveto (screen_width / 2) (screen_height / 3 * 2);
  Graphics.draw_string "Choose an Algorithm to play against!";
  draw_button "Knuth Algorithm" 300 100 50 50 Graphics.blue;
  draw_button "Genetic Algorithm" 300 100 50 50 Graphics.blue;
  if Graphics.read_key () = 'g' then (
    clear_graph ();
    curr_screen := Game)
  else ()

let draw_game_screen () =
  Graphics.clear_graph ();
  draw_details ();
  draw_board ()
(* fetch the board information from the backend *)

let draw_help_screen () =
  Graphics.clear_graph ();
  draw_details ();
  draw_board ()

let rec run_mastermind () =
  Graphics.open_graph
    (" " ^ string_of_int screen_width ^ "x" ^ string_of_int screen_height);
  match !curr_screen with
  | Title ->
      draw_title_screen ();
      run_mastermind ()
  | Algorithm ->
      draw_algo_screen ();
      run_mastermind ()
  | Game ->
      draw_game_screen ();
      run_mastermind ()
  | Help ->
      draw_help_screen ();
      run_mastermind ()

let () =
  Graphics.open_graph
    (" " ^ string_of_int screen_width ^ "x" ^ string_of_int screen_height);
  draw_details ();
  ignore (Graphics.read_key ());
  Graphics.close_graph ()

let () = run_mastermind ()
