let screen_width = 1400
let screen_height = 750

type screen =
  | Title
  | Algorithm
  | Game
  | PlayerSelection
  | RoundScreen

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
  clear_graph ();
  (* Draw title screen elements *)
  Graphics.moveto 100 200;
  Graphics.draw_string "OCaml Mastermind";
  draw_button "Start New Game" 300 100 50 50 Graphics.blue;
  if Graphics.read_key () = 's' then curr_screen := PlayerSelection else ()

  let draw_player_selection_screen () =
    clear_graph ();
    Graphics.moveto (screen_width / 2) (screen_height / 3);
    Graphics.draw_string "Select your player:";
    draw_button "Player 1" 300 100 50 50 Graphics.blue;
    draw_button "Player 2" 300 200 50 50 Graphics.blue;
    if Graphics.read_key () = '1' then curr_screen := Game else
    if Graphics.read_key () = '2' then curr_screen := Game else ()
  

let draw_algo_screen () =
  Graphics.clear_graph ();
  (* Draw algorithm screen elements *)
  Graphics.moveto (screen_width / 2) (screen_height / 3 * 2);
  Graphics.draw_string "Choose an Algorithm to play against!";
  draw_button "Knuth Algorithm" 300 100 50 50 Graphics.blue;
  draw_button "Genetic Algorithm" 300 100 50 50 Graphics.blue;
  if Graphics.read_key () = 'g' then curr_screen := Game else ()

let draw_game_screen () =
  Graphics.clear_graph ();
  draw_details ();
  draw_board ()
(* fetch the board information from the backend *)

(* let draw_help_screen () = Graphics.clear_graph (); draw_details ();
   draw_board () *)

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
(* | Help -> draw_help_screen (); run_mastermind () *)

(* let () = Graphics.open_graph (" " ^ string_of_int screen_width ^ "x" ^
   string_of_int screen_height); draw_details (); ignore (Graphics.read_key ());
   Graphics.close_graph () *)

let () = run_mastermind ()
