open Pin
open Random_guessing_algorithm

type game_record = {
  game_board : int array array;
  pin_board : int array array;
  total_rounds : int;
  algorithm : string;
  player : string;
  mutable round_number : int;
  mutable turn_number : int;
  mutable answer : int array;
}

module type Gameboard = sig
  type game = game_record

  val make_game : int -> string -> string -> game
  (** [make_game rounds algo player] makes initializes a game instance that will
      run [rounds] amount of rounds. The computer's guesses will feature the
      [algo] algorithm, and [player] is the starting player. *)

  val set_answer : game -> int array -> unit
  (** [set_answer game] sets the answer in [game] for the round *)

  val set_computer_answer : game -> unit
  (** [set_computer_answer game] sets the answer in [game] for the round *)

  val get_turn : game -> int
  (** [get_turn game] returns the current turn number. *)

  val show_board : game -> int array array
  (** [show_board game] returns the board of [game] *)

  val show_pins : game -> int array array
  (** [show_pins game] returns the pin board of [game] *)

  val update_board : game -> int array -> unit
  (** [update_board game guess] updates the next empty row of the [game] board
      with the guess array. *)

  val update_feedback : game -> int array -> unit
  (** [update_feedback game guess] updates the next empty row of the [game] pin
      board with the calculated feedback array based on [guess] *)

  val update_game : game -> int array -> unit
  (** [update_game game guess] updates the values [game] based on the user's
      [guess] *)

  val clear_board : game -> unit
  (** [clear_board game] resets all values in [game] for the next round *)

  val update_computer_board : game -> int array
  (** [update_computer_board] updates the board with the pins and feedback *)

  val check_feedback : string -> game -> bool
  (** [check_feedback feedback game] checks that the user feedback for the last
      round of the game is correct *)

  val int_array_to_string : int array -> string
  (** [int_array_to_string] converts int array to string*)

  val get_latest_feedback : game -> int array -> int array
  (** [get_latest_guess game] returns the row of the latest guess in [game]'s
      feedback board *)

  val give_motivation : game -> int array -> string
  (** [give_motivation] returns a message based on the feedback and counts of
      the pins*)
end

module Gamerecord : Gameboard = struct
  (* represents the playing board *)
  type game = game_record

  (** [make_game rounds algo player] makes initializes a game instance that will
      run [rounds] amount of rounds. The computer's guesses will feature the
      [algo] algorithm, and [player] is the starting player. *)
  let make_game rounds algo player =
    {
      game_board = Array.init 12 (fun _ -> Array.init 4 (fun _ -> 0));
      pin_board = Array.init 12 (fun _ -> Array.init 4 (fun _ -> 3));
      total_rounds = rounds;
      algorithm = algo;
      player;
      round_number = 0;
      turn_number = 0;
      answer = Array.make 4 0;
    }

  (** [get_turn game] returns the number of rounds played. *)
  let get_turn game : int = game.turn_number

  (** [update_board game guess] updates the next empty row of the [game] board
      with the guess array. *)
  let update_board board guess =
    if board.turn_number < 12 then
      Array.set board.game_board board.turn_number (Array.copy guess)
    else ()

  (** [get_latest_guess game] returns the row of the latest guess in [game]'s
      game board *)
  let get_latest_guess game =
    let ind = game.turn_number in
    game.game_board.(ind)

  (** [update_feedback game guess] updates the next empty row of the [game] pin
      board with the calculated feedback array based on [guess] *)
  let update_feedback game guess =
    let feedback =
      PinModule.to_int_array (PinModule.make_pins guess game.answer)
    in
    if game.turn_number < 12 then (
      Array.set game.pin_board game.turn_number feedback;
      game.turn_number <- game.turn_number + 1)
    else ()

  (** [update_game game guess] updates the values of [game] based on the user's
      [guess] *)
  let update_game game guess =
    let () = update_board game guess in
    update_feedback game guess

  (** [show_board game] returns the board of [game] *)
  let show_board game = game.game_board

  (** [show_pins game] returns the pin board of [game] *)
  let show_pins game = game.pin_board

  (** [set_answer game] sets the answer in [game] for the round *)
  let set_answer game answer = game.answer <- answer

  (** [clear_board game] resets all values in [game] for the next round *)
  let clear_board game =
    Array.iter
      (fun row -> Array.iteri (fun i _ -> row.(i) <- 0) row)
      game.game_board;
    Array.iter
      (fun row -> Array.iteri (fun i _ -> row.(i) <- 3) row)
      game.pin_board;
    game.turn_number <- 0;
    game.round_number <- game.round_number + 1;
    set_answer game [| 0; 0; 0; 0 |]

  let set_computer_answer game = game.answer <- Array.of_list (make_guess ())

  (** [check_feedback feedback game] checks that the user feedback for the last
      round of the game is correct *)
  let check_feedback feedback game =
    let guess = get_latest_guess game in
    PinModule.check_validation feedback guess game.answer

  (** [get_latest_guess game] returns the row of the latest guess in [game]'s
      feedback board *)
  let get_latest_feedback game guess =
    PinModule.to_int_array (PinModule.make_pins guess game.answer)

  let update_computer_board game =
    if (* Player made the code first *)
       game.algorithm = "Random" then (
      let guess = make_guess () in
      let guess_array = Array.of_list guess in
      update_board game guess_array;
      guess_array)
    else failwith "Error"

  let int_array_to_string arr =
    let result = ref "" in
    let sep = ref "" in
    Array.iter
      (fun x ->
        result := !result ^ !sep ^ string_of_int x;
        sep := ",")
      arr;
    !result

  let give_motivation game guess =
    let a = PinModule.count_reds (PinModule.make_pins guess game.answer) in
    let b = PinModule.count_whites (PinModule.make_pins guess game.answer) in
    let c = PinModule.count_nulls (PinModule.make_pins guess game.answer) in
    match (a, b, c) with
    | 4, 0, 0 -> "great job!!"
    | 3, 0, 1 -> "so close.."
    | 2, 2, 0 -> "ooooo so close"
    | 2, 1, 1 -> "hmm what next?"
    | 2, 0, 2 -> "intresting..."
    | 1, 3, 0 -> "not bad!"
    | 1, 2, 1 -> "u can do this!"
    | 1, 1, 2 -> "uhmmmm.."
    | 0, 4, 0 -> "almost there!"
    | 0, 3, 1 -> "awwww"
    | 0, 2, 2 -> "u can do better :("
    | _ -> "doing great!"
end
