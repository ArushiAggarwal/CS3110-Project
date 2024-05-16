open Pin
open Random_guessing_algorithm
open Donald_knuth_algorithm

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

  val get_round : game -> int
  (** [get_round game] returns the number of rounds played. *)

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
      pin_board = Array.init 12 (fun _ -> Array.init 4 (fun _ -> 0));
      total_rounds = rounds;
      algorithm = algo;
      player;
      round_number = 0;
      turn_number = 0;
      answer = Array.make 4 0;
    }

  (** [get_round game] returns the number of rounds played. *)
  let get_round game : int = game.round_number

  (** [update_board game guess] updates the next empty row of the [game] board
      with the guess array. *)
  let update_board board guess =
    if board.turn_number < 12 then (
      Array.set board.game_board board.turn_number guess;
      board.turn_number <- board.turn_number + 1)
    else ()

  (** [update_feedback game guess] updates the next empty row of the [game] pin
      board with the calculated feedback array based on [guess] *)
  let update_feedback game guess =
    let feedback =
      PinModule.to_int_array (PinModule.make_pins guess game.answer)
    in
    if game.turn_number < 12 then
      Array.set game.pin_board game.turn_number feedback
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

  (** [clear_board game] resets all values in [game] for the next round *)
  let clear_board game =
    Array.iter
      (fun row -> Array.iteri (fun i _ -> row.(i) <- 0) row)
      game.game_board;
    Array.iter
      (fun row -> Array.iteri (fun i _ -> row.(i) <- 0) row)
      game.pin_board;
    game.turn_number <- 0;
    game.round_number <- game.round_number + 1

  (** [set_answer game] sets the answer in [game] for the round *)

  (* let rec print arr = match arr with | [] -> "" | h :: t -> string_of_int h ^
     print t *)

  let set_answer game answer = game.answer <- answer

  (* let check_feedback feedback guess = let real_feedback *)

  let update_computer_board game i =
    if game.algorithm = "p" then
      let guess = generate_guess 42 in
      update_board game (Array.of_list guess)
    else if game.algorithm = "k" then
      let guess = knuth_algorithm (Array.to_list game.answer) in
      update_board game (Array.of_list (fst guess))
end
