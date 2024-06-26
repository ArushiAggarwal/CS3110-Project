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
  (** [update_computer_board game] updates the board with the pins and feedback *)

  val check_feedback : string -> game -> bool
  (** [check_feedback feedback game] checks that the user feedback for the last
      round of the game is correct *)

  val int_array_to_string : int array -> string
  (** [int_array_to_string arr] converts int array to string*)

  val get_latest_feedback : game -> int array -> int array
  (** [get_latest_guess game arr] returns the row of the latest guess in
      [game]'s feedback board *)

  val give_motivation : game -> int array -> string
  (** [give_motivation game guess] returns a message based on the feedback and
      counts of the pins*)
end

module Gamerecord : Gameboard
