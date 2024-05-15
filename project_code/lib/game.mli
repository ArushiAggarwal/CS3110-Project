type game_record = {
  game_board : int array array;
  pin_board : int array array;
  mutable answer : int array;
  mutable round_number : int;
  total_rounds : int;
  algorithm : string;
  player : string;
  mutable turn_number : int;
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
