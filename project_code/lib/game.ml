open Pin

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

module GameBoard = struct
  (* represents the playing board *)
  type game = game_record

  (** [make_game rounds algo player] makes initializes a game instance that will
      run [rounds] amount of rounds. The computer's guesses will feature the
      [algo] algorithm, and [player] is the starting player. *)
  let make_game rounds algo player =
    {
      game_board = Array.init 12 (fun _ -> Array.init 4 (fun _ -> 0));
      pin_board = Array.init 12 (fun _ -> Array.init 4 (fun _ -> 0));
      answer = Array.make 4 0;
      round_number = 0;
      total_rounds = rounds;
      algorithm = algo;
      player;
      turn_number = 0;
    }

  let get_round game : int = game.round_number

  (** [update_board guess board] updates the next empty row of the game board
      with the guess array *)
  let update_board guess board =
    if board.turn_number < 12 then (
      board.turn_number <- board.turn_number + 1;
      Array.set board.game_board board.turn_number guess)
    else ()

  let update_feedback guess game =
    let feedback =
      PinModule.to_int_array (PinModule.make_pins guess game.answer)
    in
    if game.turn_number < 12 then
      Array.set game.pin_board game.turn_number feedback
    else ()

  let show_board game = game.game_board
  let show_pins game = game.pin_board

  let clear_board game =
    Array.iter
      (fun row -> Array.iteri (fun i _ -> row.(i) <- 0) row)
      game.game_board;
    Array.iter
      (fun row -> Array.iteri (fun i _ -> row.(i) <- 0) row)
      game.pin_board;
    game.turn_number <- 0;
    game.round_number <- game.round_number + 1

  let set_answer game answer = game.answer <- answer
end
