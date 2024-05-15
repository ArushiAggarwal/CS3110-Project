(* module GameBoard = struct (* represents the playing board *) let game_board =
   Array.init 12 (fun _ -> Array.init 4 (fun _ -> 0))

   (* represents the feedback pin board *) let pin_board = Array.init 12 (fun _
   -> Array.init 4 (fun _ -> 0))

   (** [update_board guess board] *) let update_board guess board = Array.set
   board.game_board 0 guess end *)
