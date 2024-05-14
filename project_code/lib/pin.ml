(** This is a module implementation for an array of pins, which are indicative
    of a guess's progress in comparison to the answer. The pins are sorted from
    Red to White to Null, but are not indicative of the order of the places of
    each choice itself.

    Red indicates an arbitrary number from the guess is indeed in the answer and
    in the right place. White indicates an arbitrary number from the guess is
    not in the right place, but in the answer. Null indicates a number is not in
    the answer.

    For instance, if the guess was [[1,2,3,4]], and the answer was [[7,2,5,3]],
    the players would receive the pins [[Red, White, Null, Null]]*)

module Pin = struct
  type pin =
    | Red
    | White
    | Null

  let make_pins (arrayGuess : int array) (arrayAnswer : int array) =
    let pinArray = Array.make 4 Null in
    let ind = ref 0 in
    for i = 0 to 3 do
      if arrayGuess.(i) = arrayAnswer.(i) then
        let _ = pinArray.(!ind) <- Red in
        ind := !ind + 1
      else if Array.mem arrayGuess.(i) arrayAnswer then
        let _ = pinArray.(!ind) <- White in
        ind := !ind + 1
      else
        let _ = pinArray.(!ind) <- Null in
        ind := !ind
    done;
    pinArray

  let rec count_reds_help arr int acc =
    if int = 4 then acc
    else
      match arr.(int) with
      | Red -> count_reds_help arr (int + 1) (acc + 1)
      | White | Null -> count_reds_help arr (int + 1) acc

  let count_reds arr = count_reds_help arr 0 0

  let rec to_string_help arr acc int =
    if int = 4 then acc
    else
      match arr.(int) with
      | Red -> to_string_help arr (acc ^ "R") (int + 1)
      | White -> to_string_help arr (acc ^ "W") (int + 1)
      | Null -> to_string_help arr (acc ^ "N") (int + 1)

  (** [to_string arr] Converts an [arr] of all type [pin] to a string.*)
  let to_string_pin arr = to_string_help arr "" 0
end