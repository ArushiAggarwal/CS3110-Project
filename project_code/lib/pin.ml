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

  let makePins arrayGuess arrayAnswer =
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
end
