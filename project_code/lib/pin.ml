module type PinType = sig
  type pin

  val make_pins : int array -> int array -> pin array
  val count_reds : pin array -> int
  val count_whites : pin array -> int
  val count_nulls : pin array -> int
  val to_string_pin : pin array -> string
  val all_colors : pin array -> int * int * int
  val check_validation : string -> int array -> int array -> bool
  val to_int_array : pin array -> int array
  val list_to_string : string list -> string
end

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

module PinModule : PinType = struct
  (** AF : [pin] represents a progress mark for a guess with respect to answer.
      [Red] means a color is in the right place and in the answer. [White] means
      a color is in the answer but not in the right place. [Null] means that the
      color isn't in the answer. *)
  type pin =
    | Red
    | White
    | Null

  let pin_to_int = function
    | Red -> 0
    | White -> 1
    | Null -> 2

  let int_to_pin i = if i = 0 then Red else if i = 1 then White else Null

  (** [to_int_array arr] converts a pin [arr] to an int array. *)
  let to_int_array arr = Array.map pin_to_int arr

  let to_pin_array arr = Array.map int_to_pin arr

  (** [make_pins array_guess array_answer] Takes an an [int array] of
      [array_guess] and an [int array] of [array_answer] and creates a
      [pin_array] representative of the progress of the guess with respect to
      the answer. *)
  let make_pins (array_guess : int array) (array_answer : int array) =
    let pinArray = Array.make 4 Null in
    let ind = ref 0 in
    for i = 0 to 3 do
      if array_guess.(i) = array_answer.(i) then (
        pinArray.(!ind) <- Red;
        ind := !ind + 1)
      else if Array.mem array_guess.(i) array_answer then (
        pinArray.(!ind) <- White;
        ind := !ind + 1)
      else pinArray.(!ind) <- Null;
      ind := !ind
    done;
    let int_version = to_int_array pinArray in
    Array.sort
      (fun x y -> if x < y then -1 else if x > y then 1 else 0)
      int_version;
    to_pin_array int_version

  (** [all_colors arr] takes a pin array [arr] and returns a 3 size array of the
      multiplicity of each color. [0] -> [Red] [1] -> [White] [2] -> [Null]*)
  let all_colors arr =
    let acc = Array.make 3 0 in
    for i = 0 to 3 do
      match arr.(i) with
      | Red -> acc.(0) <- acc.(0) + 1
      | White -> acc.(1) <- acc.(1) + 1
      | Null -> acc.(2) <- acc.(2) + 1
    done;
    (acc.(0), acc.(1), acc.(2))

  (** [count_reds arr] counts the number of [Red] constructors in [arr]. *)
  let count_reds arr =
    let r, _, _ = all_colors arr in
    r

  (** [count_whites arr] counts the number of [White] constructors in [arr]. *)

  let count_whites arr =
    let _, w, _ = all_colors arr in
    w

  (** [count_nulls arr] counts the number of [Nulls] constructors in [arr]. *)

  let count_nulls arr =
    let _, _, n = all_colors arr in
    n

  let rec to_string_help arr acc int =
    if int = 4 then acc
    else
      match arr.(int) with
      | Red -> to_string_help arr (acc ^ "R") (int + 1)
      | White -> to_string_help arr (acc ^ "W") (int + 1)
      | Null -> to_string_help arr (acc ^ "N") (int + 1)

  (** [to_string_pin arr] converts an [arr] of all type [pin] to a string.*)
  let to_string_pin arr = to_string_help arr "" 0

  let char_to_pin c =
    if c = 'R' || c = 'r' then Red
    else if c = 'W' || c = 'w' then White
    else Null

  let rec check_chars str n l =
    if n = String.length str then true
    else if List.mem str.[n] l then check_chars str (n + 1) l
    else false

  (** [check_validation str guess answer] converts a [str] with
      'r','w','n','R','W','N' characters into a pin array, asserting it indeed
      matches with the pin algorithm set up, based on [guess] and [answer]. *)
  let check_validation str guess answer =
    if String.length str != 4 then false
    else if check_chars str 0 [ 'R'; 'r'; 'W'; 'w'; 'N'; 'n' ] = false then
      false
    else
      let pin_array1 = Array.make 4 Null in
      for i = 0 to 3 do
        pin_array1.(i) <- char_to_pin str.[i]
      done;
      let pin_array2 = make_pins guess answer in
      pin_array1 = pin_array2

  (* using this in main *)
  let rec list_to_string = function
    | [] -> ""
    | h :: t -> h ^ list_to_string t
end
