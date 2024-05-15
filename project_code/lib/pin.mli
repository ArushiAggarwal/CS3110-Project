module type PinType = sig
  type pin

  val make_pins : int array -> int array -> pin array
  (** [make_pins array_guess array_answer] Takes an an [int array] of
      [array_guess] and an [int array] of [array_answer] and creates a
      [pin_array] representative of the progress of the guess with respect to
      the answer. *)

  val count_reds : pin array -> int
  (** [count_reds arr] counts the number of [Red] constructors in [arr]. *)

  val to_string_pin : pin array -> string
  (** [to_string_pin arr] converts an [arr] of all type [pin] to a string.*)

  val to_int_array : pin array -> int array
  (** [to_int_array arr] converts an [arr] of all type [pin] to an int array.*)
end

module PinModule : PinType
