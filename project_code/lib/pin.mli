module type PinType = sig
  type pin
  (* type pin *)

  val make_pins : int array -> int array -> pin array
  (** [make_pins array_guess array_answer] Takes an an [int array] of
      [array_guess] and an [int array] of [array_answer] and creates a
      [pin_array] representative of the progress of the guess with respect to
      the answer. *)

  val count_reds : pin array -> int
  (** [count_reds arr] counts the number of [Red] constructors in [arr]. *)

  val to_string_pin : pin array -> string
  (** [to_string_pin arr] converts an [arr] of all type [pin] to a string.*)

  val all_colors : pin array -> int array
  (** [all_colors arr] takes a pin array [arr] and returns a 3 size array of the
      multiplicity of each color. [0] -> [Red] [1] -> [White] [2] -> [Null] *)

  val check_validation : string -> int array -> int array -> bool
  (** [check_validation str guess answer] converts a [str] with
      'r','w','n','R','W','N' characters into a pin array, asserting it indeed
      matches with the pin algorithm set up, based on [guess] and [answer]. *)

  val to_int_array : pin array -> int array
  (** [to_int_array arr] converts a pin [arr] to an int array. *)

  val list_to_string : string list -> string
  (** [to_int_array arr] converts a string feedback array [arr] representing
      pins to a string. *)
end

module PinModule : PinType
