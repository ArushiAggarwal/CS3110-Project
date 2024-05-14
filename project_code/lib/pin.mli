(** A type for setting up a pin array for feedback. *)

module type Pin = sig
  type pin

  val make_pins : int array -> int array -> pin array
  val count_reds : pin array -> int
  val to_string_pin : pin array -> string
end

module Pin : Pin
