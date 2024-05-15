module type PinType = sig
  type pin

  val make_pins : int array -> int array -> pin array
  val count_reds : pin array -> int
  val to_string_pin : pin array -> string
end

module PinModule : PinType
