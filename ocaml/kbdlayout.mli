type hand = Left | Right
type finger = Pinky | Ring | Middle | Index | Thumb

type key =
  | S(*single*) of char
  | H(*home*) of char * hand * finger
  | E(*empty space*)

type data_t = key list list

type lookup_t = (char, (hand * finger * int)) Hashtbl.t

val string_of_hand : hand -> string

val string_of_finger : finger -> string

val sample_dvorak_data : data_t
val sample_qwerty_data : data_t

val lookup_of_data : data_t -> lookup_t

val view : ?highlit_key:char option -> data_t -> 'a Vdom.vdom
