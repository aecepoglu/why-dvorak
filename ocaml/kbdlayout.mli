type hand = Left | Right
type finger = Pinky | Ring | Middle | Index | Thumb

type data_t

type lookup_t = (char, (hand * finger * int)) Hashtbl.t

val string_of_hand : hand -> string

val string_of_finger : finger -> string

val sample_colemak_data : data_t
val sample_dvorak_data : data_t
val sample_qwerty_data : data_t

val lookup_of_data : data_t -> lookup_t

val view : ?highlit_key:char option -> ?in_edit:bool -> onremove:(Vdom.mouse_event -> 'a) -> string -> data_t -> 'a Vdom.vdom
