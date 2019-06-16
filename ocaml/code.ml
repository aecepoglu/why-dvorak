type kbd_key = S of char (* single key button *)
             | Z of int (* wide button *)

let dvorak_kbd_data = [
  [ S('`'); S('1');  S('2'); S('3'); S('4'); S('5'); S('6'); S('7'); S('8'); S('9'); S('0'); S('['); S(']');  Z(4) ];
  [ Z(3);   S('\''); S(','); S('.'); S('P'); S('Y'); S('F'); S('G'); S('C'); S('R'); S('L'); S('/'); S('=');  Z(3) ];
  [ Z(4);   S('A');  S('O'); S('E'); S('U'); S('I'); S('D'); S('H'); S('T'); S('N'); S('S'); S('-'); S('\\'); Z(2) ];
  [ Z(3);   S('<');  S(';'); S('Q'); S('J'); S('K'); S('X'); S('B'); S('M'); S('W'); S('V'); S('Z');          Z(5) ];
]

let chars_of_string s =
  let rec aux i =
    if i < String.length s
    then s.[i] :: aux (i + 1)
    else []
  in
    aux 0

(************************)

type text_player_state = Ready of char list
                       | Playing of char list
                       | Stopped

open Vdom

module IntMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type model = text_player_state

let update state = function
  | `Start -> (match state with
      | Ready(text) -> Playing(text)
      | s -> s
    )
  | `NextChar -> (match state with
      | Playing(_ :: rest) -> Playing(rest)
      | s -> s
    )

let init = Ready (chars_of_string "Merhaba dunyali")

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let keyboard layout state =
  let key_rects = layout
                  |> List.mapi (fun row cols ->
                      (List.fold_left (fun (col, rects) key ->
                           let pos_x = col * 20 in
                           let pos_y = row * 40 in
                           let label, w = match key with
                             | S(c) -> String.make 1 c, 2
                             | Z(w) -> "", w
                           in
                           let fill = match key, state with
                             | Z(_), _ -> "grey"
                             | S(c), Playing(c' :: _) when c = (Char.uppercase_ascii c') -> "yellow"
                             | S(_), _ -> "white"
                           in
                           let rect = svg_elt "rect" [] ~a:[int_attr "x" pos_x;
                                                            int_attr "y" pos_y;
                                                            int_attr "width" (w * 20);
                                                            int_attr "height" 40;
                                                            attr  "fill" fill;
                                                           ]
                           in
                           let text' = svg_elt "text" [text label] ~a:[int_attr "x" (pos_x + 5);
                                                                       int_attr "y" (pos_y + 15);
                                                                      ]
                           in
                             (* the order of elements determine which shows on top *)
                             (col + w), (text' :: rect :: rects)
                         ) (0, []) cols
                       |> snd
                       |> List.rev
                      )
                    )
                  |> List.flatten
  in
    svg_elt "svg" key_rects ~a:[int_attr "width" 800; int_attr "height" 300]

let view state =
  let state_indicator = function
    | Playing(chars) -> text ("PLAYING: " ^ (chars |> List.map (String.make 1) |> String.concat ""))
    | Stopped -> text ("STOPPED: ")
    | Ready(chars) -> text ("READY: " ^ (chars |> List.map (String.make 1) |> String.concat ""))
  in
    div [
      (keyboard dvorak_kbd_data state);
      (button "Tick" `NextChar);
      (button "Start" `Start);
      (state_indicator state);
    ]

let app = simple_app ~init ~view ~update ()

let () =
  let open Js_browser in
  let run () = Vdom_blit.run app
               |> Vdom_blit.dom
               |> Element.append_child (match Document.get_element_by_id document "container" with
                                        | Some container -> container
                                        | None -> Document.body document
                                       )
  in
    Window.set_onload window run

