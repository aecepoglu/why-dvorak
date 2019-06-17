type kbd_key = S of char (* single key button *)
             | Z of int (* wide button *)

let dvorak_kbd_data = [
  [ S('`'); S('1');  S('2'); S('3'); S('4'); S('5'); S('6'); S('7'); S('8'); S('9'); S('0'); S('['); S(']');  Z(4) ];
  [ Z(3);   S('\''); S(','); S('.'); S('P'); S('Y'); S('F'); S('G'); S('C'); S('R'); S('L'); S('/'); S('=');  Z(3) ];
  [ Z(4);   S('A');  S('O'); S('E'); S('U'); S('I'); S('D'); S('H'); S('T'); S('N'); S('S'); S('-'); S('\\'); Z(2) ];
  [ Z(3);   S('<');  S(';'); S('Q'); S('J'); S('K'); S('X'); S('B'); S('M'); S('W'); S('V'); S('Z');          Z(5) ];
]

let sample_text = "A wet brown dog came running and did not bark, lifting a wet feather of a tail. The man followed in a wet black oilskin jacket, like a chauffeur, and face flushed a little. She felt him recoil in his quick walk, when he saw her. She stood up in the handbreadth of dryness under the rustic porch. He saluted without speaking, coming slowly near. She began to withdraw."
;;

let chars_of_string s =
  let rec aux i =
    if i < String.length s
    then s.[i] :: aux (i + 1)
    else []
  in
    aux 0

let string_of_chars chars =
  chars
  |> List.map (String.make 1)
  |> String.concat ""

(************************)

type text_player_state = Ready
                       | Playing of int
                       | Finished

open Vdom

type model = {
  state: text_player_state;
  passage: string;
}

let update model = function
  | `Start -> {model with state = (match model.state with
      | Ready -> Playing 0
      | s -> s
    )}
  | `NextChar -> {model with state = (match model.state with
      | Playing i when (i + 1) < String.length model.passage -> Playing (i + 1)
      | Playing _ -> Finished
      | s -> s
    )}
  | `Reset -> {model with state = (match model.state with
      | Playing _ -> Ready
      | Finished -> Ready
      | s -> s
    )}

let init = {
  state = Ready;
  passage = sample_text;
}

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let keyboard layout model =
  let key_rects = layout
                  |> List.mapi (fun row cols ->
                      (List.fold_left (fun (col, rects) key ->
                           let pos_x = col * 20 in
                           let pos_y = row * 40 in
                           let label, w = match key with
                             | S(c) -> String.make 1 c, 2
                             | Z(w) -> "", w
                           in
                           let fill = match key, model.state with
                             | Z(_), _ -> "grey"
                             | S(c), Playing i when c = (Char.uppercase_ascii model.passage.[i]) -> "yellow"
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

let view model =
  let play_button {state; _} = match state with
    | Playing _ -> button "Stop" `Reset
    | Finished -> button "Reset" `Reset
    | Ready -> button "Play" `Start
  in
  let scrolling_text {state; passage} =
    div ~a:[attr "id" "passage"] [
      match state with
      | Playing i -> text (String.sub passage i (String.length passage - i))
      | _ -> text passage
    ]
  in
    div [
      (keyboard dvorak_kbd_data model);
      (play_button model);
      (scrolling_text model);
    ]

let app = simple_app ~init ~view ~update ()

let () =
  let open Js_browser in
  let run () = Vdom_blit.run app
               |> (fun app' ->
                   let _ = Window.set_interval window
                             (fun () -> Vdom_blit.process app' `NextChar)
                             50
                   in
                     app'
                  )
               |> Vdom_blit.dom
               |> Element.append_child (match Document.get_element_by_id document "container" with
                                        | Some container -> container
                                        | None -> Document.body document
                                       )
  in
    Window.set_onload window run
