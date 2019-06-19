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

open Vdom

type stats = {
  same_hand: int;
  same_finger: int;
  distance: int;
}

type text_player_state = Ready
                       | Playing of (int * stats * Kbdlayout.hand * Kbdlayout.finger)
                       | Finished of stats

type model = {
  state: text_player_state;
  passage: string;
  lookup: Kbdlayout.lookup_t;
}

let update model = function
  | `Start -> {model with state = (match model.state with
      | Ready -> Playing (0, {
          same_hand = 0;
          same_finger = 0;
          distance = 0;
        }, Left, Thumb)
      | s -> s
    )}
  | `NextChar -> {model with state = (match model.state with
      | Playing (i, stats, prev_hand, prev_finger) when (i + 1) < String.length model.passage ->
         (match Hashtbl.find_opt model.lookup (Char.uppercase_ascii model.passage.[i]) with
          | Some (hand, finger, dist) ->
             Playing ((i + 1), {
                 same_hand = stats.same_hand + (if hand = prev_hand then 1 else 0);
                 same_finger = stats.same_finger + (if finger = prev_finger then 1 else 0);
                 distance = stats.distance + dist;
               }, hand, finger)
          | None ->
             Playing ((i + 1), {stats with distance = stats.distance + 1}, prev_hand, prev_finger)
         )
      | Playing (_, stats, _, _) -> Finished stats
      | s -> s
    )}
  | `Reset -> {model with state = (match model.state with
      | Playing _ -> Ready
      | Finished _ -> Ready
      | s -> s
    )}

let init = {
  state = Ready;
  passage = sample_text;
  lookup = Kbdlayout.lookup_of_data Kbdlayout.sample_dvorak_data;
}

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let keyboard layout model =
  let open Kbdlayout in
  let key_rects = layout
                  |> List.mapi (fun row cols ->
                      (List.fold_left (fun (col, rects) key ->
                           match key with
                           | E -> col + 1, rects
                           | H (c, _, _)
                           | S c ->
                              let fill = (match model.state with
                                  | Playing (i, _, _, _) when c = (Char.uppercase_ascii model.passage.[i]) -> "yellow"
                                  | _ -> "white"
                                ) in
                              let pos_x = col * 40 in
                              let pos_y = row * 40 in
                              let rect = svg_elt "rect" [] ~a:[int_attr "x" pos_x;
                                                               int_attr "y" pos_y;
                                                               int_attr "width" 40;
                                                               int_attr "height" 40;
                                                               attr  "fill" fill;
                                                              ]
                              in
                              let text' = svg_elt "text" [text (String.make 1 c)]
                                            ~a:[int_attr "x" (pos_x + 5);
                                                int_attr "y" (pos_y + 15);
                                               ]
                              in
                                (* the order of elements determine which shows on top *)
                                (col + 1), (text' :: rect :: rects)
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
    | Finished _ -> button "Reset" `Reset
    | Ready -> button "Play" `Start
  in
  let stats_info {state; _} =
    match state with
    | Playing (_, stats, _, _)
    | Finished stats -> elt "table" ~a:[attr "id" "stats"] [
        elt "tr" [
          elt "th" [];
          elt "th" ~a:[class_ "numerical"] [text "same hand"];
          elt "th" ~a:[class_ "numerical"] [text "same finger"];
          elt "th" ~a:[class_ "numerical"] [text "distance"];
        ];
        elt "tr" [
          elt "th" [text "Dvorak"];
          elt "td" ~a:[class_ "numerical"] [text (string_of_int stats.same_hand)];
          elt "td" ~a:[class_ "numerical"] [text (string_of_int stats.same_finger)];
          elt "td" ~a:[class_ "numerical"] [text (string_of_int stats.distance)];
        ];
      ]
    | _ -> elt "span" []
  in
  let scrolling_text {state; passage; _} =
    div ~a:[attr "id" "passage"] [
      match state with
      | Playing (i, _, _, _) -> text (String.sub passage i (String.length passage - i))
      | _ -> text passage
    ]
  in
    div [
      (keyboard Kbdlayout.sample_dvorak_data model);
      (play_button model);
      (scrolling_text model);
      (stats_info model);
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
