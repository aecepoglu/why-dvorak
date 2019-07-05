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

let find_best_in_list cmp l =
  match l with
  | h :: t -> List.fold_left cmp h t
  | [] -> failwith "list can't be empty"


(************************)

open Vdom

module Stats = struct
  type 'a t = {
    same_hand: 'a;
    same_finger: 'a;
    distance: 'a;
  }

  type key = Same_hand
           | Same_finger
           | Distance

  let get_same_hand x = x.same_hand
  let get_same_finger x = x.same_finger
  let get_distance x = x.distance

  let get k x = match k with
    | Same_hand -> x.same_hand
    | Same_finger -> x.same_finger
    | Distance -> x.distance

  let map f x = {
    same_hand = f x.same_hand;
    same_finger = f x.same_finger;
    distance = f x.distance;
  }

  let map2 f x y = {
    same_hand = f x.same_hand y.same_hand;
    same_finger = f x.same_finger y.same_finger;
    distance = f x.same_finger y.same_finger;
  }

  let init x = {
    same_hand = x;
    same_finger = x;
    distance = x;
  }

  let list x = [
    Same_hand, x.same_hand;
    Same_finger, x.same_finger;
    Distance, x.distance;
  ]
end

type typing_analysis = {
  name: string;
  layout_data: Kbdlayout.data_t;
  keyboard: Kbdlayout.lookup_t;
  stats: int Stats.t;
  last_hand: Kbdlayout.hand;
  last_finger: Kbdlayout.finger;
}

type text_player_state = Ready
                       | Playing of int
                       | Finished

type model = {
  state: text_player_state;
  analyses: typing_analysis list;
  passage: string;
}



let update_analysis letter analysis =
  match Hashtbl.find_opt analysis.keyboard letter with
  | Some (hand, finger, dist) ->
     let {stats; last_hand; last_finger; _} = analysis in
       { analysis with
         stats = {
           same_hand = stats.same_hand + (if hand = last_hand then 1 else 0);
           same_finger = stats.same_finger + (if finger = last_finger then 1 else 0);
           distance = stats.distance + dist;
         };
         last_hand = hand;
         last_finger = finger;
       }
  | None -> analysis

let update model = function
  | `Start -> {model with
               state = Playing 0;
               analyses = List.map
                            (fun analysis -> { analysis with
                                               stats = {
                                                 same_hand = 0;
                                                 same_finger = 0;
                                                 distance = 0;
                                               };
                                               last_hand = Kbdlayout.Left;
                                               last_finger = Kbdlayout.Thumb;
                                             })
                             model.analyses;
              }
  | `NextChar -> (match model.state with
      | Playing i when (i + 1) < String.length model.passage ->
         let c = Char.uppercase_ascii model.passage.[i] in
           { model with
             state = Playing (i + 1);
             analyses = List.map (update_analysis c) model.analyses;
           }
      | Playing _ -> { model with state = Finished }
      | s -> { model with state = s }
    )
  | `Reset -> {model with state = (match model.state with
      | Playing _ -> Ready
      | Finished -> Ready
      | s -> s
    )}

let init = {
  state = Ready;
  passage = sample_text;
  analyses = [
    {
      name = "Dvorak";
      layout_data = Kbdlayout.sample_dvorak_data;
      keyboard = Kbdlayout.lookup_of_data Kbdlayout.sample_dvorak_data;
      stats = Stats.init 0;
      last_hand = Kbdlayout.Left;
      last_finger = Kbdlayout.Thumb;
    };
    {
      name = "Qwerty";
      layout_data = Kbdlayout.sample_qwerty_data;
      keyboard = Kbdlayout.lookup_of_data Kbdlayout.sample_qwerty_data;
      stats = Stats.init 0;
      last_hand = Kbdlayout.Left;
      last_finger = Kbdlayout.Thumb;
    }
  ];
}

let button txt msg = input [] ~a:[onclick (fun _ -> msg); type_button; value txt]

let find_best_stats l =
  l
  |> List.fold_left
       (fun acc it -> (Stats.map2 (fun a b -> (it.name, b) :: a) acc it.stats))
       (Stats.init [])
  |> (fun x ->
       let cmp (_, a_val as a) (_, b_val as b) = if a_val < b_val
                                                           then a
                                                           else b
       in
         Stats.map (fun a -> a
                             |> find_best_in_list cmp
                             |> fst
                   ) x
     )
  

let view model =
  let play_button state = match state with
    | Playing _ -> button "◼ stop" `Reset
    | Finished -> button "↶ reset" `Reset
    | Ready -> button "▶ play" `Start
  in
  let scrolling_text state passage =
    div ~a:[attr "id" "passage"] [
      let i = (match state with
          | Playing i -> i
          | _ -> 0
        ) in
        text (String.sub passage i (String.length passage - i))
    ]
  in
  let view_stats () =
    let best_stats = find_best_stats model.analyses in
    elt "table" ~a:[attr "id" "stats"] (
      elt "tr" [
        elt "th" [];
        elt "th" ~a:[class_ "numerical"] [text "same hand"];
        elt "th" ~a:[class_ "numerical"] [text "same finger"];
        elt "th" ~a:[class_ "numerical"] [text "distance"];
      ]
      :: List.map (fun {name; stats; _} -> 
          elt "tr" (
            (elt "th" [text name])
            :: (stats
                |> Stats.list
                |> List.map (fun (k, v) ->
                              elt "td" ~a:[class_ ("numerical" ^ if name = (Stats.get k best_stats)
                                                                 then " best-stat"
                                                                 else ""
                                          )] [text (string_of_int v)]
                            )
               )
          )
        ) model.analyses
    ) in
  let view_analysis passage state analysis =
    let highlit_key = (match state with
        | Playing i -> Some (Char.uppercase_ascii passage.[i])
        | _ -> None
      ) in
      Kbdlayout.view ~highlit_key analysis.name analysis.layout_data 
  in
    div [
      div (List.map (view_analysis model.passage model.state) model.analyses);
      div [
        (play_button model.state);
        (scrolling_text model.state model.passage);
      ];
      (match model.state with
       | Playing _ | Finished -> view_stats ()
       | _ -> elt "span" []
      );
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
