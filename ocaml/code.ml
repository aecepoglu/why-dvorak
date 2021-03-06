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

  let get x = function
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
    distance = f x.distance y.distance;
  }

  let init x = {
    same_hand = x;
    same_finger = x;
    distance = x;
  }

  let string_of_key = function
    | Same_hand -> "Same Hand"
    | Same_finger -> "Same Finger"
    | Distance -> "Distance"
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
                       | Editing

type model = {
  state: text_player_state;
  analyses: typing_analysis list;
  passage: string;
}

type update_msg = Start
                | End
                | NextChar
                | Reset
                | ToggleEdit
                | ChangeText of string
                | RemoveKeyboard of typing_analysis
                | AddKeyboard of string * Kbdlayout.data_t
                | DoNothing

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

let finish_analyses model =
  let rec aux analyses = function
    | Playing i when (i + 1) < String.length model.passage ->
      let c = Char.uppercase_ascii model.passage.[i] in
        aux (List.map (update_analysis c) analyses) (Playing (i + 1))
    | Playing _
    | Finished
    | Editing -> analyses
    | Ready -> aux analyses (Playing 0)
  in
    {model with state = Finished;
                analyses = aux model.analyses model.state}

let group_typing_analyses (analyses:typing_analysis list) =
  List.fold_left
    (fun acc it -> (Stats.map2 (fun a b -> (it.name, b) :: a) acc it.stats))
    (Stats.init [])
    analyses
  |> Stats.map List.rev

let find_best_stats l =
  let cmp (_, a_val as a) (_, b_val as b) =
    if a_val < b_val
    then a
    else b
  in
    Stats.map (fun a -> a
                        |> find_best_in_list cmp
                        |> fst
              )
      (group_typing_analyses l)

let init = {
  state = Ready;
  passage = Sampletexts.sentence;
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

let update model = function
  | Start -> {model with
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
  | End -> finish_analyses model
  | NextChar -> (match model.state with
      | Playing i when (i + 1) < String.length model.passage ->
         let c = Char.uppercase_ascii model.passage.[i] in
           { model with
             state = Playing (i + 1);
             analyses = List.map (update_analysis c) model.analyses;
           }
      | Playing _ -> { model with state = Finished }
      | s -> { model with state = s }
    )
  | Reset -> {model with state = (match model.state with
      | Playing _ -> Ready
      | Finished -> Ready
      | s -> s
    )}
  | ToggleEdit -> {model with state = (match model.state with
      | Playing _ as s -> s
      | Editing -> Ready
      | Ready | Finished -> Editing
    )}
  | ChangeText passage -> {model with passage}
  | RemoveKeyboard analysis -> {model with analyses = List.filter ((<>) analysis) model.analyses}
  | AddKeyboard (name, layout_data) -> {model with
                                        analyses = ({name;
                                                     layout_data;
                                                     keyboard = Kbdlayout.lookup_of_data layout_data;
                                                     stats = Stats.init 0;
                                                     last_hand = Kbdlayout.Left;
                                                     last_finger = Kbdlayout.Thumb;
                                                    } :: model.analyses)}
  | DoNothing -> model


let button txt msg = input [] ~a:[onclick (fun _ -> msg);
                                  type_button;
                                  class_ "interactive";
                                  value txt
                                 ]

let stats_chart (analyses:typing_analysis list) =
  let stats_by_names = group_typing_analyses analyses in
  let factors = Stats.map (List.map snd) stats_by_names
                |> Stats.map (find_best_in_list max)
                |> Stats.map (fun x -> 200.0 /. (float_of_int x)) in
    svg_elt "svg" ~a:[int_attr "height" (30 + 85 * List.length analyses)] (
      List.fold_left (fun (y0, acc) k ->
          let factor = Stats.get factors k in
          let stats_by_names' = Stats.get stats_by_names k in
          let legend = svg_elt "text" ~a:[int_attr "y" (y0 + 15)] [text (Stats.string_of_key k)] in
            List.fold_left (fun (y0, acc') (name, v) ->
                let w = int_of_float (factor *. (float_of_int v)) in
                let r = svg_elt "rect" ~a:[int_attr "x" 0;
                                           int_attr "y" y0;
                                           int_attr "width" w;
                                           int_attr "height" 20;
                                          ] [] in
                let t = svg_elt "text" ~a:[int_attr "x" (w + 25);
                                           int_attr "y" (y0 + 14);
                                          ] [text name] in
                  ((y0 + 22), r :: t :: acc')
              )
              (y0 + 20, legend :: acc)
              stats_by_names'
        )
        (5, [])
        [Stats.Same_hand; Stats.Same_finger; Stats.Distance]
      |> snd
    )

let view model =
  let play_button state = div ~a:[style "display" "inline-block"] (match state with
      | Playing _ -> [ button "◼ stop" Reset; button "▶▶ end" End  ]
      | Finished  -> [ button "◀◀ reset" Reset ]
      | Ready     -> [ button "▶ play" Start ]
      | Editing   -> [ input [] ~a:[type_button; disabled true; value "▷ play"] ]
    ) in
  let edit_button state =
    input [] ~a:[type_button;
                 onclick (fun _ -> ToggleEdit);
                 class_ "interactive";
                 disabled (match state with
                     | Playing _ -> true
                     | _         -> false
                   );
                 value (match state with
                     | Editing -> "✓ save"
                     | _           -> "✎ edit"
                   );
                ]; in
  let scrolling_text state passage =
    let i = (match state with
        | Playing i -> i
        | _ -> 0
      ) in
      div [
        div ~a:[attr "id" "passage"]
          (match state with
           | Editing -> [ elt "select" ~a:[
               oninput (function
                   | "one sentence"    -> ChangeText Sampletexts.sentence
                   | "sherlock holmes" -> ChangeText Sampletexts.sherlock
                   | _ -> DoNothing
                 );
             ] [
               elt "option" ~a:[attr "selected" ""; disabled true] [text "Sample Texts"];
               elt "option" [text "one sentence"];
               elt "option" [text "sherlock holmes"];
             ];
               elt "br" [];
               elt "textarea" ~a:[int_attr "rows" 10;
                                  int_attr "cols" 80;
                                  oninput (fun s -> ChangeText s)
                                 ]
                 [text passage]
             ]
           | _ -> [ text (String.sub passage i (String.length passage - i)) ]
          )
      ]
  in
  let view_stats state analyses =
    let best_stats = find_best_stats analyses in
    let classes = match state with
      | Finished
      | Playing _ -> ""
      | _         -> "hidden"
    in
      div ~a:[class_ classes] [
        elt "table" ~a:[attr "id" "stats"] (
          elt "tr" [
            elt "th" [];
            elt "th" ~a:[class_ "numerical"] [text "same hand"];
            elt "th" ~a:[class_ "numerical"] [text "same finger"];
            elt "th" ~a:[class_ "numerical"] [text "distance"];
          ]
          :: List.map (fun {name; stats; _} -> 
              let stat_cell k = 
                elt "td" ~a:[class_ ("numerical"
                                     ^ (if name = Stats.get best_stats k
                                        then " best-stat"
                                        else "")
                                    )]
                  [text (string_of_int (Stats.get stats k))];
              in
                elt "tr" [
                  (elt "th" [text name]);
                  (stat_cell Stats.Same_hand);
                  (stat_cell Stats.Same_finger);
                  (stat_cell Stats.Distance);
                ]
            ) analyses
        );
        elt "br" [];
        stats_chart analyses
      ] in
  let view_analysis passage state analysis =
    let highlit_key = (match state with
        | Playing i -> Some (Char.uppercase_ascii passage.[i])
        | _ -> None
      ) in
    let in_edit = (match state with
        | Editing when (List.length model.analyses > 1) -> true
        | _ -> false
      ) in
      Kbdlayout.view ~highlit_key ~in_edit ~onremove:(fun _ -> RemoveKeyboard analysis) analysis.name analysis.layout_data; in
  let create_keyboard state =
    let in_edit = (match state with
        | Editing -> true
        | _ -> false
      ) in
    let layout_exists name = List.exists (fun a -> a.name = name) model.analyses in
      elt "keyboard" ~a:[class_ (if in_edit then "in-edit" else "hidden")] [
        div ~a:[class_ "title"] [text "Add New"];
        div ~a:[attr "id" "create-keyboard-button"; class_ "content"] [
          elt "select" ~a:[oninput (function
              | "Colemak" -> AddKeyboard ("Colemak", Kbdlayout.sample_colemak_data)
              | "Dvorak"  -> AddKeyboard ("Dvorak", Kbdlayout.sample_dvorak_data)
              | "Qwerty"  -> AddKeyboard ("Qwerty", Kbdlayout.sample_qwerty_data)
              | _ -> DoNothing
            ); class_ "interactive"] (
            elt "option" ~a:[attr "selected" ""; disabled true] [text "select"]
            :: (List.map
                  (fun name -> (elt "option"
                                  ~a:[disabled (layout_exists name)]
                                  [text name])
                  )
                  ["Colemak"; "Dvorak"; "Qwerty"]
               )
          )
        ]
      ]
  in
    div ~a:[style "text-align" "center"] [
      div ~a:[attr "id" "keyboards"] (create_keyboard model.state :: (List.map (view_analysis model.passage model.state) model.analyses));
      div [
        (play_button model.state);
        (edit_button model.state);
      ];
      (scrolling_text model.state model.passage);
      (view_stats model.state model.analyses);
    ]

let app = simple_app ~init ~view ~update ()

let () =
  let open Js_browser in
  let run () = Vdom_blit.run app
               |> (fun app' ->
                   let _ = Window.set_interval window
                             (fun () -> Vdom_blit.process app' NextChar)
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
