type hand = Left | Right
type finger = Pinky | Ring | Middle | Index | Thumb

type key =
  | S(*single*) of char
  | H(*home*) of char * hand * finger
  | E(*empty space*)

type node = {
  c: char option;
  x: int;
  y: int;
  mutable hand: hand;
  mutable finger: finger;
  mutable dist: int;
  mutable opened: bool;
  mutable queued: bool;
}

type data_t = key list list

type lookup_t = (char, (hand * finger * int)) Hashtbl.t


let string_of_hand = function
  | Left -> "left"
  | Right -> "right"

let string_of_finger = function
  | Pinky -> "pinky"
  | Ring -> "ring"
  | Middle -> "middle"
  | Index -> "index"
  | Thumb -> "thumb"

let sample_dvorak_data:data_t = [
  [ S('`');  S('1');  S('2'); S('3'); S('4'); S('5'); S('6'); S('7'); S('8'); S('9'); S('0'); S('['); S(']');  ];
  [ E;       S('\''); S(','); S('.'); S('P'); S('Y'); S('F'); S('G'); S('C'); S('R'); S('L'); S('/'); S('=');  ];
  [ E;       H('A', Left, Pinky);  H('O', Left, Ring); H('E', Left, Middle); H('U', Left, Index); S('I'); S('D'); H('H', Right, Index); H('T', Right, Middle); H('N', Right, Ring); H('S', Right, Pinky); S('-'); S('\\'); ];
  [ E;       S(';');  S('Q'); S('J'); S('K'); S('X'); S('B'); S('M'); S('W'); S('V'); S('Z'); E;      E;       ];
]
let sample_colemak_data:data_t = [
  [ S('`');  S('1');  S('2'); S('3'); S('4'); S('5'); S('6'); S('7'); S('8'); S('9'); S('0'); S('-'); S('='); E ];
  [ E;       S('Q'); S('W'); S('F'); S('P'); S('G'); S('J'); S('L'); S('U'); S('Y'); S(';'); S('['); S(']'); S('\\') ];
  [ E;       H('A', Left, Pinky);  H('R', Left, Ring); H('S', Left, Middle); H('T', Left, Index); S('D'); S('H'); H('N', Right, Index); H('E', Right, Middle); H('I', Right, Ring); H('O', Right, Pinky); S('\''); E; E ];
  [ E;       S('Z');  S('X'); S('C'); S('V'); S('B'); S('K'); S('M'); S(','); S('.'); S('/'); E; E; E];
]
let sample_qwerty_data:data_t = [
  [ S('`');  S('1');  S('2'); S('3'); S('4'); S('5'); S('6'); S('7'); S('8'); S('9'); S('0'); S('-'); S('=');  ];
  [ E;       S('Q'); S('W'); S('E'); S('R'); S('T'); S('Y'); S('U'); S('I'); S('O'); S('P'); S('['); S(']');  ];
  [ E;       H('A', Left, Pinky);  H('S', Left, Ring); H('D', Left, Middle); H('F', Left, Index); S('G'); S('H'); H('J', Right, Index); H('K', Right, Middle); H('L', Right, Ring); H('S', Right, Pinky); S(';'); S('\''); ];
  [ E;       S('Z');  S('X'); S('C'); S('V'); S('B'); S('N'); S('M'); S(','); S('.'); S('/'); E;      E;       ];
]

let lookup_of_data layout :lookup_t =
  (* TODO assert all rows are equal length *)
  let find_home_positions =
    let rec aux results y x = function
      | (h :: t) :: rows ->
         let results' = match h with
           | H _ -> (x, y) :: results
           | _ -> results
         in
           aux results' y (x + 1) (t :: rows)
      | [] :: rows -> aux results (y + 1) 0 rows
      | [] -> results
    in
      aux [] 0 0
  in
    print_endline "home positionns now.";
  let home_positions = find_home_positions layout in
    Printf.printf "home_positions: %s\n" (String.concat ", " (List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y) home_positions));
  let matrix = layout
               |> List.mapi (fun y cols -> List.mapi (fun x k ->
                   match k with
                   | S c -> {
                       x; y;
                       c = Some c;
                       hand = Left;
                       finger = Thumb;
                       dist = 0;
                       opened = false;
                       queued = false;
                     }
                   | H (c, hand, finger) -> {
                       x; y;
                       c = Some c;
                       hand;
                       finger;
                       dist = 0;
                       opened = false;
                       queued = false;
                     }
                   | E -> {
                       x; y;
                       c = None;
                       hand = Left;
                       finger = Thumb;
                       dist = 0;
                       opened = false;
                       queued = false;
                     }

                 ) cols)
               |> List.map (Array.of_list)
               |> Array.of_list
  in
  let width = Array.length matrix.(0) in
  let height = Array.length matrix in
  let find_neighbours x y =
    List.flatten [
      if x > 0          then [matrix.(y).(x - 1)] else [];
      if y > 0          then [matrix.(y - 1).(x)] else [];
      if x + 1 < width  then [matrix.(y).(x + 1)] else [];
      if y + 1 < height then [matrix.(y + 1).(x)] else [];
    ]
  in
  let q = Queue.create () in
  let map = Hashtbl.create (width * height) in
    List.iter (fun (x, y as it) ->
        matrix.(y).(x).queued <- true;
        Queue.push it q;
      ) home_positions;
    while not (Queue.is_empty q) do
      let node_x, node_y = Queue.pop q in
      let node = matrix.(node_y).(node_x) in
      let my_dist = node.dist + 1 in
        (match node.c with
         | Some c ->
            Printf.printf "%c with %s %s at %d\n" c
              (string_of_hand node.hand)
              (string_of_finger node.finger)
              node.dist;
            Hashtbl.add map c (node.hand, node.finger, node.dist)
         | None -> ()
        );
        find_neighbours node_x node_y
        |> List.iter (function
            | {opened = true; _} -> ()
            | {queued = false; x; y; _} as n ->
               n.hand <- node.hand;
               n.finger <- node.finger;
               n.dist <- my_dist;
               n.queued <- true;
               Queue.push (x, y) q;
            | {queued = true; dist; _} as n when dist >  my_dist ->
               n.hand <- node.hand;
               n.finger <- node.finger;
               n.dist <- my_dist;
            | _ -> ()
          );
    done;
    map

let view ?(highlit_key=None) ?(in_edit=false) ~onremove label layout =
  let open Vdom in
  let l = 40 in
  let key_buttons = layout
                    |> List.mapi (fun row cols ->
                        (List.fold_left (fun (col, rects) key ->
                             match key with
                             | E -> col + 1, rects
                             | H (c, _, _)
                             | S c ->
                                let fill = (match highlit_key with
                                      Some c' when c = c' -> "#FFA0A0"
                                    | _ -> "white"
                                  ) in
                                let pos_x = col * l in
                                let pos_y = row * l in
                                let rect = svg_elt "rect" [] ~a:[int_attr "x" pos_x;
                                                                 int_attr "y" pos_y;
                                                                 int_attr "width" l;
                                                                 int_attr "height" l;
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
  let max_row_len = List.fold_left (fun acc l -> max acc (List.length l)) 0 layout
  in
    elt "keyboard" ~a:[class_ (if in_edit then "in-edit" else "")] [
      div ~a:[class_ "title"] [text label];

      svg_elt "svg" ~a:[int_attr "width" (max_row_len * l); int_attr "height" (4 * l)]
        key_buttons;

      div ~a:[class_ "content curtain"] [
        (elt "a" ~a:[onclick onremove; class_ "interactive"] [text "remove"])
      ]
    ]
