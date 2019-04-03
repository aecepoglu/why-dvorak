open Lwt.Infix

type hand = Left | Right
type finger = Thumb | Index | Middle | Ring | Pinky

type 'a stats = {
  same_hand   : 'a;
  same_finger : 'a;
  distance    : 'a;
}

module Layout = struct
  exception BadFinger of string
  exception BadHand of string

  type t = {
    name : string;
    keys : (char, (hand * finger * int)) Hashtbl.t;
  }

  type serialized_key = {
    hand: string;
    finger: string;
    int: string
  }

  let finger_of_string = function
    | "thumb" -> Thumb
    | "index" -> Index
    | "middle" -> Middle
    | "ring" -> Ring
    | "pinky" -> Pinky
    | x -> raise (BadFinger x)

  let hand_of_string = function
    | "left" -> Left
    | "right" -> Right
    | x -> raise (BadHand x)

  (* TODO parse JSON object instead *)
  let parse name json :t =
    let open Js_of_ocaml in
    let obj = Json.unsafe_input json in
    let tbl = Hashtbl.create 64 in
    let keys = Js.object_keys obj in
      keys##forEach
        (Js.wrap_callback (fun js_key _ _ ->
             let key = (Js.to_string js_key).[0] in
             let it = Js.Unsafe.get obj js_key in
             let finger = Js.Unsafe.get it "finger" |> Js.to_string |> finger_of_string in
             let hand = Js.Unsafe.get it "hand" |> Js.to_string |> hand_of_string in
             let dist = Js.Unsafe.get it "distance" in
               Hashtbl.add tbl key (hand, finger, dist);
           )
        );
      { name;
        keys = tbl;
      }
end

let analyze_text (l:Layout.t) (text:string) :(float stats) =
  let len = String.length text in
  let rec aux s prev_hand prev_finger i =
    if i < len
    then
      let c = text.[i] in
        match Hashtbl.find_opt l.keys c with
        | Some(hand, finger, dist) ->
           let s' = {
             same_hand = (if hand = prev_hand
                          then s.same_hand + 1
                          else s.same_hand);
             same_finger = (if (hand, finger) = (prev_hand, prev_finger)
                            then s.same_finger + 1
                            else s.same_finger);
             distance = s.distance + dist;
           } in
             aux s' hand finger (i + 1)
        | None ->
           aux s prev_hand prev_finger (i + 1)
    else s
  in
    aux {same_hand=0; same_finger=0; distance=0} Left Thumb 0
    |> (fun x -> 
        let len_f = float_of_int len in {
          same_hand = (float_of_int x.same_hand) /. len_f;
          same_finger = (float_of_int x.same_finger) /. len_f;
          distance = (float_of_int x.distance) /. len_f;
        }
      )

let http_get url =
  let open Js_of_ocaml_lwt in
  Lwt.bind
    (XmlHttpRequest.get url)
    (fun r ->
       let code = r.XmlHttpRequest.code in
       let body = r.XmlHttpRequest.content in
         if code == 0 || code == 200 then Lwt.return_ok body
         else Lwt.return_error body
    )

let map_result f = function
  | Ok x -> Ok (f x)
  | Error _ as e -> e

let collect_results results =
  List.fold_left (fun acc res ->
      match (acc, res) with
      | Ok xs,       Ok x    -> Ok (x :: xs)
      | Error ys,    Error y -> Error (y :: ys)
      | Ok _,        Error y -> Error [y]
      | Error _ as e, _      -> e
    ) (Ok []) results

let () =
  print_endline "OCaml saying hi!";
  let texts = ["texts/one.txt"] in
  let layouts = ["dvorak", "layouts/drovak.json";
                 "qwerty", "layouts/qwerty.json";
                 "colemak", "layouts/colemak.json";
                ] in
    layouts
    |> List.map (fun (name, url) ->
        http_get url
        >|= map_result
              (fun body -> Layout.parse name (Js_of_ocaml.Js.string body))
      )
    |> Lwt.nchoose
    >|= collect_results
    >>= (fun result1 -> 
        List.map http_get texts
                  |> Lwt.nchoose
                  >|= collect_results
                  >|= (fun result2 -> (match (result1, result2) with
                                       | Ok a, Ok b -> Ok (a, b)
                                       | Error e, _ | _, Error e -> Error e
                                      )
                      )
      )
    >|= map_result (fun (_layouts, _texts) -> ())
    |> Lwt.ignore_result
