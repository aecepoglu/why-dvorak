open Lwt.Infix

type hand = Left | Right | Any
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
    | "thumb" -> Ok Thumb
    | "index" -> Ok Index
    | "middle" -> Ok Middle
    | "ring" -> Ok Ring
    | "pinky" -> Ok Pinky
    | x -> Error ("unknown finger: \"" ^ x ^ "\"")

  let hand_of_string = function
    | "left" -> Ok Left
    | "right" -> Ok Right
    | "any" -> Ok Any
    | x -> Error ("Bad hand " ^ x)

  (* TODO parse JSON object instead *)
  let parse name json :t =
    let tbl = Hashtbl.create 64 in
    let open Js_of_ocaml in
    let obj = Json.unsafe_input json in
    let keys = Js.object_keys obj in
      keys##forEach
        (Js.wrap_callback (fun js_key _ _ ->
             let s = Js.to_string js_key in
             let key = s.[0] in
             let it = Js.Unsafe.get obj js_key in
             let finger = Js.Unsafe.get it "finger" |> Js.to_string |> finger_of_string in
             let hand = Js.Unsafe.get it "hand" |> Js.to_string |> hand_of_string in
             let dist = Js.Unsafe.get it "distance" in
               (match (finger, hand) with
                | Ok(finger'), Ok(hand') -> Hashtbl.add tbl key (hand', finger', dist)
                | Error(e), _ | _, Error(e) -> print_endline e
               );
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
  | Ok a -> Ok (f a)
  | Error _ as b -> b

let join_promises xs =
  let rec aux acc = function
    | h :: t ->
       Lwt.bind h (fun h' -> aux (h' :: acc) t)
    | [] -> Lwt.return (List.rev acc)
  in
    aux [] xs

let join_results xs =
  let rec aux acc = function
    | (Ok x) :: t -> aux (x :: acc) t
    | (Error _ as e) :: _ -> e
    | [] -> Ok acc
  in
    aux [] xs

let (>|?=) a f = Lwt.map (map_result f) a

let bind_promise_result f = function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

let () =
  print_endline "OCaml saying hi!";
  let text_urls = ["texts/one.txt";
               "texts/two.txt";
              ] in
  let layout_urls = ["dvorak", "layouts/dvorak.json";
                 "qwerty", "layouts/qwerty.json";
                 "colemak", "layouts/colemak.json";
                ] in
  List.map http_get text_urls
  |> join_promises
  >|= join_results
  >>= bind_promise_result (fun texts ->
      layout_urls
      |> List.map (fun (name, url) ->
          http_get url
          >|?= Js_of_ocaml.Js.string
          >|?= Layout.parse name
        )
      |> join_promises
      >|= join_results
      >|?= (fun layouts -> texts, layouts)
    )
  >|= (function
      | Ok (texts, _layouts) ->
         List.iteri (fun num text -> 
             print_string text;
             let elem = Js_of_ocaml_tyxml.Tyxml_js.Html.div [
                 (Js_of_ocaml_tyxml.Tyxml_js.Html.txt (string_of_int num));
                 (Js_of_ocaml_tyxml.Tyxml_js.Html.txt text)
               ] in
               Js_of_ocaml.Dom.appendChild
                 (Js_of_ocaml.Dom_html.getElementById "texts")
                 (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element elem)
           ) texts
      | Error e -> print_endline e
    )
  |> Lwt.ignore_result
