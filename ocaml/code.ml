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
  let parse name obj :t =
    let open Js_of_ocaml in
    let tbl = Hashtbl.create 64 in
    let keys = Js.object_keys obj in
      keys##forEach
        (Js.wrap_callback (fun js_key _ _ ->
             let key = (Js.to_string js_key).[0] (* TODO unsafe access *) in
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

class layouts_list = object
  val mutable layouts : Layout.t list = []

  method add x =
    layouts <- (x :: layouts)
end

let layouts = new layouts_list

let add_layout = layouts#add

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
       let msg = r.XmlHttpRequest.content in
         if code == 0 || code == 200 then Lwt.return_ok msg
         else Lwt.return_error msg
    )

let () =
  print_endline "OCaml saying hi!";
  let layouts = ["dvorak", "layouts/drovak.json";
                 "qwerty", "layouts/qwerty.json";
                 "colemak", "layouts/colemak.json";
                ]
                |> List.map (fun (a, b) -> a, load_json b)
                |> List.map (fun (a, b) -> Layout.parse a b) in
  let texts = ["texts/one.txt";
               "texts/two.txt";
              ] in
  http_get "texts/one.txt"
  >|= (function
      | Ok x -> print_endline x;
      | Error x -> Printf.printf "Error %s" x
    )
  |> Lwt.ignore_result
