open Lwt.Infix

type hand = Left | Right
type finger = Thumb | Index | Middle | Ring | Pinky

type layout = {
  name : string;
  keys : (char, (hand * finger * int)) Hashtbl.t;
}

type 'a stats = {
  same_hand   : 'a;
  same_finger : 'a;
  distance    : 'a;
}

let analyze_text (l:layout) (text:string) :(float stats) =
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
             distance = s.distance + 1;
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
  http_get "texts/one.txt"
  >|= (function
      | Ok x -> print_endline x;
      | Error x -> Printf.printf "Error %s" x
    )
  |> Lwt.ignore_result
