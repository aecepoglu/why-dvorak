open Lwt.Infix

type hand = Left | Right
type finger = Thumb | Index | Middle | Ring | Pinky

type 'a stats = {
  same_hand   : 'a;
  same_finger : 'a;
  distance    : 'a;
}

module Layout = struct
  type t = {
    name : string;
    keys : (char, (hand * finger * int)) Hashtbl.t;
  }

  let parse_layout name lookup =
    let open Js_of_ocaml in
    let map = Hashtbl.create 64 in
      List.iter (fun (c, x) -> Hashtbl.add map c x) lookup;
      {name = (Js.to_string name); keys=map}

  let of_string_and_name obj =
    let open Js_of_ocaml.Js in
    let tbl = Hashtbl.create 64 in
    let keys = object_keys obj in
      keys##forEach
        (wrap_callback (fun js_key _ _ ->
             let key = "" in
               (* Unsafe.get obj js_key*)
               Hashtbl.add tbl 'a' (Left, Thumb, 0)
           )
        )
      ;
      {name="placeholder"; keys=tbl}


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
