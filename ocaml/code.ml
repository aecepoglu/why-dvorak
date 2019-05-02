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
    print_endline ("in parse method for " ^ name);
    let tbl = Hashtbl.create 64 in
    let open Js_of_ocaml in
    let obj = Json.unsafe_input json in
    let keys = Js.object_keys obj in
      keys##forEach
        (Js.wrap_callback (fun js_key _ _ ->
             let s = Js.to_string js_key in
             Printf.printf "key \"%s\" '%c'\n" s s.[0];
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

let collect_results results =
  let resp = List.fold_left (fun acc res ->
      match (acc, res) with
      | Ok xs, Ok x -> Ok (x :: xs)
      | (Error _ as e), _ | _, (Error _ as e) -> e
    ) (Ok []) results
  in
    (match resp with
     | Ok a -> Printf.printf "%d -> %d\n" (List.length results) (List.length a)
     | Error e -> print_endline e
    );
    resp

let () =
  print_endline "OCaml saying hi!";
  let texts = ["texts/one.txt";
               "texts/two.txt";
              ] in
  let layouts = ["dvorak", "layouts/drovak.json";
                 "qwerty", "layouts/qwerty.json";
                 "colemak", "layouts/colemak.json";
                ] in
    layouts
    |> List.map (fun (name, url) ->
        http_get url
        >|= (function
            | Ok body ->
               let str = Js_of_ocaml.Js.string body in
               Printf.printf "parsing %s: %s\n" name body;
               let parsed = Layout.parse name str in
               Printf.printf "done.\n";
               Some parsed
            | _ -> None
          )
      )
    |> Lwt.nchoose                                 (*                    *)
    >|= List.filter (function                      (*                    *)
        | None -> false                            (*                    *)
        | _ -> true                                (* use                *)
      )                                            (* 'collect_results'  *)
    >|= List.map (function                         (* here               *)
        | Some x -> x                              (*                    *)
        | None -> raise (Layout.BadFinger "TODO")  (*                    *)
      )                                            (*                    *)
    >>= (fun layouts -> 
        Printf.printf "len(texts)0: %d\n" (List.length texts);
        List.map http_get texts
        |> Lwt.nchoose
        >|= collect_results
        >|= (function
            | Ok texts' -> 
               Printf.printf "len(texts)1: %d\n" (List.length texts');
               Ok (layouts, texts')
            | Error x -> Error x
          )
      )
    >|= (function
        | Ok (_layouts, texts) ->
           (match (collect_results [Ok("ahmet"); Ok("emre"); Ok("cepoglu")]) with
            | Ok(results) -> Printf.printf "num results %d\n" (List.length results)
            | Error(e) -> print_endline e
           );
           Printf.printf "num texts: %d\n" (List.length texts);
           List.iteri (fun num text -> 
               print_string text;
               let elem = Tyxml_js.Html.div [
                   (Tyxml_js.Html.txt (string_of_int num));
                   (Tyxml_js.Html.txt text)
                 ] in
                 Js_of_ocaml.Dom.appendChild
                   (Js_of_ocaml.Dom_html.getElementById "texts")
                   (Tyxml_js.To_dom.of_element elem)
             ) texts;
           Some ()
        | Error _ -> None
      )
    |> Lwt.ignore_result
