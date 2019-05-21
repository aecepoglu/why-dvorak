open Lwt.Infix

type hand = Left | Right | Any
type finger = Thumb | Index | Middle | Ring | Pinky

type 'a stats = {
  same_hand   : 'a;
  same_finger : 'a;
  distance    : 'a;
}

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

let bind_promise_result f = function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

let array_find pred xs =
  let rec aux i =
    if i >= Array.length xs
    then None
    else
      let x = xs.(i) in
        if pred x
        then Some x
        else aux (i + 1)
  in
    aux 0

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

let (>|?=) a f = Lwt.map (map_result f) a


module Text = struct
  type t = {
    url: string;
    content: string;
  }

  let load url =
    http_get url
    >|?= (fun content -> {url; content})
end


module Layout = struct
  exception BadFinger of string
  exception BadHand of string

  type t = {
    name : string;
    keys : (char, (hand * finger * int)) Hashtbl.t;
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

module Highcharts = struct
  open Js_of_ocaml
  open Js_of_ocaml.Js.Unsafe

  let create (dom_id:string) props =
    fun_call
      (global##.Highcharts##.chart)
      [| inject (Js.string dom_id); props |]

  let set_x_categories chart (categories:string array) =
    Js.array_get
      (chart##.xAxis)
      0
    |> Js.Optdef.to_option
    |> (function
        | Some x -> fun_call
                      (x##.update)
                      [| obj
                           [|"categories", (inject (Js.array categories))|]
                      |]
        | None -> ()
      )

  let set_data chart layout_name stats =
    Js.to_array (chart##.series)
    |> array_find (fun x -> x##.name = layout_name)
    |> (function
        | Some x -> fun_call (x##.setData) stats.same_finger
        | None -> ()
      )
end


let () =
  print_endline "OCaml saying hi!!";
  let text_urls = ["texts/one.txt";
                   "texts/two.txt";
                  ] in
  let layout_urls = ["dvorak", "layouts/dvorak.json";
                     "qwerty", "layouts/qwerty.json";
                     "colemak", "layouts/colemak.json";
                    ] in
  List.map Text.load text_urls
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
      | Ok (texts, layouts) ->
         let open Js_of_ocaml.Js.Unsafe in
         let (
           same_finger_chart,
           same_hand_chart,
           distance_chart
         ) = [
           "fingerChart", "Same Finger Count %";
           "handChart", "Same Hand Count %";
           "distanceChart", "Distance Travelled"
         ] |> List.map (fun (dom_id, title) ->
             Highcharts.create
               dom_id
               (obj [|
                   "chart", obj [|
                       "type", inject "column"
                     |];
                   "title", obj [|
                       "text", inject ""
                     |];
                   "subtitle", obj[|
                       "text", inject ""
                     |];
                   "xAxis", obj [|
                       "categories", inject [];
                       "crosshair", inject true;
                     |];
                   "yAxis", obj [|
                       "min", inject 0;
                       "title", obj [|
                           "text", inject title;
                         |]
                     |];
                   "tooltip", obj [||];
                   "plotOptions", obj [|
                       "column", obj [|
                           "pointPadding", inject 0.2;
                           "borderWidth", inject 0;
                         |];
                     |];
                   "series", layouts
                             |> List.map (fun (l:Layout.t) -> obj [|
                                 "name", inject l.name;
                                 "data", inject [| 0 |];
                               |])
                             |> Array.of_list
                             |> inject;
                 |])
             )
           |> (function
               | [a; b; c] -> a, b, c
               | _ ->
                  let x = Highcharts.create "abc" (obj [||]) in
                    (x, x, x)
             )
         in
         let _ = (
           let text_names = (Array.of_list (List.map (fun (x:Text.t) -> x.url) texts)) in
             List.iter
               (fun c -> Highcharts.set_x_categories c text_names)
               [ same_finger_chart;
                 same_hand_chart;
                 distance_chart;
               ]
         ) in
         List.iteri (fun num (text:Text.t) -> 
             print_string text.url;
             let elem = Js_of_ocaml_tyxml.Tyxml_js.Html.div [
                 (Js_of_ocaml_tyxml.Tyxml_js.Html.txt (string_of_int num));
                 (Js_of_ocaml_tyxml.Tyxml_js.Html.txt text.url)
               ] in
               Js_of_ocaml.Dom.appendChild
                 (Js_of_ocaml.Dom_html.getElementById "texts")
                 (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_element elem)
           ) texts;
         let _results = List.map
                          (fun (layout:Layout.t) ->
                             List.fold_right (fun (text:Text.t) acc ->
                                 let stats = analyze_text layout text.url in
                                   {
                                     same_hand = stats.same_hand :: acc.distance;
                                     same_finger = stats.same_finger :: acc.distance;
                                     distance = stats.distance :: acc.distance;
                                   }
                               )
                               texts
                               {same_hand = []; same_finger = []; distance = []}
                          )
                          layouts
         in
           (*
           let open Js_of_ocaml.Dom_html in
           let js_string_of_float x = 
             Js_of_ocaml.Js.string (string_of_float x)
           in
             (getElementById "distanceChart")##.innerHTML :=
               js_string_of_float distance;
             (getElementById "fingerChart")##.innerHTML :=
               js_string_of_float same_finger;
             (getElementById "handChart")##.innerHTML :=
               js_string_of_float same_hand;
           *)
           ()
      | Error e -> print_endline e
    )
  |> Lwt.ignore_result
